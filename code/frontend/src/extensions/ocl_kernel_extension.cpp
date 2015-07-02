/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details
 * regarding third party software licenses.
 */

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#pragma GCC diagnostic ignored "-Wuninitialized"
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
	#include "clang/AST/StmtVisitor.h"
	#include <clang/AST/Expr.h>

	#include <clang/Basic/FileManager.h>
#pragma GCC diagnostic pop

#include "insieme/frontend/clang.h"

#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/extensions/insieme_pragma_extension.h"
#include "insieme/frontend/extensions/ocl_kernel_extension.h"
#include "insieme/frontend/extensions/frontend_cleanup_extension.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/utils/error_report.h"

namespace fe = insieme::frontend;

using namespace insieme::frontend;
using namespace clang;

namespace insieme {
namespace frontend {
namespace extensions {

namespace {
//////////////////////////////////////////////////////////////////
///
// Register call expression handlers to be used during the clang to IR conversion
//void Converter::registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler) {
//	auto it = callExprHanlders.insert( std::make_pair(funcDecl, handler) );
//	assert_false(it.second) << "Handler for function declaration already registered.";
//}
//  Function to convert Clang attributes of declarations to IR annotations (local version) currently used for:
// 	-> OpenCL address spaces
core::NodeAnnotationPtr convertAttribute(const clang::ValueDecl* varDecl, conversion::Converter& convFact) {
	if (!varDecl->hasAttrs()) {
		return insieme::core::NodeAnnotationPtr();
	}

	std::ostringstream ss;
	annotations::ocl::BaseAnnotation::AnnotationList declAnnotation;
	try {
		for (AttrVec::const_iterator I = varDecl->attr_begin(), E = varDecl->attr_end(); I != E; ++I) {
			if (AnnotateAttr * attr = dyn_cast<AnnotateAttr>(*I)) {
				std::string&& sr = attr->getAnnotation().str();

				//check if the declaration has attribute __private
				if ( sr == "__private" ) {
					VLOG(2) << "           OpenCL address space __private";
					declAnnotation.push_back(
							std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::PRIVATE )
					);
					continue;
				}

				//check if the declaration has attribute __local
				if ( sr == "__local" ) {
					VLOG(2) << "           OpenCL address space __local";
					declAnnotation.push_back(
							std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::LOCAL )
					);
					continue;
				}

				// TODO global also for global variables

				//check if the declaration has attribute __global
				if ( sr == "__global" ) {
					// keywords global and local are only allowed for parameters

					if(isa<const clang::ParmVarDecl>(varDecl) || varDecl->getType().getTypePtr()->isPointerType()) {
						VLOG(2) << "           OpenCL address space __global";
						declAnnotation.push_back(
								std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::GLOBAL )
						);
						continue;
					}
					ss << "Address space __global not allowed for local scalar variable";
					throw &ss;
				}

				//check if the declaration has attribute __constant
				if ( sr == "__constant" ) {
					if ( isa<const clang::ParmVarDecl>(varDecl) ) {
						VLOG(2) << "           OpenCL address space __constant";
						declAnnotation.push_back(
								std::make_shared<annotations::ocl::AddressSpaceAnnotation>( annotations::ocl::AddressSpaceAnnotation::addressSpace::CONSTANT )
						);
						continue;
					}
					ss << "Address space __constant not allowed for local variable";
					throw &ss;
				}
			}

			// Throw an error if an unhandled attribute is found
			ss << "Unexpected attribute";
			throw &ss;// FIXME define an exception class for this error
		}}
	catch ( std::ostringstream *errMsg ) {
		//show errors if unexpected patterns were found
		fe::utils::compilerMessage(fe::utils::DiagnosticLevel::Warning,
				varDecl->getLocStart(),
				errMsg->str(),
				convFact.getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

} // end anonymous namespace

insieme::core::ExpressionPtr OclKernelExtension::Visit(const clang::Expr* expr, insieme::frontend::conversion::Converter& convFact) {
	if(!llvm::isa<clang::ExtVectorElementExpr>(expr))
		return nullptr;

	const clang::ExtVectorElementExpr* vecElemExpr = llvm::cast<clang::ExtVectorElementExpr>(expr);

	core::ExpressionPtr&& base = convFact.convertExpr( vecElemExpr->getBase() );

	core::ExpressionPtr retIr;

	llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

	core::TypePtr&& exprTy = convFact.convertType( (vecElemExpr)->getType() );
	unsigned int pos = 0u;
	core::IRBuilder builder = convFact.getIRBuilder();
	const core::lang::BasicGenerator& gen(builder.getLangBasic());

	// catch hi and lo, even and odd
	if(accessor == "hi" || accessor == "lo" || accessor == "even" || accessor == "odd" ) {
		core::TypePtr ty = base->getType();
		core::RefTypePtr refTy = ty.isa<core::RefTypePtr>();

		core::VectorTypePtr vecTy = refTy ? refTy->getElementType().as<core::VectorTypePtr>() : ty.as<core::VectorTypePtr>();
		core::TypePtr elementTy = vecTy->getElementType();
		unsigned vecSize = vecTy->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();
		unsigned halfVecSize = vecSize/2;

		// length is always half of original vector length
		core::LiteralPtr newVecSize = builder.getIntParamLiteral(halfVecSize);

		core::TypePtr resultTy = builder.vectorType(elementTy, builder.concreteIntTypeParam(halfVecSize));
		if(refTy) resultTy = builder.refType(resultTy);

		if(accessor == "hi" || accessor == "lo") {
			// start at 0 for lo, vecSize/2 for hi
			core::LiteralPtr start = accessor == "lo" ? builder.getIntParamLiteral(0) : newVecSize;

			// select ref or non-ref projection function
			core::ExpressionPtr fct = refTy ? gen.getVectorRefProjection() : gen.getVectorProjection();

			return builder.callExpr(resultTy, fct, base, start.as<core::ExpressionPtr>(), newVecSize.as<core::ExpressionPtr>());
		}

		if(accessor == "even" || accessor == "odd" ) {
			unsigned odd = accessor == "odd" ? 1 : 0;

			if(refTy) {
				assert_fail() << "Vector functions 'even' and 'odd' are not supported as l-values";
			}

			// non-ref type
			core::ExpressionList indices;
			for(unsigned i = 0; i < 4; ++i) {
				indices.push_back(builder.uintLit(i*2 + odd));
			}

			core::ExpressionPtr indexVector = builder.vectorExpr(indices);

			return builder.vectorPermute(base, indexVector);
		}
	}
	//translate OpenCL accessor string to index
	else if ( accessor == "x" ) pos = 0u;
	else if ( accessor == "y" ) pos = 1u;
	else if ( accessor == "z" ) pos = 2u;
	else if ( accessor == "w" ) pos = 3u;
	else if ( (accessor.front() == 's' || accessor.front() == 'S') && accessor.size() == 2) {
		// the input string is in a form sXXX
		// we skip the s and return the value to get the number
		llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
		std::string posStr = numStr;

		if(posStr.at(0) <= '9')
		pos = posStr.at(0) - '0';
		else if(posStr.at(0) <= 'F')
		pos = (10 + posStr.at(0) - 'A');//convert A .. E to 10 .. 15
		else if(posStr.at(0) <= 'e')
		pos = (10 + posStr.at(0) - 'a');//convert a .. e to 10 .. 15
		else
		assert(posStr.at(0) <= 'e' && "Invalid vector accessing string");
	} else if ( accessor.size() <= 16 ) { // opencl vector permutation
		vector<core::ExpressionPtr> args;

		// expression using x, y, z and w
		auto acc = accessor.begin();
		if(*acc == 'S' || *acc == 's') { // expression using s0 .. sE
			++acc;// skip the s
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				if(*I <= '9')
				pos = *I - '0';
				else if(*I <= 'E')
				pos = (10 + (*I)-'A'); //convert A .. E to 10 .. 15
				else if(*I <= 'e')
				pos = (10 + (*I)-'a');//convert a .. e to 10 .. 15
				else
				assert(*I <= 'e' && "Unexpected accessor in ExtVectorElementExpr");

				args.push_back(builder.uintLit(pos));
			}
			return (retIr = builder.vectorPermute(convFact.tryDeref(base), builder.vectorExpr(args)) );
		} else {
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				args.push_back(builder.uintLit(*I == 'w' ? 3 : (*I)-'x')); //convert x, y, z, w to 0, 1, 2, 3
			}
			return (retIr = builder.vectorPermute(convFact.tryDeref(base), builder.vectorExpr(args)) );
		}

	} else {
		assert_le(accessor.size(), 16) << "ExtVectorElementExpr has unknown format";
	}

	// The type of the index is always uint<4>
	core::ExpressionPtr&& idx = builder.uintLit(pos);
	// if the type of the vector is a refType, we deref it
	base = convFact.tryDeref(base);

	return (retIr = builder.callExpr(exprTy, gen.getVectorSubscript(), base, idx));

}

insieme::core::TypePtr OclKernelExtension::Visit(const clang::QualType& type, insieme::frontend::conversion::Converter& convFact) {
	if(!llvm::isa<clang::ExtVectorType>(type.getTypePtr()))
		return nullptr;

	const clang::ExtVectorType* vecTy = llvm::cast<clang::ExtVectorType>(type.getTypePtr());

    // get vector datatype
 	const QualType qt = vecTy->getElementType();
 	//const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
 	core::TypePtr&& subType = convFact.convertType(qt);

 	// get the number of elements
 	size_t num = vecTy->getNumElements();
 	core::IntTypeParamPtr numElem = core::ConcreteIntTypeParam::get(convFact.getNodeManager(), num);

 	//note: members of OpenCL vectors are never refs
 	return convFact.getIRBuilder().vectorType( subType, numElem);

}


/** FuncDeclPostVisit will add the OpenCL kernel annotation.
    User provided post clang function decl visitor. Will be called after clang decl
    was visted by the insieme function decl visitor and returns a modified IR expression.
    See FrontendExtension FuncDeclPostVisit

    @param decl clang function decl
    @param type IR ExpressionPtr
    @param convFact insieme conversion factory
    @return modified version of IR ExpressionPtr */
insieme::core::ExpressionPtr OclKernelExtension::FuncDeclPostVisit(const clang::FunctionDecl* decl, core::ExpressionPtr expr, insieme::frontend::conversion::Converter& convFact, bool symbolic) {

	if(!symbolic) {
		// check Attributes of the function definition
		annotations::ocl::BaseAnnotation::AnnotationList kernelAnnotation;

		if (decl->hasAttrs()) {
			const clang::AttrVec attrVec = decl->getAttrs();

			for (clang::AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
				if (clang::AnnotateAttr * attr = llvm::dyn_cast<clang::AnnotateAttr>(*I)) {
					//get annotate string
					llvm::StringRef&& sr = attr->getAnnotation();

					//check if it is an OpenCL kernel function
					if ( sr == "__kernel" ) {
						VLOG(1) << "is OpenCL kernel function";
						kernelAnnotation.push_back( std::make_shared<annotations::ocl::KernelFctAnnotation>() );
					}
				}
				else if ( clang::ReqdWorkGroupSizeAttr* attr = llvm::dyn_cast<clang::ReqdWorkGroupSizeAttr>(*I) ) {
					kernelAnnotation.push_back(
							std::make_shared<annotations::ocl::WorkGroupSizeAnnotation>( attr->getXDim(), attr->getYDim(), attr->getZDim() )
					);
				}
			}
		}

		// if OpenCL related annotations have been found, create OclBaseAnnotation and add it to the funciton's attribute
		if (!kernelAnnotation.empty()) {
			core::ExpressionPtr lambda = convFact.getLambdaFromCache(decl);
			lambda->addAnnotation( std::make_shared<annotations::ocl::BaseAnnotation>(kernelAnnotation) );
			convFact.addToLambdaCache(decl, lambda);
		}
	}
    return nullptr;
}


insieme::core::TypePtr OclKernelExtension::TypeDeclPostVisit(const clang::TypeDecl* decl, core::TypePtr type, insieme::frontend::conversion::Converter& convFact) {
	//decl->dump();
    return nullptr;
}


insieme::core::ExpressionPtr OclKernelExtension::ValueDeclPostVisit(const clang::ValueDecl* decl, core::ExpressionPtr expr, insieme::frontend::conversion::Converter& convFact) {
    // check var Decls
	if (const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(decl)){
		//varDecl->dump();

		// Add OpenCL attributes
		insieme::core::NodeAnnotationPtr&& attr = convertAttribute(varDecl, convFact);
		if (attr) {
			core::ExpressionPtr irVar = convFact.lookUpVariable(varDecl);
			irVar->addAnnotation(attr);
		}
	}
	return nullptr;
}

//////////////////////////////////////////////////////////////////////////////////////
//               opencl kernel file post processing

insieme::core::ProgramPtr OclKernelExtension::IRVisit(insieme::core::ProgramPtr& prog) {
	insieme::core::NodeManager& mgr(prog->getNodeManager());
	// call OpenCL kernel post processing
	ocl::Compiler oclCompiler(prog, mgr);
	return oclCompiler.lookForOclAnnotations();
}


FrontendExtension::flagHandler OclKernelExtension::registerFlag(boost::program_options::options_description& options) {
    //register omp flag
    options.add_options()("fopenclkernel", boost::program_options::value<bool>(&flagActivated)->implicit_value(true), "OpenCL Kernel support");
    //create lambda
    auto lambda = [&](const ConversionJob& job) {
        return flagActivated;
    };
    return lambda;
}

boost::optional<std::string> OclKernelExtension::isPrerequisiteMissing(ConversionSetup& setup) const {
	bool missingIPE = true;
	bool missingFEC = true;
	for(auto extPtr : setup.getExtensions()) {
		if(dynamic_cast<insieme::frontend::extensions::InsiemePragmaExtension*>(extPtr.get())) {
			missingIPE = false;
		}
		if(dynamic_cast<insieme::frontend::extensions::FrontendCleanupExtension*>(extPtr.get())) {
			missingFEC = false;
		}
	}

	if(missingFEC || missingIPE) {
		std::string warning("OclKernelExtension needs:");
		warning.append(missingIPE ? " InsiemePragmaExtension" : "");
		warning.append(missingFEC ? " FrontendCleanupExtension" : "");
		return boost::optional<std::string>(warning);
	}

	return boost::optional<std::string>();
}


} //namespace extensions
} //namespace frontend
} //namespace insieme

/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include "insieme/frontend/convert.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_annotations.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"

#include "insieme/core/program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/c_info/naming.h"
#include "insieme/c_info/location.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/Frontend/TextDiagnosticPrinter.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace {

//TODO put it into a class
void printErrorMsg(std::ostringstream& errMsg, const frontend::ClangCompiler& clangComp, const clang::Decl* decl) {

	SourceManager& manager = clangComp.getSourceManager();
    clang::SourceLocation&& errLoc = decl->getLocStart();
    errMsg << " at location (" << frontend::utils::Line(errLoc, manager) << ":" <<
            frontend::utils::Column(errLoc, manager) << ")." << std::endl;

    clang::Preprocessor& pp = clangComp.getPreprocessor();
    pp.Diag(errLoc, pp.getDiagnostics().getCustomDiagID(Diagnostic::Warning, errMsg.str()));
}

// Covert clang source location into a c_info::SourceLocation object to be inserted in an CLocAnnotation
c_info::SourceLocation convertClangSrcLoc(SourceManager& sm, const SourceLocation& loc) {
	FileID&& fileId = sm.getFileID(loc);
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	return c_info::SourceLocation(fileEntry->getName(), sm.getSpellingLineNumber(loc), sm.getSpellingColumnNumber(loc));
};

} // End empty namespace

namespace insieme {
namespace frontend {
namespace conversion {

core::ProgramPtr ASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain) {
	clang::idx::Entity&& funcEntity = clang::idx::Entity::get(
			const_cast<FunctionDecl*>(funcDecl), const_cast<clang::idx::Program&>( mProg.getClangProgram() ));
	std::pair<FunctionDecl*, clang::idx::TranslationUnit*>&& ret = mProg.getClangIndexer().getDefinitionFor(funcEntity);
	assert(ret.first && ret.second);

	mFact.currTU = &mProg.getTranslationUnit(ret.second);

	mFact.ctx.globalFuncMap.clear();
	analysis::GlobalVarCollector globColl(ret.second, mFact.program.getClangIndexer(), mFact.ctx.globalFuncMap);
	globColl(funcDecl);
	DVLOG(1) << globColl;
	mFact.ctx.globalStruct = globColl.createGlobalStruct(mFact);
	if(mFact.ctx.globalStruct.first)
		mFact.ctx.globalVar = mFact.builder.variable( mFact.builder.refType(mFact.ctx.globalStruct.first) );

	core::ExpressionPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>(mFact.convertFunctionDecl(funcDecl, true));
	assert(lambdaExpr && "Conversion of function did not return a lambda expression");
	mProgram = core::Program::addEntryPoint(mFact.getNodeManager(), mProgram, lambdaExpr, isMain /* isMain */);

	return mProgram;
}

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::NodeManager& mgr, Program& prog):
	// cppcheck-suppress exceptNew
	stmtConv( ConversionFactory::makeStmtConverter(*this) ),
	// cppcheck-suppress exceptNew
	typeConv( ConversionFactory::makeTypeConverter(*this) ),
	// cppcheck-suppress exceptNew
	exprConv( ConversionFactory::makeExprConverter(*this) ),
	// cppcheck-suppress exceptNew
	mgr(mgr), builder(mgr), program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()), currTU(NULL) { }


core::ExpressionPtr ConversionFactory::tryDeref(const core::ExpressionPtr& expr) const {
	// core::ExpressionPtr retExpr = expr;
	if(core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr( refTy->getElementType(), mgr.basic.getRefDeref(), expr );
	}
	return expr;
}

/* Function to convert Clang attributes of declarations to IR annotations (local version)
 * currently used for:
 * 	* OpenCL address spaces
 */
core::AnnotationPtr ConversionFactory::convertAttribute(const clang::VarDecl* varDecl) const {
    if(!varDecl->hasAttrs())
    	return core::AnnotationPtr();

	const AttrVec attrVec = varDecl->getAttrs();
	std::ostringstream ss;
	ocl::BaseAnnotation::AnnotationList declAnnotation;
	try {
	for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
		if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
			std::string sr = attr->getAnnotation().str();

			//check if the declaration has attribute __private
			if(sr == "__private") {
				DVLOG(2) << "           OpenCL address space __private";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::PRIVATE ));
				continue;
			}

			//check if the declaration has attribute __local
			if(sr == "__local") {
				DVLOG(2) << "           OpenCL address space __local";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::LOCAL ));
				continue;
			}

            // TODO global also for global variables

			//check if the declaration has attribute __global
			if(sr == "__global") {
				// keywords global and local are only allowed for parameters
				if(isa<const clang::ParmVarDecl>(varDecl)) {
					DVLOG(2) << "           OpenCL address space __global";
					declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::GLOBAL ));
					continue;
				}
				ss << "Address space __global not allowed for local variable";
				throw &ss;
			}

			//check if the declaration has attribute __constant
			if(sr == "__constant") {
				if(isa<const clang::ParmVarDecl>(varDecl)) {
					DVLOG(2) << "           OpenCL address space __constant";
					declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>( ocl::AddressSpaceAnnotation::addressSpace::CONSTANT ));
					continue;
				}
				ss << "Address space __constant not allowed for local variable";
				throw &ss;
			}
		}

		// Throw an error if an unhandled attribute is found
		ss << "Unexpected attribute";
		throw &ss; // FIXME define an exception class for this error
	}}
	catch(std::ostringstream *errMsg) {
        //show errors if unexpected patterns were found
        printErrorMsg(*errMsg, currTU->getCompiler(), varDecl);
	}
	return std::make_shared<ocl::BaseAnnotation>(declAnnotation);
}

core::ExpressionPtr ConversionFactory::lookUpVariable(const clang::VarDecl* varDecl) {
	ConversionContext::VarDeclMap::const_iterator fit = ctx.varDeclMap.find(varDecl);
	if(fit != ctx.varDeclMap.end()) {
		// variable found in the map.
		return fit->second;
	}

	QualType&& varTy = varDecl->getType();
	core::TypePtr&& type = convertType( varTy.getTypePtr() );
	if( !(varTy.isConstQualified() || isa<const clang::ParmVarDecl>(varDecl) ||
	        (type->getNodeType() == core::NT_VectorType && !varTy.getTypePtr()->isExtVectorType()) )) {
		// add a ref in the case of variables which are not const or declared as function parameters
		type = builder.refType(type);
	}

	// check whether this is variable is defined as global or static
	if(varDecl->hasGlobalStorage()) {
		assert(ctx.globalVar && "Accessing global variable within a function not receiving the global struct");
		// access the global data structure
		return builder.memberAccessExpr(tryDeref(ctx.globalVar), core::Identifier(varDecl->getNameAsString()));
	}

	// variable is not in the map, create a new var and add it
	core::VariablePtr&& var = builder.variable(type);
	// add the var in the map
	ctx.varDeclMap.insert( std::make_pair(varDecl, var) );

	// Add the C name of this variable as annotation
	var->addAnnotation( std::make_shared<c_info::CNameAnnotation>(varDecl->getNameAsString()) );

	// Add OpenCL attributes
	core::AnnotationPtr&& attr = convertAttribute(varDecl);
	if(attr)
		var->addAnnotation(attr);

	return var;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT VARIABLE DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

core::ExpressionPtr ConversionFactory::defaultInitVal( const core::TypePtr& type ) const {
	if( *type == *mgr.basic.getAlpha() ) {
		return mgr.basic.getNull();
	}
	// handle integers initialization
    if ( mgr.basic.isInt(type) ) {
        // initialize integer value
        return builder.literal("0", type);
    }
    if ( mgr.basic.isChar(type) ) {
		// initialize integer value
		return builder.literal("\'\\0\'", type);
	}
    // handle reals initialization
    if ( mgr.basic.isReal(type) ) {
        // in case of floating types we initialize with a zero value
        return builder.literal("0.0", type);
    }
    // handle refs initialization
    if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type) ) {
        // initialize pointer/reference types with the null value
    	return builder.refVar( defaultInitVal(refTy->getElementType()) );
    }
    // handle strings initialization
    if ( *type == *mgr.basic.getString() ) {
        return builder.literal("", type);
    }
    // handle booleans initialization
    if ( *type == *mgr.basic.getBool() ) {
        // boolean values are initialized to false
        return builder.literal("false", mgr.basic.getBool());
    }
    // Handle structs initialization
    if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(type) ) {
    	core::StructExpr::Members members;
    	const core::NamedCompositeType::Entries& entries = structTy->getEntries();
    	std::for_each(entries.begin(), entries.end(),
    		[ this, &members ](const core::NamedCompositeType::Entry& curr) {
    			members.push_back(core::StructExpr::Member(curr.first, this->defaultInitVal(curr.second)));
    		}
    	);
    	return builder.structExpr(structTy, members);
    }
    if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(type) ) {
		// todo
    	assert(unionTy); // silent compiler warning
	}

    //----------------  INTIALIZE VECTORS ---------------------------------
//    const Type* elemTy = NULL;
//    size_t arraySize = 0;
//    if ( ty->isExtVectorType() ) {
//    	const TypedefType* typedefType = dyn_cast<const TypedefType>(ty);
//        assert(typedefType && "ExtVectorType has unexpected class");
//        const ExtVectorType* vecTy = dyn_cast<const ExtVectorType>( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
//        assert(vecTy && "ExtVectorType has unexpected class");
//
//        elemTy = vecTy->getElementType()->getUnqualifiedDesugaredType();
//		arraySize = vecTy->getNumElements();
//    }

    // handle vectors initialization
    if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type) ) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		return builder.callExpr(vecTy, mgr.basic.getVectorInitUniform(), initVal, mgr.basic.getIntTypeParamLiteral(vecTy->getSize()));
    }
    // handle arrays initialization
    if ( core::ArrayTypePtr&& vecTy = core::dynamic_pointer_cast<const core::ArrayType>(type) ) {
    	// FIXME
    	assert(vecTy); // silent compiler warning
    	// initialization for arrays is missing, returning NULL!
    	return mgr.basic.getNull();
    }
    assert(false && "Default initialization type not defined");
}

core::ExpressionPtr ConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit) const {
	// get kind of initialized value
	core::NodeType kind = type->getNodeType();
	if (kind == core::NT_RefType) {
		kind = core::static_pointer_cast<const core::RefType>(type)->getElementType()->getNodeType();
	}

	// if no init expression is provided => use undefined for given set of types
	if(!expr && (kind == core::NT_StructType || kind == core::NT_UnionType
			  || kind == core::NT_ArrayType || kind == core::NT_VectorType)) {
		if(core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		    // FIXME add zero initialization of references if needed
			const core::TypePtr& res = refTy->getElementType();
			return builder.refVar( builder.callExpr( res, mgr.basic.getUndefined(), mgr.basic.getTypeLiteral(res) ) );
		}
		return builder.callExpr( type, (zeroInit ? mgr.basic.getInitZero() : mgr.basic.getUndefined()), mgr.basic.getTypeLiteral(type));
	} else if (!expr)
		return defaultInitVal(type);

	if(const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr ))
		return convertInitializerList( listExpr, type );





	core::ExpressionPtr&& retExpr = convertExpr( expr );

	if (core::analysis::isCallOf(retExpr, mgr.basic.getRefVar()) ||
		core::analysis::isCallOf(retExpr, mgr.basic.getRefNew()) ) {

		return retExpr;
	}


	if(retExpr->getType()->getNodeType() == core::NT_RefType && type->getNodeType() == core::NT_RefType ) {
		return builder.refVar( tryDeref(retExpr) );
	}

	if(type->getNodeType() == core::NT_RefType) {
		retExpr = builder.refVar( retExpr );
	}

	return retExpr;
}

core::DeclarationStmtPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {

	// logging
	DVLOG(1) << "\n****************************************************************************************\n"
			 << "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n"
			 << "-> at location: (" << utils::location(varDecl->getLocation(), currTU->getCompiler().getSourceManager()) << "): ";
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang VarDecl: \n"
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::DeclarationStmtPtr retStmt;

	if(const VarDecl* definition = varDecl->getDefinition()) {
        clang::QualType clangType = definition->getType();
		if(!clangType.isCanonical())
			clangType = clangType->getCanonicalTypeInternal();

		if(definition->hasGlobalStorage()) {
			// once we encounter static variables we do remove the declaration
			return core::DeclarationStmtPtr();
		}

		// lookup for the variable in the map
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(lookUpVariable(definition));
		assert(var);

        // flag to determine if a variable should be initialized with zeros instead of uninitialized
        bool zeroInit = false;

        // check for annotations which would lead to a zero init annotation
		if(var->hasAnnotation(ocl::BaseAnnotation::KEY)){
		    auto declarationAnnotation = var->getAnnotation(ocl::BaseAnnotation::KEY);
		    for(ocl::BaseAnnotation::AnnotationList::const_iterator I = declarationAnnotation->getAnnotationListBegin();
		            I < declarationAnnotation->getAnnotationListEnd(); ++I) {
		        if(ocl::AddressSpaceAnnotationPtr as = std::dynamic_pointer_cast<ocl::AddressSpaceAnnotation>(*I)){
		            if(ocl::AddressSpaceAnnotation::addressSpace::LOCAL == as->getAddressSpace() ||
		                    ocl::AddressSpaceAnnotation::addressSpace::PRIVATE == as->getAddressSpace()) {
		                //TODO check why this fails:
		                //assert(!definition->getInit() && "OpenCL local variables cannot have an initialization expression");
		                zeroInit = true;
		                std::cout << "init with zeros\n";
		            }
		        }
		    }
		}

/*
		if(definition->getAnnotation().str() == "__local") {
		    zeroInit = true;
*/

		// initialization value
		core::ExpressionPtr&& initExpr = convertInitExpr(definition->getInit(), var->getType(), zeroInit);

		retStmt = builder.declarationStmt( var, initExpr );
	} else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");

	}
	// logging
	DVLOG(1) << "Converted into IR stmt: ";
	DVLOG(1) << "\t" << *retStmt;
	return retStmt;
}

void ConversionFactory::attachFuncAnnotations(const core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl) {
	// ---------------------- Add annotations to this function ------------------------------
	//check Attributes of the function definition
	ocl::BaseAnnotation::AnnotationList kernelAnnotation;
	if(funcDecl->hasAttrs()) {
		const clang::AttrVec attrVec = funcDecl->getAttrs();

		for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
			if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
				//get annotate string
				llvm::StringRef sr = attr->getAnnotation();

				//check if it is an OpenCL kernel function
				if(sr == "__kernel") {
					DVLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<ocl::KernelFctAnnotation>() );
				}
			}
			else if(ReqdWorkGroupSizeAttr* attr = dyn_cast<ReqdWorkGroupSizeAttr>(*I)) {
				kernelAnnotation.push_back(std::make_shared<ocl::WorkGroupSizeAnnotation>(
						attr->getXDim(), attr->getYDim(), attr->getZDim())
				);
			}
		}
	}
	// --------------------------------- OPENCL ---------------------------------------------
	// if OpenCL related annotations have been found, create OclBaseAnnotation and
	// add it to the funciton's attribute
	if(!kernelAnnotation.empty())
		node->addAnnotation( std::make_shared<ocl::BaseAnnotation>(kernelAnnotation) );

	// --------------------------------- C NAME ----------------------------------------------
	// annotate with the C name of the function
	node->addAnnotation( std::make_shared<c_info::CNameAnnotation>( funcDecl->getName() ) );

	// ----------------------- SourceLocation Annotation -------------------------------------
	// for each entry function being converted we register the location where it was originally
	// defined in the C program
	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(funcDecl->getLocStart(), funcDecl->getLocEnd());
	PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);
	if(fit != pragmaMap.getDeclarationMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

	assert(currTU && "Translation unit not correctly set");
	node->addAnnotation( std::make_shared<c_info::CLocAnnotation>(
		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.first),
		convertClangSrcLoc(currTU->getCompiler().getSourceManager(), loc.second))
	);
}

core::LambdaExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
	mFact.currTU = &tu;
//	core::StatementPtr&& bodyStmt = mFact.convertStmt( body );
//	core::ExpressionPtr&& callExpr = mFact.createCallExpr( toVector<core::StatementPtr>(bodyStmt), mgr.basic.getUnit() );

//	c_info::CLocAnnotation::ArgumentList args;
//	if(core::CaptureInitExprPtr&& captureExpr = core::dynamic_pointer_cast<const core::CaptureInitExpr>(callExpr)) {
//		// look for variable names
//		for_each(captureExpr->getArguments().begin(), captureExpr->getArguments().end(), [ &args ](const core::ExpressionPtr& expr){
//			// because this callexpr was created out of a stmt block, we are sure
//			// input arguments are Variables
//			core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(expr);
//			assert(var && "Argument of call expression is not a variable.");
//			// we also have to look at the CNameAnnotation in order to find the name of the original variable
//
//			std::shared_ptr<c_info::CNameAnnotation>&& nameAnn = var->getAnnotation(c_info::CNameAnnotation::KEY);
//			assert(nameAnn && "Variable has not CName associated");
//			args.push_back( nameAnn->getName() );
//		});
//	}

//	core::LambdaExprPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>( callExpr->getFunctionExpr() );
//	// ------ Adding source location annotation (CLocAnnotation) -------
//	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(body->getLocStart(), body->getLocEnd());
//	PragmaStmtMap::StmtMap::const_iterator fit = mFact.getPragmaMap().getStatementMap().find(body);
//	if(fit != mFact.getPragmaMap().getStatementMap().end()) {
//		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
//		loc.first = fit->second->getStartLocation();
//	}
//
//	lambdaExpr.addAnnotation( std::make_shared<c_info::CLocAnnotation>(
//		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.first),
//		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.second),
//		false, // this is not a function decl
//		args)
//	);
//
//	return lambdaExpr;
	return core::LambdaExprPtr();
}


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

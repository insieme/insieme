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

#include <functional>

#include "insieme/frontend/clang.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/utils/source_locations.h"

#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/castTool.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/analysis/prunable_decl_visitor.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/dump/text_dump.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/annotations/c/location.h"
#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/c/extern_c.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/c/include.h"

#include "insieme/frontend/extensions/variadic_arguments_extension.h"
#include "insieme/frontend/extensions/cpp11_extension.h"

using namespace clang;
using namespace insieme;


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//   ANONYMOUS NAMESPACE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace {

/// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(SourceManager& sm, const SourceLocation& loc) {
	FileID&& fileId = sm.getMainFileID();
	assert(!fileId.isInvalid() && "File is not valid!");
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	assert(fileEntry);
	return annotations::c::SourceLocation(fileEntry->getName(), sm.getSpellingLineNumber(loc), sm.getSpellingColumnNumber(loc));
};



bool isCppConstructor (const core::ExpressionPtr& expr){
	// constructor
	if (core::analysis::isConstructorCall(expr)){
		return true;
	}
	// array constructor
	core::NodeManager&  mgr = expr->getNodeManager();
	if (core::analysis::isCallOf(expr, mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor()))
		return true;
	//if (core::CallExprPtr call = expr.isa<core::CallExprPtr>()) {
	//	if (*mgr.getLangExtension<core::lang::IRppExtensions>().getVectorCtor() ==*call->getFunctionExpr())
	//		return true;
	//	if (*mgr.getLangExtension<core::lang::IRppExtensions>().getArrayCtor() ==*call->getFunctionExpr())
	//		return true;
	return false;
}


/// convert a initialization for a global
core::ExpressionPtr convertInitForGlobal (insieme::frontend::conversion::Converter& converter, const clang::VarDecl* var, core::TypePtr elementType){

	// and get the initial value
	core::ExpressionPtr initValue;

	if (var->hasInit() && !var->isStaticLocal()) {
		initValue = converter.convertExpr ( var->getInit() ) ;
	}
	else if (clang::VarDecl* outDecl = const_cast<clang::VarDecl*>(var)->getOutOfLineDefinition ()){
		// initialization be out of class or something else, beware of dependent types
		if (!outDecl->getAnyInitializer()->getType().getTypePtr()->isDependentType() &&
			!outDecl->getAnyInitializer()->isInstantiationDependent())
			initValue = converter.convertExpr ( outDecl->getAnyInitializer() ) ;
	}

	// globals are just assigned, so do it carefully
	if (initValue){
		initValue = converter.getInitExpr (elementType.as<core::RefTypePtr>()->getElementType(), initValue);

		// globals have a little issue with constructor initialization, backend restores right operation
		if (isCppConstructor(initValue)) {
			core::IRBuilder builder( initValue->getNodeManager() );
			initValue = builder.deref(initValue);
		}
	}

	return initValue;
}


} // End empty namespace
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace insieme {
namespace frontend {

// ----------- conversion ------------

tu::IRTranslationUnit convert(core::NodeManager& manager, const path& unit, const ConversionSetup& setup) {
	// just delegate operation to converter
	Program program(manager, unit, setup);
	conversion::Converter c(manager, program);
	c.registerClangHandler<VariadicArgumentsPlugin>();

	// enable cpp11 if needed
	if (setup.getStandard() ==  ConversionSetup::Cxx11)
		c.registerClangHandler<Cpp11Plugin>();

	// add them and fire the conversion
	return c.convert();
}


namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERTER
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//////////////////////////////////////////////////////////////////
///
Converter::Converter(core::NodeManager& mgr, const Program& prog) :
		staticVarCount(0), mgr(mgr), builder(mgr),
		program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()),
		irTranslationUnit(mgr), used(false)
	{

	if (prog.isCxx()){
		typeConvPtr = std::make_shared<CXXTypeConverter>(*this);
		exprConvPtr = std::make_shared<CXXExprConverter>(*this);
		stmtConvPtr = std::make_shared<CXXStmtConverter>(*this);
	} else{
		typeConvPtr = std::make_shared<CTypeConverter>(*this);
		exprConvPtr = std::make_shared<CExprConverter>(*this);
		stmtConvPtr = std::make_shared<CStmtConverter>(*this);
	}
	// tag the translation unit with as C++ if case
	irTranslationUnit.setCXX(prog.isCxx());
}

const std::list<std::shared_ptr<extensions::ClangStagePlugin>> Converter::getClangHandlers() const {
	return userProvidedConv;
}

tu::IRTranslationUnit Converter::convert() {
	assert(!used && "This one must only be used once!");
	used = true;

	assert(getCompiler().getASTContext().getTranslationUnitDecl());

	// Thread private requires to collect all the variables which are marked to be threadprivate
	omp::collectThreadPrivate(getPragmaMap(), thread_private);

	// collect all type definitions
	auto declContext = clang::TranslationUnitDecl::castToDeclContext(getCompiler().getASTContext().getTranslationUnitDecl());

	struct TypeVisitor : public analysis::PrunableDeclVisitor<TypeVisitor> {

		Converter& converter;
		TypeVisitor(Converter& converter) : converter(converter) {}

        Converter& getConverter() {
            return converter;
        }

		void VisitRecordDecl(const clang::RecordDecl* typeDecl) {
			// we do not convert templates or partial spetialized classes/functions, the full
			// type will be found and converted once the instantaion is found
			auto irTy = converter.convertType(typeDecl->getTypeForDecl());
			//utils::addHeaderForDecl(irTy, typeDecl, converter.getProgram().getStdLibDirs(), converter.getProgram().getUserIncludeDirs());
		}
		void VisitTypedefDecl(const clang::TypedefDecl* typedefDecl) {
			if (!typedefDecl->getTypeForDecl()) return;

			// get contained type
			auto res = converter.convertType(typedefDecl->getTypeForDecl());

			// frequently structs and their type definitions have the same name => in this case symbol == res and should be ignored
			auto symbol = converter.getIRBuilder().genericType(typedefDecl->getQualifiedNameAsString());
			if (res != symbol && res.isa<core::NamedCompositeTypePtr>()) {	// also: skip simple type-defs
				converter.getIRTranslationUnit().addType(symbol, res);
			}
		//	if (res.isa<core::StructTypePtr>())
		//		utils::addHeaderForDecl(res, typedefDecl, converter.getProgram().getStdLibDirs(), converter.getProgram().getUserIncludeDirs());
		}
	} typeVisitor(*this);

	//typeVisitor.TraverseDecl(llvm::cast<clang::Decl>(declContext));
	typeVisitor.traverseDeclCtx (declContext);

	// collect all global declarations
	struct GlobalVisitor : public analysis::PrunableDeclVisitor<GlobalVisitor> {

		Converter& converter;
		GlobalVisitor(Converter& converter) : converter(converter) {}

        Converter& getConverter() {
            return converter;
        }

		void VisitVarDecl(const clang::VarDecl* var) {
			// variables to be skipped
			if (!var->hasGlobalStorage()) { return; }
			if (var->hasExternalStorage()) { return; }
			if (var->isStaticLocal()) { return; }
			if (converter.getProgram().getInterceptor().isIntercepted(var->getQualifiedNameAsString())) { return; }

			auto builder = converter.getIRBuilder();
			// obtain type
			auto type = converter.convertType(var->getType().getTypePtr());

			// all globals are mutable ..
			auto elementType = type;
			type = builder.refType(type);

			auto literal = builder.literal(type, utils::buildNameForVariable(var));
			if (insieme::utils::set::contains(converter.getThreadprivates(), var)) {
				omp::addThreadPrivateAnnotation(literal);
			}

			// NOTE: not all staticDataMember are seen here, so we take care of the "unseen"
			// ones in lookUpVariable
			auto initValue = convertInitForGlobal(converter, var, type);
			converter.getIRTranslationUnit().addGlobal(literal, initValue);
		}
	} varVisitor(*this);

	varVisitor.traverseDeclCtx(declContext);

	// collect all global declarations
	struct FunctionVisitor : public analysis::PrunableDeclVisitor<FunctionVisitor> {

		Converter& converter;
		bool externC;
		FunctionVisitor(Converter& converter, bool Ccode) : converter(converter), externC(Ccode) {}

        Converter& getConverter() {
            return converter;
        }

		void VisitLinkageSpec(const clang::LinkageSpecDecl* link) {
			bool isC =  link->getLanguage () == clang::LinkageSpecDecl::lang_c;
			FunctionVisitor vis(converter, isC);
			vis.traverseDeclCtx(llvm::cast<clang::DeclContext> (link));
		}

		void VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {
			if (funcDecl->isTemplateDecl()) return;
			//if (!funcDecl->doesThisDeclarationHaveABody()) return;
			core::ExpressionPtr irFunc = converter.convertFunctionDecl(funcDecl);
			if (externC) {
				annotations::c::markAsExternC(irFunc.as<core::LiteralPtr>());
			}
			return;
		}
	} funVisitor(*this, false);

	funVisitor.traverseDeclCtx(declContext);

	// handle entry points (marked using insieme pragmas)
	for(pragma::PragmaPtr pragma : program.getPragmaList()) {
		// only interested in insieme-mark pragmas
		if (pragma->getType() != "insieme::mark") continue;
		const pragma::Pragma& insiemePragma = *pragma;
		if(!insiemePragma.isDecl()) continue;

		// this is a declaration, if it's a function add it to the entry points of the program
		const clang::FunctionDecl* funcDecl = dyn_cast<const clang::FunctionDecl>(insiemePragma.getDecl());
		assert(funcDecl && "Pragma insieme only valid for function declarations.");
		getIRTranslationUnit().addEntryPoints(convertFunctionDecl(funcDecl).as<core::LiteralPtr>());
	}

	//std::cout << " ==================================== " << std::endl;
	//std::cout << getIRTranslationUnit() << std::endl;
	//std::cout << " ==================================== " << std::endl;
	
	// that's all
	return irTranslationUnit;
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::materializeReadOnlyParams(const core::StatementPtr& body, const vector<core::VariablePtr>& params){

	vector<core::StatementPtr> decls;
	core::StatementPtr newBody = body;

	for (auto currParam : params){
		auto fit = this->wrapRefMap.find(currParam);
		if ( fit != this->wrapRefMap.end() ) {

			// if the variable is never written in the function body, avoid materialization
			const core::VariablePtr& wrap = fit->second;
			if (core::analysis::isReadOnly(body, wrap)){
				// replace read uses
				newBody = core::transform::replaceAllGen (mgr, newBody, builder.deref(wrap), currParam, true);
				newBody = core::transform::replaceAllGen (mgr, newBody, wrap, builder.refVar(currParam), true);
				// this variables might apear in annotations inside:
				core::visitDepthFirstOnce (newBody, [&] (const core::StatementPtr& node){
					//if we have a OMP annotation
					if (node->hasAnnotation(omp::BaseAnnotation::KEY)){
						auto anno = node->getAnnotation(omp::BaseAnnotation::KEY);
						assert(anno);
						anno->replaceUsage (wrap, currParam);
					}
				});
				//cleanup the wrap cache to avoid future uses, this var does not exist anymore
				wrapRefMap.erase(currParam);
			}
			else{
				//   if i have a READ operation on a struct:   v= x->a;
				//   it wont be recognized as read only as the base is pased by reference
				//	this turns into an extra copy at the begining of every function

				// other case materialize a var, declare it before body
				decls.push_back( this->builder.declarationStmt(fit->second, this->builder.refVar( fit->first ) ));
			}
		}
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if (!decls.empty()) {
		// push the old body
		decls.push_back(newBody);
		newBody = builder.compoundStmt(decls);
	}
	return newBody;
}


//////////////////////////////////////////////////////////////////
///
void Converter::printDiagnosis(const clang::SourceLocation& loc){

	clang::Preprocessor& pp = getPreprocessor();
	// print warnings and errors:
	while (!warnings.empty()){
		if (getSourceManager().isLoadedSourceLocation (loc)){
			std::cerr << "loaded location:\n";
			std::cerr << "\t" << *warnings.begin() << std::endl;
		}
		else{
			pp.Diag(loc, pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Warning, *warnings.begin()) );
		}
		warnings.erase(warnings.begin());
	}
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::tryDeref(const core::ExpressionPtr& expr) const {
	// core::ExpressionPtr retExpr = expr;
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), mgr.getLangBasic().getRefDeref(), expr);
	}
	return expr;
}

//////////////////////////////////////////////////////////////////
///
core::TypePtr Converter::tryDeref(const core::TypePtr& type) const {
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return refTy->getElementType();
	}
	return type;
}

//////////////////////////////////////////////////////////////////
///
// Register call expression handlers to be used during the clang to IR conversion
//void Converter::registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler) {
//	auto it = callExprHanlders.insert( std::make_pair(funcDecl, handler) );
//	assert( !it.second && "Handler for function declaration already registered." );
//}
//  Function to convert Clang attributes of declarations to IR annotations (local version) currently used for:
// 	-> OpenCL address spaces
core::NodeAnnotationPtr Converter::convertAttribute(const clang::ValueDecl* varDecl) const {
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
				getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::lookUpVariable(const clang::ValueDecl* valDecl) {
	VLOG(1) << "LOOKUP Variable: " << valDecl->getNameAsString();
	if (VLOG_IS_ON(1)) valDecl->dump();

	// Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	auto varCacheHit = varDeclMap.find(valDecl);
	if (varCacheHit != varDeclMap.end()) {
		// variable found in the map
		return varCacheHit->second;
	}

	// The variable has not been converted into IR variable yet, therefore we create the IR variable and insert it
	// to the map for successive lookups

	// Conversion of the variable type
	QualType&& varTy = valDecl->getType();
	core::TypePtr&& irType = convertType( varTy.getTypePtr() );
	assert(irType && "type conversion for variable failed");

	VLOG(2)	<< "clang type: " << varTy.getAsString();
	VLOG(2)	<< "ir type:    " << irType;

	//// check whenever the variable is marked to be volatile
	if (varTy.isVolatileQualified()) {
		irType = builder.volatileType(irType);
	}

	bool isOclVector = !!varTy->getUnqualifiedDesugaredType()->isExtVectorType();
	bool isGCCVector = !!varTy->getUnqualifiedDesugaredType()->isVectorType();
	if (!(varTy.isConstQualified() ||    						// is a constant
		 varTy.getTypePtr()->isReferenceType()  ||             // is a c++ reference
 	 	 (isa<const clang::ParmVarDecl>(valDecl) && 			// is the declaration of a parameter
		 ((irType->getNodeType() != core::NT_VectorType && irType->getNodeType() != core::NT_ArrayType) ||
		   isOclVector || isGCCVector) ))) {
		// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
		// only exception are OpenCL vectors and gcc-vectors
		irType = builder.refType(irType);
	}
	else{
		// beware of const pointers
		if (utils::isRefArray(irType) && varTy.isConstQualified())
			irType = builder.refType(irType);

        // if is a constant obj:
        // might be intercepted
        // might be generic
        if (varTy.isConstQualified()) {
            if((lookupTypeDetails(irType)->getNodeType() == core::NT_StructType) ||
               (getProgram().getInterceptor().isIntercepted(valDecl->getQualifiedNameAsString()))) {
                irType = builder.refType(irType);
            }
        }


	}

	// if is a global variable, a literal will be generated, with the qualified name
	// (two qualified names can not coexist within the same TU)
	const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);
	if (varDecl && varDecl->hasGlobalStorage()) {
		VLOG(2)	<< varDecl->getQualifiedNameAsString() << " with global storage";
		// we could look for it in the cache, but is fast to create a new one, and we can not get
		// rid if the qualified name function
		std::string name = utils::buildNameForVariable(varDecl);
		if (getProgram().getInterceptor().isIntercepted(varDecl->getQualifiedNameAsString())) {
			name = varDecl->getQualifiedNameAsString();
		}

 		if(varDecl->isStaticLocal()) {
			VLOG(2)	<< "         isStaticLocal";
            if(staticVarDeclMap.find(varDecl) != staticVarDeclMap.end()) {
                name = staticVarDeclMap.find(varDecl)->second;
            } else {
                std::stringstream ss;
                //get source location and src file path
                //hash this string to create unique variable names
                clang::FullSourceLoc f(varDecl->getLocation(),getSourceManager());
                std::hash<std::string> str_hash;
                ss << name << str_hash(getSourceManager().getFileEntryForID(f.getFileID())->getName()) << staticVarCount++;
                staticVarDeclMap.insert(std::pair<const clang::VarDecl*,std::string>(varDecl,ss.str()));
                name = ss.str();
            }
        }

		// global/static variables are always leftsides (refType) -- solves problem with const
		if(!irType.isa<core::RefTypePtr>() ) {
			irType = builder.refType(irType);
		}

		if (varDecl->isStaticLocal()){
			if (!irType.isa<core::RefTypePtr>()) irType = builder.refType(irType);		// this happens whenever a static variable is constant
			irType = builder.refType (mgr.getLangExtension<core::lang::StaticVariableExtension>().wrapStaticType(irType.as<core::RefTypePtr>().getElementType()));
		}

		core::ExpressionPtr globVar =  builder.literal(name, irType);
		if (varDecl->isStaticLocal()){
			globVar = builder.accessStatic(globVar.as<core::LiteralPtr>());
		}

		// some member statics might be missing because of defined in a template which was ignored
		// since this is the fist time we get access to the complete type, we can define the
		// suitable initialization
		// if the variable is intercepted, we ignore the declaration, will be there once the header is attached
		if (varDecl->isStaticDataMember() && !getProgram().getInterceptor().isIntercepted(varDecl->getQualifiedNameAsString())){
			VLOG(2)	<< "         is static data member";
			auto initValue = convertInitForGlobal(*this, varDecl, irType);
			// as we don't see them in the globalVisitor we have to take care of them here
			getIRTranslationUnit().addGlobal(globVar.as<core::LiteralPtr>(), initValue);
		}

		// OMP threadPrivate
 		if (insieme::utils::set::contains (thread_private, varDecl)){
			omp::addThreadPrivateAnnotation(globVar);
		}

		utils::addHeaderForDecl(globVar, valDecl, program.getStdLibDirs());
		varDeclMap.insert( { valDecl, globVar } );
		return globVar;
	}

	// The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
	// the IR variable and insert it into the map for future lookups
	core::VariablePtr&& var = builder.variable( irType );
	VLOG(2) << "IR variable" << var.getType()->getNodeType() << "" << var<<":"<<varDecl->getNameAsString();
	VLOG(2) << "IR var type" << var.getType();

	varDeclMap.insert( { valDecl, var } );

	if ( !valDecl->getNameAsString().empty() ) {
		// Add the C name of this variable as annotation
		core::annotations::attachName(var,varDecl->getNameAsString());
	}

	// Add OpenCL attributes
	insieme::core::NodeAnnotationPtr&& attr = convertAttribute(valDecl);
	if (attr) {
		var->addAnnotation(attr);
	}

	if(annotations::c::hasIncludeAttached(irType)) {
		VLOG(2) << " header " << annotations::c::getAttachedInclude(irType);
	}
	return var;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::defaultInitVal(const core::TypePtr& valueType) const {

	// get type details
	core::TypePtr type = lookupTypeDetails(valueType);

	// Primitive types
	if (mgr.getLangBasic().isInt(type)) {
		// initialize integer value
		return builder.literal("0", type);
	}
	if (mgr.getLangBasic().isChar(type)) {
		// initialize integer value
		return builder.literal("\'\\0\'", type);
	}
	// handle reals initialization
	if (mgr.getLangBasic().isReal(type)) {
		// in case of floating types we initialize with a zero value
		if(mgr.getLangBasic().isReal4(type))
			return builder.literal("0.0f", type);
		if(mgr.getLangBasic().isReal8(type))
			return builder.literal("0.0", type);
		if(mgr.getLangBasic().isReal16(type))
			return builder.literal("0.0", type);
	}
	// handle booleans initialization
	if (mgr.getLangBasic().isBool(type)) {
		// boolean values are initialized to false
		return builder.literal("false", mgr.getLangBasic().getBool());
	}


	// FIXME: All types should have a default initialitation to undefined value
//	if (mgr.getLangBasic().isPrimitive(type)){
//		return builder.callExpr(mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(type));
//	}

	// handle strings initializationvalDec
	if (mgr.getLangBasic().isString(type)) {
		return builder.literal("", type);
	}

	// resolve symbol


	// Initialization for volatile types
	if (core::analysis::isVolatileType(type)) {
		return builder.callExpr(mgr.getLangBasic().getVolatileMake(),
				defaultInitVal(core::analysis::getVolatileType(type)));
	}

	core::TypePtr curType = type;
	if (type->getNodeType() == core::NT_RecType) {
		curType = type.as<core::RecTypePtr>()->unroll();
	}

	// Handle structs initialization
	if ( curType.isa<core::StructTypePtr>()) {
		return builder.getZero(type);
	}

	// Handle unions initialization
	if ( curType.isa<core::UnionTypePtr>()) {
		return builder.getZero(type);
	}

	// handle vectors initialization
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(curType)) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		return builder.callExpr(vecTy,
				mgr.getLangBasic().getVectorInitUniform(),
				initVal,
				builder.getIntTypeParamLiteral(vecTy->getSize())
		);
	}

	// handle any-ref initialization
	if (mgr.getLangBasic().isAnyRef(type)) {
		return mgr.getLangBasic().getRefNull();
	}

	assert(core::analysis::isRefType(curType) && "We cannot initialize any different type of non-ref");

	core::RefTypePtr refType = curType.as<core::RefTypePtr>();

	// handle arrays initialization
	if ( core::ArrayTypePtr&& arrTy = core::dynamic_pointer_cast<const core::ArrayType>(refType->getElementType())) {
		return builder.getZero(arrTy);
	}

	// handle refs initialization
	// initialize pointer/reference types with undefined
	core::TypePtr elementType = refType->getElementType();

	core::ExpressionPtr initValue;
	if (elementType->getNodeType() == core::NT_RefType) {
		// ref<ref<...>> => this is a pointer, init with 0 (null)
		initValue = builder.callExpr(elementType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elementType));
	} else {
		initValue = defaultInitVal(elementType);
	}
	return builder.refVar(initValue);


	// LOG(ERROR) << "Default initializer for type: '" << *type << "' not supported!";
	// assert(false && "Default initialization type not defined");
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::convertVarDecl(const clang::VarDecl* varDecl) {
	// logging
	VLOG(1)	<< "\n****************************************************************************************\n"
			<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
			<< utils::location(varDecl->getLocation(), getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)	<< "Dump of clang VarDecl: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::StatementPtr retStmt;
	if ( const VarDecl* definition = varDecl->getDefinition()) {
		// lookup for the variable in the map
		core::ExpressionPtr var = lookUpVariable(definition);
		printDiagnosis(definition->getLocStart());

		//TODO we should visit only staticlocal!!! change to isStaticLocal
		//if (definition->hasGlobalStorage()) {
		if (definition->isStaticLocal()) {
			// is the declaration of a variable with global storage, this means that is an static
			// static needs to be initialized during first execution of function.
			// but the var remains in the global storage (is an assigment instead of decl)
			//
			assert(var);
			assert(var.isa<core::CallExprPtr>());

			// the variable is being unwrapped by default in lookupVariable
			// we want the inner static object
			auto lit = var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>();

			if (definition->getInit()) {
				auto initIr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(),
							var->getType().as<core::RefTypePtr>().getElementType(), false);

				auto call = initIr.isa<core::CallExprPtr>();
				if(call && call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->isConstructor()) {
					//this can also be done by substituting the first param of ctor by the unwrapped static var
					initIr = builder.deref(initIr);
				}
				retStmt = builder.initStaticVariable(lit, initIr);
			} else {
				retStmt = builder.getNoOp();
			}
		}
		else{
			bool isConstant = false;
			// print diagnosis messages
			assert(var.isa<core::VariablePtr>());
			core::TypePtr initExprType;
			if(var->getType().isa<core::RefTypePtr>())
				initExprType = var->getType().as<core::RefTypePtr>()->getElementType();
			else if (IS_CPP_REF(var->getType()))
				initExprType = var->getType();
			 else{
				// is a constant variable (left side is not ref, right side does not need to create refvar)
				// const char name[] = "constant string";
				// vector<char,16> vX = "constatn string"
				isConstant = true;
				initExprType = var->getType();
			}
			assert( initExprType );

			// initialization value
			core::ExpressionPtr initExpr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(), initExprType, false);
			assert(initExpr && "not correct initialization of the variable");

			// some Cpp cases do not create new var
			if (!IS_CPP_REF(var->getType()) && !isCppConstructor(initExpr) && !isConstant){
				initExpr = builder.refVar(initExpr);
			}

			// finnaly create the var initialization
			retStmt = builder.declarationStmt(var.as<core::VariablePtr>(), initExpr);
		}
	} else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");
	}

	VLOG(2)	<< "End of converting VarDecl";
	VLOG(1)	<< "Converted into IR stmt: ";
	VLOG(1)	<< "\t" << *retStmt;
	return retStmt;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::attachFuncAnnotations(const core::ExpressionPtr& node, const clang::FunctionDecl* funcDecl) {
// ----------------------------------- Add annotations to this function -------------------------------------------
// check Attributes of the function definition
	annotations::ocl::BaseAnnotation::AnnotationList kernelAnnotation;

	if (funcDecl->hasAttrs()) {
		const clang::AttrVec attrVec = funcDecl->getAttrs();

		for (AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
			if (AnnotateAttr * attr = dyn_cast<AnnotateAttr>(*I)) {
				//get annotate string
				llvm::StringRef&& sr = attr->getAnnotation();

				//check if it is an OpenCL kernel function
				if ( sr == "__kernel" ) {
					VLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<annotations::ocl::KernelFctAnnotation>() );
				}
			}
			else if ( ReqdWorkGroupSizeAttr* attr = dyn_cast<ReqdWorkGroupSizeAttr>(*I) ) {
				kernelAnnotation.push_back(
						std::make_shared<annotations::ocl::WorkGroupSizeAnnotation>( attr->getXDim(), attr->getYDim(), attr->getZDim() )
				);
			}
		}
	}

	pragma::attachPragma(node,funcDecl,*this).as<core::StatementPtr>();

// -------------------------------------------------- C NAME ------------------------------------------------------

// check for overloaded operator "function" (normal function has kind OO_None)
	clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
	if (operatorKind != OO_None) {
		string operatorAsString = boost::lexical_cast<string>(operatorKind);
		core::annotations::attachName(node,("operator" + operatorAsString));
	} else if( !funcDecl->getNameAsString().empty() ) {
		// annotate with the C name of the function
		core::annotations::attachName(node,(funcDecl->getNameAsString()));
	}

// ---------------------------------------- SourceLocation Annotation ---------------------------------------------
	/*
	 * for each entry function being converted we register the location where it was originally defined in the C program
	 */
	std::pair<SourceLocation, SourceLocation> loc { funcDecl->getLocStart(), funcDecl->getLocEnd() };
	fe::pragma::PragmaStmtMap::DeclMap::const_iterator fit = pragmaMap.getDeclarationMap().find(funcDecl);

	if (fit != pragmaMap.getDeclarationMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

	node->addAnnotation(
			std::make_shared < annotations::c::CLocAnnotation
					> (convertClangSrcLoc(getSourceManager(), loc.first), convertClangSrcLoc(
							getSourceManager(), loc.second)));

// ---------------------------------------------------- OPENCL ----------------------------------------------------
// if OpenCL related annotations have been found, create OclBaseAnnotation and add it to the funciton's attribute
	if (!kernelAnnotation.empty()) {
		// create new marker node
		core::MarkerExprPtr&& marker = builder.markerExpr(node);
		marker->addAnnotation( std::make_shared<annotations::ocl::BaseAnnotation>(kernelAnnotation) );
		return marker;
	}

	return node;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr
Converter::convertInitExpr(const clang::Type* clangType, const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit)  {
	core::ExpressionPtr retIr;

	// if there is no initialization expression
	if (!expr) {

		// extract kind
		core::NodeType kind = type->getNodeType();

		if (type.isa<core::RefTypePtr>()) {
			return (zeroInit)?builder.getZero(type):
				              builder.undefined(type);
		}

		// If the type of this declaration is translated as a array type then it may also include
		// C99 variable array declaration where the size of the array is encoded into the type. This
		// is not supported by the IR type system therefore we have to catch the situation and
		// allocate the correct amount of memory
		if (kind == core::NT_ArrayType && clangType && llvm::isa<clang::VariableArrayType>(clangType)) {
			// get the size
			auto size = convertExpr(llvm::dyn_cast<clang::VariableArrayType>(clangType)->getSizeExpr());
			auto arrType = type.as<core::ArrayTypePtr>();

			return retIr =
				builder.callExpr(type, mgr.getLangBasic().getArrayCreate1D(),
					builder.getTypeLiteral(arrType->getElementType()), builder.castExpr(mgr.getLangBasic().getUInt8(), size)
			);
		}

		// if no init expression is provided => use zero or undefined value
		return retIr = zeroInit ? builder.getZero(type) : builder.undefined(type);
	}

	auto initExpr = convertExpr(expr);
	// Convert the expression like any other expression
 	return getInitExpr ( type, initExpr);
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr Converter::convertExpr(const clang::Expr* expr) const {
       assert(expr && "Calling convertExpr with a NULL pointer");
       return exprConvPtr->Visit(const_cast<Expr*>(expr));
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr Converter::convertStmt(const clang::Stmt* stmt) const {
       assert(stmt && "Calling convertStmt with a NULL pointer");
       return stmtutils::tryAggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));

}
/////////////////////////////////////////////////////////////////
//
core::FunctionTypePtr Converter::convertFunctionType(const clang::FunctionDecl* funcDecl){
	const clang::Type* type= GET_TYPE_PTR(funcDecl);
	core::FunctionTypePtr funcType = convertType(type).as<core::FunctionTypePtr>();

	// check whether it is actually a member function
	core::TypePtr ownerClassType;
	core::FunctionKind funcKind;
	if (const auto* decl = llvm::dyn_cast<clang::CXXConstructorDecl>(funcDecl)) {
		funcKind = core::FK_CONSTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXDestructorDecl>(funcDecl)) {
		funcKind = core::FK_DESTRUCTOR;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else if (const auto* decl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)) {
		funcKind = core::FK_MEMBER_FUNCTION;
		ownerClassType = convertType(decl->getParent()->getTypeForDecl());
	} else {
		// it is not a member function => just take the plain function
		assert(funcType->isPlain());
		return funcType;
	}

	core::TypePtr thisType = builder.refType(ownerClassType);

	// update return type
	core::TypePtr returnType;
	switch (funcKind){
		case core::FK_MEMBER_FUNCTION:
			returnType = funcType.getReturnType();
			break;
		case core::FK_CONSTRUCTOR:
		case core::FK_DESTRUCTOR:
			returnType = thisType;
			break;
		default:
			assert(false && "invalid state!");
			break;
	}

	// update function type
	core::TypeList params;
	params.push_back(thisType);
	for(auto cur : funcType->getParameterTypes()) {
		params.push_back(cur);
	}

	// build resulting function type
	return builder.functionType(params, returnType, funcKind);
}

//////////////////////////////////////////////////////////////////
//
core::TypePtr Converter::convertType(const clang::Type* type) {
	assert(type && "Calling convertType with a NULL pointer");
	return typeConvPtr->convert( type);
}

namespace {

	core::StatementPtr prepentInitializerList(const clang::CXXConstructorDecl* ctorDecl, const core::TypePtr& classType, const core::StatementPtr& body, Converter& converter) {
		auto& mgr = body.getNodeManager();
		core::IRBuilder builder(mgr);

		// nameless/anonymous structs/unions result in non-generic classtype
		// structs unions with name should be generic types
		assert( ( ctorDecl->getParent()->getName().empty() ||
				(!ctorDecl->getParent()->getName().empty() && classType.isa<core::GenericTypePtr>())) && "for convenion, this literal must keep the generic type");

		core::StatementList initList;
		for(auto it = ctorDecl->init_begin(); it != ctorDecl->init_end(); ++it) {

			core::StringValuePtr ident;
			core::ExpressionPtr expr;
			core::ExpressionPtr init;

			core::StatementPtr  initStmt;

			if((*it)->isBaseInitializer ()){

				expr = converter.convertExpr((*it)->getInit());
				init = builder.literal("this", builder.refType(classType));

				if(!insieme::core::analysis::isConstructorCall(expr)) {
					// base init is a non-userdefined-default-ctor call, drop it
					continue;
				}

				// if the expr is a constructor then we are initializing a member an object,
				// we have to substitute first argument on constructor by the
				core::CallExprAddress addr = core::CallExprAddress(expr.as<core::CallExprPtr>());
				expr = core::transform::replaceNode (mgr, addr->getArgument(0), init).as<core::CallExprPtr>();
				initList.push_back (expr);
			}
			else if ((*it)->isMemberInitializer ()){

				// construct the member access based on the type and the init expression
				core::TypePtr membTy = converter.convertType((*it)->getMember()->getType().getTypePtr());
				core::LiteralPtr genThis = builder.literal("this", builder.refType (classType));
				expr = converter.convertExpr((*it)->getInit());
				ident = builder.stringValue(((*it)->getMember()->getNameAsString()));
				init =  builder.callExpr (builder.refType(membTy),
										  builder.getLangBasic().getCompositeRefElem(), genThis,
										  builder.getIdentifierLiteral(ident), builder.getTypeLiteral(membTy));

				// parameter is some kind of cpp ref, but we want to use the value, unwrap it
				if (!IS_CPP_REF(init.getType().as<core::RefTypePtr>()->getElementType()) &&
					IS_CPP_REF(expr->getType())){
					expr = builder.deref(builder.toIRRef(expr));
				}
				// parameter is NOT cpp_ref but left hand side is -> wrap into cppref
				else if(IS_CPP_REF(init.getType().as<core::RefTypePtr>()->getElementType()) &&
					!IS_CPP_REF(expr->getType())) {

					if(core::analysis::isCppRef(init.getType().as<core::RefTypePtr>()->getElementType())) {
						expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), expr);
					} else if(core::analysis::isConstCppRef(init.getType().as<core::RefTypePtr>()->getElementType())){
						expr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), expr);
					} else { assert(false); }
				}
				else{
						expr = builder.tryDeref(expr);
				}
				initList.push_back(builder.assign( init, expr));
			}
			if ((*it)->isIndirectMemberInitializer ()){
				assert(false && "indirect init not implemented");
			}
			if ((*it)->isInClassMemberInitializer ()){
				assert(false && "in class member not implemented");
			}
			if ((*it)->isDelegatingInitializer ()){
				assert(false && "delegating init not implemented");
			}
			if ((*it)->isPackExpansion () ){
				assert(false && "pack expansion not implemented");
			}
		}

		// check whether there is something to do
		if (initList.empty()) return body;


		//ATTENTION: this will produce an extra compound around the  initializer list and old body
		// let fun ... {
		//   { intializer stuff };
		//   { original body };
		// }

		// build new
		return builder.compoundStmt(
				builder.compoundStmt(initList),
				body
		);
	}
}

//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
core::ExpressionPtr Converter::convertFunctionDecl(const clang::FunctionDecl* funcDecl) {

	VLOG(1) << "======================== FUNC: "<< funcDecl->getNameAsString() << " ==================================";

	// switch to the declaration containing the body (if there is one)
	funcDecl->hasBody(funcDecl); // yes, right, this one has the side effect of updating funcDecl!!

	// obtain function type
	auto funcTy = convertFunctionType(funcDecl);

	// check whether function has already been converted
	auto pos = lambdaExprCache.find(funcDecl);
	if (pos != lambdaExprCache.end()) {
		return pos->second;		// done
	}

	// check whether function should be intersected
	if( getProgram().getInterceptor().isIntercepted(funcDecl) ) {
		auto irExpr = getProgram().getInterceptor().intercept(funcDecl, *this);
		lambdaExprCache[funcDecl] = irExpr;
		VLOG(2) << "\tintercepted: " << irExpr;
		return irExpr;
	}

	// handle pure virtual functions
	if( funcDecl->isPure() && llvm::isa<clang::CXXMethodDecl>(funcDecl)){
		VLOG(2) << "\tpure virtual function " << funcDecl;

		std::string callName = funcDecl->getNameAsString();
		core::ExpressionPtr retExpr = builder.literal(callName, funcTy);

		VLOG(2) << retExpr << " " << retExpr.getType();
		lambdaExprCache[funcDecl] = retExpr;
		return retExpr;
	}

	// handle external functions
	if(!funcDecl->hasBody()) {
		// TODO: move this to the interceptor
		if (funcDecl->getNameAsString() == "free") {
			//handle special function -- "free" -- here instead of in CallExr
			auto retExpr = builder.getLangBasic().getRefDelete();

			// handle issue with typing of free when not including stdlib.h
			core::FunctionTypePtr freeTy = typeCache[GET_TYPE_PTR(funcDecl)].as<core::FunctionTypePtr>();
			typeCache[GET_TYPE_PTR(funcDecl)]=  builder.functionType(freeTy->getParameterTypeList(), builder.getLangBasic().getUnit());

			lambdaExprCache[funcDecl] = retExpr;
			return retExpr;
		}

		// handle extern functions
		auto retExpr = builder.literal(utils::buildNameForFunction(funcDecl), funcTy);

		// attach header file info
		utils::addHeaderForDecl(retExpr, funcDecl, program.getStdLibDirs(), program.getUserIncludeDirs());
		lambdaExprCache[funcDecl] = retExpr;
		return retExpr;
	}

	// --------------- convert potential recursive function -------------

	// -- assume function is recursive => add variable to lambda expr cache --
	core::LiteralPtr symbol = builder.literal(funcTy, utils::buildNameForFunction(funcDecl));
	assert(lambdaExprCache.find(funcDecl) == lambdaExprCache.end());
	lambdaExprCache[funcDecl] = symbol;

	// -- conduct the conversion of the lambda --
	core::LambdaExprPtr lambda;
	{
		assert(funcDecl->hasBody() && "At this point function should have a body!");

		// init parameter set
		vector<core::VariablePtr> params;
		std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [&](ParmVarDecl* currParam) {
			params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
		});

		// convert function body
		//   - set up context to contain current list of parameters and convert body
		Converter::ParameterList oldList = curParameter;
		curParameter = &params;

		core::StatementPtr body = convertStmt( funcDecl->getBody() );
		curParameter = oldList;

		// add initializer list
		if (funcTy->isConstructor()) {
			body = prepentInitializerList(llvm::cast<clang::CXXConstructorDecl>(funcDecl), funcTy->getObjectType(), body, *this);
		}

		// some cases value parameters have to be materialized in the
		// body of the function, to be able to written.
		body =  materializeReadOnlyParams(body,params);

		// handle potential this pointer
		if (funcTy->isMember()) {
			auto thisType = funcTy->getParameterTypes()[0];

			// add this as a parameter
			auto thisVar = builder.variable(thisType);
			params.insert(params.begin(), thisVar);

			// handle this references in body,
			body = core::transform::replaceAllGen (mgr, body, builder.literal("this", thisType), thisVar);
		}

		// build the resulting lambda
		lambda = builder.lambdaExpr(funcTy, params, body);
		VLOG(2) << lambda << " + function declaration: " << funcDecl;
	}


	// update cache
	assert_eq(lambdaExprCache[funcDecl], symbol) << "Don't touch this!";

	// finally, add some sugar
	attachFuncAnnotations(lambda, funcDecl);

	// if the conversion is complete
	lambdaExprCache[funcDecl] = symbol;

	// register function within resulting translation unit
	getIRTranslationUnit().addFunction(symbol, lambda);

	// annotate and return results
	return symbol;
}

//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
core::ExpressionPtr Converter::getInitExpr (const core::TypePtr& type, const core::ExpressionPtr& init){

	// null expression is allowed on globals initializations
	if (!init) return init;

	core::TypePtr elementType = lookupTypeDetails(type);
	if (core::encoder::isListType(init->getType())) {
		core::ExpressionPtr retIr;
		vector<core::ExpressionPtr> inits = core::encoder::toValue<vector<core::ExpressionPtr>>(init);

		// if recursive
		assert(!elementType.isa<core::RecTypePtr>() && "we dont work with recursive types in the frontend, only gen types");

		if ( core::lang::isSIMDVector(elementType) )  {
			auto internalVecTy = core::lang::getSIMDVectorType(elementType);
			auto membTy = internalVecTy.as<core::SingleElementTypePtr>()->getElementType();
			//TODO MOVE INTO SOME BUILDER HELPER
			auto initOp = mgr.getLangExtension<core::lang::SIMDVectorExtension>().getSIMDInitPartial();
			ExpressionList elements;
			// get all values of the init expression
			for (size_t i = 0; i < inits.size(); ++i) {
				elements.push_back(getInitExpr(membTy, inits[i] ));
			}

			return builder.callExpr(
					elementType,
					initOp,
					core::encoder::toIR(type->getNodeManager(), elements),
					builder.getIntTypeParamLiteral(internalVecTy->getSize()));

		}

		// if array or vector
		if (  elementType.isa<core::VectorTypePtr>() || elementType.isa<core::ArrayTypePtr>()) {
			auto membTy = elementType.as<core::SingleElementTypePtr>()->getElementType();
			ExpressionList elements;
			// get all values of the init expression
			for (size_t i = 0; i < inits.size(); ++i) {
				elements.push_back(getInitExpr(membTy, inits[i] ));
			}
			retIr = builder.vectorExpr(elements);

			// if the sizes dont fit is a partial initialization
			if (elementType.isa<core::VectorTypePtr>() &&
				*retIr->getType().as<core::VectorTypePtr>()->getSize() !=
				*elementType.isa<core::VectorTypePtr>()->getSize())
				return builder.callExpr(
						builder.getLangBasic().getVectorInitPartial(),
						core::encoder::toIR(type->getNodeManager(), elements),
						builder.getIntTypeParamLiteral(elementType.isa<core::VectorTypePtr>()->getSize())
					);

			return retIr;
		}

		// if struct
		if ( core::StructTypePtr&& structTy = elementType.isa<core::StructTypePtr>() ) {
			core::StructExpr::Members members;
			for (size_t i = 0; i < inits.size(); ++i) {
				const core::NamedTypePtr& curr = structTy->getEntries()[i];
				members.push_back(builder.namedValue(
							curr->getName(),
							getInitExpr (curr->getType(), inits[i])));
			}
			retIr = builder.structExpr(structTy, members);
			return retIr;
		}


		// any other case (unions may not find a list of expressions, there is an spetial encoding)
		assert(false && "fallthrow");
	}

	if ( core::UnionTypePtr unionTy = elementType.isa<core::UnionTypePtr>() ) {

		// here is the thing, the field comes hiden in a literal (the function called)
		// and the expression is the first paramenter
		if (init.as<core::CallExprPtr>()->getFunctionExpr().isa<core::LiteralPtr>()){
			core::StringValuePtr name =  init.as<core::CallExprPtr>()->getFunctionExpr().as<core::LiteralPtr>()->getValue();
			core::TypePtr entityType;
			for (unsigned i = 0; i < unionTy->getEntries().size(); ++i)
				if (*unionTy->getEntries()[i]->getName() == *name)
					entityType = unionTy->getEntries()[0]->getType();
			assert(entityType && "the type of the entity could not be found");
			return  builder.unionExpr(unionTy, name,getInitExpr(entityType, init.as<core::CallExprPtr>()[0]));
		}
		// it might be that is an empy initialization, retrieve the type to avoid nested variable creation
		return init.as<core::CallExprPtr>()[0];
	}

	// the initialization is not a list anymore, this a base case
	//if types match, we are done
	if(core::types::isSubTypeOf(lookupTypeDetails(init->getType()), elementType)) return init;

	////////////////////////////////////////////////////////////////////////////
	// if the type missmatch we might need to take some things in consideration:

	// constructor
	if (isCppConstructor(init)) return init;

	if (init->getType().isa<core::RefTypePtr>() &&
		core::types::isSubTypeOf(lookupTypeDetails(init->getType().as<core::RefTypePtr>()->getElementType()), elementType)) {
		return builder.deref(init);
	}

	if (core::analysis::isCppRef(elementType) && init->getType().isa<core::RefTypePtr>())
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), init);
	if (core::analysis::isConstCppRef(elementType) && init->getType().isa<core::RefTypePtr>())
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), init);
	if (core::analysis::isConstCppRef(elementType) && core::analysis::isCppRef(init->getType()))
		return builder.callExpr (mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp(), init);

	if (IS_CPP_REF(init->getType())){
		if (elementType.isa<core::RefTypePtr>())
			return builder.toIRRef(init);
		else
			return builder.deref(builder.toIRRef(init));  // might be a call by value to a function, and we need to derref
	}


	if (builder.getLangBasic().isAny(elementType) ) return init;

	if (builder.getLangBasic().isPrimitive (elementType) && builder.getLangBasic().isPrimitive(init->getType()))
		return utils::castScalar(elementType, init);

	if ( elementType.isa<core::VectorTypePtr>() ){
		core::ExpressionPtr initVal = init;
		if (utils::isRefVector(init->getType()))
			initVal =  builder.deref(initVal);
		//it can be a partial initialization
		if(core::types::isSubTypeOf(initVal->getType(), elementType))
			return initVal;

		return utils::cast( initVal, elementType);
	}

    //FIXME: check if this is enough
    //or if we need further checks
	if (core::analysis::isVolatileType(elementType)) {
		if(!core::analysis::isVolatileType(init->getType())) {
			return builder.makeVolatile(init);
		}
		return init;
	}
/*
    //if lhs and rhs are struct type we only have
    //to check if the types are equal
    if (init->getType().isa<core::StructTypePtr>() && elementType.isa<core::StructTypePtr>()) {
        //if (core::types::isSubTypeOf(lookupTypeDetails(init->getType()), elementType))
            return init;
    }
*/

    // the case of enum type initializations
    if(mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(init->getType())) {
        return utils::castScalar(elementType, init);
    }

	// the case of the Null pointer:
	if (core::analysis::isCallOf(init, builder.getLangBasic().getRefReinterpret()))
		return builder.refReinterpret(init.as<core::CallExprPtr>()[0], elementType.as<core::RefTypePtr>()->getElementType());

	if (utils::isRefArray(init->getType()) && utils::isRefArray(type)){
		return builder.refReinterpret(init, type.as<core::RefTypePtr>()->getElementType());
	}

	std::cerr << "initialization fails: \n\t" << init << " : " << init->getType() << std::endl;
	std::cerr << "type details: \n\t" << lookupTypeDetails(init->getType()) << std::endl;
	std::cerr << "\t target: " << type << std::endl;

	assert(false && " fallthrow");
	return init;
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace


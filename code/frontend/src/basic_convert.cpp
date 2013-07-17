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
#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/frontend/utils/source_locations.h"
//
//#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/castTool.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/indexer.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/header_tagger.h"
#include "insieme/frontend/utils/ir_utils.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/static_vars.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/dump/text_dump.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/ocl/ocl_annotations.h"
#include <clang/AST/CXXInheritance.h>
#include <clang/AST/StmtVisitor.h>

using namespace clang;
using namespace insieme;

// NOTE: no one can deal with the translation unit ANYWHERE out of the basic_converter.
#define SET_TU(X) \
		auto old_translation_unit = currTU; \
		currTU = getTranslationUnitForDefinition(X);

#define RESTORE_TU(X) \
		currTU = old_translation_unit;


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


} // End empty namespace
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						C CONVERSION FACTORY
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//////////////////////////////////////////////////////////////////
///
const insieme::frontend::TranslationUnit* ConversionFactory::getTranslationUnitForDefinition(const FunctionDecl*& funcDecl) {
	
	
	if(const clang::FunctionDecl* fd = llvm::dyn_cast<clang::FunctionDecl>(funcDecl)) {
		switch( fd->getTemplatedKind() ) {
			case clang::FunctionDecl::TemplatedKind::TK_NonTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplate:
				break;
			case clang::FunctionDecl::TemplatedKind::TK_MemberSpecialization:
			case clang::FunctionDecl::TemplatedKind::TK_FunctionTemplateSpecialization:
				return currTU;
			case clang::FunctionDecl::TemplatedKind::TK_DependentFunctionTemplateSpecialization:
				break;
		}
	}

	// if the function is not defined in this translation unit, maybe it is defined in another we already
	// loaded use the clang indexer to lookup the definition for this function declarations
	utils::Indexer::TranslationUnitPair&& ret =
			program.getIndexer().getDefAndTUforDefinition (funcDecl);

	// function declaration not found. return the current translation unit
	if ( !ret.first ) {return currTU;}
	assert(ret.second && "translation unit not found");

	// update the funcDecl pointer to point to the correct function declaration
	funcDecl = llvm::cast<FunctionDecl> ( ret.first);

	return ret.second;
}

//////////////////////////////////////////////////////////////////
///
ConversionFactory::ConversionFactory(core::NodeManager& mgr, Program& prog, bool isCpp) :
		mgr(mgr), builder(mgr),
		// cppcheck-suppress exceptNew
		program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end())
		{

		if (isCpp){
			typeConvPtr = std::make_shared<CXXTypeConverter>(*this);
			exprConvPtr = std::make_shared<CXXExprConverter>(*this, prog);
			stmtConvPtr = std::make_shared<CXXStmtConverter>(*this);
		} else{
			typeConvPtr = std::make_shared<CTypeConverter>(*this);
			exprConvPtr = std::make_shared<CExprConverter>(*this, prog);
			stmtConvPtr = std::make_shared<CStmtConverter>(*this);
		}

}

//////////////////////////////////////////////////////////////////
///
//void ConversionFactory::buildGlobalStruct(analysis::GlobalVarCollector& globColl){

	////~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//// Thread private requires to collect all the variables which are marked to be threadprivate
	//omp::collectThreadPrivate(getPragmaMap(), ctx.thread_private);

	////~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	////Omp flush clause forces the flushed variable to be volatile
	////omp::collectVolatile(getPragmaMap(), ctx.volatiles);
	////~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	//ctx.globalStruct = globColl.createGlobalStruct();
	//if (ctx.globalStruct.first) {
	//	ctx.globalVar = builder.variable(builder.refType(ctx.globalStruct.first));
	//}

	//ctx.globalIdentMap = globColl.getIdentifierMap();
	//ctx.globalFuncSet = globColl.getUsingGlobals();

	//VLOG(1) << "globals collected";
	//VLOG(2) << ctx.globalStruct.first;
	//VLOG(2) << ctx.globalStruct.second;
	//VLOG(2) << ctx.globalVar;
//}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr ConversionFactory::materializeReadOnlyParams(const core::StatementPtr& body, const vector<core::VariablePtr>& params){

	vector<core::StatementPtr> decls;
	core::StatementPtr newBody = body;

	for (auto currParam : params){
		auto fit = this->ctx.wrapRefMap.find(currParam);
		if ( fit != this->ctx.wrapRefMap.end() ) {

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
				ctx.wrapRefMap.erase(currParam);
			}
			else{

				// FIXME:  structs pased as value will be wrapped ANYWAY...
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
void ConversionFactory::printDiagnosis(const clang::SourceLocation& loc){

	// TODO: warnings intoduced by INSIEME are not print because some 
	// source location issues, debug and fix this.
	//    --  loop iterator thing
	//    -- constancy of member functions (which one to call when two)
/*	clang::Preprocessor& pp = getCurrentPreprocessor();
	// print warnings and errors:
	while (!ctx.warnings.empty()){

		if (getCurrentSourceManager().isLoadedSourceLocation (loc)){
			std::cerr << "loaded location:\n";
			std::cerr << "\t" << *ctx.warnings.begin() << std::endl;
		}
		else{
			pp.Diag(loc, pp.getDiagnostics().getCustomDiagID(DiagnosticsEngine::Warning, *ctx.warnings.begin()) );
		}
		ctx.warnings.erase(ctx.warnings.begin());
	}
	*/
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::tryDeref(const core::ExpressionPtr& expr) const {
	// core::ExpressionPtr retExpr = expr;
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(expr->getType())) {
		return builder.callExpr(refTy->getElementType(), mgr.getLangBasic().getRefDeref(), expr);
	}
	return expr;
}

//////////////////////////////////////////////////////////////////
///
core::TypePtr ConversionFactory::tryDeref(const core::TypePtr& type) const {
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return refTy->getElementType();
	}
	return type;
}

//////////////////////////////////////////////////////////////////
///
// Register call expression handlers to be used during the clang to IR conversion
//void ConversionFactory::registerCallExprHandler(const clang::FunctionDecl* funcDecl, CustomFunctionHandler& handler) {
//	auto it = callExprHanlders.insert( std::make_pair(funcDecl, handler) );
//	assert( !it.second && "Handler for function declaration already registered." );
//}
//  Function to convert Clang attributes of declarations to IR annotations (local version) currently used for:
// 	-> OpenCL address spaces
core::NodeAnnotationPtr ConversionFactory::convertAttribute(const clang::ValueDecl* varDecl) const {
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
				currTU->getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::lookUpVariable(const clang::ValueDecl* valDecl) {
	VLOG(1) << "LOOKUP Variable: " << valDecl->getNameAsString();
	if (VLOG_IS_ON(1)) valDecl->dump();

	// Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	auto varCacheHit = ctx.varDeclMap.find(valDecl);
	if (varCacheHit != ctx.varDeclMap.end()) {
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

	bool isOclVector = !!dyn_cast<const ExtVectorType>(varTy->getUnqualifiedDesugaredType());
	if (!(varTy.isConstQualified() ||    						// is a constant
		 varTy.getTypePtr()->isReferenceType()  ||             // is a c++ reference
 	 	 (isa<const clang::ParmVarDecl>(valDecl) && 			// is the declaration of a parameter
		 ((irType->getNodeType() != core::NT_VectorType && irType->getNodeType() != core::NT_ArrayType) ||
		   isOclVector ) ))) {
		// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
		// only exception are OpenCL vectors
		irType = builder.refType(irType);
	}

	// if is a global variable, a literal will be generated, with the qualified name 
	// (two qualified names can not coexist within the same TU)
	const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);
	if (varDecl && varDecl->hasGlobalStorage()) {
		VLOG(2)	<< "with global storage";
		// we could look for it in the cache, but is fast to create a new one, and we can not get
		// rid if the qualified name function
		std::string name = program.getGlobalCollector().getName(varDecl);

		// global/static variables are always leftsides (refType) -- solves problem with const
		if(!irType.isa<core::RefTypePtr>() ) {
			irType = builder.refType(irType);
		}

		if (program.getGlobalCollector().isStatic(varDecl)){
			if (!irType.isa<core::RefTypePtr>()) irType = builder.refType(irType);		// this happens whenever a static variable is constant
			irType = builder.refType (mgr.getLangExtension<core::lang::StaticVariableExtension>().wrapStaticType(irType.as<core::RefTypePtr>().getElementType()));
		}

		core::ExpressionPtr globVar =  builder.literal(name, irType);
		if (program.getGlobalCollector().isExtern(varDecl)){
			globVar =  builder.literal(varDecl->getQualifiedNameAsString(), globVar->getType());
		 	annotations::c::markExtern(globVar.as<core::LiteralPtr>());
		}

		if (program.getGlobalCollector().isStatic(varDecl)){
			globVar = builder.accessStatic(globVar.as<core::LiteralPtr>());
		}

		// OMP threadPrivate
 		if (insieme::utils::set::contains (ctx.thread_private, varDecl)){
			omp::addThreadPrivateAnnotation(globVar);
		}

		ctx.varDeclMap.insert( { valDecl, globVar } );
		return globVar;
	}

	// The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
	// the IR variable and insert it into the map for future lookups
	core::VariablePtr&& var = builder.variable( irType );
	VLOG(2) << "IR variable" << var.getType()->getNodeType() << "" << var<<":"<<varDecl->getNameAsString();
	VLOG(2) << "IR var type" << var.getType();

	ctx.varDeclMap.insert( { valDecl, var } );

	if ( !valDecl->getNameAsString().empty() ) {
		// Add the C name of this variable as annotation
		var->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (valDecl->getNameAsString()));
	}

	// Add OpenCL attributes
	insieme::core::NodeAnnotationPtr&& attr = convertAttribute(valDecl);
	if (attr) {
		var->addAnnotation(attr);
	}

	return var;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::defaultInitVal(const core::TypePtr& type) const {
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
		return builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(arrTy));
	}

	// handle refs initialization
	// initialize pointer/reference types with undefined
	core::TypePtr elemType = refType->getElementType();

	core::ExpressionPtr initValue;
	if (elemType->getNodeType() == core::NT_RefType) {
		// ref<ref<...>> => this is a pointer, init with 0 (null)
		initValue = builder.callExpr(elemType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elemType));
	} else {
		initValue = defaultInitVal(elemType);
	}
	return builder.refVar(initValue);


	// LOG(ERROR) << "Default initializer for type: '" << *type << "' not supported!";
	// assert(false && "Default initialization type not defined");
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {
	// logging
	VLOG(1)	<< "\n****************************************************************************************\n"
			<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
			<< utils::location(varDecl->getLocation(), getCurrentSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)	<< "Dump of clang VarDecl: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::StatementPtr retStmt;
	if ( const VarDecl* definition = varDecl->getDefinition()) {
		// lookup for the variable in the map
		core::ExpressionPtr&& var = lookUpVariable(definition);
		printDiagnosis(definition->getLocStart());

		if (definition->hasGlobalStorage()) {
			// is the declaration of a variable with global storage, this means that is an static 
			// static needs to be initialized during first execution of function.
			// but the var remains in the global storage (is an assigment instead of decl)
			//
			assert(var);
			assert(var.isa<core::CallExprPtr>());
			// the variable is being unwrapped by default in lookupVariable
			// we want the inner static object
			auto lit = var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>();

			if (definition->getInit())
				retStmt = builder.initStaticVariable(lit, convertInitExpr(definition->getType().getTypePtr(), 
																		  definition->getInit(), 
																		  var->getType().as<core::RefTypePtr>().getElementType(), false));
			else
				retStmt = builder.getNoOp();
		}
		else{
			// print diagnosis messages
			assert(var.isa<core::VariablePtr>());
			// initialization value
			core::ExpressionPtr&& initExpr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(), var->getType(), false);
			// this assertion is not valid for void& initialization
			//	ASSERT_EQ_TYPES (var->getType(), initExpr->getType());
			assert(initExpr && "not correct initialization of the variable");
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
core::ExpressionPtr ConversionFactory::attachFuncAnnotations(const core::ExpressionPtr& node,
		const clang::FunctionDecl* funcDecl) {
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
		node->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > ("operator" + operatorAsString));
	} else if ( dyn_cast<CXXDestructorDecl>(funcDecl) ) {
		node->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > ("__dtor_"+funcDecl->getNameAsString().substr(1)));
	} else {
		// annotate with the C name of the function
		node->addAnnotation(std::make_shared < annotations::c::CNameAnnotation > (funcDecl->getNameAsString()));
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
					> (convertClangSrcLoc(getCurrentSourceManager(), loc.first), convertClangSrcLoc(
							getCurrentSourceManager(), loc.second)));

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
/// InitListExpr describes an initializer list, which can be used to initialize objects of different
/// types, InitListExpr including struct/class/union types, arrays, and vectors. For example:
/// struct foo x = { 1, { 2, 3 } };
/// In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the
/// type of the LHS expression.
core::ExpressionPtr
ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type)  {
	const ConversionFactory& convFact = *this;
	core::ExpressionPtr retIr;

//	ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_EXPR_CONVERSION(initList, retIr);

	core::TypePtr currType = type;

	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type)) {
		currType = refType->getElementType();
	}

	// Handles recursive types. Unroll once in order to reveal the actual type (hopefully it will be
	// a struct type)
	if (currType->getNodeType() == core::NT_RecType) {
		currType = currType.as<core::RecTypePtr>()->unroll();
	}

	if (currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType) {

		auto elemTy = currType.as<core::SingleElementTypePtr>()->getElementType();

		ExpressionList elements;
		// get all values of the init expression
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
			const clang::Expr* subExpr = initList->getInit(i);

			auto convExpr = convertInitExpr(NULL /*FIXME*/, subExpr, elemTy, false);

			assert(convExpr && "convExpr is empty");

			elements.push_back(utils::cast(convExpr, elemTy));
		}

		retIr = builder.vectorExpr(elements);
	}


	// in the case the initexpr is used to initialize a struct/class we need to create a structExpr
	// to initialize the structure
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType) ) {

		core::StructExpr::Members members;
		for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {

			const core::NamedTypePtr& curr = structTy->getEntries()[i];
			members.push_back(builder.namedValue(
						curr->getName(),
						convertInitExpr(NULL, initList->getInit(i), curr->getType(), false))
				);
		}
		retIr = builder.structExpr(structTy, members);
	}

	// in the case the initexpr is used to initialize a union
	//
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(currType)) {

		auto ie = convertInitExpr(NULL, initList->getInit(0), unionTy->getEntries()[0]->getType(), false);
		retIr = builder.unionExpr(unionTy, unionTy->getEntries()[0]->getName(), ie);

	//	core::StructExpr::Members members;
	//	for (size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
	//		const core::NamedTypePtr& curr = structTy->getEntries()[i];
	//		members.push_back(
	//				builder.namedValue(curr->getName(), convertInitExpr(initList->getInit(i), curr->getType(), false)));
	//	}
	//	retIr = builder.structExpr(members);
	}

	assert(retIr && "Couldn't convert initialization expression");

	// create vector initializator
	return retIr;
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr
ConversionFactory::convertInitExpr(const clang::Type* clangType, const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit)  {
	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	//FIXME: LOG isn't possible because of non existence of convFact
	//LOG_EXPR_CONVERSION(retIr);

	// get kind of initialized value
	core::NodeType&& kind =
		(type->getNodeType() != core::NT_RefType ? type->getNodeType() : GET_REF_ELEM_TYPE(type)->getNodeType() );

	// if there is no initialization expression
	if (!expr) {

		// If the type of this declaration is translated as a array type then it may also include
		// C99 variable array declaration where the size of the array is encoded into the type. This
		// is not supported by the IR type system therefore we have to catch the situation and
		// allocate the correct amount of memory
		if (kind == core::NT_ArrayType && clangType && llvm::isa<clang::VariableArrayType>(clangType)) {
			// get the size
			auto size = convertExpr(llvm::dyn_cast<clang::VariableArrayType>(clangType)->getSizeExpr());
			auto arrType = GET_REF_ELEM_TYPE(type).as<core::ArrayTypePtr>();

			return retIr = builder.refVar(
				builder.callExpr(GET_REF_ELEM_TYPE(type), mgr.getLangBasic().getArrayCreate1D(),
					builder.getTypeLiteral(arrType->getElementType()), builder.castExpr(mgr.getLangBasic().getUInt8(), size)
				)
			);
		}

		// if no init expression is provided => use undefined for given set of types
		if (kind == core::NT_StructType ||
			kind == core::NT_UnionType  ||
			kind == core::NT_ArrayType  ||
			kind == core::NT_VectorType)
		{
			if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
				const core::TypePtr& res = refTy->getElementType();

				return retIr = builder.refVar((zeroInit?builder.getZero(res):builder.undefined(res)));
			}

			return retIr = zeroInit ? builder.getZero(type) : builder.undefined(type);

		} else {
			return retIr = defaultInitVal(type);
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		retIr = utils::cast( convertInitializerList(listExpr, type), type);
		return retIr;
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	// ============================================================================================
	// =============================== Handling of special cases  =================================
	// ============================================================================================

	if( core::analysis::isConstructorCall(retIr)){
		return retIr;
	}

	// if is a constructor call, we are done
	if (llvm::isa<clang::CXXConstructExpr>(expr) && retIr.isa<core::CallExprPtr>()){		// here you might even check whether it is a constructor call in the IR
		return retIr;
	}

	// If this is an initialization of an array using array.create (meaning it was originally a
	// malloc) then we expliticly invoke the ref.new to allocate the memory on the heap
	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		return retIr = builder.refNew(retIr);
	}

	// In the case the object we need to initialize is a ref<array...> then we are not allowed to
	// deref the actual initializer, therefore we assign the object as it is
	if ( utils::isRefArray(retIr->getType()) && utils::isRefArray(type ) ) {
		return retIr = utils::cast(retIr, type);
	}

	// If we have a string literal as initializer and we need to assign it to a ref<array<...>> we
	// can directly cast it using the ref.vector.to.ref.array and perform the assignment. We do not
	// need to create a copy of the object in the right hand side
	if ( utils::isRefVector(retIr->getType()) && retIr->getNodeType() == core::NT_Literal &&
		 utils::isRefArray(type ) ) {
		return retIr = utils::cast(retIr, type);
	}

	// this is a C++ reference ( int& ref = x)
	if (clangType && clangType->isReferenceType()){
		core::TypePtr targetType = this->convertType(clangType);
		core::TypePtr srcType = retIr->getType();

		// if is a CPP ref, convert to IR
		if (core::analysis::isCppRef(srcType)) {
			// same type, just assign
			if (srcType == targetType && *srcType == *targetType){
				return retIr;
			}
			else {
				// we can not assign a a const ref to a ref
				assert(false && "we can not assign this, what is this?");
			}

		}
		else if (core::analysis::isConstCppRef(srcType)) {

			// if we are here, we are initializing a const ref from a const ref or from a ref
			if (core::analysis::isCppRef(targetType)) {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefCppToConstCpp());
			}
			else {
				return retIr;
			}
		}
		else{
			//this reference is initialized with a variable
			if (core::analysis::isCppRef(targetType)) {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToCpp(), retIr);
			}
			else {
				return builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getRefIRToConstCpp(), retIr);
			}
		}
	}

	// FIXME: if this is needed, maybe need to add a var to create a ref
//	// inner expression is null<int<X>> and outer is array, then rebuild something like
//	//  ref.null(type<array<...>>)
//	if (core::analysis::isCallOf( retIr, mgr.getLangBasic().getGetNull())){
//		return builder.deref(builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(type)));
//	}

	// ============================== End Special Handlings =======================================

	// Anytime we have to initialize a ref<'a> from another type of object we have to deref the
	// object in the right hand side and create a copy (ref.var).
	if (type->getNodeType() == core::NT_RefType ) {
		retIr = utils::cast(retIr, GET_REF_ELEM_TYPE(type));
		retIr = builder.refVar(retIr);
	} else {
		retIr = utils::cast(retIr, type);
	}

	assert(retIr);
	return retIr;
}

//
////////////////////////////////////////////////////////////////////
///// the globalVar parameter is added at the FIRST position of the function parameters
//core::FunctionTypePtr ConversionFactory::addGlobalsToFunctionType( const core::FunctionTypePtr& funcType) {
//       const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();
//       std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);
//       std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin() + 1);
//       // function is receiving a reference to the global struct as the first argument
//       argTypes[0] = builder.refType(ctx.globalStruct.first);
//       return builder.functionType(argTypes, funcType->getReturnType());
//}


//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
       assert(expr && "Calling convertExpr with a NULL pointer");
       return exprConvPtr->Visit(const_cast<Expr*>(expr));
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt* stmt) const {
       assert(currTU && "translation unit is null");
       assert(stmt && "Calling convertStmt with a NULL pointer");
       return stmtutils::tryAggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));

}
/////////////////////////////////////////////////////////////////
//
core::FunctionTypePtr ConversionFactory::convertFunctionType(const clang::FunctionDecl* funcDecl){
	SET_TU(funcDecl);
	const clang::Type* type= GET_TYPE_PTR(funcDecl);
	core::FunctionTypePtr funcType = convertType(type).as<core::FunctionTypePtr>();
//	if (!ignoreGlobals && ctx.globalFuncSet.find(funcDecl) != ctx.globalFuncSet.end() ) {
		//funcType = addGlobalsToFunctionType(funcType);
	//}
	RESTORE_TU();
	return funcType;
}

//////////////////////////////////////////////////////////////////
//
core::TypePtr ConversionFactory::convertType(const clang::Type* type) {
	assert(type && "Calling convertType with a NULL pointer");
	auto fit = ctx.typeCache.find(type);
	if(fit == ctx.typeCache.end()) {
		core::TypePtr&& retTy = typeConvPtr->convert( type );
		ctx.typeCache.insert( {type, retTy} );
		return retTy;
	}
	return fit->second;
}

//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {
	VLOG(1) << "======================== FUNC: "<< funcDecl->getNameAsString() << " ==================================";
	SET_TU(funcDecl);
	// check if the funcDecl was already converted into an lambdaExpr, before asserting that
	// funcDecl has a body -> intercepted functions may have no body
	ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(funcDecl);
	if (fit != ctx.lambdaExprCache.end()) {
		RESTORE_TU();
		return fit->second;
	}

	// FIXME: find a better place for this (where this fun is called)
	if (isEntryPoint){
		omp::collectThreadPrivate(getPragmaMap(), ctx.thread_private);
	}

	//intercept functionDecls here
	if( getProgram().getInterceptor().isIntercepted(funcDecl) ) {
		auto irExpr = getProgram().getInterceptor().intercept(funcDecl, *this);
		ctx.lambdaExprCache.insert( {funcDecl, irExpr} );
		RESTORE_TU();
		VLOG(2) << "\tintercepted: " << irExpr;
		return irExpr;
	}

	//TODO check/get definitionAndTU for declareation here (bernhard)

	if( funcDecl->isPure() && llvm::isa<clang::CXXMethodDecl>(funcDecl)){
		VLOG(2) << "\tpure virtual function " << funcDecl;

		auto funcTy = convertFunctionType(funcDecl);
		std::string callName = funcDecl->getNameAsString();
		core::ExpressionPtr retExpr = builder.literal(callName, funcTy);

		//const clang::CXXMethodDecl* methodDecl = llvm::cast<clang::CXXMethodDecl>(funcDecl);
		//const clang::Type* recordType = methodDecl->getParent()->getTypeForDecl();
		//auto classType =  convertType (recordType);
		//retExpr = memberize(funcDecl, retExpr, builder.refType(classType), core::FK_MEMBER_FUNCTION);
		VLOG(2) << retExpr << " " << retExpr.getType();

		RESTORE_TU();
		return retExpr;
	}

	if(!funcDecl->hasBody()) {
		core::ExpressionPtr retExpr;
		if (funcDecl->getNameAsString() == "free") {
			//handle special function -- "free" -- here instead of in CallExr
			retExpr = builder.getLangBasic().getRefDelete();
		}
		else {
			//handle extern functions  -- here instead of in CallExr
			auto funcTy = convertFunctionType(funcDecl).as<core::FunctionTypePtr>();
			std::string callName = funcDecl->getNameAsString();
			retExpr = builder.literal(callName, funcTy);

			// attach header file info
			utils::addHeaderForDecl(retExpr, funcDecl, program.getStdLibDirs());
		}
		RESTORE_TU();
		return retExpr;
	}

	assert(currTU && "currTU not set");
	assert(funcDecl->hasBody() && "Function has no body!");

	VLOG(1) << "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;
	VLOG(1) << "#----------------------------------------------------------------------------------#";
	VLOG(1)
		<< "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl << "-> at location: ("
				<< utils::location(funcDecl->getSourceRange().getBegin(), getCurrentSourceManager())
				<< "): " << std::endl << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
				<< "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl
				<< "\tRec var map: " << ctx.recVarExprMap.size() << " :" << ctx.recVarExprMap;


	if (ctx.isResolvingRecFuncBody) {
  		// check if this type has a typevar already associated, in such case return it
  		ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(funcDecl);

  		if (fit != ctx.recVarExprMap.end()) {
  			// we are resolving a parent recursive type, so when one of the recursive functions in the
  			// connected components are called, the introduced mu variable has to be used instead.
			RESTORE_TU();
  			return fit->second;
  		}
	}

	// retrieve the strongly connected components for this function
	// It is strongly connected or strong if it contains a directed path from u to v and a directed
	// path from v to u for every pair of vertices u, v.
	// FIXME:: we have a problem here with BOOST
	std::cout << funcDecl->getQualifiedNameAsString() << std::endl;
	std::set<const FunctionDecl*>&& components = program.getCallGraph().getStronglyConnectedComponents( funcDecl );
	if (!components.empty()) {
		std::set<const FunctionDecl*>&& subComponents = program.getCallGraph().getSubComponents( funcDecl );
		for (const auto& cur: subComponents){

			const FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
			VLOG(2) << "Analyzing FuncDecl as sub component: " << decl->getNameAsString();
			SET_TU(decl);

			if ( currTU && !isa<CXXConstructorDecl>(decl) ) { // not for constructors

				// look up the lambda cache to see if this function has been
				// already converted into an IR lambda expression.
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(decl);
				if ( fit == ctx.lambdaExprCache.end() ) {
					// perform the conversion only if this is the first time this
					// function is encountred
					convertFunctionDecl(decl, false);
					ctx.recVarExprMap.clear();
				}
			}
			// reset the translation unit
			RESTORE_TU();
		}

		// we are dealing with a recursive type
		VLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
				<< "Number of components in the cycle: " << components.size();

		for (std::set<const FunctionDecl*>::value_type c : components){
			VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
		}

		// Creates the variable which should be used as a placeholder for invoking the given
		// function call and isert it in the map (recVarExprMap) used to store such variables
		// which are valid during the conversion of the given recursive function cycle
		auto createRecVar = [&] (const clang::FunctionDecl* funDecl) {
			if (ctx.recVarExprMap.find(funDecl) != ctx.recVarExprMap.end()) { return; }

			// we create a TypeVar for each type in the mutual dependence
			core::FunctionTypePtr funcType = convertFunctionType(funDecl);

			// it might be a member function, fix the type
			if (const clang::CXXMethodDecl* mem = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)){
				const clang::Type* ownerTy = (llvm::cast<clang::TypeDecl> (mem->getParent()))->getTypeForDecl();
				auto l = funcType->getParameterTypeList();
				l.insert(l.begin(), builder.refType(convertType(ownerTy)));
				funcType = builder.functionType(l, funcType->getReturnType(), core::FK_MEMBER_FUNCTION);
			}
			core::VariablePtr&& var = builder.variable( funcType );
			ctx.recVarExprMap.insert( { funDecl, var } );
		};


		if (!ctx.isRecSubFunc) {
			createRecVar(funcDecl);
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert( {funcDecl, ctx.currVar} );
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if (!ctx.isRecSubFunc) {

			for( const auto& fd : components) { createRecVar(fd); }
		}
		if (VLOG_IS_ON(2)) {
			VLOG(2) << "MAP: ";
			for(auto c : ctx.recVarExprMap){
				VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "] " << c.second << " " << c.second->getType();
			}

		}
	} // endif function call components

	// init parameter set
	vector<core::VariablePtr> params;

	// before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	// global struct or not
	core::VariablePtr parentGlobalVar = ctx.globalVar;



//	if (!isEntryPoint && ctx.globalFuncSet.find(funcDecl) != ctx.globalFuncSet.end()) {
//		// declare a new variable that will be used to hold a reference to the global data stucture
//		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
//		ctx.globalVar = var;
//		params.push_back(var);
//	}
//
	std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [ &params, this ] (ParmVarDecl* currParam) {
		params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
	});

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert((components.empty() || (!components.empty() && !ctx.isResolvingRecFuncBody))
			&& "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");

	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = true;
	}

	VLOG(2) << "Visiting function body!";

	// set up context to contain current list of parameters and convert body
	ConversionContext::ParameterList oldList = ctx.curParameter;
	ctx.curParameter = &params;

	if (VLOG_IS_ON(2)) {
		VLOG(2) << "Dump of stmt body: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		funcDecl->getBody()->dump();
	}

	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	ctx.curParameter = oldList;

	// some cases value parameters have to be materialized in the
	// body of the function, to be able to writte on them.
	// by default well materialize all paramenters
	body =  materializeReadOnlyParams(body,params);


	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = false;
	}

//	// ADD THE GLOBALS
//	if (isEntryPoint && ctx.globalVar) {
//		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
//		assert(ctx.globalVar && ctx.globalStruct.second);
//
//		const StatementList& oldStmts = compStmt->getStatements();
//
//		std::vector<core::StatementPtr> stmts;
//
//		stmts = std::vector<core::StatementPtr>(oldStmts.size() + 1);
//		stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew(ctx.globalStruct.second));
//		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin() + 1);
//
//		body = builder.compoundStmt(stmts);
//	}

	core::FunctionTypePtr funcType = convertFunctionType(funcDecl).as<core::FunctionTypePtr>();

	// reset old global var
	ctx.globalVar = parentGlobalVar;

	VLOG(2)	<< "function type: " << funcType << "\nparams: " << params << "\n" << body;

	if (components.empty()) {

		core::LambdaExprPtr retLambdaExpr = builder.lambdaExpr(funcType, params, body);

		// Adding the lambda function to the list of converted functions
		assert( (funcDecl == program.getIndexer().getDefinitionFor(funcDecl)) && "wrong function declaration in lambdaExprCache");
		ctx.lambdaExprCache.insert( { funcDecl, retLambdaExpr} );

		VLOG(2) << retLambdaExpr << " + function declaration: " << funcDecl;
		auto func =  attachFuncAnnotations(retLambdaExpr, funcDecl);
		RESTORE_TU();
		return func;
	}

	//////////////////////////////////////////////////////////////
	// ahead this point a recursive lambda expression is generated
	// out of the lambdas enclosed. those have being bounded to
	// variables, but now need to be converted

	core::LambdaPtr retLambdaNode;

	// member functions must use the first parameter as THIS
	if (const clang::CXXMethodDecl* metDcl = llvm::dyn_cast<clang::CXXMethodDecl>(funcDecl)){

		const clang::Type* ownerClangTy = (llvm::cast<clang::TypeDecl> (metDcl->getParent()))->getTypeForDecl();
		auto ownerTy = builder.refType(convertType(ownerClangTy));
		auto thisVar = builder.variable(ownerTy);
		params.insert(params.begin(), thisVar);
		core::FunctionTypePtr newFunctionType = builder.functionType(extractTypes(params), funcType->getReturnType(), core::FK_MEMBER_FUNCTION);
		core::LiteralPtr thisLit =  builder.literal("this", ownerTy);
		body = core::transform::replaceAllGen (mgr, body, thisLit, thisVar, true);
		retLambdaNode = builder.lambda( newFunctionType, params, body );
	}
	else{
		retLambdaNode = builder.lambda( funcType, params, body );
	}

	// this is a recursive function call
	// if we are visiting a nested recursive type it means someone else will take care of building the rectype
	// node, we just return an intermediate type
	if (ctx.isRecSubFunc) {
		RESTORE_TU();
		return retLambdaNode;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	vector<core::LambdaBindingPtr> definitions;
	definitions.push_back(builder.lambdaBinding(recVarRef, retLambdaNode));

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

 	for(auto fd : components) {
		ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(fd);
		assert(tit != ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
		ctx.currVar = tit->second;

		// test whether function has already been resolved
		if (*tit->second == *recVarRef) {
			continue;
		}

		// we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
		// for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
		// is enabled only when the isRecSubType flag is true
		ctx.recVarExprMap.erase(fd);

		const core::LambdaPtr& lambda = convertFunctionDecl(fd).as<core::LambdaPtr>();

		assert(lambda && "Resolution of sub recursive lambda yields a wrong result");

		/// === CXX ===
		// it might be that is a member and needs to be memberized
		if (llvm::isa<clang::CXXMethodDecl>(fd)){
			//FIXME: ask the master how to make this easyer

			vector<core::LambdaBindingPtr> tmpDef;
			tmpDef.push_back( builder.lambdaBinding(ctx.currVar, lambda) );
			core::LambdaExprPtr&& tmpExpr = builder.lambdaExpr(lambda);
			const clang::Type* ownerTy = (llvm::cast<clang::TypeDecl> (llvm::cast<clang::CXXMethodDecl>(fd)->getParent()))->getTypeForDecl();
			core::ExpressionPtr funcExpr = memberize (fd, tmpExpr.as<core::ExpressionPtr>(),
													  builder.refType(convertType( ownerTy)), core::FK_MEMBER_FUNCTION).as<core::ExpressionPtr>();

			ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(fd);
			definitions.push_back( builder.lambdaBinding(tit->second, funcExpr.as<core::LambdaExprPtr>()->getLambda() ));
		}
		else{ //any other case
			definitions.push_back( builder.lambdaBinding(ctx.currVar, lambda) );
		}
		// reinsert the TypeVar in the map in order to solve the other recursive types
		ctx.recVarExprMap.insert( {fd, ctx.currVar} );
	}

	// we reset the behavior of the solver
	ctx.currVar = NULL;
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& lambdaDef = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, lambdaDef);

	// Adding the lambda function to the list of converted functions
	assert( (funcDecl == program.getIndexer().getDefinitionFor(funcDecl)) && "wrong function declaration in lambdaExprCache");
	ctx.lambdaExprCache.insert( {funcDecl, retLambdaExpr} );

	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	for(const auto& fd : components) {

		auto fit = ctx.recVarExprMap.find(fd);
		assert(fit != ctx.recVarExprMap.end());

		const FunctionDecl* decl = const_cast<FunctionDecl*>(fd);

		// update the translation unit
		SET_TU(decl);

		core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, lambdaDef);

		assert( (decl== program.getIndexer().getDefinitionFor(decl)) && "wrong function declaration in lambdaExprCache");
		ctx.lambdaExprCache.insert( {decl, func} );

		func = attachFuncAnnotations(func, decl);

		// restore TU
		RESTORE_TU();
	}

	// Clear the variables so that when we resolve the recursive function the actual recursive
	// lambda is utilized
	ctx.recVarExprMap.clear();

	VLOG(2) << "Converted Into: " << *retLambdaExpr;
	// attachFuncAnnotations(retLambdaExpr, funcDecl);

	RESTORE_TU();
	return retLambdaExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							CXX STUFF
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

////////////////////////////////////////////////////////////////////////////////
//
core::NodePtr  ConversionFactory::memberize (const clang::FunctionDecl* funcDecl,
												   core::ExpressionPtr func,
												   core::TypePtr ownerClassType,
											   	   core::FunctionKind funcKind){

	// is a lambda?  for rec member functions
	// is a lambdaExpr?

	core::FunctionTypePtr funcTy = func.getType().as<core::FunctionTypePtr>();

	// NOTE: has being already memberized???
	if (funcTy.isMemberFunction() ||
		funcTy.isConstructor() ||
		funcTy.isDestructor() ){
		return func;
	}

	// return type depends on type of function
	core::TypePtr retTy;
	switch (funcKind){
		case core::FK_MEMBER_FUNCTION:
			retTy = funcTy.getReturnType();
			break;
		case core::FK_CONSTRUCTOR:
		case core::FK_DESTRUCTOR:  //FIXME: what type returns a destructor???
			retTy = ownerClassType;
			break;
		default:
			assert(false && "not implemented");
	}
	//FIXME NEEDS FURTHER REFACTORING

	if(func.isa<core::LambdaExprPtr>()) {
		SET_TU(funcDecl);

		// update parameter list with a class-typed parameter in the first possition
		core::LambdaExprPtr lambdaExpr = func.as<core::LambdaExprPtr>();
		auto params = lambdaExpr->getParameterList();
		auto thisVar = builder.variable(ownerClassType);
		core::VariableList paramList = params.getElements();
		paramList.insert(paramList.begin(), thisVar);

		// build the new functiontype
		core::FunctionTypePtr newFunctionType = builder.functionType(extractTypes(paramList), retTy, funcKind);

		// every usage of this has being defined as a literal "this" typed alike the class
		// substute every usage of this with the right variable
		core::StatementPtr body = lambdaExpr->getBody();
		core::LiteralPtr thisLit =  builder.literal("this", ownerClassType);
		core::StatementPtr newBody = core::transform::replaceAllGen (mgr, body, thisLit, thisVar, true);

		// build the member function
		core::LambdaExprPtr memberized =  builder.lambdaExpr (newFunctionType, paramList, newBody);

		// cache it
		ctx.lambdaExprCache.erase(funcDecl);
		ctx.lambdaExprCache[funcDecl] = memberized;

		RESTORE_TU();
		return memberized;
	} else if(func.isa<core::LiteralPtr>()) {  // literals, for intercepted funcs
		SET_TU(funcDecl);

		//only literals -- used for intercepted/pureVirtual functions
		core::LiteralPtr funcLiteral = func.as<core::LiteralPtr>();

		// update parameter list with a class-typed parameter in the first possition
		core::TypeList paramTys = funcTy->getParameterTypeList();
		paramTys.insert(paramTys.begin(), ownerClassType);

		// build the new functiontype
		auto newFunctionType = builder.functionType(paramTys, retTy, funcKind);

		core::ExpressionPtr retExpr = builder.literal(funcLiteral.getStringValue(), newFunctionType);

		//assert(false && "not properly implement currently");
		RESTORE_TU();
		return retExpr;
	} else if (core::VariablePtr var = func.isa<core::VariablePtr>()){ // we deal with a recursive member function.
		// recursive types should be already converted somewhere else
		return var; //core::ExpressionPtr();
	} else {
		assert(false && "something went wrong");
		return core::ExpressionPtr();
	}
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::convertFunctionDecl (const clang::CXXConstructorDecl* ctorDecl){
	const clang::FunctionDecl* ctorAsFunct = llvm::cast<clang::FunctionDecl>(ctorDecl);
	assert(ctorAsFunct);

	VLOG(1) << "======================== CTOR: "<< ctorDecl->getNameAsString() << " ==================================";

	SET_TU(ctorAsFunct);
	assert(currTU && "currTU not set");

	// NOTE: even if we have a correct indexed declaration, this might not have a body (intercepted/extern objs)
	// don't pannic, the declaration must be handled by the upcoming convertFunctionDecl, and there will
	// be nicely intercepted.

	if (!ctorAsFunct){
		RESTORE_TU();
		return core::LambdaExprPtr();
	}

	const clang::Type* recordType = (llvm::cast<clang::TypeDecl> (llvm::cast<clang::CXXMethodDecl>(ctorDecl)->getParent()))->getTypeForDecl();
	core::TypePtr irClassType =  convertType (recordType);

	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	core::ExpressionPtr oldCtor = convertFunctionDecl(ctorAsFunct).as<core::ExpressionPtr>();

	if( !oldCtor.isa<core::LambdaExprPtr>() ) {
		RESTORE_TU();
		return oldCtor;
	}

	core::FunctionTypePtr ty = oldCtor.as<core::LambdaExprPtr>().getType().as<core::FunctionTypePtr>();
	//  has being already memberized??? then is already solved
	if (ty.isMemberFunction() ||
		ty.isConstructor() ||
		ty.isDestructor() ){
		RESTORE_TU();
		return oldCtor.as<core::LambdaExprPtr>();
	}

	// NOTE: this, and other stuff will be handled by memberize
	// -- HERE WE ONLY NEED TO CARE ABOUT INITIALIZATION LIST --

	// generate code for each initialization
	core::StatementList newBody;

	// for each initializer, transform it
	clang::CXXConstructorDecl::init_const_iterator it  = llvm::cast<clang::CXXConstructorDecl>(ctorAsFunct)->init_begin();
	clang::CXXConstructorDecl::init_const_iterator end = llvm::cast<clang::CXXConstructorDecl>(ctorAsFunct)->init_end();
	for(; it != end; it++){

		core::StringValuePtr ident;
		core::StatementPtr initStmt;

		// the translated initialization expression
		core::ExpressionPtr expr;
		// the variable to be initialized
		core::ExpressionPtr init;

		if((*it)->isBaseInitializer ()){

			expr = convertExpr((*it)->getInit());
			init = builder.literal("this", builder.refType(irClassType));

			if(!insieme::core::analysis::isConstructorCall(expr)) {
				// base init is a non-userdefined-default-ctor call, drop it
				continue;
			}
		}
		else if ((*it)->isMemberInitializer ()){
			// create access to the member of the struct/class
			ident = builder.stringValue(((*it)->getMember()->getNameAsString()));

			core::TypePtr memberTy = irClassType.as<core::StructTypePtr>()->getTypeOfMember(ident);
			init = builder.callExpr( builder.refType( memberTy ),
									 gen.getCompositeRefElem(),
									 toVector<core::ExpressionPtr>  (builder.literal("this", builder.refType(irClassType)),
									   								 builder.getIdentifierLiteral(ident),
																	 builder.getTypeLiteral(memberTy) ));

			expr = convertExpr((*it)->getInit());
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

		// if the expr is a constructor then we are initializing a member an object,
		// we have to substitute first argument on constructor by the
		// right reference to the member object (addressed by init)
		//  -> is a call expression of a constructor
		core::ExpressionPtr ptr;
		if (expr.isa<core::CallExprPtr>() &&
			(ptr = expr.as<core::CallExprPtr>().getFunctionExpr()).isa<core::LambdaExprPtr>() &&
			 ptr.as<core::LambdaExprPtr>().getType().as<core::FunctionTypePtr>().isConstructor()){

				// for each of the argumets, if uses a parameter in the paramenter list, avoid the
				// wrap of the variable
				const clang::CXXConstructExpr* ctor= llvm::cast<clang::CXXConstructExpr>((*it)->getInit());
				for (unsigned i=0; i <ctor->getNumArgs ();i++){
					const clang::DeclRefExpr* param= utils::skipSugar<DeclRefExpr> (ctor->getArg(i));
					if (param){
						core::ExpressionPtr tmp = lookUpVariable(param->getDecl());
						core::CallExprAddress addr(expr.as<core::CallExprPtr>());
						expr = core::transform::replaceNode (mgr, addr->getArgument(1+i), tmp).as<core::CallExprPtr>();
					}
				}
				// to end with, replace the "this" placeholder with the right position
				core::CallExprAddress addr(expr.as<core::CallExprPtr>());
				initStmt = core::transform::replaceNode (mgr, addr->getArgument(0), init).as<core::CallExprPtr>();
		}
		else{
			//otherwise is a regular assigment like intialization
			//
			core::ExpressionPtr expr = convertExpr((*it)->getInit());
			initStmt = utils::createSafeAssigment(init,expr);
		}

		// append statement to initialization list
		newBody.push_back(initStmt);
	}
	//ATTENTION: this will produce an extra compound for the initializer list
	// let fun ... {
	//   { intializer stuff };
	//   { original body };
	// }
	core::StatementPtr newb = builder.compoundStmt(newBody);
    newBody.clear();
    newBody.push_back(materializeReadOnlyParams (newb, oldCtor.as<core::LambdaExprPtr>().getLambda().getParameterList()));
	// push original body
	core::StatementPtr body = oldCtor.as<core::LambdaExprPtr>().getBody();
	newBody.push_back(body);

	// NOTE: function type and paramList do not change here
	core::LambdaExprPtr newCtor =  builder.lambdaExpr  (ty,
														oldCtor.as<core::LambdaExprPtr>().getLambda().getParameterList(),
														builder.compoundStmt(newBody));

	RESTORE_TU();
	return newCtor;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							AST CONVERTER
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::CallExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
	mFact.setTranslationUnit(&tu);

	core::StatementPtr bodyStmt = mFact.convertStmt( body );
	auto callExpr = core::transform::outline(mgr, bodyStmt);

	annotations::c::CLocAnnotation::ArgumentList args;
	auto lambdaExpr = callExpr->getFunctionExpr().as<core::LambdaExprPtr>();

	// ------ Adding source location annotation (CLocAnnotation) -------
	std::pair<SourceLocation, SourceLocation> loc = std::make_pair(body->getLocStart(), body->getLocEnd());
	auto fit = mFact.getPragmaMap().getStatementMap().find(body);
	if(fit != mFact.getPragmaMap().getStatementMap().end()) {
		// the statement has a pragma associated with, when we do the rewriting, the pragma needs to be overwritten
		loc.first = fit->second->getStartLocation();
	}

	lambdaExpr.addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.first),
		convertClangSrcLoc(tu.getCompiler().getSourceManager(), loc.second),
		false, // this is not a function decl
		args)
	);

	return callExpr;
}

core::ProgramPtr ASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain /*=false*/) {

	// Handling of the translation unit: we have to make sure to load the translation unit where the function is
	// defined before starting the parser otherwise reading literals results in wrong values.
	const TranslationUnit* rightTU = mFact.getTranslationUnitForDefinition(funcDecl);
	assert(rightTU && "Translation unit for function not found.");
	mFact.setTranslationUnit(rightTU);


	const core::ExpressionPtr& expr = mFact.convertFunctionDecl(funcDecl, true).as<core::ExpressionPtr>();

	core::ExpressionPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>(expr);

	// A marker node is allowed if it contains a lambda expression
	if (!lambdaExpr) {
		lambdaExpr = dynamic_pointer_cast<const core::MarkerExpr>(expr);

		if (lambdaExpr) {
			assert(	static_pointer_cast<const core::MarkerExpr>(expr)->getSubExpression()->getNodeType() == core::NT_LambdaExpr &&
					"Conversion of function returned a marker expression which does not contain a lambda expression");
		}
	}
	assert( lambdaExpr && "Conversion of function did not return a lambda expression");
	
	// globals in the main function, globals might need to be initialized
	if (isMain)
		lambdaExpr = addGlobalsInitialization(lambdaExpr.as<core::LambdaExprPtr>());

	return core::Program::addEntryPoint(mFact.getNodeManager(), mProgram, lambdaExpr /*, isMain */);
}

/////////////////////////////////////////////////////////
//
core::LambdaExprPtr ASTConverter::addGlobalsInitialization(const core::LambdaExprPtr& mainFunc){

	VLOG(1) << "";
	VLOG(1) << "************************************************************************************";
	VLOG(1) << "******************** Initialize Globals at program start ***************************";
	VLOG(1) << "************************************************************************************";
	VLOG(1) << "";

	// 4 casses:
	// extern, do nothing
	// static in some function, we need to initialize the constructor flag
	// with init, assign value
	// without init, do not initialize

	// we only want to init what we use, so we check it
	core::NodeSet usedLiterals;
	core::visitDepthFirstOnce (mainFunc, [&] (const core::LiteralPtr& literal){
				usedLiterals.insert(literal);
			});

	core::IRBuilder builder(mainFunc->getNodeManager());
	core::StatementList inits;

	// ~~~~~~~~~~~~~~~~~~ INITIALIZE GLOBALS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	for (auto git = globalCollector.globalsInitialization_begin(); git != globalCollector.globalsInitialization_end(); ++git){

		if (mFact.getProgram().getInterceptor().isIntercepted(git->second->getQualifiedNameAsString())){
			continue;
		}

		VLOG(2) << "initializing global: " << git->first;

		if(const clang::Expr* init = git->second->getInit()){
			core::LiteralPtr var = builder.literal(git->first, builder.refType(mFact.convertType(git->second->getType().getTypePtr())));
			core::ExpressionPtr initValue;
			//FIXME: why this is not done in the visitor???
			if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( init )) {
				initValue =  mFact.convertInitializerList(listExpr, var->getType());
			}
			else
				initValue = mFact.convertExpr(init);

			if(initValue->getType().isa<core::RefTypePtr>()){
				initValue = utils::cast( initValue, var->getType().as<core::RefTypePtr>().getElementType());
			}
			core::StatementPtr assign = builder.assign (var, initValue);
			inits.push_back(assign);
		}
	}
	
	// ~~~~~~~~~~~~~~~~~~ PREPARE STATICS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~A
	for (auto sit = globalCollector.staticInitialization_begin(); sit != globalCollector.staticInitialization_end(); ++sit){
		VLOG(2) << "initializing static: " << (*sit)->getQualifiedNameAsString();
		core::ExpressionPtr var = mFact.lookUpVariable((*sit));
		core::LiteralPtr litUse = var.isa<core::LiteralPtr>();
		if (!litUse){
			litUse = var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>();
		}
		assert (litUse && " no literal? who handled this global?");

		// no need to touch it if never used
		if (contains(usedLiterals, litUse)){
			inits.push_back(builder.createStaticVariable(var.as<core::CallExprPtr>().getArgument(0).as<core::LiteralPtr>()));
		}
	}

	if (inits.empty())
		return mainFunc;

	return (core::transform::insert ( mainFunc->getNodeManager(), core::LambdaExprAddress(mainFunc)->getBody(), inits, 0)).as<core::LambdaExprPtr>();
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace


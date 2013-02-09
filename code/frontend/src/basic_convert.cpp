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
#include "insieme/frontend/analysis/global_variables.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/error_report.h"
#include "insieme/frontend/cpp/temporary_handler.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/ocl/ocl_compiler.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/map_utils.h"

#include "insieme/utils/timer.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include <clang/Basic/FileManager.h>
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include <clang/AST/ASTConsumer.h>
#include <clang/AST/ASTContext.h>
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/CXXInheritance.h>
#include <clang/AST/StmtVisitor.h>

// [3.0]
//#include "clang/Index/Entity.h"
//#include "clang/Index/Indexer.h"

using namespace clang;
using namespace insieme;

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//   ANONYMOUS NAMESPACE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
namespace {

// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
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
//clang [3.0] const clang::idx::TranslationUnit* ConversionFactory::getTranslationUnitForDefinition(FunctionDecl*& funcDecl) {
//////////////////////////////////////////////////////////////////
///
const insieme::frontend::TranslationUnit* ConversionFactory::getTranslationUnitForDefinition(FunctionDecl*& funcDecl) {

	// if the function is not defined in this translation unit, maybe it is defined in another we already
	// loaded use the clang indexer to lookup the definition for this function declarations
	ConversionFactory::TranslationUnitPair&& ret = 
			program.getIndexer().getDefAndTUforDefinition (funcDecl);

	// function declaration not found. return the current translation unit
	if ( !ret.first ) {return NULL;}
	assert(ret.first && ret.second && "Translation unit for function not found");

	// update the funcDecl pointer to point to the correct function declaration 
	funcDecl = llvm::cast<FunctionDecl> ( ret.first);
	return ret.second;

/* clang [3.0]
	clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, program.getClangProgram());
	ConversionFactory::TranslationUnitPair&& ret = program.getClangIndexer().getDefinitionFor(funcEntity);



	// update the funcDecl pointer to point to the correct function declaration 
	funcDecl = ret.first;
	return ret.second;
	*/
}

//////////////////////////////////////////////////////////////////
///
ConversionFactory::ConversionFactory(core::NodeManager& mgr, Program& prog) :
		mgr(mgr), builder(mgr),
		stmtConvPtr(std::make_shared<CStmtConverter>(*this)),
		typeConvPtr(std::make_shared<CTypeConverter>(*this, prog)),
		exprConvPtr(std::make_shared<CExprConverter>(*this, prog)),
		// cppcheck-suppress exceptNew
		program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()){
}

//////////////////////////////////////////////////////////////////
///
ConversionFactory::ConversionFactory(core::NodeManager& mgr, Program& prog,
	std::shared_ptr<StmtConverter> stmtConvPtr,
	std::shared_ptr<TypeConverter> typeConvPtr,
	std::shared_ptr<ExprConverter> exprConvPtr) :
		mgr(mgr), builder(mgr), 
		stmtConvPtr(stmtConvPtr),
		typeConvPtr(typeConvPtr),
		exprConvPtr(exprConvPtr),
		program(prog), pragmaMap(prog.pragmas_begin(), prog.pragmas_end()) {
}

//////////////////////////////////////////////////////////////////
///
void ConversionFactory::collectGlobalVar(const clang::FunctionDecl* funcDecl) {
	// Extract globals starting from this entry point
	FunctionDecl* def = const_cast<FunctionDecl*>(funcDecl);
 // clang [3.0]	const clang::idx::TranslationUnit* clangTU = getTranslationUnitForDefinition(def);
	const TranslationUnit* clangTU = getTranslationUnitForDefinition(def);

	ctx.globalFuncMap.clear();
	analysis::GlobalVarCollector globColl(*this, clangTU , program.getIndexer(), ctx.globalFuncMap);

	globColl(funcDecl);
	globColl(getProgram().getTranslationUnits());

	VLOG(1) << globColl;

	//~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Thread private requires to collect all the variables which are marked to be threadprivate
	omp::collectThreadPrivate(getPragmaMap(), ctx.thread_private);

	//~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//Omp flush clause forces the flushed variable to be volatile
	//omp::collectVolatile(mFact.getPragmaMap(), mFact.ctx.volatiles);
	//~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	ctx.globalStruct = globColl.createGlobalStruct();
	if (ctx.globalStruct.first) {
		ctx.globalVar = builder.variable(builder.refType(ctx.globalStruct.first));
	}
	ctx.globalIdentMap = globColl.getIdentifierMap();

	VLOG(2) << ctx.globalStruct.first;
	VLOG(2) << ctx.globalStruct.second;
	VLOG(2) << ctx.globalVar;
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
				currTU.top()->getCompiler()
		);
	}
	return std::make_shared < annotations::ocl::BaseAnnotation > (declAnnotation);
}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::lookUpVariable(const clang::ValueDecl* valDecl) {

	// Lookup the map of declared variable to see if the current varDecl is already associated with an IR entity
	ConversionContext::VarDeclMap::const_iterator fit = ctx.varDeclMap.find(valDecl);
	if (fit != ctx.varDeclMap.end()) {
		// variable found in the map
		return fit->second;
	}

	// The variable has not been converted into IR variable yet, therefore we create the IR variable and insert it
	// to the map for successive lookups
	
	// Conversion of the variable type
	QualType&& varTy = valDecl->getType();
	VLOG(2)	<< varTy.getAsString(); // cm

	core::TypePtr&& irType = convertType( varTy.getTypePtr() );


	//// check wether the variable is marked to be volatile 
	if (varTy.isVolatileQualified()) {
		irType = builder.volatileType(irType);
	}

	bool isOclVector = !!dyn_cast<const ExtVectorType>(varTy->getUnqualifiedDesugaredType());
	if (!(varTy.isConstQualified()
			|| (isa<const clang::ParmVarDecl>(valDecl) && ((irType->getNodeType() != core::NT_VectorType
					&& irType->getNodeType() != core::NT_ArrayType) || isOclVector ) ))) {
		// if the variable is not const, or a function parameter or an array type we enclose it in a ref type
		// only exception are OpenCL vectors
		irType = builder.refType(irType);
	}

	// Check whether this is variable is defined as global or static. If static, it means the variable has been already
	// defined in the global data structure so we don't have to create an IR variable but access (via the memberAccess
	// operation) the relative member of the global data structure.
	const clang::VarDecl* varDecl = cast<clang::VarDecl>(valDecl);
	if (varDecl && varDecl->hasGlobalStorage()) {
		assert( ctx.globalVar && "Accessing global variable within a function not receiving the global struct");
		// access the global data structure
		const core::lang::BasicGenerator& gen = builder.getLangBasic();

		auto&& fit = ctx.globalIdentMap.find(varDecl);


		VLOG(2) << "  " << llvm::cast<clang::NamedDecl>(varDecl)->getNameAsString() << " "<< ctx.globalIdentMap.size();
		VLOG(2) << "  " << varDecl;
		VLOG(2) << "  " << ctx.globalIdentMap;
		assert( fit != ctx.globalIdentMap.end() && "Variable not within global identifiers");

		// LOG(DEBUG) << *fit;

		const core::TypePtr& memberTy = ctx.globalStruct.first->getTypeOfMember(fit->second);
		assert( memberTy && "Member not found within global struct");
		assert( ctx.globalVar->getType()->getNodeType() == core::NT_RefType && 
			    "Global data structure passed as a non-ref");

		core::ExpressionPtr&& retExpr = builder.callExpr(
				builder.refType( memberTy ),
				gen.getCompositeRefElem(),
				toVector<core::ExpressionPtr>(
						ctx.globalVar, builder.getIdentifierLiteral(fit->second), builder.getTypeLiteral(memberTy)
				)
		);

		auto&& vit = std::find(ctx.thread_private.begin(), ctx.thread_private.end(), varDecl);
		if (vit != ctx.thread_private.end()) {
			omp::addThreadPrivateAnnotation(retExpr);
		}

		return utils::cast(retExpr, irType);
	}

	// The variable is not in the map and not defined as global (or static) therefore we proceed with the creation of
	// the IR variable and insert it into the map for future lookups
	core::VariablePtr&& var = builder.variable( irType );
	VLOG(2) << "IR variable" << var.getType()->getNodeType() << "" << var<<":"<<varDecl->getNameAsString();

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
	//if ( mgr.getLangBasic().isAnyRef(type) ) {
	//return mgr.getLangBasic().getNull();
	//}
	// handle integers initialization
	
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
		return builder.literal("0.0", type);
	}
	// handle strings initializationvalDec
	if (mgr.getLangBasic().isString(type)) {
		return builder.literal("", type);
	}

	// handle booleans initialization
	if (mgr.getLangBasic().isBool(type)) {
		// boolean values are initialized to false
		return builder.literal("false", mgr.getLangBasic().getBool());
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
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(curType)) {
		return builder.callExpr(structTy, mgr.getLangBasic().getInitZero(), builder.getTypeLiteral(structTy));
	}

	// Handle unions initialization
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(curType)) {
		assert(unionTy);
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
		return mgr.getLangBasic().getNull();
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
core::DeclarationStmtPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {
	assert(!currTU.empty() && "translation unit is null");
	// logging
	VLOG(1)	<< "\n****************************************************************************************\n"
			<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
			<< utils::location(varDecl->getLocation(), currTU.top()->getCompiler().getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)	<< "Dump of clang VarDecl: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::DeclarationStmtPtr retStmt;
	assert(!currTU.empty() && "translation unit is null");
	if ( const VarDecl* definition = varDecl->getDefinition()) {

		if (definition->hasGlobalStorage()) {
			// once we encounter static variables we do remove the declaration
			throw GlobalVariableDeclarationException();
		}

		// lookup for the variable in the map
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(lookUpVariable(definition));

		assert(!currTU.empty() && "translation unit is null");
		assert(var);

		// initialization value
		core::ExpressionPtr&& initExpr = convertInitExpr(definition->getType().getTypePtr(), definition->getInit(), var->getType(), false);
		assert(initExpr && "not correct initialization of the variable");

		retStmt = builder.declarationStmt(var, initExpr);
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

	assert(!currTU.empty() && "Translation unit not correctly set");
	node->addAnnotation(
			std::make_shared < annotations::c::CLocAnnotation
					> (convertClangSrcLoc(currTU.top()->getCompiler().getSourceManager(), loc.first), convertClangSrcLoc(
							currTU.top()->getCompiler().getSourceManager(), loc.second)));

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
ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
	const ConversionFactory& convFact = *this;
	START_LOG_EXPR_CONVERSION(initList);

	core::ExpressionPtr retIr;

//	ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_EXPR_CONVERSION(retIr);

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
		retIr = builder.structExpr(members);
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
ConversionFactory::convertInitExpr(const clang::Type* clangType, const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit) const {

	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_EXPR_CONVERSION(retIr);

	// get kind of initialized value
	core::NodeType&& kind =
		(type->getNodeType() != core::NT_RefType ? type->getNodeType() : GET_REF_ELEM_TYPE(type)->getNodeType() );

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

				return retIr = builder.refVar(
						builder.callExpr(res,
								(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
								builder.getTypeLiteral(res)));
			}
			return retIr = builder.callExpr(type,
					(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
					builder.getTypeLiteral(type));
		} else {
			return retIr = defaultInitVal(type);
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		return retIr = utils::cast( convertInitializerList(listExpr, type), type);
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	// ============================================================================================
	// =============================== Handling of special cases  =================================
	// ============================================================================================
	
	// If this is an initialization of an array using array.create (meaning it was originally a
	// malloc) then we expliticly invoke the ref.new to allocate the memory on the heap 
	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		return retIr = builder.refNew(retIr);
	}

	// In the case the object we need to initialize is a ref<array...> then we are not allowed to
	// deref the actual initializer, therefore we assign the object as it is 
	if ( utils::isRefArray(retIr->getType()) && utils::isRefArray(type ) ) 
	{
		return retIr = utils::cast(retIr, type);
	}

	// If we have a string literal as initializer and we need to assign it to a ref<array<...>> we
	// can directly cast it using the ref.vector.to.ref.array and perform the assignment. We do not
	// need to create a copy of the object in the right hand side 
	if ( utils::isRefVector(retIr->getType()) && retIr->getNodeType() == core::NT_Literal &&
		 utils::isRefArray(type ) ) 
	{
		return retIr = utils::cast(retIr, type);
	}
	// ============================== End Special Handlings =======================================
	
	// Anythime we have to initialize a ref<'a> from another type of object we have to deref the
	// object in the right hand side and create a copy (ref.var). 
	if (type->getNodeType() == core::NT_RefType ) {
		retIr = builder.refVar(utils::cast(retIr, GET_REF_ELEM_TYPE(type)));
	} else {
		retIr = utils::cast(retIr, type);
	}

	assert(retIr);

	return retIr;
}

//////////////////////////////////////////////////////////////////
/// the globalVar parameter is added at the FIRST position of the function parameters
core::FunctionTypePtr ConversionFactory::addGlobalsToFunctionType(const core::IRBuilder& builder,
		const core::TypePtr& globals, const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin() + 1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType(argTypes, funcType->getReturnType());

}

//////////////////////////////////////////////////////////////////
///
core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return exprConvPtr->Visit(const_cast<Expr*>(expr));
}

//////////////////////////////////////////////////////////////////
///
core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt* stmt) const {
	assert(!currTU.empty() && "translation unit is null");
	assert(stmt && "Calling convertStmt with a NULL pointer");
	return stmtutils::tryAggregateStmts(builder, stmtConvPtr->Visit(const_cast<Stmt*>(stmt)));
}

//////////////////////////////////////////////////////////////////
///
core::TypePtr ConversionFactory::convertType(const clang::Type* type) {
	assert(type && "Calling convertType with a NULL pointer");
	auto fit = ctx.typeCache.find(type);
	if(fit == ctx.typeCache.end()) {
		core::TypePtr&& retTy = typeConvPtr->Visit( const_cast<Type*>(type) );
		ctx.typeCache.insert( {type, retTy} );
		return retTy;
	}

	return fit->second;
}

//////////////////////////////////////////////////////////////////
///  CONVERT FUNCTION DECLARATION
core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {

	assert(!currTU.empty() && funcDecl->hasBody() && "Function has no body!");

	VLOG(1) << "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;

	VLOG(1) << "#----------------------------------------------------------------------------------#";
	VLOG(1)
		<< "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl << "-> at location: ("
				<< utils::location(funcDecl->getSourceRange().getBegin(), currTU.top()->getCompiler().getSourceManager())
				<< "): " << std::endl << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
				<< "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl << "\tEmpty map: "
				<< ctx.recVarExprMap.size();


	if (ctx.isResolvingRecFuncBody) { 
  		// check if this type has a typevar already associated, in such case return it
  		ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(funcDecl);

  		if (fit != ctx.recVarExprMap.end()) {
  			// we are resolving a parent recursive type, so when one of the recursive functions in the
  			// connected components are called, the introduced mu variable has to be used instead.
  			return fit->second;
  		}
	}


	if (!ctx.isRecSubFunc) {
		// add this type to the type graph (if not present)
		exprConvPtr->funcDepGraph.addNode(funcDecl);
		if (VLOG_IS_ON(2)) {
			exprConvPtr->funcDepGraph.print(std::cout);
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConvPtr->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	if (!components.empty()) {
		std::set<const FunctionDecl*>&& subComponents = exprConvPtr->funcDepGraph.getSubComponents( funcDecl );

		for (auto cur: subComponents){

			FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
			VLOG(2) << "Analyzing FuncDecl as sub component: " << decl->getNameAsString();

			//clang[3.0]const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);
			const TranslationUnit* rightTU = this->getTranslationUnitForDefinition(decl);

			if ( rightTU && !isa<CXXConstructorDecl>(decl) ) { // not for constructors
				// update the translation unit
				// [3.0] this->currTU = &Program::getTranslationUnit(clangTU);
				this->currTU.push(rightTU);

				// look up the lambda cache to see if this function has been
				// already converted into an IR lambda expression.
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(decl);
				if ( fit == ctx.lambdaExprCache.end() ) {
					// perform the conversion only if this is the first time this
					// function is encountred
					convertFunctionDecl(decl, false);
					ctx.recVarExprMap.clear();
				}

				// reset the translation unit
				currTU.pop();
			}
		}
	}

	ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(funcDecl);
	if (fit != ctx.lambdaExprCache.end()) {
		return fit->second;
	}

	if (!components.empty()) {
		// we are dealing with a recursive type
		//VLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
				//<< "Number of components in the cycle: " << components.size();

		std::for_each(components.begin(), components.end(), [ ] (std::set<const FunctionDecl*>::value_type c) {
			//VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
		});

		/** 
		 * Creates the variable which should be used as a placeholder for invoking the given
		 * function call and isert it in the map (recVarExprMap) used to store such variables
		 * which are valid during the conversion of the given recursive function cycle
		 */
		auto createRecVar = [&] (const clang::FunctionDecl* funDecl) { 
			if (ctx.recVarExprMap.find(funDecl) != ctx.recVarExprMap.end()) { return; }

			// we create a TypeVar for each type in the mutual dependence
			core::FunctionTypePtr funcType = convertType(GET_TYPE_PTR(funDecl)).as<core::FunctionTypePtr>();
			
			// In the case the function is receiving the global variables the signature needs to be
			// modified by allowing the global struct to be passed as an argument
			if ( ctx.globalFuncMap.find(funDecl) != ctx.globalFuncMap.end() ) {
				funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
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
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
				[] (ConversionContext::RecVarExprMap::value_type c) {
					VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "] " << c.second << " " << c.second->getType();
				});

		}
	} // endif function call components

	// init parameter set
	vector<core::VariablePtr> params;

	// before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	// global struct or not
	core::VariablePtr parentGlobalVar = ctx.globalVar;

	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );
		ctx.globalVar = var;
	}

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

	// if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	// the body of this function
	vector<core::StatementPtr> decls;
	for (auto currParam : params){
		auto fit = this->ctx.wrapRefMap.find(currParam);

		if ( fit != this->ctx.wrapRefMap.end() ) {
			// LOG(INFO) << "Replace";
			decls.push_back( this->builder.declarationStmt(fit->second, this->builder.refVar( fit->first ) ));
			/*
			 * replace this parameter in the body, example:
			 *
			 * int f(int a) {
			 *  for (...) {
			 *   x = a; <- if all the occurencies of a will not be replaced the semantics of
			 *   		   the code will not be preserved
			 *   a = i;
			 *  }
			 * }
			 *
			 *  as the variable can olny appear in the RHS of expression, we have to sobstitute it with its
			 *  dereference
			 */
			body = core::static_pointer_cast<const core::Statement>(
					core::transform::replaceAll( this->builder.getNodeManager(), body, fit->first,
							this->tryDeref(fit->second))
			);
		}
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if (!decls.empty()) {
		// push the old body
		decls.push_back(body);
		body = builder.compoundStmt(decls);
	}

	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = false;
	}

	// ADD THE GLOBALS
	if (isEntryPoint && ctx.globalVar) {
		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
		assert(ctx.globalVar && ctx.globalStruct.second);

		const StatementList& oldStmts = compStmt->getStatements();

		std::vector<core::StatementPtr> stmts;

		stmts = std::vector<core::StatementPtr>(oldStmts.size() + 1);
		stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew(ctx.globalStruct.second));
		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin() + 1);

		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType(GET_TYPE_PTR(funcDecl));
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	// if this function gets the globals in the capture list we have to create a different type
	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	// reset old global var, thisVar, and offsetTable
	ctx.globalVar = parentGlobalVar;

	VLOG(2)	<< funcType << "\n" << params << "\n" << body;

	if (components.empty()) {

		core::LambdaExprPtr retLambdaExpr = builder.lambdaExpr(funcType, params, body);

		// Adding the lambda function to the list of converted functions
		ctx.lambdaExprCache.insert( { funcDecl, retLambdaExpr} );

		VLOG(2) << retLambdaExpr << " + function declaration: " << funcDecl;
		return attachFuncAnnotations(retLambdaExpr, funcDecl);
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );

	// this is a recurive function call
	// if we are visiting a nested recursive type it means someone else will take care of building the rectype
	// node, we just return an intermediate type
	if (ctx.isRecSubFunc) {
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
		if (*tit->second == *recVarRef) { continue; }

		// we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
		// for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
		// is enabled only when the isRecSubType flag is true
		ctx.recVarExprMap.erase(fd);

		// clang [3.0] 
		//clang::idx::Entity&& funcEntity =
		//	clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), program.getClangProgram());

		// if the function is not defined in this translation unit, maybe it is defined in another we already loaded
		// use the clang indexer to lookup the definition for this function declarations
		ConversionFactory::TranslationUnitPair&& ret = program.getIndexer().getDefAndTUforDefinition(llvm::cast<Decl>(fd));

		if ( ret.first ) {
			fd = llvm::cast<FunctionDecl>(ret.first);
			assert(ret.second && "Error loading translation unit for function definition");
			currTU.push(ret.second);
		}

		const core::LambdaPtr& lambda = convertFunctionDecl(fd).as<core::LambdaPtr>();
		assert(lambda && "Resolution of sub recursive lambda yields a wrong result");

		if (ret.first){
			currTU.pop();
		}

		definitions.push_back( builder.lambdaBinding(ctx.currVar, lambda) );

		// reinsert the TypeVar in the map in order to solve the other recursive types
		ctx.recVarExprMap.insert( {fd, ctx.currVar} );
	} 

	ctx.currVar = NULL;

	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert( {funcDecl, retLambdaExpr} );
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	for(const auto& fd : components) {

		auto fit = ctx.recVarExprMap.find(fd);
		assert(fit != ctx.recVarExprMap.end());

		FunctionDecl* decl = const_cast<FunctionDecl*>(fd);
		//clang [3.0]const clang::idx::TranslationUnit* clangTU = getTranslationUnitForDefinition(decl);
		const TranslationUnit* rightTU = getTranslationUnitForDefinition(decl);

		assert (rightTU);

		// update the translation unit
		currTU.push(rightTU);

		core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
		ctx.lambdaExprCache.insert( {decl, func} );

		func = attachFuncAnnotations(func, decl);

		// restore TU
		currTU.pop();
	}

	// Clear the variables so that when we resolve the recursive function the actuall recursive
	// lambda is utilized 
	ctx.recVarExprMap.clear();

	VLOG(2) << "Converted Into: " << *retLambdaExpr;
	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							AST CONVERTER
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::CallExprPtr ASTConverter::handleBody(const clang::Stmt* body, const TranslationUnit& tu) {
	mFact.currTU.push(&tu);
	
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

	mFact.currTU.pop();
	return callExpr;
}

core::ProgramPtr ASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain) {
	// Handling of the translation unit: we have to make sure to load the translation unit where the function is
	// defined before starting the parser otherwise reading literals results in wrong values.
	FunctionDecl* def = const_cast<FunctionDecl*>(funcDecl);
	//clang [3.0]const clang::idx::TranslationUnit* clangTU = mFact.getTranslationUnitForDefinition(def);
	const TranslationUnit* rightTU = mFact.getTranslationUnitForDefinition(def);
	assert(rightTU && "Translation unit for function not found.");
	mFact.currTU.push(rightTU);

	insieme::utils::Timer t("Globals.collect");
	mFact.collectGlobalVar(funcDecl);

//	VLOG(1) << globColl;
//
//	//~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	// Thread private requires to collect all the variables which are marked to be threadprivate
//	omp::collectThreadPrivate(mFact.getPragmaMap(), mFact.ctx.thread_private);
//
//	//~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//	//Omp flush clause forces the flushed variable to be volatile
//	//omp::collectVolatile(mFact.getPragmaMap(), mFact.ctx.volatiles);
//	//~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//	mFact.ctx.globalStruct = globColl.createGlobalStruct();
//	if (mFact.ctx.globalStruct.first) {
//		mFact.ctx.globalVar = mFact.builder.variable(mFact.builder.refType(mFact.ctx.globalStruct.first));
//	}
//	mFact.ctx.globalIdentMap = globColl.getIdentifierMap();
//
//	VLOG(2) << mFact.ctx.globalStruct.first;
//	VLOG(2) << mFact.ctx.globalStruct.second;
//	VLOG(2) << mFact.ctx.globalVar;

	t.stop();
	LOG(INFO) << t;

	const core::ExpressionPtr& expr = mFact.convertFunctionDecl(def, true).as<core::ExpressionPtr>();

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

	mProgram = core::Program::addEntryPoint(mFact.getNodeManager(), mProgram, lambdaExpr /*, isMain */);
	mFact.currTU.pop();
	return mProgram;
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

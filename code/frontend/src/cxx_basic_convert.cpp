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

#include "insieme/annotations/c/naming.h"
#include "insieme/annotations/c/location.h"
#include "insieme/annotations/ocl/ocl_annotations.h"

#include "clang/Basic/FileManager.h"
#include <clang/Frontend/TextDiagnosticPrinter.h>
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>
#include <clang/AST/CXXInheritance.h>
#include "clang/AST/StmtVisitor.h"

// clang [3.0]
//#include "clang/Index/Entity.h"
//#include "clang/Index/Indexer.h"

using namespace clang;
using namespace insieme;

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

namespace insieme {
namespace frontend {
namespace conversion {

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX CONVERSION FACTORY
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
CXXConversionFactory::CXXConversionFactory(core::NodeManager& mgr, Program& prog) :
	ConversionFactory::ConversionFactory(mgr, prog,
			std::make_shared<CXXStmtConverter>(*this),
			std::make_shared<CXXTypeConverter>(*this, prog),
			std::make_shared<CXXExprConverter>(*this, prog)) {
}

CXXConversionFactory::~CXXConversionFactory() {}

void CXXConversionFactory::collectGlobalVar(const clang::FunctionDecl* funcDecl) {
	// Extract globals starting from this entry point
	FunctionDecl* def = const_cast<FunctionDecl*>(funcDecl);
	const TranslationUnit* rightTU = getTranslationUnitForDefinition(def);

	ctx.globalFuncMap.clear();
	analysis::CXXGlobalVarCollector globColl(*this, rightTU, program.getIndexer(), ctx.globalFuncMap,
				cxxCtx.polymorphicClassMap, cxxCtx.offsetMap, cxxCtx.virtualFunctionIdMap,
				cxxCtx.finalOverriderMap);

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
		if (!cxxCtx.polymorphicClassMap.empty()) {
				updateVFuncOffsetTableExpr();
				updateVFuncTableExpr();
			}
	}
	ctx.globalIdentMap = globColl.getIdentifierMap();

	VLOG(2) << ctx.globalStruct.first;
	VLOG(2) << ctx.globalStruct.second;
	VLOG(2) << ctx.globalVar;
}

core::ExpressionPtr CXXConversionFactory::defaultInitVal(const core::TypePtr& type) const {
	//if ( mgr.getLangBasic().isAnyRef(type) ) {
	//return mgr.getLangBasic().getNull();
	//}
	// handle integers initialization
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
	// handle refs initialization
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		// initialize pointer/reference types with undefined
		core::TypePtr elemType = refTy->getElementType();

		core::ExpressionPtr initValue;
		if (elemType->getNodeType() == core::NT_RefType) {
			// ref<ref<...>> => this is a pointer, init with 0 (null)
			initValue = builder.callExpr(elemType, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(elemType));
		} else {
			initValue = defaultInitVal(elemType);
		}

		return builder.refVar(initValue);
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

	// Handle structs initialization
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(type)) {
		return builder.callExpr(structTy, mgr.getLangBasic().getInitZero(), builder.getTypeLiteral(structTy));
	}

	// Handle unions initialization
	if ( core::UnionTypePtr&& unionTy = core::dynamic_pointer_cast<const core::UnionType>(type)) {
		assert(unionTy);
		// TODO: for now silent compiler warning
	}

	// handle vectors initialization
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type)) {
		core::ExpressionPtr&& initVal = defaultInitVal(vecTy->getElementType());
		core::ExpressionPtr ret = builder.callExpr(vecTy,
				mgr.getLangBasic().getVectorInitUniform(),
				initVal,
				builder.getIntTypeParamLiteral(vecTy->getSize())
		);
		return ret;
	}

	// handle arrays initialization
	if ( core::ArrayTypePtr&& arrTy = core::dynamic_pointer_cast<const core::ArrayType>(type)) {
		if (arrTy->getElementType()->getNodeType() == core::NT_RefType) {
			const core::RefTypePtr& ref = core::static_pointer_cast<const core::RefType>(arrTy->getElementType());
			if (ref->getElementType()->getNodeType() != core::NT_VectorType) {
				return builder.callExpr(mgr.getLangBasic().getGetNull(), builder.getTypeLiteral(arrTy));
			}
		}
		return builder.callExpr(arrTy, mgr.getLangBasic().getUndefined(), builder.getTypeLiteral(arrTy));
	}

	// handle any-ref initialization
	if (mgr.getLangBasic().isAnyRef(type)) {
		return mgr.getLangBasic().getNull();
	}

	// Initialization for volatile types
	if (core::analysis::isVolatileType(type)) {
		return builder.callExpr(mgr.getLangBasic().getVolatileMake(),
				defaultInitVal(core::analysis::getVolatileType(type)));
	}

	LOG(ERROR) << "Default initializer for type: '" << *type << "' not supported!";
	assert(false && "Default initialization type not defined");
}

core::DeclarationStmtPtr CXXConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {
	assert(currTU && "translation unit is null");

	VLOG(1)	<< "\n****************************************************************************************\n"
			<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
			<< utils::location(varDecl->getLocation(), currTU->getCompiler().getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2) << "Dump of clang VarDecl: \n"
				<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		varDecl->dump();
	}

	core::DeclarationStmtPtr retStmt;
	assert(currTU && "translation unit is null");
	if ( const VarDecl* definition = varDecl->getDefinition()) {
		clang::QualType clangType = definition->getType();
		if (!clangType.isCanonical()) {
			clangType = clangType->getCanonicalTypeInternal();
		}
		const Type* typePtr = clangType.getTypePtr();

		if (definition->hasGlobalStorage()) {
			// once we encounter static variables we do remove the declaration
			throw GlobalVariableDeclarationException();
		}

		// lookup for the variable in the map
		core::VariablePtr&& var = core::dynamic_pointer_cast<const core::Variable>(lookUpVariable(definition));

		assert(currTU && "translation unit is null");
		assert(var);
		/* removed due to match OpenCL standard
		 // flag to determine if a variable should be initialized with zeros instead of uninitialized
		 bool zeroInit = false;

		 core::NodeType kind = var->getNodeType();
		 if (kind == core::NT_RefType) {
		 kind = core::static_pointer_cast<const core::RefType>(var->getType())->getElementType()->getNodeType();
		 }
		 // check for annotations which would lead to a zero init annotation
		 if(kind == core::NT_ArrayType || kind == core::NT_VectorType) {
		 if(var->hasAnnotation(annotations::ocl::BaseAnnotation::KEY)){
		 auto&& declarationAnnotation = var->getAnnotation(annotations::ocl::BaseAnnotation::KEY);
		 for(annotations::ocl::BaseAnnotation::AnnotationList::const_iterator I = declarationAnnotation->getAnnotationListBegin();
		 I < declarationAnnotation->getAnnotationListEnd(); ++I) {
		 if(annotations::ocl::AddressSpaceAnnotationPtr&& as = std::dynamic_pointer_cast<annotations::ocl::AddressSpaceAnnotation>(*I)){
		 if(annotations::ocl::AddressSpaceAnnotation::addressSpace::LOCAL == as->getAddressSpace() ||
		 annotations::ocl::AddressSpaceAnnotation::addressSpace::PRIVATE == as->getAddressSpace()) {
		 //TODO check why this fails:
		 //assert(!definition->getInit() && "OpenCL local variables cannot have an initialization expression");
		 zeroInit = true;
		 }
		 }
		 }
		 }
		 }
		 */
		/*
		 if(definition->getAnnotation().str() == "__local") {
		 zeroInit = true;
		 */

		// check if we have a reference or pointer -> need the pointee-type
		if (typePtr->isPointerType() || typePtr->isReferenceType()) {
			//VLOG(2) << var << " isPointerType " << typePtr->isPointerType();
			//VLOG(2) << var << " isReferenceType " << typePtr->isReferenceType();

			// get pointeetype ("deref" pointerType)
			typePtr = typePtr->getPointeeType().getTypePtr();
			//VLOG(2) << var << " PointeeType->isStructOrClassType " << typePtr->isStructureOrClassType();
		}

//		if (typePtr->isArrayType()) {
//			VLOG(2) << "isArrayType";
//		}

		//change thisstack2 only if we have a CXX object, pointer-to-CXX object, reference-to-CXX-object
		if (typePtr->isStructureOrClassType()) {
			cxxCtx.thisStack2 = var;
		}

		// initialization value
		core::ExpressionPtr&& initExpr = convertInitExpr(definition->getInit(), var->getType(), false);
		assert(initExpr && "not correct initialization of the variable");

		retStmt = builder.declarationStmt(var, initExpr);
	}

	else {
		// this variable is extern
		assert(varDecl->isExternC() && "Variable declaration is not extern");

	}

	VLOG(2) << "End of converting VarDecl";
	VLOG(1) << "Converted into IR stmt: ";
	VLOG(1) << "\t" << *retStmt;
	return retStmt;
}

core::ExpressionPtr CXXConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type,
		const bool zeroInit) const {
	core::ExpressionPtr retIr;
	// ATTACH_OMP_ANNOTATIONS(retIr, initList);
	LOG_EXPR_CONVERSION(retIr);

	// get kind of initialized value
	core::NodeType&& kind =
	(type->getNodeType() != core::NT_RefType ? type->getNodeType() : GET_REF_ELEM_TYPE(type)->getNodeType() );

	if (!expr) {
		// if no init expression is provided => use undefined for given set of types
		if (kind == core::NT_StructType || kind == core::NT_UnionType || kind == core::NT_ArrayType
				|| kind == core::NT_VectorType) {
			if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
				const core::TypePtr& res = refTy->getElementType();
				return (retIr = builder.refVar(
						builder.callExpr(res,
								(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
								builder.getTypeLiteral(res))));
			}
			return (retIr = builder.callExpr(type,
					(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
					builder.getTypeLiteral(type)));
		} else {
			return (retIr = defaultInitVal(type));
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr )) {
		return (retIr = convertInitializerList(listExpr, type));
	}

	// init the cpp class / struct - check here for enabled cpp in compiler lang options
	if (kind == core::NT_StructType && currTU->getCompiler().getPreprocessor().getLangOpts().CPlusPlus == 1) {

		if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
			const core::TypePtr& res = refTy->getElementType();
			retIr = builder.refVar(
					builder.callExpr(res,
							(zeroInit ? mgr.getLangBasic().getInitZero() : mgr.getLangBasic().getUndefined()),
							builder.getTypeLiteral(res)));
		}assert(retIr && "call expression is empty");
		return retIr;
	}

	// Convert the expression like any other expression
	retIr = convertExpr(expr);

	if (core::analysis::isCallOf(retIr, mgr.getLangBasic().getArrayCreate1D())) {
		retIr = builder.callExpr(builder.refType(retIr->getType()), mgr.getLangBasic().getRefNew(), retIr);
	}

	// fix type if necessary (also converts "Hello" into ['H','e',...])
	core::TypePtr valueType = type;
	if (type->getNodeType() == core::NT_RefType) {
		valueType = core::analysis::getReferencedType(valueType);
	}

	retIr = utils::cast(retIr, valueType);

	// if result is a reference type => create new local variable
	if (type->getNodeType() == core::NT_RefType) {
		retIr = builder.callExpr(type, mgr.getLangBasic().getRefVar(), retIr);
	}

	return retIr;
}

// the THIS parameter is added on the last position of the function parameters
core::FunctionTypePtr CXXConversionFactory::addThisArgToFunctionType(const core::IRBuilder& builder,
		const core::TypePtr& structTy, const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes()->getElements();

	std::vector<core::TypePtr> argTypes(oldArgs.size() + 1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin());
	// move THIS to the last position
	argTypes[oldArgs.size()] = builder.refType(structTy);
	return builder.functionType(argTypes, funcType->getReturnType());
}

// update __class member in all the dynamic baseClasses of the given recDecl
vector<core::StatementPtr> CXXConversionFactory::updateClassId(const clang::CXXRecordDecl* recDecl,
		core::ExpressionPtr expr,
		unsigned int classId) {
	bool hasPolymorphicBaseClass = false;
	vector<core::StatementPtr> retVec;
	core::TypePtr classTypePtr;

	ConversionContext::ClassDeclMap::const_iterator cit = ctx.classDeclMap.find(recDecl);
	if (cit != ctx.classDeclMap.end()) {
		classTypePtr = cit->second;
	}assert(classTypePtr && "no class declaration to type pointer mapping");

	for (clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end();
			bit++) {
		const CXXBaseSpecifier* base = bit;
		const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

		hasPolymorphicBaseClass |= baseRecord->isPolymorphic();
	}

	if (recDecl->isPolymorphic() && !hasPolymorphicBaseClass) {
		//update __class
		core::StringValuePtr ident = builder.stringValue("__class");
		const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

		expr = builder.callExpr(
				builder.refType(memberTy),
				builder.getLangBasic().getCompositeRefElem(),
				toVector<core::ExpressionPtr>(expr, builder.getIdentifierLiteral(ident),
						builder.getTypeLiteral(memberTy)));

		const core::StatementPtr& assign = builder.callExpr(builder.getLangBasic().getUnit(),
				builder.getLangBasic().getRefAssign(), expr,
				builder.literal(builder.getLangBasic().getUInt4(), toString(classId)));

		retVec.push_back(assign);
	} else {
		for (clang::CXXRecordDecl::base_class_const_iterator bit = recDecl->bases_begin(); bit != recDecl->bases_end();
				bit++) {
			const CXXBaseSpecifier* base = bit;
			const CXXRecordDecl* baseRecord = base->getType()->getAsCXXRecordDecl();

			if (baseRecord->isPolymorphic()) {
				core::StringValuePtr ident = builder.stringValue(baseRecord->getNameAsString());
				const core::TypePtr& memberTy = classTypePtr.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);

				//expr = expr->baseRecord
				core::ExpressionPtr resExpr = builder.callExpr(
						builder.refType(memberTy),
						builder.getLangBasic().getCompositeRefElem(),
						toVector<core::ExpressionPtr>(expr, builder.getIdentifierLiteral(ident),
								builder.getTypeLiteral(memberTy)));

				const vector<core::StatementPtr>& result = CXXConversionFactory::updateClassId(baseRecord, resExpr,
						classId);
				retVec.insert(retVec.end(), result.begin(), result.end());
			}
		}
	}
	return retVec;
}

// create initializations statements for the offsetTable
vector<core::StatementPtr> CXXConversionFactory::initOffsetTable() {
	std::vector<core::StatementPtr> initOffsetTableStmts;
	//VLOG(2) << "OffsetMap: " << ctx.offsetMap;
	for (CXXConversionFactory::CXXConversionContext::OffsetMap::iterator it = cxxCtx.offsetMap.begin();
			it != cxxCtx.offsetMap.end(); ++it) {
		//VLOG(2) << "Offset:" << it->first.first->getNameAsString() << it->first.second->getNameAsString() << " = " << it->second;

		//access to the offset array
		core::ExpressionPtr vFuncOffset = cxxCtx.offsetTableExpr;

		unsigned int row = cxxCtx.polymorphicClassMap.find(it->first.first)->second.first;
		unsigned int col = cxxCtx.polymorphicClassMap.find(it->first.second)->second.first;
		int offset = it->second;

		core::ExpressionPtr op = builder.getLangBasic().getVectorRefElem();
		core::TypePtr&& resTy = builder.refType(
				builder.vectorType(
						builder.getLangBasic().getInt4(),
						core::ConcreteIntTypeParam::get(builder.getNodeManager(), cxxCtx.polymorphicClassMap.size())
				)
		);
		vFuncOffset = builder.callExpr(resTy, op, vFuncOffset,
				builder.literal(builder.getLangBasic().getUInt4(), toString(row)));

		op = builder.getLangBasic().getVectorRefElem();
		resTy = builder.refType(builder.getLangBasic().getInt4());
		vFuncOffset = builder.callExpr(resTy, op, vFuncOffset,
				builder.literal(builder.getLangBasic().getUInt4(), toString(col)));

		//assign the offset
		op = builder.getLangBasic().getRefAssign();
		resTy = builder.getLangBasic().getUnit();

		initOffsetTableStmts.push_back(
				builder.callExpr(resTy, op, vFuncOffset,
						builder.literal(builder.getLangBasic().getInt4(), toString(offset))));
	}
	return initOffsetTableStmts;
}

// create initializations statements for the vFuncTable
vector<core::StatementPtr> CXXConversionFactory::initVFuncTable() {
	std::vector<core::StatementPtr> initVFuncTableStmts;

	for (CXXConversionFactory::CXXConversionContext::FinalOverriderMap::iterator foit = cxxCtx.finalOverriderMap.begin();
			foit != cxxCtx.finalOverriderMap.end(); foit++) {
		const clang::CXXRecordDecl* recDecl = foit->first;
		const vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>& finalOverriders =
				foit->second;
		unsigned int classId = cxxCtx.polymorphicClassMap.find(recDecl)->second.first;

		for (vector<std::pair<const clang::CXXMethodDecl*, const clang::CXXMethodDecl*>>::const_iterator it =
				finalOverriders.begin(); it != finalOverriders.end(); it++) {
			const clang::CXXMethodDecl* toBeOverriden = it->first; //the function which will be overriden by the "final overrider"
			const clang::CXXMethodDecl* overrider = it->second; //the actual function which will be dispatch for a virtual function call

			//get FunctionId
			unsigned int functionId = cxxCtx.virtualFunctionIdMap.find(toBeOverriden)->second;

			//get Offset (offset[recDecl,toBeOverriden->parent])
			int offset = cxxCtx.offsetMap.find(std::make_pair(recDecl, toBeOverriden->getParent()))->second;

			//create initExpr
			VLOG(2)
				<< "vfuncinit: vFuncTable[" << recDecl->getNameAsString() << "]["
						<< toBeOverriden->getParent()->getNameAsString() << "] = "
						<< overrider->getParent()->getNameAsString() << "::" << overrider->getNameAsString()
						<< " finally overrides " << toBeOverriden->getParent()->getNameAsString() << "::"
						<< toBeOverriden->getNameAsString();

			// create access to row: vfuncTable[classId]
			core::ExpressionPtr vFunctionTable = cxxCtx.vFuncTableExpr;
			core::ExpressionPtr op = builder.getLangBasic().getVectorRefElem();
			vFunctionTable = builder.callExpr(op, vFunctionTable,
					builder.literal(builder.getLangBasic().getUInt4(), toString(classId)));

			// create access to element of row vfuncTable[classId][offset + functionId] (should be type ref<anyRef>)
			op = builder.getLangBasic().getVectorRefElem();
			vFunctionTable = builder.callExpr(op, vFunctionTable,
					builder.literal(builder.getLangBasic().getUInt4(), toString(offset + functionId)));

			core::ExpressionPtr vFuncPointerExpr;
			if (overrider->isPure()) {
				//abstract functions have no declaration
				vFuncPointerExpr = builder.getLangBasic().getNull();
			} else {
				core::TypePtr classTypePtr = convertType(recDecl->getTypeForDecl());
				assert(classTypePtr && "no class declaration to type pointer mapping");
				core::ExpressionPtr thisStack2old = cxxCtx.thisStack2;
				cxxCtx.thisStack2 = builder.variable(builder.refType(classTypePtr));

				//Convert virtual function, and get function pointer WITHOUT this/return-adjustment
				core::ExpressionPtr vFuncExpr = core::static_pointer_cast<const core::LambdaExpr>(
						convertFunctionDecl(overrider, false));

				if (overrider == toBeOverriden) {
					// function DOESN'T NEED this/return adjustment
					// as overrider and toBeOverriden are the same -> nothing overriden
					VLOG(2)
						<< "no this-adjustment needed:	 " << overrider->getParent()->getNameAsString() << "::"
								<< overrider->getNameAsString() << " finally overrides "
								<< toBeOverriden->getParent()->getNameAsString() << "::"
								<< toBeOverriden->getNameAsString();

					const clang::Type* overriderResultType = overrider->getResultType().getTypePtr();
					const clang::Type* toBeOverridenResultType = toBeOverriden->getResultType().getTypePtr();
					if ((overriderResultType->isPointerType() || overriderResultType->isReferenceType())
							&& overriderResultType->getPointeeType().getTypePtr()->isStructureOrClassType()) {
						clang::CXXRecordDecl* orResRecDecl =
								overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
						clang::CXXRecordDecl* tboResRecDecl =
								toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
						VLOG(2)
							<< "no return-adjustment needed: " << orResRecDecl->getNameAsString() << " "
									<< tboResRecDecl->getNameAsString();
					}
				} else {
					//function NEEDS THIS ADJUSTMENT
					VLOG(2)
						<< "this-adjustment needed:	" << overrider->getParent()->getNameAsString() << "::"
								<< overrider->getNameAsString() << " finally overrides "
								<< toBeOverriden->getParent()->getNameAsString() << "::"
								<< toBeOverriden->getNameAsString();

					//functions which are overriding some derived functions need this/return-adjustment
					for (clang::CXXMethodDecl::method_iterator it = overrider->begin_overridden_methods();
							it != overrider->end_overridden_methods(); it++) {
						VLOG(2)
							<< "vFuncTable[" << recDecl->getNameAsString() << "]["
									<< toBeOverriden->getParent()->getNameAsString() << "] = "
									<< overrider->getParent()->getNameAsString() << "::" << overrider->getNameAsString()
									<< " needs this-adjustment: " << "from "
									<< toBeOverriden->getParent()->getNameAsString() << " to "
									<< overrider->getParent()->getNameAsString();

						// add this adjustment from toBeOverriden->getParent to overrider->getParent -> IR: expand-operator
						// thunk(this, args, ...) {
						// 	 toType newThis = expand(this,fromType, toType, path)
						//	 vfunc(newThis, args...)
						//	 returnAdjustment /*if needed*/
						// }(this, args, ...)
						// create "thunk" for this-adjustment: new function, taking the arguments of vFuncExpr, adjust this, call vFuncExpr

						//fromRecDecl -> recDecl of toBeOverriden->getParent
						const clang::CXXRecordDecl* fromRecDecl = toBeOverriden->getParent();
						//fromTy = IR-typeOf(toBeOverriden->getParent)
						core::TypePtr fromTy = convertType(fromRecDecl->getTypeForDecl());
						assert(fromTy && "no class declaration to type pointer mapping");

						//toRecDecl -> recDecl of overrider->getParent
						const clang::CXXRecordDecl* toRecDecl = overrider->getParent();
						//toTy = IR-typeOf(overrider->getParent)
						core::TypePtr toTy = convertType(toRecDecl->getTypeForDecl());
						assert(toTy && "no class declaration to type pointer mapping");

						//get the function type for the overrider
						core::FunctionTypePtr vFuncTy = core::static_pointer_cast<const core::FunctionType>(
								convertType(GET_TYPE_PTR(overrider)));
						core::FunctionTypePtr thunkTy;

						if (ctx.globalFuncMap.find(overrider) != ctx.globalFuncMap.end()) {
							// declare a new variable that will be used to hold a reference to the global data stucture
							vFuncTy = addGlobalsToFunctionType(builder, ctx.globalStruct.first, vFuncTy);
						}

						thunkTy = addThisArgToFunctionType(builder, fromTy, vFuncTy); //function type of thunk
						vFuncTy = addThisArgToFunctionType(builder, toTy, vFuncTy); //adjusted function type of virtual function
						VLOG(2)
							<< "vFuncTy: " << vFuncTy;
						VLOG(2)
							<< "thunkTy: " << thunkTy;

						vector<core::ExpressionPtr> vFuncArgs; //arguments of vFuncExpr
						vector<core::VariablePtr> thunkParams; //parameter of thunk

						//create "new" thisVar-> with type toTy
						core::VariablePtr&& thunkThis = builder.variable( builder.refType(fromTy) ); //the "this" variable used in the thunk
						core::VariablePtr&& adjustedThis = builder.variable( builder.refType(toTy) ); //the adjusted "this" used in the vFunc

						//create variables for all parameters to be used in the thunk for vFunc
						// BUT NOT THE LAST ONE -> "this" TODO: move this to 1. position of parameters/arguments
						for (vector<core::TypePtr>::const_iterator it =
								(thunkTy->getParameterTypes()->getTypes()).begin();
								it != (thunkTy->getParameterTypes()->getTypes()).end() - 1 /*leave out the last one -> "this"*/;
								it++) {
							const core::VariablePtr& var = builder.variable(*it);
							vFuncArgs.push_back(var);
							thunkParams.push_back(var);
						}

						//add "this" AT END (TODO: put "this" at the beginning of parameters/arguments)
						thunkParams.push_back(thunkThis);
						vFuncArgs.push_back(adjustedThis);
						VLOG(2)
							<< "thunkParams: " << thunkParams;
						VLOG(2)
							<< "vFuncArgs:	 " << vFuncArgs;

						// create dataPath for expansion from fromTy to toTy
						core::datapath::DataPathBuilder dpManager(mgr);
						clang::CXXBasePaths paths;
						if (toRecDecl->isDerivedFrom(fromRecDecl, paths)) {
							for (clang::CXXBasePaths::paths_iterator bp = paths.begin(); bp != paths.end(); bp++) {
								for (clang::CXXBasePath::iterator bpe = bp->begin(); bpe != bp->end(); bpe++) {
									const CXXRecordDecl* currRecDecl = bpe->Class;
									//VLOG(2) << currRecDecl->getNameAsString();
									dpManager.member(currRecDecl->getNameAsString());
								}
								//VLOG(2) << fromRecDecl->getNameAsString();
								dpManager.member(fromRecDecl->getNameAsString());
							}
							//ref.expand(ref<'a>, datapath, type<'b>) -> ref<'b>
							//VLOG(2) << builder.getLangBasic().getRefExpand();
							//VLOG(2) << thunkThis;
							//VLOG(2) << dpManager.getPath();
							//VLOG(2) << toTy;
							//VLOG(2) << builder.callExpr(builder.refType(toTy), builder.getLangBasic().getRefExpand(), toVector<core::ExpressionPtr>(thunkThis, dpManager.getPath(), builder.getTypeLiteral(toTy)
						}

						// "expand" the actual this to the adjustedThis --> actual this-adjustment
						const core::StatementPtr& adjustedThisAssign = builder.declarationStmt(
								adjustedThis,
								builder.callExpr(
										builder.refType(toTy),
										builder.getLangBasic().getRefExpand(),
										toVector<core::ExpressionPtr>(thunkThis, dpManager.getPath(),
												builder.getTypeLiteral(toTy))));

						core::TypePtr thunkResTy; //result type of thunk
						core::TypePtr vFuncResTy = vFuncTy->getReturnType(); //result type of vFuncExpr

						//create call to vFuncExpr
						core::ExpressionPtr callVFunc = builder.callExpr(vFuncResTy, vFuncExpr, vFuncArgs);

						//check if function has a return value
						core::StatementPtr retCallVFunc;
						if (overrider->getResultType().getTypePtr()->isVoidType()) {
							//function with void as return type -> nothing to be done

							retCallVFunc = static_cast<core::StatementPtr>(callVFunc);
							// return Type of the thunk is the same as the return type of the virtual function
							thunkResTy = vFuncResTy;
						} else {
							//function has return value

							//check if return value needs return-adjustment
							const clang::Type* overriderResultType = overrider->getResultType().getTypePtr();
							const clang::Type* toBeOverridenResultType = toBeOverriden->getResultType().getTypePtr();
							if ((overriderResultType->isPointerType() || overriderResultType->isReferenceType())
									&& (toBeOverridenResultType->isPointerType()
											|| toBeOverridenResultType->isReferenceType())
									&& overriderResultType->getPointeeType().getTypePtr()->isStructureOrClassType()
									&& toBeOverridenResultType->getPointeeType().getTypePtr()->isStructureOrClassType()) {

								//	C++ Standard, 10.3.5
								//	for covariant return types we need a return-adjustment
								//	covariant:	- return types of overrider and toBeOverriden are pointer/reference of classes
								//				- class of the return type of overrider is the same as class of the return type of toBeOverriden,
								//				  or an unambigous and accessible direct or indirect base class of the return type of toBeOverriden
								//				- both pointers/references have the same cv-qualifiers, or return type of overrider has less

								//	adjust from overrider->returnType to toBeOverriden->returnType
								clang::CXXRecordDecl* orResRecDecl =
										overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
								clang::CXXRecordDecl* tboResRecDecl =
										toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl();
								clang::CXXBasePaths paths;
								if (orResRecDecl->isDerivedFrom(tboResRecDecl, paths)) {
									//we need return adjustment
									VLOG(2)
										<< "needs return-adjustment from overrider to toBeOverriden ";
									VLOG(2)
										<< "overrider: " << overriderResultType->isPointerType() << " "
												<< overriderResultType->isReferenceType() << " "
												<< overriderResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl()->getNameAsString();
									VLOG(2)
										<< "toBeOverriden: " << toBeOverridenResultType->isPointerType() << " "
												<< toBeOverridenResultType->isReferenceType() << " "
												<< toBeOverridenResultType->getPointeeType().getTypePtr()->getAsCXXRecordDecl()->getNameAsString();

									// if we have a pointer get access to the element
									if (overriderResultType->isPointerType()) {
										callVFunc = exprutils::getCArrayElemRef(builder, callVFunc);
									}

									//get return value of callVFunc, and walk along path from overrider_ResultType to toBeOverriden_ResultType
									for (clang::CXXBasePaths::paths_iterator bp = paths.begin(); bp != paths.end();
											bp++) {
										for (clang::CXXBasePath::iterator bpe = bp->begin(); bpe != bp->end(); bpe++) {

											const CXXRecordDecl* baseRecDecl = bpe->Class;
											if (baseRecDecl == orResRecDecl) {
												//step over first node in path as it is the result type of overrider
												continue;
											} else {
												// find the class type - if not converted yet, converts and adds it
												core::TypePtr baseClassTypePtr = convertType(
														baseRecDecl->getTypeForDecl());
												assert(
														baseClassTypePtr && "no class declaration to type pointer mapping");

												core::StringValuePtr ident = builder.stringValue(
														baseRecDecl->getName().data());

												core::TypePtr resType = builder.refType(baseClassTypePtr);
												core::ExpressionPtr op =
														builder.getLangBasic().getCompositeMemberAccess();
												core::TypePtr structTy = callVFunc->getType();

												if (structTy->getNodeType() == core::NT_RefType) {
													// skip over reference wrapper
													structTy = core::analysis::getReferencedType(structTy);
													op = builder.getLangBasic().getCompositeRefElem();
												}

												const core::TypePtr& memberTy = core::static_pointer_cast<
														const core::NamedCompositeType>(structTy)->getTypeOfMember(
														ident);
												callVFunc = builder.callExpr(resType, op, callVFunc,
														builder.getIdentifierLiteral(ident),
														builder.getTypeLiteral(memberTy));
											}
										}
										//add the final access: FOO.Bar.toBeOverridenResultType

										// find the class type - if not converted yet, converts and adds it
										core::TypePtr baseClassTypePtr = convertType(tboResRecDecl->getTypeForDecl());
										assert(baseClassTypePtr && "no class declaration to type pointer mapping");

										core::StringValuePtr ident = builder.stringValue(
												tboResRecDecl->getName().data());
										core::TypePtr resType = builder.refType(baseClassTypePtr);

										//final return Type of the thunk
										thunkResTy = resType;
										core::ExpressionPtr op = builder.getLangBasic().getCompositeMemberAccess();

										core::TypePtr structTy = callVFunc->getType();
										if (structTy->getNodeType() == core::NT_RefType) {
											// skip over reference wrapper
											structTy = core::analysis::getReferencedType(structTy);
											op = builder.getLangBasic().getCompositeRefElem();
										}

										const core::TypePtr& memberTy = core::static_pointer_cast<
												const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);
										callVFunc = builder.callExpr(resType, op, callVFunc,
												builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

									}
								} else {
									//we DON'T need return adjustment -> thunkResTy is the same as of vFunc
									thunkResTy = vFuncResTy;
								}
							} else {
								// no return adjustment needed -> return Type of the thunk is the same as the return type of the virtual function
								thunkResTy = vFuncResTy;
							}

							retCallVFunc = builder.returnStmt(utils::cast(callVFunc, thunkResTy));
						}

						//create thunkBody: create newThis, expand, callVfunc and if needed add return adjustment
						core::CompoundStmtPtr&& thunkBody = builder.compoundStmt(
								adjustedThisAssign,
								retCallVFunc
						);

						core::ExpressionPtr thunkExpr = builder.lambdaExpr(thunkResTy, thunkBody, thunkParams);

						vFuncExpr = thunkExpr;
					}
				}

				//build functionPointer variable out of expression
				vFuncPointerExpr = builder.refVar(vFuncExpr);

				op = builder.getLangBasic().getRefToAnyRef();
				vFuncPointerExpr = builder.callExpr(builder.getLangBasic().getAnyRef(), op, vFuncPointerExpr);
				VLOG(2)
					<< vFuncPointerExpr;

				VLOG(2)
					<< vFuncPointerExpr;
				cxxCtx.thisStack2 = thisStack2old;
			}

			//assign the functionPointer (as anyRef)
			op = builder.getLangBasic().getRefAssign();
			core::ExpressionPtr vFunctionTableAssign = builder.callExpr(op, vFunctionTable, vFuncPointerExpr);
			VLOG(2)
				<< vFunctionTableAssign;

			initVFuncTableStmts.push_back(vFunctionTableAssign);
		}
	}
	return initVFuncTableStmts;
}

//create/update access vfunc offset table
void CXXConversionFactory::updateVFuncOffsetTableExpr() {
	VLOG(2) << cxxCtx.offsetTableExpr;
	core::StringValuePtr ident = builder.stringValue("__vfunc_offset");
	const core::TypePtr& memberTy =
			( core::analysis::getReferencedType(ctx.globalVar->getType()) ).as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);
	//core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
	core::TypePtr resType = builder.refType(memberTy);
	core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
	cxxCtx.offsetTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));
	VLOG(2) << cxxCtx.offsetTableExpr;
}

//create/update access vfunc table
void CXXConversionFactory::updateVFuncTableExpr() {
	VLOG(2) << cxxCtx.vFuncTableExpr;
	core::StringValuePtr ident = builder.stringValue("__vfunc_table");
	const core::TypePtr& memberTy =
			( core::analysis::getReferencedType(ctx.globalVar->getType()) ).as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);
	//core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
	core::TypePtr resType = builder.refType(memberTy);
	core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
	cxxCtx.vFuncTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));
	VLOG(2) << cxxCtx.vFuncTableExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CXX CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::NodePtr CXXConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {

	/** HACK **/
	cpp::TemporaryHandler tempHandler(this);
	/** HACKEND **/

	//Save the scope objects created in the previous scope
	CXXConversionFactory::CXXConversionContext::ScopeObjects parentScopeObjects = cxxCtx.scopeObjects;
	while (!cxxCtx.scopeObjects.empty()) {
		cxxCtx.scopeObjects.pop();
	}

	// the function is pure virtual/abstract
	if (funcDecl->isPure()) {
		//so it has no body -> need to get body from derived class
		assert(false && "Abstract (pure virtual) member function not handled yet");
	}

	// the function is not extern, a lambdaExpr has to be created
	VLOG(2) << funcDecl->getNameAsString();
	assert(currTU && funcDecl->hasBody() && "Function has no body!");

	VLOG(1)	<< "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;
	VLOG(1)	<< "#----------------------------------------------------------------------------------#";
	VLOG(1)	<< "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl << "-> at location: ("
			<< utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager())
			<< "): " << std::endl << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
			<< "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl << "\tEmpty map: "
			<< ctx.recVarExprMap.size();

	if (!ctx.isRecSubFunc) {
		// add this type to the type graph (if not present)
		exprConvPtr->funcDepGraph.addNode(funcDecl);
		if (VLOG_IS_ON(2)) {
			exprConvPtr->funcDepGraph.print(std::cout);
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConvPtr->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	// save the current translation unit
	const TranslationUnit* oldTU = currTU;

	if (!components.empty()) {
		std::set<const FunctionDecl*>&& subComponents = exprConvPtr->funcDepGraph.getSubComponents( funcDecl );

		std::for_each(subComponents.begin(), subComponents.end(),
				[&] (const FunctionDecl* cur) {

					FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
					VLOG(2) << "Analyzing FuncDecl as sub component: " << decl->getNameAsString();
					const TranslationUnit* rightTU = this->getTranslationUnitForDefinition(decl);

					if ( rightTU && !isa<CXXConstructorDecl>(decl) ) { // not for constructors
						// update the translation unit
						//this->currTU = &Program::getTranslationUnit(clangTU);
						this->currTU = rightTU;
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
				}
		);
	}

	// reset the translation unit
	currTU = oldTU;

	// we have a c++ method declaration and the special case constructor
	bool isCXX = false;
	bool isCtor = false;
	bool isOverloadedOp = false;
	bool isDtor = false;

	// bool isCXXOperator = false;
	const CXXRecordDecl * baseClassDecl;
	if (const CXXConstructorDecl* cxxCtorDecl =dyn_cast<CXXConstructorDecl>(funcDecl)) {
		baseClassDecl = cxxCtorDecl->getParent();
		VLOG(2) << "Name of the class: " << baseClassDecl->getNameAsString();
		assert(baseClassDecl->getNameAsString()==cxxCtorDecl->getNameAsString() && "wrong constructor");
		isCtor = true;
		isCXX = true;
	} else if (const CXXDestructorDecl* cxxDtorDecl =dyn_cast<CXXDestructorDecl>(funcDecl)) {
		baseClassDecl = cxxDtorDecl->getParent();
		isDtor = true;
		isCXX = true;
	} else if (const CXXMethodDecl* cxxMethodDecl = dyn_cast<CXXMethodDecl>(funcDecl)) {
		if (cxxMethodDecl->isInstance()) {
			baseClassDecl = cxxMethodDecl->getParent();
			VLOG(2) << "Name of the class: " << baseClassDecl->getNameAsString();
			isCXX = true;
		}
	}

	// check for overloaded operator "function" (normal function has kind OO_None)
	clang::OverloadedOperatorKind operatorKind = funcDecl->getOverloadedOperator();
	if (operatorKind != OO_None) {
		// isCXXOperator = true;
		isOverloadedOp = true;
	}

	if (!(isCXX || isOverloadedOp)) {
		ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(funcDecl);
		if (fit != ctx.lambdaExprCache.end()) {
			//restore the parent scope objects first
			cxxCtx.scopeObjects = parentScopeObjects;
			return fit->second;
		}
	}

	if (!components.empty()) {
		// we are dealing with a recursive type
		VLOG(1)
			<< "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
					<< "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(), [ ] (std::set<const FunctionDecl*>::value_type c) {
			VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
		});

		if (!ctx.isRecSubFunc) {
			if (ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end()) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert( std::make_pair(funcDecl, ctx.currVar) );
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if (!ctx.isRecSubFunc) {
			std::for_each(components.begin(), components.end(),
					[ this ] (std::set<const FunctionDecl*>::value_type fd) {

						if ( this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end() ) {
							core::FunctionTypePtr funcType =
							core::static_pointer_cast<const core::FunctionType>( this->convertType(GET_TYPE_PTR(fd)) );
							// In the case the function is receiving the global variables the signature needs to be
							// modified by allowing the global struct to be passed as an argument
					if ( this->ctx.globalFuncMap.find(fd) != this->ctx.globalFuncMap.end() ) {
						funcType = addGlobalsToFunctionType(this->builder, this->ctx.globalStruct.first, funcType);
					}
					core::VariablePtr&& var = this->builder.variable( funcType );
					this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
				}
			});
		}
		if (VLOG_IS_ON(2)) {
			VLOG(2)
				<< "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
					[] (ConversionContext::RecVarExprMap::value_type c) {
						VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
					});
		}
	}

	// find the class type
	core::TypePtr classTypePtr;
	if (isCXX) {
		ConversionContext::ClassDeclMap::const_iterator cit = ctx.classDeclMap.find(baseClassDecl);
		if (cit != ctx.classDeclMap.end()) {
			classTypePtr = cit->second;
		}
		assert(classTypePtr && "no class declaration to type pointer mapping");
	}

	// init parameter set
	vector<core::VariablePtr> params;

	/*
	 * before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	 * global struct or not
	 */
	core::VariablePtr parentGlobalVar = ctx.globalVar;
	core::ExpressionPtr parentOffsetTableExpr = cxxCtx.offsetTableExpr;
	core::ExpressionPtr parentVFuncTableExpr = cxxCtx.vFuncTableExpr;

	VLOG(2) << funcDecl;

	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );

		VLOG(2) << funcDecl << params;
		ctx.globalVar = var;

		// we have polymorphicClasses -> need offset/vFuncTable
		if( !cxxCtx.polymorphicClassMap.empty()) {
			// create/update access to offsetTable
			updateVFuncOffsetTableExpr();
//			core::StringValuePtr ident = builder.stringValue("__vfunc_offset");
//			const core::TypePtr& memberTy =
//			core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
//			core::TypePtr resType = builder.refType(memberTy);
//			core::ExpressionPtr op = builder.getLangBasic().getCompositeRefElem();
//			ctx.offsetTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

			// create/update access to vFuncTable
			updateVFuncTableExpr();
//			ident = builder.stringValue("__vfunc_table");
//			const core::TypePtr& memberTy2 =
//			core::static_pointer_cast<const core::NamedCompositeType>( core::analysis::getReferencedType(ctx.globalVar->getType()) )->getTypeOfMember(ident);
//			resType = builder.refType(memberTy2);
//			op = builder.getLangBasic().getCompositeRefElem();
//			ctx.vFuncTableExpr = builder.callExpr(resType, op, ctx.globalVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy2));
		}
	}

	std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [ &params, this ] (ParmVarDecl* currParam) {
		params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
	});

	// for cpp methods add the type of THIS at the end of the parameter list
	core::ExpressionPtr parentThisVar = cxxCtx.thisVar;
	if (isCXX) {
		core::VariablePtr&& var = builder.variable( builder.refType(classTypePtr) );
		//core::VariablePtr var = ctx.thisStack2;
		params.push_back( var );
		cxxCtx.thisVar = var;
	}

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert(
			(components.empty() || (!components.empty() && !ctx.isResolvingRecFuncBody)) && "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");
	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = true;
	}

	VLOG(2)
		<< "Visiting function body!";
	// set up context to contain current list of parameters and convert body
	ConversionContext::ParameterList oldList = ctx.curParameter;
	ctx.curParameter = &params;

	if (VLOG_IS_ON(2)) {
		VLOG(2)
			<< "Dump of stmt body: \n"
					<< "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		funcDecl->getBody()->dump();
	}

	//Save thisStack2
	core::ExpressionPtr thisStack2old = cxxCtx.thisStack2;

	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	//VLOG(2) << "convertFunctionDecl: thisStack2old " << thisStack2old << "thisStack2 " << ctx.thisStack2;
	//reset thisStack2
	cxxCtx.thisStack2 = thisStack2old;

	ctx.curParameter = oldList;

	/*
	 * if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	 * the body of this function
	 */
	vector<core::StatementPtr> decls;
	std::for_each(params.begin(), params.end(), [ &decls, &body, this ] (core::VariablePtr currParam) {
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

	});

	if (isCXX) {
		assert(cxxCtx.thisStack2 && "THIS - thisStack2 is empty");
		body = core::static_pointer_cast<const core::Statement>(
				core::transform::replaceAll(this->builder.getNodeManager(), body, cxxCtx.thisStack2, cxxCtx.thisVar));
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if (isCXX && isCtor) {
		// update __class if constructor and has polymorphic baseclass
		if (baseClassDecl->isPolymorphic()) {
			unsigned int classId = cxxCtx.polymorphicClassMap.find(baseClassDecl)->second.first;

			// update "__class" in all dynamic bases classes to the given classId
			vector<core::StatementPtr> && vec = updateClassId(baseClassDecl, cxxCtx.thisVar, classId);

			// add the declarations before the function body
			for (std::vector<core::StatementPtr>::const_iterator it = vec.begin(); it != vec.end(); it++) {
				decls.push_back(*it);
			}
		}

		//handle initializers (member/base classes)
		const core::lang::BasicGenerator& gen = builder.getLangBasic();
		const clang::CXXConstructorDecl *ctorDecl = dyn_cast<CXXConstructorDecl>(funcDecl);

		// there are constructor initializers that has to be handled - these are inserted befor the body
		for (clang::CXXConstructorDecl::init_const_iterator iit = ctorDecl->init_begin(),
				iend = ctorDecl->init_end(); iit != iend; iit++) {
			clang::CXXCtorInitializer *initializer = *iit;

			core::StringValuePtr ident;

			if (initializer->isMemberInitializer()) {
				const FieldDecl *fieldDecl = initializer->getMember();

				VLOG(2) << initializer << " -> " << fieldDecl->getNameAsString() << " = " << convertExpr(initializer->getInit());
				ident = builder.stringValue(fieldDecl->getNameAsString());
			}
			if (initializer->isBaseInitializer()) {
				const CXXRecordDecl *baseClass = initializer->getBaseClass()->getAsCXXRecordDecl();

				VLOG(2) << initializer << " -> " << baseClass->getNameAsString() << " = " << convertExpr(initializer->getInit());
				ident = builder.stringValue(baseClass->getNameAsString());
			}

			const core::TypePtr& memberTy =
					core::static_pointer_cast<const core::NamedCompositeType>(classTypePtr)->getTypeOfMember(ident);

			// create access to the member of the struct/class
			core::ExpressionPtr&& init = builder.callExpr(
					builder.refType( memberTy ),
					gen.getCompositeRefElem(),
					toVector<core::ExpressionPtr>( cxxCtx.thisVar, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy) )
			);

			core::ExpressionPtr parentThisStack = cxxCtx.thisStack2;
			cxxCtx.thisStack2 = init;
			core::ExpressionPtr expr = convertExpr(initializer->getInit());
			VLOG(2) << cxxCtx.thisVar << cxxCtx.thisStack2 << parentThisStack;
			cxxCtx.thisStack2 = parentThisStack;

			// create the assign (class->Member = initializerExpr)
			core::StatementPtr assign = builder.callExpr(gen.getUnit(), gen.getRefAssign(), init, tryDeref(expr));

			// add assignment
			decls.push_back(assign);
		}
	}

	// if we introduce new decls we have to introduce them just before the body of the function
	if ( !decls.empty() ) {
		// if there decls to be added before the body (initializer, vFuncTables, ... )
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

		if (cxxCtx.polymorphicClassMap.empty()) {
			// there were no polymorphic classes found -> only the global variables have to be handled

			stmts = std::vector<core::StatementPtr>(oldStmts.size() + 1);
			stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew(ctx.globalStruct.second));
			std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin() + 1);
		} else {
			//init the ctx variables for easier access to OffsetTable and the vfuncTable
			updateVFuncOffsetTableExpr();
			updateVFuncTableExpr();

			// polymorphic classes found: global variables + init virtual function table offset and virtual function table

			//initialize offsetTable
			std::vector<core::StatementPtr>&& initOffsetTableStmts = initOffsetTable();

			// init vFuncTable with the function pointers to the virtual functions
			std::vector<core::StatementPtr>&& initVFuncTableStmts = initVFuncTable();

			stmts = std::vector<core::StatementPtr>(oldStmts.size()+1+initOffsetTableStmts.size()+initVFuncTableStmts.size());

			stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew( ctx.globalStruct.second ));
			std::copy(initOffsetTableStmts.begin(), initOffsetTableStmts.end(), stmts.begin()+1);
			std::copy(initVFuncTableStmts.begin(), initVFuncTableStmts.end(), stmts.begin()+1+initOffsetTableStmts.size());
			std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin()+1+initOffsetTableStmts.size()+initVFuncTableStmts.size());
		}
		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType(GET_TYPE_PTR(funcDecl));
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	tempHandler.handleTemporariesinScope(funcDecl, funcType, params, cxxCtx.scopeObjects, true, true, false);

	// if this function gets the globals in the capture list we have to create a different type
	if (!isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end()) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	if (isCXX) {
		funcType = addThisArgToFunctionType(builder, classTypePtr, funcType);
	}

	//if this is a constructor return the objects that is passed to it
	if (isCXX) {
		const core::CompoundStmtPtr& compStmt = builder.compoundStmt(body);
		const StatementList& oldStmts = compStmt->getStatements();
		std::vector<core::StatementPtr> stmts = oldStmts;

		if (isCXX && !isDtor) {

			tempHandler.handleTemporariesinScope(params, stmts, cxxCtx.downStreamScopeObjects, false, false);
		}
		if (isCtor) {

			stmts.push_back(builder.returnStmt(utils::cast(cxxCtx.thisVar, cxxCtx.thisVar.getType())));

		}
		body = builder.compoundStmt(stmts);
	}
	// reset old global var, thisVar, and offsetTable
	ctx.globalVar = parentGlobalVar;
	cxxCtx.offsetTableExpr = parentOffsetTableExpr;
	cxxCtx.vFuncTableExpr = parentVFuncTableExpr;
	cxxCtx.thisVar = parentThisVar;
	cxxCtx.scopeObjects = parentScopeObjects;

	VLOG(2)	<< funcType << "\n" << params << "\n" << body;

	if (components.empty()) {

		core::LambdaExprPtr retLambdaExpr;

		if (!isCtor) {

			retLambdaExpr = builder.lambdaExpr(funcType, params, body);

		} else {

			retLambdaExpr = builder.lambdaExpr(params[params.size() - 1].getType(), body, params);
		}

		// Adding the lambda function to the list of converted functions
		ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));

		VLOG(2) << retLambdaExpr << " + function declaration: " << funcDecl;
		return attachFuncAnnotations(retLambdaExpr, funcDecl);
		//return retLambdaExpr;
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );

	// this is a recurive function call
	if (ctx.isRecSubFunc) {
		/*
		 * if we are visiting a nested recursive type it means someone else will take care of building the rectype
		 * node, we just return an intermediate type
		 */
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

	std::for_each(components.begin(), components.end(),
			[ this, &definitions, &recVarRef ] (std::set<const FunctionDecl*>::value_type fd) {
/*
				ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
				assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
				this->ctx.currVar = tit->second;

				// test whether function has already been resolved
			if (*tit->second == *recVarRef) {
				return;
			}

			// we remove the variable from the list in order to fool the solver, 
			// in this way it will create a descriptor for this type (and he will 
			// not return the TypeVar associated with this recursive type). This behaviour
			// is enabled only when the isRecSubType flag is true
			this->ctx.recVarExprMap.erase(fd);

			// if the function is not defined in this translation unit, maybe it is defined 
			// in another we already loaded use the clang indexer to lookup the definition 
			// for this function declarations
			clang::idx::Entity&& funcEntity =
			clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), this->program.getClangProgram());
			ConversionFactory::TranslationUnitPair&& ret = this->program.getClangIndexer().getDefinitionFor(funcEntity);
			const TranslationUnit* oldTU = this->currTU;
			if ( ret.first ) {
				fd = ret.first;
				assert(ret.second && "Error loading translation unit for function definition");
				this->currTU = &Program::getTranslationUnit(ret.second);
			}

			const core::LambdaPtr& lambda =
			core::static_pointer_cast<const core::Lambda>(this->convertFunctionDecl(fd));
			assert(lambda && "Resolution of sub recursive lambda yields a wrong result");
			this->currTU = oldTU;
			// attach name annotation to the lambda

			definitions.push_back( this->builder.lambdaBinding(this->ctx.currVar, lambda) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;*/
		});
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert(std::make_pair(funcDecl, retLambdaExpr));
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
			[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
				auto fit = this->ctx.recVarExprMap.find(fd);
				assert(fit != this->ctx.recVarExprMap.end());

				FunctionDecl* decl = const_cast<FunctionDecl*>(fd);
				const TranslationUnit* rightTU = this->getTranslationUnitForDefinition(decl);

				assert ( rightTU );
				// save old TU
			const TranslationUnit* oldTU = this->currTU;

			// update the translation unit
			//this->currTU = &Program::getTranslationUnit(clangTU);
			this->currTU = rightTU;

			core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(decl, func) );

			func = this->attachFuncAnnotations(func, decl);

			currTU = oldTU;
		});

	VLOG(2) << "Converted Into: " << *retLambdaExpr;

	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

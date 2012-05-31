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

#include "insieme/frontend/cxx_convert.h"
#include "insieme/frontend/analysis/global_variables.h"

#include "insieme/utils/logging.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/type_utils.h"

#include "clang/Basic/FileManager.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/Frontend/TextDiagnosticPrinter.h>

using namespace clang;
using namespace insieme;

namespace fe = insieme::frontend;

namespace insieme {
namespace frontend {
namespace conversion {

core::ProgramPtr CXXASTConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl, bool isMain) {
	/*
	 * Handling of the translation unit: we have to make sure to load the translation unit where the function is
	 * defined before starting the parser otherwise reading literals results in wrong values.
	 */
	FunctionDecl* def = const_cast<FunctionDecl*>(funcDecl);
	const clang::idx::TranslationUnit* clangTU = mFact.getTranslationUnitForDefinition(def);
	assert(clangTU && "Translation unit for function not found.");
	const TranslationUnit* oldTU = mFact.currTU;

	mFact.setTranslationUnit(Program::getTranslationUnit(clangTU));

	// Extract globals starting from this entry point
	mFact.ctx.globalFuncMap.clear();
	analysis::CXXGlobalVarCollector globColl(mFact, clangTU, mFact.program.getClangIndexer(), mFact.ctx.globalFuncMap,
			mFact.ctx.polymorphicClassMap, mFact.ctx.offsetMap, mFact.ctx.virtualFunctionIdMap,
			mFact.ctx.finalOverriderMap);

	insieme::utils::Timer t("Globals.collect");
	globColl(def);

	VLOG(1)
		<< globColl;

	//~~~~ Handling of OMP thread private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Thread private requires to collect all the variables which are marked to be threadprivate
	omp::collectThreadPrivate(mFact.getPragmaMap(), mFact.ctx.thread_private);

	//~~~~ Handling of OMP flush  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//Omp flush clause forces the flushed variable to be volatile
	//omp::collectVolatile(mFact.getPragmaMap(), mFact.ctx.volatiles);
	//~~~~~~~~~~~~~~~~ end hack ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	mFact.ctx.globalStruct = globColl.createGlobalStruct();
	if (mFact.ctx.globalStruct.first) {
		mFact.ctx.globalVar = mFact.builder.variable(mFact.builder.refType(mFact.ctx.globalStruct.first));
		if (!mFact.ctx.polymorphicClassMap.empty()) {
			mFact.updateVFuncOffsetTableExpr();
			mFact.updateVFuncTableExpr();
		}
	}
	mFact.ctx.globalIdentMap = globColl.getIdentifierMap();
	VLOG(2)
		<< mFact.ctx.globalStruct.first;
	VLOG(2)
			<< mFact.ctx.globalStruct.second;
	VLOG(2)
			<< mFact.ctx.globalVar;
	t.stop();
	LOG(INFO)
		<< t;

	const core::ExpressionPtr& expr = core::static_pointer_cast<const core::Expression>(
			mFact.convertFunctionDecl(def, true));

	core::ExpressionPtr&& lambdaExpr = core::dynamic_pointer_cast<const core::LambdaExpr>(expr);

	// A marker node is allowed if it contains a lambda expression
	if (!lambdaExpr) {
		lambdaExpr = dynamic_pointer_cast<const core::MarkerExpr>(expr);

		if (lambdaExpr) {
			assert(
					static_pointer_cast<const core::MarkerExpr>(expr)->getSubExpression()->getNodeType() == core::NT_LambdaExpr && "Conversion of function returned a marker expression which does not contain a lambda expression");
		}
	}assert( lambdaExpr && "Conversion of function did not return a lambda expression");
	mProgram = core::Program::addEntryPoint(mFact.getNodeManager(), mProgram, lambdaExpr /*, isMain */);
	mFact.currTU = oldTU;
	return mProgram;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//											CONVERT VARIABLE DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::defaultInitVal(const core::TypePtr& type) const {
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

	LOG(ERROR)
		<< "Default initializer for type: '" << *type << "' not supported!";
	assert(false && "Default initialization type not defined");
}

core::DeclarationStmtPtr ConversionFactory::convertVarDecl(const clang::VarDecl* varDecl) {
	assert(currTU && "translation unit is null");
	// logging
	VLOG(1)
		<< "\n****************************************************************************************\n"
				<< "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n" << "-> at location: ("
				<< utils::location(varDecl->getLocation(), currTU->getCompiler().getSourceManager()) << "): ";
	if (VLOG_IS_ON(2)) {
		VLOG(2)
			<< "Dump of clang VarDecl: \n"
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

		//change thisstack2 only if we have a CXX object, pointer-to-CXX object, reference-to-CXX-object
		if (typePtr->isStructureOrClassType()) {

			ctx.thisStack2 = var;

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
// logging
	VLOG(2)
		<< "End of converting VarDecl";
	VLOG(1)
		<< "Converted into IR stmt: ";
	VLOG(1)
		<< "\t" << *retStmt;
	return retStmt;
}


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

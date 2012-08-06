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

#include "insieme/simple_backend/formatting/operator_formatting.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/statement_converter.h"
#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/job_manager.h"
#include "insieme/simple_backend/ir_extensions.h"

namespace insieme {
namespace simple_backend {
namespace formatting {

	using namespace core;

	namespace {

		/**
		 * A utility function aiming on inlining the given lazy expression.
		 */
		ExpressionPtr evalLazy(const NodePtr& lazy);

		/**
		 * Handles the initialization of a ref-variable.
		 */
		void handleRefConstructor(StmtConverter& converter, const TypePtr& resultType, const NodePtr& initValue, bool isNew);
	}

	FormatTable getBasicFormatTable(const core::lang::BasicGenerator& basic) {

		FormatTable res;
		IRExtensions extended(basic.getNodeManager());

		// use macros for specifying formats
		#include "insieme/simple_backend/formatting/formats_begin.inc"

		ADD_FORMATTER(res, basic.getRefDeref(), {

				// special handling of derefing result of ref.new or ref.var => bogus
				ExpressionPtr arg = ARG(0);
				if (core::analysis::isCallOf(arg, basic.getRefVar()) || core::analysis::isCallOf(arg, basic.getRefNew())) {
					// skip ref.var / ref.new
					CallExprPtr call = static_pointer_cast<const CallExpr>(arg);
					STMT_CONVERTER.convert(call->getArgument(0));
				} else if (core::analysis::isCallOf(arg, basic.getGetNull())) {
					// no * infront of 0
					VISIT_ARG(0);
				} else {
					// just add deref
					OUT("*");
					VISIT_ARG(0);
				}
		});

		ADD_FORMATTER(res, basic.getRefAssign(), {
				NodeManager& manager = CONTEXT.getNodeManager();
				ExpressionPtr target = static_pointer_cast<const Expression>(ARG(0));
				TypePtr valueType = static_pointer_cast<const RefType>(target->getType())->getElementType();
				converter.convert(CallExpr::get(manager, valueType, basic.getRefDeref(), toVector<ExpressionPtr>(target)));
				OUT(" = ");
				VISIT_ARG(1);
		});

		ADD_FORMATTER_DETAIL(res, basic.getRefVar(), false, {
//				handleRefConstructor(STMT_CONVERTER, CALL->getType(), ARG(0), false);


				// get some manager
				const core::lang::BasicGenerator& basic = CONTEXT.getNodeManager().getLangBasic();

				// extract type
				core::ExpressionPtr initValue = call->getArgument(0);
				core::TypePtr type = initValue->getType();
				const TypeManager::TypeInfo& valueTypeInfo = CONTEXT.getTypeManager().getTypeInfo(CODE, type);

				// check whether new memory location needs to be initialized
				if (!(core::analysis::isCallOf(initValue, basic.getVectorInitUndefined()) ||
						core::analysis::isCallOf(initValue, basic.getUndefined()))) {

					if (type->getNodeType() != core::NT_ArrayType) {

						// initialized variant
						OUT("&(("); OUT(valueTypeInfo.rValueName); OUT("){"); VISIT_ARG(0); OUT("})");

					} else {
						// not memcpy for arrays
						if (core::analysis::isRefType(type)) {
							OUT("&");
						}
						VISIT_ARG(0);
					}

				} else {

					// uninitialized variant
					OUT("&(("); OUT(valueTypeInfo.rValueName); OUT("){0})");

				}

		});


		ADD_FORMATTER_DETAIL(res, basic.getRefNew(), false, {
				//handleRefConstructor(STMT_CONVERTER, ARG(0), true);

				// use new operator of target type
				TypePtr resType = CALL->getType();
				const TypeManager::TypeInfo& info = CONTEXT.getTypeManager().getTypeInfo(CODE, resType);

				if (core::analysis::isCallOf(ARG(0), basic.getUndefined())) {
					CODE << "malloc(sizeof(" << info.lValueName << "))";
					return;
				}

				CODE->addDependency(info.utilities);

//				CODE << "_ref_new_" << CONTEXT.getNameManager().getName(resType) << "(";
				VISIT_ARG(0);
//				CODE << ")";
		});

		ADD_FORMATTER_DETAIL(res, basic.getRefDelete(), false, {

				// TODO: fix when frontend is producing correct code

				// do not free non-heap variables
				if (ARG(0)->getNodeType() == NT_Variable) {
					VariablePtr var = static_pointer_cast<const Variable>(ARG(0));
					if (CONTEXT.getVariableManager().getInfo(var).location != VariableManager::HEAP) {
						return;
					}
				}

				assert(ARG(0)->getType()->getNodeType() == NT_RefType && "Cannot free a non-ref type!");

				OUT("free(");
				VISIT_ARG(0);
				OUT(")");
		});

		// -- volatile operators --
		ADD_FORMATTER_DETAIL(res, basic.getVolatileMake(), false, { VISIT_ARG(0); });
		ADD_FORMATTER_DETAIL(res, basic.getVolatileRead(), false, { VISIT_ARG(0); });

		// -- flush operator --
		ADD_FORMATTER_DETAIL(res, basic.getFlush(), false, { OUT("ISBR_FLUSH("); VISIT_ARG(0); OUT(")"); });


		ADD_FORMATTER_DETAIL(res, basic.getIsNull(), false, {
				OUT("(");
				VISIT_ARG(0);
				OUT("==0)");
		});

		ADD_FORMATTER_DETAIL(res, basic.getGetNull(), false, {
				// this is just 0 (the rest is implicit in C)
				OUT("0");
		});

		ADD_FORMATTER(res, basic.getPtrEq(), {
				VISIT_ARG(0);
				OUT(" == ");
				VISIT_ARG(1);
		});

		ADD_FORMATTER_DETAIL(res, basic.getRefToAnyRef(), false, {
				OUT("(void*)");
				// externalize data before being converted to a void*
				STMT_CONVERTER.convertAsParameterToExternal(ARG(0));
		});

		ADD_FORMATTER_DETAIL(res, basic.getAnyRefToRef(), false, {

				// the name of the result type
				const TypePtr& resType = CALL->getType();

				OUT("(");
				OUT(CONTEXT.getTypeManager().getTypeName(CODE, resType));
				OUT(")");
				VISIT_ARG(0);
		});


		ADD_FORMATTER(res, basic.getScalarToArray(), {
				// get name of resulting type
//				TypeManager& typeManager = CONTEXT.getTypeManager();
//
//				const TypePtr& type = static_pointer_cast<const core::RefType>(call->getType())->getElementType();
//				const string& name = typeManager.getTypeInfo(CODE, type).lValueName;
//				OUT("&((");
//				OUT(name);
//				OUT("){");
				VISIT_ARG(0);
//				OUT("})");
		});

		ADD_FORMATTER(res, basic.getVectorToArray(), {
				// get name of resulting type
//				TypeManager& typeManager = CONTEXT.getTypeManager();
//
//				core::NodeManager& manager = CALL->getNodeManager();
//				core::IRBuilder builder(manager);
//
//				const TypePtr array = call->getType();
//				const string& name = typeManager.getTypeInfo(CODE, array).lValueName;
//				OUT("((");
//				OUT(name);
//				OUT("){(");
//				VISIT_ARG(0);
//				OUT(").data})");

				OUT("(");
				VISIT_ARG(0);
				OUT(").data");
		});

		ADD_FORMATTER(res, basic.getRefVectorToRefArray(), {
				// get name of resulting type
				TypeManager& typeManager = CONTEXT.getTypeManager();

				core::NodeManager& manager = CALL->getNodeManager();
				core::IRBuilder builder(manager);

				const TypePtr array = call->getType();
				const string& name = typeManager.getTypeInfo(CODE, array).lValueName;

//				OUT("&((");
				OUT("((");
				OUT(name);
				OUT("){(*");
				VISIT_ARG(0);
				OUT(").data})");
		});

		ADD_FORMATTER(res, basic.getVectorReduction(), {
				// type of the operation:
				// (vector<'elem,#l>, 'res, ('elem, 'res) -> 'res) -> 'res

				// get vector size
				core::ExpressionPtr vector = ARG(0);
				assert(
					( vector->getNodeType() == core::NT_VectorExpr
					|| vector->getNodeType() == core::NT_Variable )
							&& "Only supporting vector expression or variable!");

				core::VectorExprPtr vectorExpr = dynamic_pointer_cast<const core::VectorExpr>(vector);
				core::VectorTypePtr vectorType = static_pointer_cast<const core::VectorType>(vector->getType());

				core::IntTypeParamPtr vectorSize = vectorType->getSize();
				assert(vectorSize->getNodeType() == NT_ConcreteIntTypeParam && "Only supported for fixed vector sizes!");
				std::size_t size = static_pointer_cast<const core::ConcreteIntTypeParam>(vectorSize)->getValue();

				// compose unfolded reduction expression
				core::ExpressionPtr res = ARG(1);
				core::ExpressionPtr op = ARG(2);
				core::ExpressionPtr subscript = op->getNodeManager().getLangBasic().getVectorSubscript();
				core::TypePtr elementType = vectorType->getElementType();

				core::IRBuilder builder(op->getNodeManager());
				for (std::size_t i = 0; i<size; i++) {
					ExpressionPtr element;
					if (vectorExpr) {
						element = vectorExpr->getExpressions()[i];
					} else {
						element = builder.callExpr(elementType, subscript, toVector(vector, builder.uintLit(i)));
					}

					res = builder.callExpr(res->getType(), op, toVector(res, element));
				}

				// add code of reduction expression
				STMT_CONVERTER.convert(res);
		});


		ADD_FORMATTER(res, basic.getArrayCreate1D(), {

				// type of Operator: (type<'elem>, uint<8>) -> array<'elem,1>

				// create array using a constructor
				const TypeManager::TypeInfo& info = CONTEXT.getTypeManager().getTypeInfo(CODE, CALL->getType());
				const string& typeName = info.rValueName;

				const core::TypePtr elementType = static_pointer_cast<const core::ArrayType>(CALL->getType())->getElementType();
				const TypeManager::TypeInfo& elemInfo = CONTEXT.getTypeManager().getTypeInfo(CODE, elementType);
				const string& elementName = elemInfo.rValueName;

				CODE->addDependency(info.utilities);
				OUT("((");
				OUT(typeName);
				OUT("){malloc(sizeof(");
				OUT(elementName);
				OUT(")*");
				VISIT_ARG(1);
				OUT(")})");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArraySubscript1D(), false, {
//				bool isRef = call->getType()->getNodeType() == NT_RefType;
//				if (isRef) OUT("&(");
//				VISIT_ARG(0); OUT(".data["); VISIT_ARG(1); OUT("]");
//				if (isRef) OUT(")");

				if (core::analysis::isCallOf(ARG(0), basic.getRefDeref())) {
					VISIT(static_pointer_cast<const core::CallExpr>(ARG(0))->getArgument(0));
				} else {
					VISIT_ARG(0);
				}
				OUT("["); VISIT_ARG(1); OUT("]");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayRefElem1D(), false, {
//				OUT("&((*"); VISIT_ARG(0); OUT(").data["); VISIT_ARG(1); OUT("]"); OUT(")");
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("])");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayRefProjection1D(), false, {
				OUT("/* totally unclear */ &("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayView(), true, {
				VISIT_ARG(0); OUT("+"); VISIT_ARG(1);
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayViewPreInc(), true, { OUT("++"); VISIT_ARG(0); });
		ADD_FORMATTER_DETAIL(res, basic.getArrayViewPostInc(), true, { VISIT_ARG(0); OUT("++"); });
		ADD_FORMATTER_DETAIL(res, basic.getArrayViewPreDec(), true, { OUT("--"); VISIT_ARG(0); });
		ADD_FORMATTER_DETAIL(res, basic.getArrayViewPostDec(), true, { VISIT_ARG(0); OUT("--"); });


		ADD_FORMATTER_DETAIL(res, basic.getVectorSubscript(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT(".data["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorRefElem(), false, {
				OUT("&((*"); VISIT_ARG(0); OUT(").data["); VISIT_ARG(1); OUT("]"); OUT(")");
		});


		ADD_FORMATTER_DETAIL(res, basic.getVectorRefProjection(), false, {
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorInitUniform(), false, {

				// define resulting vector structure (includes init_uniform constructor)
				const string& typeName = CONTEXT.getTypeManager().getTypeName(CODE, CALL->getType());

				// use constructor to generate the vector
				OUT(typeName);
				OUT("_init_uniform(");
				VISIT_ARG(0);
				OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorInitUndefined(), false, {
				const string& typeName = CONTEXT.getTypeManager().getTypeName(CODE, CALL->getType());

				// just create a instance, random data inside
				OUT("(");
				OUT(typeName);
				OUT("){}");
		});


		// struct operations
		ADD_FORMATTER(res, basic.getCompositeRefElem(), {
				// NodeType type = static_pointer_cast<const RefType>(call->getType())->getElementType()->getNodeType();
				//if (!(type == NT_ArrayType || type == NT_VectorType)) {
					OUT("&"); // for all other types, the address operator is needed (for arrays and vectors implicite)
				//}
				OUT("((*"); VISIT_ARG(0); OUT(")."); VISIT_ARG(1); OUT(")");
		});
		ADD_FORMATTER(res, basic.getCompositeMemberAccess(), { VISIT_ARG(0); OUT("."); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getBoolToInt(), { VISIT_ARG(0); });

		ADD_FORMATTER(res, basic.getRealAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getUnsignedIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getUnsignedIntAnd(), { VISIT_ARG(0); OUT("&"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntOr(), { VISIT_ARG(0); OUT("|"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntXor(), { VISIT_ARG(0); OUT("^"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntNot(), { OUT("~"); VISIT_ARG(0); });

		ADD_FORMATTER(res, basic.getUnsignedIntLShift(), { VISIT_ARG(0); OUT("<<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntRShift(), { VISIT_ARG(0); OUT(">>"); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getUnsignedIntPreInc(), { OUT("++(*"); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, basic.getUnsignedIntPostInc(), { OUT("(*"); VISIT_ARG(0); OUT(")++"); });
		ADD_FORMATTER(res, basic.getUnsignedIntPreDec(), { OUT("--(*"); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, basic.getUnsignedIntPostDec(), { OUT("(*"); VISIT_ARG(0); OUT(")--"); });


		ADD_FORMATTER(res, basic.getSignedIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getSignedIntLShift(), { VISIT_ARG(0); OUT("<<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntRShift(), { VISIT_ARG(0); OUT(">>"); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getSignedIntPreInc(), { OUT("++(*"); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, basic.getSignedIntPostInc(), { OUT("(*"); VISIT_ARG(0); OUT(")++"); });
		ADD_FORMATTER(res, basic.getSignedIntPreDec(), { OUT("--(*"); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER(res, basic.getSignedIntPostDec(), { OUT("(*"); VISIT_ARG(0); OUT(")--"); });

		ADD_FORMATTER(res, basic.getSignedIntAnd(), { VISIT_ARG(0); OUT("&"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntOr(), { VISIT_ARG(0); OUT("|"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntXor(), { VISIT_ARG(0); OUT("^"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntNot(), { OUT("~"); VISIT_ARG(0); });

		ADD_FORMATTER(res, basic.getBoolLAnd(), { VISIT_ARG(0); OUT("&&"); STMT_CONVERTER.convert(evalLazy(ARG(1))); });
		ADD_FORMATTER(res, basic.getBoolLOr(), { VISIT_ARG(0); OUT("||"); STMT_CONVERTER.convert(evalLazy(ARG(1))); });
		ADD_FORMATTER(res, basic.getBoolNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getBoolEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getBoolLNot(), { OUT("!"); VISIT_ARG(0); });

		ADD_FORMATTER(res, basic.getCharNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getCharEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getCharGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getCharGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getCharLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getCharLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getRefEqual(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getUnsignedIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getUnsignedIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getSignedIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getSignedIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getRealEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(res, basic.getRealLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(res, basic.getRealToInt(), {
				OUT("(");
				OUT(CONTEXT.getTypeManager().getTypeName(CODE, CALL->getType(), true));
				OUT(")");
				VISIT_ARG(0);
		});

		// string conversion
		ADD_FORMATTER_DETAIL(res, basic.getStringToCharPointer(), false, {

//				core::IRBuilder builder(CONTEXT.getNodeManager());
//				TypePtr array = builder.arrayType(CONTEXT.getLangBasic().getChar());
//
//				OUT("&((");
//				OUT(CONTEXT.getTypeManager().getTypeName(CODE, array, true));
//				OUT("){");
				VISIT_ARG(0);
//				OUT("})");
		});

		ADD_FORMATTER(res, extended.lazyITE, {
				OUT("("); VISIT_ARG(0); OUT(")?("); VISIT_ARG(1); OUT("):("); VISIT_ARG(2); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, extended.initGlobals, false, {

				// only one initialization is allowed
				assert(!CONTEXT.getVariableManager().getGlobalVarFragment() && "Already initialized!");

				TypePtr globalType = ARG(0)->getType();
				if (globalType->getNodeType() == NT_RefType) {
					globalType = static_pointer_cast<const core::RefType>(globalType)->getElementType();
				}

				// create the global variable definition
				CodeFragmentPtr globals = CodeFragment::createNew("global data");

				// get type of global struct
				globals << CONTEXT.getTypeManager().getTypeName(globals, globalType) << " " << IRExtensions::GLOBAL_ID << ";\n";

				CODE->addDependency(globals);
				CONTEXT.getVariableManager().setGlobalVarFragment(globals);


				// use statement manager to produce initialization code
				LiteralPtr globalLiteral = Literal::get(CONTEXT.getNodeManager(), ARG(0)->getType(), IRExtensions::GLOBAL_ID);
				STMT_CONVERTER.initStruct(globalLiteral, ARG(0));
		});

		ADD_FORMATTER_DETAIL(res, basic.getSizeof(), false, {
				OUT("sizeof(");
				GenericTypePtr type = dynamic_pointer_cast<const GenericType>(
						static_pointer_cast<const Expression>(ARG(0))->getType()
				);
				assert(type && "Illegal argument to sizeof operator");
				TypePtr target = type->getTypeParameter()[0];
				OUT(CONTEXT.getTypeManager().getTypeName(CODE, target, true));
				OUT(")");
		});


		// handle parallel operators
		ADD_FORMATTER_DETAIL(res, basic.getParallel(), false, { OUT("isbr_parallel("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(res, basic.getMerge(), false, { OUT("isbr_merge("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(res, basic.getBarrier(), false, { OUT("isbr_barrier("); VISIT_ARG(0); OUT(")"); });

		ADD_FORMATTER_DETAIL(res, basic.getGetThreadGroup(), false, { OUT("isbr_getThreadGroup("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(res, basic.getGetThreadId(), false, { OUT("isbr_getThreadId("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(res, basic.getGetGroupSize(), false, { OUT("isbr_getGroupSize("); VISIT_ARG(0); OUT(")"); });


		ADD_FORMATTER_DETAIL(res, basic.getPFor(), false, {
				CONTEXT.getJobManager().createPFor(CODE, call);
		});


		ADD_FORMATTER_DETAIL(res, basic.getLockCreate(), false, {
				OUT("isbr_lock_create()");
		});

		ADD_FORMATTER_DETAIL(res, basic.getLockAcquire(), false, {
				OUT("isbr_lock_acquire("); VISIT_ARG(0); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getLockRelease(), false, {
				OUT("isbr_lock_release("); VISIT_ARG(0); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getPick(), false, {
			OUT("0");
		});

		ADD_FORMATTER_DETAIL(res, basic.getSelect(), false,  {
			core::IRBuilder builder(ARG(0)->getNodeManager());
			OUT("((");
			STMT_CONVERTER.convert(builder.callExpr(ARG(2), ARG(0), ARG(1)));
			OUT(")?("); VISIT_ARG(0); OUT("):("); VISIT_ARG(1); OUT("))");
		});

		ADD_FORMATTER_DETAIL(res, basic.getCloogFloor(), false, {
			// ((a*b>0)?(a/b):(-(-a/b+(-a%b!=0))))
			OUT("((("); VISIT_ARG(0); OUT(")*("); VISIT_ARG(1); OUT(")>0)?");
			OUT("(("); VISIT_ARG(0); OUT(")/("); VISIT_ARG(1); OUT(")):");
			OUT("(-(-("); VISIT_ARG(0); OUT(")/("); VISIT_ARG(1); OUT(")+(-("); VISIT_ARG(0); OUT(")%("); VISIT_ARG(1); OUT(")!=0))))");
		});

		ADD_FORMATTER_DETAIL(res, basic.getCloogCeil(), false, {

			// ((a*b>0)?(a/b + (a%b!=0)):(-(-a/b)))
			OUT("(("); VISIT_ARG(0); OUT("*"); VISIT_ARG(1); OUT(">0)?");
			OUT("("); VISIT_ARG(0); OUT("/"); VISIT_ARG(1); OUT("+("); VISIT_ARG(0); OUT("%"); VISIT_ARG(1); OUT("!=0)):");
			OUT("(-(-"); VISIT_ARG(0); OUT("/"); VISIT_ARG(1); OUT("!=0)))");
		});

		ADD_FORMATTER_DETAIL(res, basic.getCloogMod(), true, {
			OUT("("); VISIT_ARG(0); OUT(")%("); VISIT_ARG(1); OUT(")");
		});

		#include "insieme/simple_backend/formatting/formats_end.inc"

		return res;
	}


	namespace detail {

		/**
		 * A utility function to obtain the n-th argument within the given call expression.
		 *
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the requested argument
		 * @return the requested argument or a NULL pointer in case there is no such argument
		 */
		core::ExpressionPtr getArgument(const core::CallExprPtr& call, unsigned n) {
			auto arguments = call->getArguments();
			if (n < arguments.size()) {
				return arguments[n];
			}
			return core::ExpressionPtr();
		}

		/**
		 * A utility function visiting the n-th argument of a call expression.
		 *
		 * @param converter the converter to be used for the actual conversion
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the argument to be visited; in case there is no such argument, nothing will be visited
		 */
		void visitArgument(StmtConverter& converter, const core::CallExprPtr& call, unsigned n) {
			core::ExpressionPtr argument = getArgument(call, n);
			if (argument) {
				converter.convert(argument);
			}
		}
	}


	namespace {

		ExpressionPtr evalLazy(const NodePtr& lazy) {

			NodeManager& manager = lazy->getNodeManager();

			ExpressionPtr exprPtr = dynamic_pointer_cast<const Expression>(lazy);
			assert(exprPtr && "Lazy is not an expression!");

			FunctionTypePtr funType = dynamic_pointer_cast<const FunctionType>(exprPtr->getType());
			assert(funType && "Illegal lazy type!");

			// form call expression
			CallExprPtr call = CallExpr::get(manager, funType->getReturnType(), exprPtr, toVector<ExpressionPtr>());
			return core::transform::tryInlineToExpr(manager, call);
		}

		void handleRefConstructor(StmtConverter& converter, const TypePtr& resultType, const NodePtr& initValue, bool isNew) {

			// check input parameters
			assert(dynamic_pointer_cast<const Expression>(initValue) && "Init Value is not an expression!");

			// quick check for arrays => extra handling
			const core::lang::BasicGenerator& basic = converter.getConversionContext().getLangBasic();
//			if (core::analysis::isCallOf(initValue, basic.getArrayCreate1D()) ||
//				core::analysis::isCallOf(initValue, basic.getArrayCreateND())) {
//
//				// vector creation is sufficient
//				converter.convert(initValue);
//				return;
//			}


			// extract type
			CodeFragmentPtr code = converter.getCurrentCodeFragment();
			TypePtr type = static_pointer_cast<const Expression>(initValue)->getType();
			string typeName = converter.getConversionContext().getTypeManager().getTypeName(code, type, true);
			string resTypeName = converter.getConversionContext().getTypeManager().getTypeName(code, resultType, false);

			// use stack or heap allocator
			string allocator = (isNew)?"malloc":"alloca";

			// TODO: use pattern matching!

			// check for vector init undefined and undefined
			if (core::analysis::isCallOf(initValue, basic.getVectorInitUndefined()) || core::analysis::isCallOf(initValue, basic.getUndefined())) {
				code << allocator << "(sizeof(" << typeName << "))";
				return;
			}

			if (isNew && core::analysis::isCallOf(initValue, basic.getVectorInitUniform())) {
				NodePtr param = static_pointer_cast<const CallExpr>(initValue)->getArguments()[0];

				// iterate through multiple vector init uniform calls
				while (core::analysis::isCallOf(param, basic.getVectorInitUniform())) {
					param = static_pointer_cast<const CallExpr>(param)->getArguments()[0];
				}

				// innermost has to be a ref-var call with a literal 0
				if (core::analysis::isCallOf(param, basic.getRefVar()) || core::analysis::isCallOf(param, basic.getRefNew())) {
					const NodePtr& refVar = static_pointer_cast<const CallExpr>(param)->getArguments()[0];
					if (LiteralPtr literal = dynamic_pointer_cast<const Literal>(refVar)) {
						string value = literal->getStringValue();
						if (basic.isInitZero(literal) || value == "0" || value == "0.0" || value == "\0") {
							code << "calloc(sizeof(" << typeName << "), 1)";
							return;
						}
					}
					if (core::analysis::isCallOf(refVar, basic.getInitZero())) {
						code << "calloc(sizeof(" << typeName << "), 1)";
						return;
					}
				}
			}

			// TODO: use memset for other initializations => see memset!!

			if (!isNew) {
				converter.convert(initValue);
				return;
			}

			code << "((" << resTypeName << ")";
			code << "memcpy(";
			code << allocator << "(";
			code << "sizeof(";
			code << typeName;

			code << ")), &(";
			converter.convert(initValue);
			code << "), sizeof(";

//			code << ")), &((";
//			code << typeName;
//			code << "[]){";
//			converter.convert(initValue);
//			code << "}), sizeof(";

			code << typeName;
			code << ")))";
		}
	}

} // end namespace formatting
} // end namespace simple_backend
} // end namespace insieme

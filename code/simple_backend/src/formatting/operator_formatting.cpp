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
#include "insieme/core/expressions.h"
#include "insieme/core/ast_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/simple_backend/code_management.h"
#include "insieme/simple_backend/statement_converter.h"
#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/job_manager.h"


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
		void handleRefConstructor(StmtConverter& converter, const NodePtr& initValue, bool isNew);
	}

	FormatTable getBasicFormatTable(const core::lang::BasicGenerator& basic) {

		FormatTable res;

		// use macros for specifying formats
		#include "insieme/simple_backend/formatting/formats_begin.inc"

		ADD_FORMATTER(res, basic.getRefDeref(), {
//				NodeType type = static_pointer_cast<const RefType>(ARG(0)->getType())->getElementType()->getNodeType();
				// do not add a dereferning operator to arrays and vectory => implicite within C
//				if (!(type == NT_ArrayType || type == NT_VectorType)) {
					OUT("*");
//				}
				VISIT_ARG(0);
		});

		ADD_FORMATTER(res, basic.getRefAssign(), {
				NodeManager& manager = CONTEXT.getNodeManager();
				ExpressionPtr target = static_pointer_cast<const Expression>(ARG(0));
				TypePtr valueType = static_pointer_cast<const RefType>(target->getType())->getElementType();
				converter.convert(CallExpr::get(manager, valueType, basic.getRefDeref(), toVector<ExpressionPtr>(target)));
				OUT(" = ");
				VISIT_ARG(1);
		});

		ADD_FORMATTER_DETAIL(res, basic.getRefVar(), false, { handleRefConstructor(STMT_CONVERTER, ARG(0), false); });
		ADD_FORMATTER_DETAIL(res, basic.getRefNew(), false, { handleRefConstructor(STMT_CONVERTER, ARG(0), true); });

		ADD_FORMATTER(res, basic.getRefDelete(), {

				const TypePtr& type = ARG(0)->getType();
				assert(type->getNodeType() == NT_RefType && "Cannot free a non-ref type!");

				OUT("free((*");
				VISIT_ARG(0);
				OUT(")");

				const TypePtr& elementType = static_pointer_cast<const RefType>(type)->getElementType();
				auto elementNodeType = elementType->getNodeType();
				if (elementNodeType == NT_ArrayType) {
					OUT(".data");
				}
				OUT(")");
		});

		ADD_FORMATTER(res, basic.getScalarToArray(), {
				// get name of resulting type
				TypeManager& typeManager = CONTEXT.getTypeManager();

				const TypePtr& type = static_pointer_cast<const core::RefType>(call->getType())->getElementType();
				const string& name = typeManager.getTypeInfo(CODE, type).lValueName;
				OUT("&((");
				OUT(name);
				OUT("){");
				VISIT_ARG(0);
				OUT(",{1}})");
		});

		ADD_FORMATTER(res, basic.getArrayCreate1D(), {


				const string& typeName = CONTEXT.getTypeManager().getTypeName(CODE, CALL->getType());

				OUT("((" + typeName + "){");

				// test whether the size is fixed to 1
				if (ARG(1)->getNodeType() == NT_Literal && static_pointer_cast<const Literal>(ARG(1))->getValue() == "1") {
					// special handling of arrays with a single element
					ASTBuilder builder(call->getNodeManager());
					NodePtr init = ARG(0);
					if (core::analysis::isCallOf(init, basic.getRefVar())) {
						init = builder.refNew(static_pointer_cast<const CallExpr>(init)->getArguments()[0]);
						STMT_CONVERTER.convert(init);
					} else if (core::analysis::isCallOf(init, basic.getRefNew())) {
						STMT_CONVERTER.convert(init);
					} else {
						STMT_CONVERTER.convert(builder.refNew(static_pointer_cast<const Expression>(ARG(0))));
					}

				} else {

					// ensure array is randomly initialized
					ExpressionPtr initValue = ARG(0);
					assert(!core::analysis::isCallOf(initValue, basic.getRefVar()) && "Initialization of arrays based on ref-elements not supported yet!" );
					assert(core::analysis::isCallOf(initValue, basic.getUndefined()) && "Initializing arrays with concrete values not supported yet.");

					// all arrays are allocated on the HEAP
					OUT("malloc(");
					OUT("sizeof(");
					TypePtr type = static_pointer_cast<const Expression>(ARG(0))->getType();
					OUT(CONTEXT.getTypeManager().getTypeName(CODE, type, true));
					OUT(")*");
					VISIT_ARG(1);
					OUT(")");
				}

				OUT(",{");
				VISIT_ARG(1);
				OUT("}})");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArraySubscript1D(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT(".data["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayRefElem1D(), false, {

//				RefTypePtr targetType = static_pointer_cast<const RefType>(ARG(0)->getType());
//				NodeType elementType = static_pointer_cast<const SingleElementType>(targetType->getElementType())->getElementType()->getNodeType();
//				if (elementType != NT_VectorType && elementType != NT_ArrayType ) {
					OUT("&");
//				}

				// check whether input variable needs to be dereferenced
//				bool insertDeref = (ARG(0)->getNodeType() == NT_Variable);
//				insertDeref = insertDeref && CONTEXT.getVariableManager().getInfo(static_pointer_cast<const Variable>(ARG(0))).location == VariableManager::STACK;
//
//				if (insertDeref) {
					OUT("((*"); VISIT_ARG(0); OUT(").data["); VISIT_ARG(1); OUT("]"); OUT(")");
//				} else {
//					OUT("("); VISIT_ARG(0); OUT(".data["); VISIT_ARG(1); OUT("]"); OUT(")");
//				}
		});

		ADD_FORMATTER_DETAIL(res, basic.getArrayRefProjection1D(), false, {
				OUT("/* totaly unclear */ &("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorSubscript(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorRefProjection(), false, {
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(res, basic.getVectorInitUniform(), false, { OUT("{}"); });
		ADD_FORMATTER_DETAIL(res, basic.getVectorInitUndefined(), false, { OUT("{}"); });


		// struct operations
		ADD_FORMATTER(res, basic.getCompositeRefElem(), {
				// NodeType type = static_pointer_cast<const RefType>(call->getType())->getElementType()->getNodeType();
				//if (!(type == NT_ArrayType || type == NT_VectorType)) {
					OUT("&"); // for all other types, the address operator is needed (for arrays and vectors implicite)
				//}
				OUT("((*"); VISIT_ARG(0); OUT(")."); VISIT_ARG(1); OUT(")");
		});
		ADD_FORMATTER(res, basic.getCompositeMemberAccess(), { VISIT_ARG(0); OUT("."); VISIT_ARG(1); });

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


		// string conversion
		ADD_FORMATTER_DETAIL(res, basic.getStringToCharPointer(), false, { OUT("&("); VISIT_ARG(0); OUT(")"); });


		ADD_FORMATTER(res, basic.getIfThenElse(), {
				OUT("("); VISIT_ARG(0); OUT(")?(");
				STMT_CONVERTER.convert(evalLazy(ARG(1)));
				OUT("):(");
				STMT_CONVERTER.convert(evalLazy(ARG(2)));
				OUT(")");
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

		void handleRefConstructor(StmtConverter& converter, const NodePtr& initValue, bool isNew) {

			// check input parameters
			assert(dynamic_pointer_cast<const Expression>(initValue) && "Init Value is not an expression!");

			// quick check for arrays => extra handling
			const core::lang::BasicGenerator& basic = converter.getConversionContext().getLangBasic();
			if (core::analysis::isCallOf(initValue, basic.getArrayCreate1D()) ||
				core::analysis::isCallOf(initValue, basic.getArrayCreateND())) {

				// vector creation is sufficient
				converter.convert(initValue);
				return;
			}


			// extract type
			CodeFragmentPtr code = converter.getCurrentCodeFragment();
			TypePtr type = static_pointer_cast<const Expression>(initValue)->getType();
			string typeName = converter.getConversionContext().getTypeManager().getTypeName(code, type, true);

			// use stack or heap allocator
			string allocator = (isNew)?"malloc":"alloca";

			// special handling of some initialization values
			string stmt = toString(*initValue);

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
						string value = literal->getValue();
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

			code << "memcpy(";
			code << allocator << "(";
			code << "sizeof(";
			code << typeName;
			code << ")), &((";
			code << typeName;
			code << "[]){";
			converter.convert(initValue);
			code << "}), sizeof(";
			code << typeName;
			code << "))";
		}
	}

} // end namespace formatting
} // end namespace simple_backend
} // end namespace insieme

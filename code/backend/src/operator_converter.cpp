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

#include "insieme/backend/operator_converter.h"

#include <functional>

#include "insieme/backend/converter.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/ir_extensions.h"

#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/backend/operator_converter_begin.inc"

#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace backend {

	namespace {


		c_ast::ExpressionPtr getAssignmentTarget(ConversionContext& context, const core::ExpressionPtr& expr) {

			// convert expression
			c_ast::ExpressionPtr res = context.getConverter().getStmtConverter().convertExpression(context, expr);

			// special handling for variables representing data indirectly
			if (expr->getNodeType() == core::NT_Variable) {
				core::VariablePtr var = static_pointer_cast<const core::Variable>(expr);

				if (context.getVariableManager().getInfo(var).location == VariableInfo::INDIRECT) {
					return res;
				}
			}

			// a deref is required (implicit in C)
			return c_ast::deref(res);
		}

	}


	OperatorConverterTable getBasicOperatorTable(core::NodeManager& manager) {
		const core::lang::BasicGenerator& basic = manager.getBasicGenerator();

		OperatorConverterTable res;

//		std::function<int(int,int)> x;
//
//		x = &(((struct {
//			int dummy;
//			static int f(int a, int b) { return a; };
//		}){0}).f);


		// -- booleans --
		res[basic.getBoolLAnd()] = OP_CONVERTER({ return c_ast::logicAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getBoolLOr()]  = OP_CONVERTER({ return c_ast::logicOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getBoolLNot()] = OP_CONVERTER({ return c_ast::logicNot(CONVERT_ARG(0)); });

		res[basic.getBoolEq()]   = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getBoolNe()]   = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getBoolToInt()] = OP_CONVERTER({ return CONVERT_ARG(0); });


		// -- unsigned integers --

		res[basic.getUnsignedIntAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntMod()] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getUnsignedIntAnd()] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntOr()] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntXor()] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntNot()] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		res[basic.getUnsignedIntLShift()] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntRShift()] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getUnsignedIntPreInc()]  = OP_CONVERTER({ return c_ast::preInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getUnsignedIntPostInc()] = OP_CONVERTER({ return c_ast::postInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getUnsignedIntPreDec()]  = OP_CONVERTER({ return c_ast::preDec(getAssignmentTarget(context, ARG(0))); });
		res[basic.getUnsignedIntPostDec()] = OP_CONVERTER({ return c_ast::postDec(getAssignmentTarget(context, ARG(0))); });

		res[basic.getUnsignedIntEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getUnsignedIntLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- unsigned integers --

		res[basic.getSignedIntAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntMod()] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getSignedIntAnd()] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntOr()] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntXor()] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntNot()] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		res[basic.getSignedIntLShift()] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntRShift()] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getSignedIntPreInc()]  = OP_CONVERTER({ return c_ast::preInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getSignedIntPostInc()] = OP_CONVERTER({ return c_ast::postInc(getAssignmentTarget(context, ARG(0))); });
		res[basic.getSignedIntPreDec()]  = OP_CONVERTER({ return c_ast::preDec(getAssignmentTarget(context, ARG(0))); });
		res[basic.getSignedIntPostDec()] = OP_CONVERTER({ return c_ast::postDec(getAssignmentTarget(context, ARG(0))); });

		res[basic.getSignedIntEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getSignedIntLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- reals --

		res[basic.getRealAdd()] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealSub()] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealMul()] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealDiv()] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getRealEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRealLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		res[basic.getRealToInt()] = OP_CONVERTER({ return c_ast::cast(CONVERT_RES_TYPE, CONVERT_ARG(0)); });

		// -- characters --

		res[basic.getCharEq()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharNe()] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharGe()] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharGt()] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharLt()] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getCharLe()] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });


		// -- references --

		res[basic.getRefEqual()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		res[basic.getRefDeref()] = OP_CONVERTER({
			const core::TypePtr elementType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(elementType);
			context.getDependencies().insert(info.definition);
			return c_ast::deref(CONVERT_ARG(0));
		});

		res[basic.getRefAssign()] = OP_CONVERTER({
			return c_ast::assign(getAssignmentTarget(context, ARG(0)), CONVERT_ARG(1));
		});

		res[basic.getRefVar()] = OP_CONVERTER({ return CONVERT_ARG(0); });
		res[basic.getRefNew()] = OP_CONVERTER( {

			// get result type information
			core::RefTypePtr resType = static_pointer_cast<const core::RefType>(call->getType());
			const RefTypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(resType);

			// special handling for requesting a un-initialized memory cell
			if (core::analysis::isCallOf(ARG(0), LANG_BASIC.getUndefined())) {
				context.getIncludes().insert(string("stdlib.h"));

				c_ast::ExpressionPtr size = c_ast::sizeOf(CONVERT_TYPE(resType->getElementType()));
				return c_ast::call(context.getConverter().getCNodeManager()->create("malloc"), size);
			}

			// use a call to the ref_new operator of the ref type
			context.getDependencies().insert(info.newOperator);
			return c_ast::call(info.newOperatorName, CONVERT_ARG(0));

		});


		// -- strings --

		res[basic.getStringToCharPointer()] = OP_CONVERTER({
			// resulting code:  &((<array_type>){string_pointer})
			core::TypePtr array = static_pointer_cast<const core::RefType>(call->getType())->getElementType();
			return c_ast::ref(c_ast::init(CONVERT_TYPE(array), CONVERT_ARG(0)));
		});


		// -- arrays --

		res[basic.getArraySubscript1D()] = OP_CONVERTER({
			return c_ast::subscript(c_ast::access(CONVERT_ARG(0), "data"), CONVERT_ARG(1));
		});

		res[basic.getArrayRefElem1D()] = OP_CONVERTER({
			return c_ast::ref(c_ast::subscript(c_ast::access(c_ast::parenthese(c_ast::deref(CONVERT_ARG(0))), "data"), CONVERT_ARG(1)));
		});

		res[basic.getScalarToArray()] = OP_CONVERTER({
			// initialize an array instance
			//   Operator Type: (ref<'a>) -> ref<array<'a,1>>
			const TypeInfo& info = GET_TYPE_INFO(core::analysis::getReferencedType(call->getType()));
			context.getDependencies().insert(info.definition);
			c_ast::InitializerPtr res = c_ast::init(info.rValueType, CONVERT_ARG(0));
			if (context.getConverter().getConfig().supportArrayLength) {
				res->values.push_back(C_NODE_MANAGER->create<c_ast::OpaqueCode>("{1}"));
			}
			return c_ast::ref(res);
		});

		// -- vectors --

		res[basic.getVectorToArray()] = OP_CONVERTER({
			// initialize an array instance
			//   Operator Type: (vector<'elem,#l>) -> array<'elem,1>
			const TypeInfo& info = GET_TYPE_INFO(call->getType());
			context.getDependencies().insert(info.definition);
			c_ast::InitializerPtr res = c_ast::init(info.rValueType, c_ast::access(CONVERT_ARG(0), "data"));
			if (context.getConverter().getConfig().supportArrayLength) {
				res->values.push_back(C_NODE_MANAGER->create<c_ast::OpaqueCode>("{1}"));
			}
			return res;
		});


		// -- structs --

		res[basic.getCompositeRefElem()] = OP_CONVERTER({
			// signature of operation:
			//		(ref<'a>, identifier, type<'b>) -> ref<'b>

			// add a dependency to the accessed type definition before accessing the type
			const core::TypePtr elementType = core::analysis::getReferencedType(ARG(0)->getType());
			const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(elementType);
			context.getDependencies().insert(info.definition);

			// access the type
			return c_ast::ref(c_ast::access(c_ast::deref(CONVERT_ARG(0)), CONVERT_ARG(1)));
		});


		// -- others --

		res[basic.getIfThenElse()] = OP_CONVERTER({
			// IF-THEN-ELSE literal: (bool, () -> 'b, () -> 'b) -> 'b")
			core::CallExprPtr callA = core::CallExpr::get(NODE_MANAGER, call->getType(), ARG(1), vector<core::ExpressionPtr>());
			core::CallExprPtr callB = core::CallExpr::get(NODE_MANAGER, call->getType(), ARG(2), vector<core::ExpressionPtr>());
			return c_ast::ite(CONVERT_ARG(0), CONVERT_EXPR(callA), CONVERT_EXPR(callB));
		});


		// -- IR extensions --

		IRExtensions ext(manager);
		res[ext.lazyITE] = OP_CONVERTER({
			// simple translation of lazy ITE into C/C++ ?: operators
			return c_ast::ite(CONVERT_ARG(0), CONVERT_ARG(1), CONVERT_ARG(2));
		});

		// table complete => return table
		return res;
	}



} // end namespace backend
} // end namespace insieme

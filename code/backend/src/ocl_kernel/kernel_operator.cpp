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

#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/ocl_kernel/kernel_operator.h"
#include "insieme/backend/ocl_kernel/kernel_extensions.h"
#include "insieme/backend/ocl_kernel/kernel_type_handler.h"

#include "insieme/core/ir_builder.h"
#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace ocl_kernel{

	c_ast::ExpressionPtr getAssignmentTarget(ConversionContext& context, const core::ExpressionPtr& expr) {
		c_ast::ExpressionPtr res = context.getConverter().getStmtConverter().convertExpression(context, expr);
		return c_ast::deref(res);
	}

	OperatorConverterTable& addOpenCLKernelSpecificOps(core::NodeManager& manager, OperatorConverterTable& table) {

		auto& ext = manager.getLangExtension<Extensions>();
		const core::lang::BasicGenerator& basic = manager.getLangBasic();
		core::IRBuilder builder(manager);

		#include "insieme/backend/operator_converter_begin.inc"

		table[ext.unwrapLocal] 		= OP_CONVERTER({ return CONVERT_ARG(0); });
		table[ext.unwrapGlobal] 	= OP_CONVERTER({ return CONVERT_ARG(0); });
		table[ext.unwrapConst] 		= OP_CONVERTER({ return CONVERT_ARG(0); });

		table[ext.wrapLocal] 		= OP_CONVERTER({ return CONVERT_ARG(0); });
		table[ext.wrapGlobal] 		= OP_CONVERTER({ return CONVERT_ARG(0); });
		table[ext.wrapConst] 		= OP_CONVERTER({ return CONVERT_ARG(0); });

		table[ext.kernelWrapper] = OP_CONVERTER({
			// verify that argument is indeed a lambda exression => nothing else supported!
			assert(ARG(0)->getNodeType() == core::NT_LambdaExpr && "Argument has to be a lambda!");

			// extract and convert lambda expression
			core::LambdaExprPtr lambda = static_pointer_cast<const core::LambdaExpr>(ARG(0));
			auto res = CONVERT_ARG(0);

			// modify lambda expression just converted by setting the OCL kernel flag
			c_ast::FunctionPtr fun = GET_FUNCTION_INFO(lambda).function;
			fun->flags = fun->flags | c_ast::Function::OCL_KERNEL;

			return C_NODE_MANAGER->create<c_ast::Literal>("");
		});

		table[basic.getVectorInitUniform()]	= OP_CONVERTER({
			// IR: vector.init.uniform(cast<real<4>>(2), 4); ==> C: (float4)(2);
			c_ast::TypePtr cType = CONVERT_TYPE(call->getType());
			return c_ast::cast(cType, CONVERT_ARG(0));
		});

		table[basic.getVectorRefElem()] = OP_CONVERTER({
			// IR: v32&[12]  ==> C: v32.sC
			std::string str = oclRefTypeToString(LANG_BASIC, call->getArgument(0)->getType());
			if(!str.empty()){
				core::LiteralPtr lit;
				if (core::CastExprPtr cast = dynamic_pointer_cast<const core::CastExpr>(call->getArgument(1)))
					lit = static_pointer_cast<const core::Literal>(cast->getSubExpression());
				else
					lit = static_pointer_cast<const core::Literal>(call->getArgument(1));
				std::stringstream stream;
				stream << std::hex << utils::numeric_cast<int>(lit->getStringValue());
				return c_ast::ref(c_ast::access(c_ast::deref(CONVERT_ARG(0)), "s" + stream.str()));
			}

			return c_ast::ref(c_ast::subscript(c_ast::access(c_ast::deref(CONVERT_ARG(0)), "data"), CONVERT_ARG(1)));
		});

		table[ext.convertBuiltin] = OP_CONVERTER({
			core::TypePtr type = static_pointer_cast<const core::GenericType>(call->getArgument(1)->getType())->getTypeParameter(0);
			c_ast::ExpressionPtr fun = C_NODE_MANAGER->create<c_ast::Literal>("convert_" + oclTypeToString(LANG_BASIC, type));
			return c_ast::call(fun, CONVERT_ARG(0));
		});

		// unsigned integers
		table[builder.pointwise(basic.getUnsignedIntAdd())] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntSub())] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntMul())] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntDiv())] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntMod())] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getUnsignedIntAnd())] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntOr())] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntXor())] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntNot())] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		table[builder.pointwise(basic.getUnsignedIntLShift())] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntRShift())] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getUnsignedIntPreInc())]  = OP_CONVERTER({ return c_ast::preInc(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getUnsignedIntPostInc())] = OP_CONVERTER({ return c_ast::postInc(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getUnsignedIntPreDec())]  = OP_CONVERTER({ return c_ast::preDec(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getUnsignedIntPostDec())] = OP_CONVERTER({ return c_ast::postDec(getAssignmentTarget(context, ARG(0))); });

		table[builder.pointwise(basic.getUnsignedIntEq())] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntNe())] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntGe())] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntGt())] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntLt())] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getUnsignedIntLe())] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		//signed integers
		table[builder.pointwise(basic.getSignedIntAdd())] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntSub())] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntMul())] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntDiv())] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntMod())] = OP_CONVERTER({ return c_ast::mod(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getSignedIntAnd())] = OP_CONVERTER({ return c_ast::bitwiseAnd(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntOr())] =  OP_CONVERTER({ return c_ast::bitwiseOr(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntXor())] = OP_CONVERTER({ return c_ast::bitwiseXor(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntNot())] = OP_CONVERTER({ return c_ast::bitwiseNot(CONVERT_ARG(0)); });

		table[builder.pointwise(basic.getSignedIntLShift())] = OP_CONVERTER({ return c_ast::lShift(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntRShift())] = OP_CONVERTER({ return c_ast::rShift(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getSignedIntPreInc())]  = OP_CONVERTER({ return c_ast::preInc(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getSignedIntPostInc())] = OP_CONVERTER({ return c_ast::postInc(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getSignedIntPreDec())]  = OP_CONVERTER({ return c_ast::preDec(getAssignmentTarget(context, ARG(0))); });
		table[builder.pointwise(basic.getSignedIntPostDec())] = OP_CONVERTER({ return c_ast::postDec(getAssignmentTarget(context, ARG(0))); });

		table[builder.pointwise(basic.getSignedIntEq())] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntNe())] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntGe())] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntGt())] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntLt())] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getSignedIntLe())] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		// reals
		table[builder.pointwise(basic.getRealAdd())] = OP_CONVERTER({ return c_ast::add(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealSub())] = OP_CONVERTER({ return c_ast::sub(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealMul())] = OP_CONVERTER({ return c_ast::mul(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealDiv())] = OP_CONVERTER({ return c_ast::div(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getRealEq())] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealNe())] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealGe())] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealGt())] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealLt())] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getRealLe())] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		// char
		table[builder.pointwise(basic.getCharEq())] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getCharNe())] = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getCharGe())] = OP_CONVERTER({ return c_ast::ge(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getCharGt())] = OP_CONVERTER({ return c_ast::gt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getCharLt())] = OP_CONVERTER({ return c_ast::lt(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getCharLe())] = OP_CONVERTER({ return c_ast::le(CONVERT_ARG(0), CONVERT_ARG(1)); });

		/*
		// booleans
		table[builder.pointwise(basic.getBoolLAnd())] = OP_CONVERTER({ return c_ast::logicAnd(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); });
		table[builder.pointwise(basic.getBoolLOr())]  = OP_CONVERTER({ return c_ast::logicOr(CONVERT_ARG(0), CONVERT_EXPR(inlineLazy(ARG(1)))); });
		table[builder.pointwise(basic.getBoolLNot())] = OP_CONVERTER({ return c_ast::logicNot(CONVERT_ARG(0)); });

		table[builder.pointwise(basic.getBoolEq())]   = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });
		table[builder.pointwise(basic.getBoolNe())]   = OP_CONVERTER({ return c_ast::ne(CONVERT_ARG(0), CONVERT_ARG(1)); });

		table[builder.pointwise(basic.getBoolToInt())] = OP_CONVERTER({ return CONVERT_ARG(0); });
		*/


		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme

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
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/backend/operator_converter_begin.inc"

namespace insieme {
namespace backend {


	OperatorConverterTable getBasicOperatorTable(const core::lang::BasicGenerator& basic) {

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

		res[basic.getUnsignedIntPreInc()]  = OP_CONVERTER({ return c_ast::preInc(CONVERT_ARG(0)); });
		res[basic.getUnsignedIntPostInc()] = OP_CONVERTER({ return c_ast::postInc(CONVERT_ARG(0)); });
		res[basic.getUnsignedIntPreDec()]  = OP_CONVERTER({ return c_ast::preDec(CONVERT_ARG(0)); });
		res[basic.getUnsignedIntPostDec()] = OP_CONVERTER({ return c_ast::postDec(CONVERT_ARG(0)); });

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

		res[basic.getSignedIntPreInc()]  = OP_CONVERTER({ return c_ast::preInc(CONVERT_ARG(0)); });
		res[basic.getSignedIntPostInc()] = OP_CONVERTER({ return c_ast::postInc(CONVERT_ARG(0)); });
		res[basic.getSignedIntPreDec()]  = OP_CONVERTER({ return c_ast::preDec(CONVERT_ARG(0)); });
		res[basic.getSignedIntPostDec()] = OP_CONVERTER({ return c_ast::postDec(CONVERT_ARG(0)); });

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


		res[basic.getRefVar()]    = OP_CONVERTER({ return CONVERT_ARG(0); });
		res[basic.getRefDeref()]  = OP_CONVERTER({ return c_ast::deref(CONVERT_ARG(0)); });
		res[basic.getRefAssign()] = OP_CONVERTER({ return c_ast::assign(CONVERT_ARG(0), CONVERT_ARG(1)); });


		res[basic.getRefEqual()] = OP_CONVERTER({ return c_ast::eq(CONVERT_ARG(0), CONVERT_ARG(1)); });

//		// special handling of derefing result of ref.new or ref.var => bogus
//		ExpressionPtr arg = ARG(0);
//		if (core::analysis::isCallOf(arg, basic.getRefVar()) || core::analysis::isCallOf(arg, basic.getRefNew())) {
//			// skip ref.var / ref.new
//			CallExprPtr call = static_pointer_cast<const CallExpr>(arg);
//			STMT_CONVERTER.convert(call->getArgument(0));
//		} else {
//			// just add deref
//			OUT("*");
//			VISIT_ARG(0);
//		}

//		LITERAL(RefVar,    		"ref.var",    			"('a) -> ref<'a>")
//		LITERAL(RefNew,    		"ref.new",    			"('a) -> ref<'a>")
//		LITERAL(RefDelete, 		"ref.delete", 			"(ref<'a>) -> unit")
//		LITERAL(RefEqual,  		"ref.eq",				"(ref<'a>, ref<'a>) -> bool")
//		LITERAL(RefAssign, 		"ref.assign", 			"(ref<'a>,'a) -> unit")
//		LITERAL(RefDeref,  		"ref.deref",  			"(ref<'a>) -> 'a")
//		LITERAL(ScalarToArray, 	"scalar.to.array", 		"(ref<'a>) -> ref<array<'a,1>>")


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

//		LITERAL(ArraySubscript1D, 	"array.subscript.1D", 	"(array<'elem,1>, uint<8>) -> 'elem")
//		LITERAL(ArraySubscriptND, 	"array.subscript.ND", 	"(array<'elem,#n>, vector<uint<8>,#n>) -> 'elem")
//
//		LITERAL(ArrayRefElem1D, 	"array.ref.elem.1D", 	"(ref<array<'elem,1>>, uint<8>) -> ref<'elem>")
//		LITERAL(ArrayRefElemND, 	"array.ref.elem.ND", 	"(ref<array<'elem,#n>>, vector<uint<8>,#n>) -> ref<'elem>")



		// table complete => return table
		return res;
	}



} // end namespace backend
} // end namespace insieme

/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/analysis/cba/haskell/arithmetic_analysis.h"

#include "insieme/analysis/cba/common/set.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/lang/reference.h"

extern "C" {

	namespace ia = insieme::analysis::cba;
	namespace hat = ia::haskell;

	// Analysis
	ia::ArithmeticSet* hat_arithmetic_value(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	ArithmeticSet getArithmeticValue(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		ArithmeticSet* res_ptr = hat_arithmetic_value(ctxt.getHaskellContext(), expr_hs);
		ArithmeticSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme


extern "C" {

	using insieme::analysis::cba::ArithmeticSet;

	using namespace insieme::core;
	using namespace insieme::analysis::cba::haskell;

	arithmetic::Value* hat_mk_arithmetic_value(Context* ctx_c, const size_t* addr_hs, size_t length_hs) {
		assert_true(ctx_c) << "hat_mk_arithmetic_value called without context";
		// build NodeAddress
		NodeAddress addr(ctx_c->getRoot());
		for(size_t i = 0; i < length_hs; i++) {
			addr = addr.getAddressOfChild(addr_hs[i]);
		}

		// add de-ref operations (assumed implicitly by analysis)
		ExpressionPtr expr = addr.as<ExpressionPtr>();
		while(lang::isReference(expr)) {
			expr = lang::buildRefDeref(expr);
		}

		// build value
		return new arithmetic::Value(expr);
	}

	arithmetic::Product::Factor* hat_mk_arithemtic_factor(arithmetic::Value* value_c, int exponent) {
		auto ret = new pair<arithmetic::Value, int>(*value_c, exponent);
		delete value_c;
		return ret;
	}

	arithmetic::Product* hat_mk_arithmetic_product(arithmetic::Product::Factor* factors_c[], size_t length) {
		auto ret = new arithmetic::Product();
		for(size_t i = 0; i < length; i++) {
			*ret *= arithmetic::Product(factors_c[i]->first, factors_c[i]->second);
			delete factors_c[i];
		}
		return ret;
	}

	arithmetic::Formula::Term* hat_mk_arithmetic_term(arithmetic::Product* term_c, int64_t coeff) {
		auto ret = new pair<arithmetic::Product, arithmetic::Rational>(*term_c, arithmetic::Rational(coeff));
		delete term_c;
		return ret;
	}

	arithmetic::Formula* hat_mk_arithmetic_formula(arithmetic::Formula::Term* terms_c[], size_t length) {
		auto ret = new arithmetic::Formula();
		for(size_t i = 0; i < length; i++) {
			*ret += arithmetic::Formula(terms_c[i]->first, terms_c[i]->second);
			delete terms_c[i];
		}
		return ret;
	}

	ArithmeticSet* hat_mk_arithmetic_set(arithmetic::Formula* formulas_c[], long long length) {
		return ArithmeticSet::fromArray(formulas_c, length);
	}

}

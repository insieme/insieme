/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/haskell/arithmetic_analysis.h"

#include "insieme/core/arithmetic/arithmetic.h"

extern "C" {

	namespace ia = insieme::analysis;
	namespace hat = ia::haskell;

	// Analysis
	ia::ArithmeticSet* hat_arithmetic_value(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace haskell {

	ArithmeticSet getArithmeticValue(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		ArithmeticSet* res_ptr = hat_arithmetic_value(ctxt.getHaskellContext(), expr_hs);
		ArithmeticSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme


extern "C" {

	using insieme::analysis::ArithmeticSet;

	using namespace insieme::core;
	using namespace insieme::analysis::haskell;

	arithmetic::Value* hat_mk_arithmetic_value(Context* ctx_c, const size_t* addr_hs, size_t length_hs) {
		assert_true(ctx_c) << "hat_mk_arithmetic_value called without context";
		// build NodeAddress
		NodeAddress addr(ctx_c->getRoot());
		for(size_t i = 0; i < length_hs; i++) {
			addr = addr.getAddressOfChild(addr_hs[i]);
		}

		return new arithmetic::Value(addr.as<ExpressionPtr>());
	}

	arithmetic::Product::Factor* hat_mk_arithemtic_factor(arithmetic::Value* value_c, int exponent) {
		auto ret = new pair<arithmetic::Value, int>(*value_c, exponent);
		delete value_c;
		return ret;
	}

	arithmetic::Product* hat_mk_arithmetic_product(const arithmetic::Product::Factor** factors_c, size_t length) {
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

	arithmetic::Formula* hat_mk_arithmetic_formula(const arithmetic::Formula::Term** terms_c, size_t length) {
		auto ret = new arithmetic::Formula();
		for(size_t i = 0; i < length; i++) {
			*ret += arithmetic::Formula(terms_c[i]->first, terms_c[i]->second);
			delete terms_c[i];
		}
		return ret;
	}

	ArithmeticSet* hat_mk_arithmetic_set(const arithmetic::Formula** formulas_c, int length) {
		if(length < 0) {
			return new ArithmeticSet(ArithmeticSet::getUniversal());
		}

		auto ret = new ArithmeticSet();
		for(int i = 0; i < length; i++) {
			ret->insert(*formulas_c[i]);
			delete formulas_c[i];
		}
		return ret;
	}

}

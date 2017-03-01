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
 *
 */
#include "insieme/analysis/cba/haskell/reference_analysis.h"
#include "insieme/core/lang/reference.h"

extern "C" {

	namespace ia = insieme::analysis::cba;
	namespace hat = ia::haskell;

	// Analysis
	int hat_check_null(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	int hat_check_extern(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	ia::MemoryLocationSet* hat_memory_locations(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {


	enum Result {
		Yes=0, Maybe=1, No=2
	};

	namespace {

		Result checkNull(Context& ctxt, const core::ExpressionAddress& expr) {
			auto expr_hs = ctxt.resolveNodeAddress(expr);
			return (Result)hat_check_null(ctxt.getHaskellContext(), expr_hs);
		}

	}

	bool isNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == Yes;
	}

	bool mayBeNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == Maybe;
	}

	bool notNull(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkNull(ctxt, expr) == No;
	}


	namespace {

		Result checkExtern(Context& ctxt, const core::ExpressionAddress& expr) {
			auto expr_hs = ctxt.resolveNodeAddress(expr);
			return (Result)hat_check_extern(ctxt.getHaskellContext(), expr_hs);
		}

	}

	bool isExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == Yes;
	}

	bool mayBeExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == Maybe;
	}

	bool notExtern(Context& ctxt, const core::ExpressionAddress& expr) {
		return checkExtern(ctxt, expr) == No;
	}

	MemoryLocationSet getReferencedMemoryLocations(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		MemoryLocationSet* res_ptr = hat_memory_locations(ctxt.getHaskellContext(), expr_hs);
		MemoryLocationSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme


extern "C" {

	using insieme::analysis::cba::MemoryLocationSet;

	using namespace insieme::core;
	using namespace insieme::analysis::cba::haskell;

	ia::MemoryLocation* hat_mk_memory_location(Context* ctx_c, const size_t* addr_hs, size_t length_hs) {
		assert_true(ctx_c) << "hat_mk_memory_location called without context";
		// build NodeAddress
		NodeAddress addr(ctx_c->getRoot());
		for(size_t i = 0; i < length_hs; i++) {
			addr = addr.getAddressOfChild(addr_hs[i]);
		}

		// build value
		return new ia::MemoryLocation(std::move(addr));
	}

	MemoryLocationSet* hat_mk_memory_location_set(const ia::MemoryLocation** locations_c, int length) {
		if(length < 0) {
			return new MemoryLocationSet(MemoryLocationSet::getUniversal());
		}

		auto ret = new MemoryLocationSet();
		for(int i = 0; i < length; i++) {
			ret->insert(*locations_c[i]);
			delete locations_c[i];
		}
		return ret;
	}

}

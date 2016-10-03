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

#include "insieme/analysis/haskell/reference_analysis.h"
#include "insieme/core/lang/reference.h"

extern "C" {

	namespace ia = insieme::analysis;
	namespace hat = ia::haskell;

	// Analysis
	int hat_check_null(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	int hat_check_extern(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);
	ia::MemoryLocationSet* hat_memory_locations(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
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
} // end namespace analysis
} // end namespace insieme


extern "C" {

	using insieme::analysis::MemoryLocationSet;

	using namespace insieme::core;
	using namespace insieme::analysis::haskell;

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

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
#include "insieme/analysis/cba/haskell/symbolic_value_analysis.h"


extern "C" {

	namespace ia = insieme::analysis::cba;
	namespace hat = ia::haskell;

	// Analysis
	hat::SymbolicValueSet* hat_hs_symbolic_values(hat::StablePtr ctx, const hat::HaskellNodeAddress expr_hs);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	SymbolicValueSet getSymbolicValue(Context& ctxt, const core::ExpressionAddress& expr) {
		auto expr_hs = ctxt.resolveNodeAddress(expr);
		SymbolicValueSet* res_ptr = hat_hs_symbolic_values(ctxt.getHaskellContext(), expr_hs);
		SymbolicValueSet res(std::move(*res_ptr));
		delete res_ptr;
		return res;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme


extern "C" {

	using namespace insieme::core;
	using namespace insieme::analysis::cba::haskell;

	SymbolicValueSet* hat_c_mk_symbolic_value_set(const NodePtr* values[], long long size) {
		if(size < 0) {
			return new SymbolicValueSet(SymbolicValueSet::getUniversal());
		}

		auto dss = new SymbolicValueSet();
		for(int i = 0; i < size; i++) {
			dss->insert(values[i]->as<ExpressionPtr>());
			delete values[i];
		}

		return dss;
	}

}

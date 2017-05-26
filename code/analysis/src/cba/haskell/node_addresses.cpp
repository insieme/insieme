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

#include "insieme/analysis/cba/haskell/node_addresses.h"

extern "C" {

	NodeAddress* hat_mk_c_node_address(Context* ctx_c, const size_t indices[], size_t length_hs) {
		assert_true(ctx_c) << "hat_mk_c_node_address called without context";

		// build NodeAddress
		NodeAddress addr(ctx_c->getRoot());
		for(size_t i = 0; i < length_hs; i++) {
			addr = addr.getAddressOfChild(indices[i]);
		}

		return new NodeAddress(std::move(addr));
	}

	NodeAddressSet* hat_mk_c_node_address_set(const NodeAddress* addrs[], long long length) {
		if(length < 0) {
			return new NodeAddressSet(NodeAddressSet::getUniversal());
		}

		auto ret = new NodeAddressSet();
		for(long long i = 0; i < length; i++) {
			ret->insert(*addrs[i]);
			delete addrs[i];
		}

		return ret;
	}

}

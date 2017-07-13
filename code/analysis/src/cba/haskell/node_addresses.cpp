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

#include <cstdlib>
#include <cstring>
#include <sstream>
#include <string>

#include "insieme/analysis/cba/common/set.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/dump/binary_haskell.h"

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

	NodeAddressSet* hat_mk_c_node_address_set(NodeAddress* addrs[], long long length) {
		return NodeAddressSet::fromArray(addrs, length);
	}

	NodePtr* hat_c_mk_ir_tree(Context* ctx_c, const char* data, size_t size) {
		assert_true(ctx_c);
		assert_true(size > 0);

		std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);
		buffer.str({data, size});

		auto loaded_tree = dump::binary::loadIR(buffer, ctx_c->getRoot().getNodeManager());
		assert_true(loaded_tree) << "could not load received data";

		// Since this function is called by Haskell, `data` is freed in Haskell afterwards.
		return new NodePtr(loaded_tree);
	}

	void hat_c_parse_ir_statement(const char* data, size_t size, char** data_c, size_t* size_c) {
		assert_true(size > 0);

		NodeManager mgr;
		IRBuilder builder(mgr);
		StatementPtr stmt = builder.parseStmt({data, size});
		assert_true(stmt);

		std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);
		dump::binary::haskell::dumpIR(buffer, stmt);

		std::string dump = buffer.str();
		*data_c = (char*) malloc(dump.size());
		assert_true(*data_c);
		memcpy(*data_c, dump.c_str(), dump.size());
		*size_c = dump.size();
	}

	char* hat_c_pretty_print_tree(const char* data, size_t size) {
		assert_true(size > 0);

		std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);
		buffer.str({data, size});

		NodeManager mgr;
		auto loaded_tree = dump::binary::loadIR(buffer, mgr);
		assert_true(loaded_tree) << "could not load received data";

		auto dump = toString(dumpOneLine(loaded_tree));
		char* dump_c = (char*) malloc(dump.size());
		assert_true(dump_c);
		memcpy(dump_c, dump.c_str(), dump.size());
		return dump_c;
	}

}

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

#include "insieme/analysis/cba/haskell/context.h"

#include <map>
#include <sstream>
#include <vector>

#include "insieme/utils/assert.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/ir_builder.h"

#include "insieme/analysis/cba/common/failure.h"


using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary::haskell;

extern "C" {

	using insieme::analysis::cba::haskell::Context;
	using insieme::analysis::cba::haskell::StablePtr;

	// Haskell Object Management
	void hat_freeStablePtr(StablePtr ptr);

	// Haskell Context
	StablePtr hat_initialize_context(const Context* ctx_c, const char* dump_c, size_t size_c);
	void hat_print_statistic(StablePtr hs_context);
	void hat_dump_assignment(StablePtr hs_context);
	StablePtr hat_set_timelimit(StablePtr hs_context, long long t);

	// NodePath
	StablePtr hat_mk_node_address(StablePtr ctx_hs, const size_t* path_c, size_t length_c);
	size_t hat_node_path_length(StablePtr addr_hs);
	void hat_node_path_poke(StablePtr addr_hs, size_t* path_c);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	// ------------------------------------------------------------ Context

	Context::Context() : context_hs(nullptr), root() {}

	Context::Context(const core::NodePtr& node) : context_hs(nullptr) {
		setRoot(node);
	}

	Context::~Context() {
		clear();
	}

	void Context::setTimelimit(long long us) {
		context_hs = hat_set_timelimit(context_hs, us);
	}

	void Context::dumpStatistics() const {
		if (!context_hs) {
			std::cout << "No statistics available.\n";
			return;
		}
		hat_print_statistic(context_hs);
	}

	void Context::dumpSolution() const {
		if (!context_hs) {
			std::cout << "No statistics available.\n";
			return;
		}
		std::cout << "Dumping assignment ...\n";
		hat_dump_assignment(context_hs);
	}

	StablePtr Context::getHaskellContext() const {
		assert_true(context_hs) << "Context not initialized!";
		return context_hs;
	}

	void Context::setHaskellContext(StablePtr new_ctxt) {
		if (context_hs == new_ctxt)
			return;

		if (context_hs)
			hat_freeStablePtr(context_hs);

		context_hs = new_ctxt;
	}

	NodePtr Context::getRoot() const {
		assert_true(context_hs) << "Unable to obtain context of non-initialized ";
		return root;
	}

	void Context::setRoot(const core::NodePtr& node) {
		// clear potential old state
		clear();

		// -- set up new state --
		root = node;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format (includes builtins)
		dumpIR(buffer, root);

		// get data as C string
		const string dump = buffer.str();
		const char* dump_c = dump.c_str();

		// pass to Haskell
		context_hs = hat_initialize_context(this, dump_c, dump.size());
		assert_true(context_hs) << "could not initialize Haskell context";
	}

	HaskellNodeAddress Context::resolveNodeAddress(const core::NodeAddress& addr) {

		// support the conversion of null-addresses
		if (!addr) return nullptr;

		// install new root node if necessary
		if (!root) {
			setRoot(addr.getRootNode());
		}

		// check that the root node is matching
		assert_eq(root, addr.getRootNode()) << "root node does not match this context";

		// check if address has already been moved to Haskell
		auto pos = addresses.find(addr);
		if (pos != addresses.end()) return pos->second;

		// empty address corresponds to root node in Haskell
		size_t length_c = addr.getDepth() - 1;
		vector<size_t> path_c(length_c);

		for(size_t i = 0; i < length_c; i++) {
			path_c[i] = addr.getParentAddress(length_c - 1 - i).getIndex();
		}

		StablePtr addr_hs = hat_mk_node_address(context_hs, path_c.data(), length_c);
		assert_true(addr_hs) << "could not pass NodePath to Haskell";

		addresses[addr] = addr_hs;
		return addr_hs;
	}

	core::NodeAddress Context::resolveNodeAddress(const HaskellNodeAddress& addr_hs) {

		// support the conversion of null-addresses
		if (!addr_hs) return core::NodeAddress();

		assert_true(root) << "No root node linked to this context! Did you forget to initialize it first?";

		vector<size_t> dst(hat_node_path_length(addr_hs));
		hat_node_path_poke(addr_hs, dst.data());

		NodeAddress addr(root);
		for(auto i : dst) {
			addr = addr.getAddressOfChild(i);
		}

		return addr;
	}

	void Context::clear() {
		// check whether it has been initialized
		if (!context_hs) return; // if not, we are fine

		// delete converted addresses
		for(auto pair : addresses) {
			hat_freeStablePtr(pair.second);
		}

		// delete remaining context information on the haskell side
		hat_freeStablePtr(context_hs);

		// reset fields
		context_hs = nullptr;
		root = core::NodeAddress();
		addresses.clear();
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

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

#include "insieme/analysis/haskell/context.h"

#include <map>
#include <sstream>
#include <vector>

#include "insieme/utils/assert.h"
#include "insieme/utils/container_utils.h"

#include "insieme/core/dump/binary_haskell.h"
#include "insieme/core/ir_builder.h"

#include "insieme/analysis/common/failure.h"


using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary::haskell;

extern "C" {

	using insieme::analysis::haskell::Context;
	using insieme::analysis::haskell::StablePtr;

	// Haskell Object Management
	void hat_freeStablePtr(StablePtr ptr);

	// Haskell Context
	StablePtr hat_initialize_context(const Context* ctx_c, const char* dump_c, size_t size_c);
	void hat_update_context(Context* trg, StablePtr hs_context);


	// NodePath
	StablePtr hat_mk_node_address(StablePtr ctx_hs, const size_t* path_c, size_t length_c);
	size_t hat_node_path_length(StablePtr addr_hs);
	void hat_node_path_poke(StablePtr addr_hs, size_t* path_c);

}

namespace insieme {
namespace analysis {
namespace haskell {

	// ------------------------------------------------------------ Context

	Context::Context() : context_hs(nullptr), root() {}

	Context::Context(const core::NodePtr& node) : context_hs(nullptr) {
		setRoot(node);
	}

	Context::~Context() {
		clear();
	}

	StablePtr Context::getHaskellContext() const {
		assert_true(context_hs) << "Context not initialized!";
		return context_hs;
	}

	void Context::setHaskellContext(StablePtr new_ctxt) {
		if (context_hs) hat_freeStablePtr(context_hs);
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
} // end namespace analysis
} // end namespace insieme

extern "C" {

	void hat_update_context(Context* trg, StablePtr hs_context) {
		trg->setHaskellContext(hs_context);
	}

}

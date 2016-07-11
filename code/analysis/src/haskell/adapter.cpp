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

#include "insieme/analysis/haskell/adapter.h"

#include "insieme/analysis/common/failure.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/lang/lang.h"

#include <cstdint>
#include <map>
#include <sstream>
#include <vector>

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary;

extern "C" {

	// Haskell object management
	typedef void* StablePtr;
	void hat_freeStablePtr(StablePtr ptr);

	// environment bracket
	void hs_init(int, char*[]);
	void hs_exit(void);

	// Tree functions
	StablePtr hat_passTree(const char* dump, size_t length);
	size_t hat_tree_length(const StablePtr dump);
	void hat_tree_print(const StablePtr tree);
	void hat_tree_printNode(const StablePtr addr);

	// Address functions
	StablePtr hat_passAddress(const StablePtr tree, const size_t* path, size_t length);
	size_t hat_addr_length(const StablePtr addr);
	void hat_addr_toArray(const StablePtr addr, size_t* dst);

	// Analysis
	StablePtr hat_findDeclr(const StablePtr var);
	int hat_checkBoolean(const StablePtr expr);

}

namespace insieme {
namespace analysis {
namespace haskell {

	struct HSobject {

		StablePtr ptr;

		HSobject(StablePtr ptr) : ptr(ptr) {}

		~HSobject() {
			hat_freeStablePtr(ptr);
		}

	};

	// ------------------------------------------------------------ Tree

	Tree::Tree(std::shared_ptr<HSobject> tree, const NodePtr& original)
		: tree(tree), original(original) {}

	size_t Tree::size() const {
		return hat_tree_length(tree->ptr);
	}

	void Tree::print() const {
		hat_tree_print(tree->ptr);
	}

	// ------------------------------------------------------------ Address

	Address::Address(std::shared_ptr<HSobject> addr) : addr(addr) {}

	size_t Address::size() const {
		return hat_addr_length(addr->ptr);
	}

	void Address::printNode() const {
		hat_tree_printNode(addr->ptr);
	}

	// Add the address contained in the Haskell buffer to the given root node,
	// returning a proper NodeAddress.
	NodeAddress Address::toNodeAddress(const NodePtr& root) const {
		NodeAddress ret(root);
		vector<size_t> dst(size());
		hat_addr_toArray(addr->ptr, dst.data());

		for (auto i : dst) {
			ret = ret.getAddressOfChild(i);
		}

		return ret;
	}

	// ------------------------------------------------------------ Environment

	Environment::Environment() {
		hs_init(0, nullptr);
	}

	Environment::~Environment() {
		hs_exit();
	}

	Environment& Environment::getInstance() {
		static Environment instance;
		return instance;
	}

	// TODO: use unexported functions from binary dumper
	template <typename T>
	void write(ostream& out, T value) {
		out.write((char*) &value, sizeof(T));
	}

	Tree Environment::passTree(const NodePtr& root) {
		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		dumpIR(buffer, root);

		// find builtins
		NodeSet covered;
		map<string, NodeAddress> builtins;
		visitDepthFirstOnce(NodeAddress(root), [&] (const ExpressionAddress& expr) {
			if (covered.contains(expr.getAddressedNode())) {
				return;
			}

			covered.insert(expr.getAddressedNode());

			if (core::lang::isBuiltIn(expr)) {
				builtins[core::lang::getConstructName(expr)] = expr;
			}
		});

		// attach builtins
		write<uint32_t>(buffer, builtins.size());
		for (auto& builtin : builtins) {
			// dump key
			write<uint32_t>(buffer, builtin.first.length());
			buffer.write(builtin.first.c_str(), builtin.first.length());

			// dump value
			string addr = toString(builtin.second);
			write<uint32_t>(buffer, addr.length());
			buffer.write(addr.c_str(), addr.length());
		}

		// get data as C string
		const string dumps = buffer.str();
		const char* dumpcs = dumps.c_str();

		// pass to Haskell
		auto tree = make_shared<HSobject>(hat_passTree(dumpcs, dumps.size()));
		return {tree, root};
	}

	Address Environment::passAddress(const NodeAddress& addr, const Tree& tree) {
		// check if NodeAddress is related given tree
		assert_eq(tree.original, addr.getRootNode()) << "NodeAddress' root does not match given tree";

		// empty address corresponds to root node in Haskell
		size_t length_c = addr.getDepth() - 1;
		vector<size_t> addr_c(length_c);

		for (size_t i = 0; i < length_c; i++) {
			addr_c[i] = addr.getParentAddress(length_c - 1 - i).getIndex();
		}

		return make_shared<HSobject>(hat_passAddress(tree.tree->ptr, addr_c.data(), length_c));
	}

	boost::optional<Address> Environment::findDeclr(const Address& var) {
		boost::optional<Address> ret;
		if (StablePtr target_hs = hat_findDeclr(var.addr->ptr)) {
			ret = make_shared<HSobject>(target_hs);
		}
		return ret;
	}

	BooleanAnalysisResult Environment::checkBoolean(const Address& expr) {
		auto res = static_cast<BooleanAnalysisResult>(hat_checkBoolean(expr.addr->ptr));
		if (res == BooleanAnalysisResult_Neither) {
			std::vector<std::string> msgs{"Boolean Analysis Error"};
			throw AnalysisFailure(msgs);
		}
		return res;
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

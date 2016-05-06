#include "insieme/analysis/haskell/adapter.h"

#include <vector>
#include <sstream>

#include "insieme/core/dump/binary_dump.h"

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
	void hat_tree_printNode(const StablePtr tree, const StablePtr addr);

	// Address functions
	StablePtr hat_passAddress(const size_t* path, size_t length);
	size_t hat_addr_length(const StablePtr addr);
	void hat_addr_toArray(const StablePtr addr, size_t* dst);

	// Analysis
	StablePtr hat_findDeclr(const StablePtr tree, const StablePtr var);

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

	size_t Tree::size() const {
		return hat_tree_length(tree->ptr);
	}

	Tree::Tree(std::shared_ptr<HSobject> tree) : tree(tree) {}

	void Tree::printNode(const Address& node) const {
		hat_tree_printNode(tree->ptr, node.addr->ptr);
	}

	// ------------------------------------------------------------ Address

	Address::Address(std::shared_ptr<HSobject> addr) : addr(addr) {}

	size_t Address::size() const {
		return hat_addr_length(addr->ptr);
	}

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

	Tree Environment::passTree(const NodePtr& root) {
		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		dumpIR(buffer, root);

		// get data as C string
		const string dumps = buffer.str();
		const char* dumpcs = dumps.c_str();

		// pass to Haskell
		return make_shared<HSobject>(hat_passTree(dumpcs, dumps.size()));
	}

	Address Environment::passAddress(const NodeAddress& addr) {
		// empty address corresponds to root node in Haskell
		size_t length_c = addr.getDepth() - 1;
		vector<size_t> addr_c(length_c);

		for (size_t i = 0; i < length_c; i++) {
			addr_c[i] = addr.getParentAddress(length_c - 1 - i).getIndex();
		}

		return make_shared<HSobject>(hat_passAddress(addr_c.data(), length_c));
	}

	boost::optional<Address> Environment::findDeclr(Tree& tree, Address& var) {
		boost::optional<Address> ret;
		if (StablePtr target_hs = hat_findDeclr(tree.tree->ptr, var.addr->ptr)) {
			ret = make_shared<HSobject>(target_hs);
		}
		return ret;
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

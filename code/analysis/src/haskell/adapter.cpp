#include "insieme/analysis/haskell/adapter.h"

#include "insieme/core/dump/binary_dump.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump::binary;

extern "C" {

	// Haskell object management
	typedef void* StablePtr;
	void freeStablePtr(StablePtr ptr);

	// environment bracket
	void hs_init(int, char*[]);
	void hs_exit(void);

	// IR dump functions
	StablePtr passIRdump(const char* dump, const size_t length);
	size_t nodeCount(const StablePtr dump);

}

namespace insieme {
namespace analysis {
namespace haskell {

	struct HSobject {

		StablePtr object;

		HSobject(StablePtr object) : object(object) {}

		~HSobject() {
			freeStablePtr(object);
		}

	};

	IRtree::IRtree(std::shared_ptr<HSobject> tree) : tree(tree) {}

	size_t IRtree::node_count() {
		return nodeCount(tree->object);
	}

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

	IRtree Environment::passTree(const NodePtr& root) {
		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		dumpIR(buffer, root);

		// get data as cstring
		const string dumps = buffer.str();
		const char* dumpcs = dumps.c_str();

		// pass to haskell
		return make_shared<HSobject>(passIRdump(dumpcs, dumps.size()));
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

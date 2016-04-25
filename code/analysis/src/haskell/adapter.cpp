#include "insieme/analysis/haskell/adapter.h"

using namespace std;

extern "C" {

	typedef void* StablePtr;

	// environment bracket
	void hs_init(int, char*[]);
	void hs_exit(void);

	// IR dump functions
	StablePtr passIRdump(const char* dump, const size_t length);
	size_t nodeCount(const StablePtr dump);

	// StablePtr functions
	void freeStablePtr(StablePtr  ptr);

}

namespace insieme {
namespace analysis {
namespace haskell {

	ir_tree::ir_tree(StablePtr  tree) : tree(tree) {}

	ir_tree::~ir_tree() {
		freeStablePtr(tree);
	}

	size_t ir_tree::node_count() {
		return nodeCount(tree);
	}

	env::env() {
		hs_init(0, 0);
	}

	env::~env() {
		hs_exit();
	}

	env& env::instance() {
		static env* instance = new env;
		return *instance;
	}

	ir_tree env::passDump(const char* dump, size_t length) {
		return passIRdump(dump, length);
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

#include <iostream>

extern "C" {
	void my_enter(void);
	void my_exit(void);
	void* passIRdump(unsigned char* dump, unsigned int length);
	int nodeCount(void* dump);
	void freeIRdump(void* dump);
}

namespace insieme {
namespace analysis {
namespace haskell {

	void enter(void) {
		my_enter();
	}

	void exit(void) {
		my_exit();
	}

	void* passDump(unsigned char* dump, unsigned int length) {
		return passIRdump(dump, length);
	}

	void freeDump(void* dump) {
		freeIRdump(dump);
	}

	int getNodeCount(void* dump) {
		return nodeCount(dump);
	}

} // end namespace haskell
} // end namespace analysis
} // end namespace insieme

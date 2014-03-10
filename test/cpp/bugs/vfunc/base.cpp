#include "base.h"

void Base::runProc() {
	std::cout << "Base::runProc" << std::endl;
	loadArguments();
	run();
}

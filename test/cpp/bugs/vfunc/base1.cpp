#include "base1.h"

void Base1::runProc() {
	std::cout << "Base1::runProc" << std::endl;
	loadArguments();
	run();
}

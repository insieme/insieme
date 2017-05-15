#include <cstdlib>
#include <iostream>

#include "%PROJECT%/%MODULE%/answer.h"

using namespace %PROJECT%::%MODULE%;

int main() {
	std::cout << "The answer is " << answer() << std::endl;
	return EXIT_SUCCESS;
}

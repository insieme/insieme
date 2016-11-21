#include <cstdlib>
#include <iostream>

#include "%PROJECT%/%MODULE%/answer.h"

using namespace std;
using namespace %PROJECT%::%MODULE%;

int main() {
	cout << "The answer is: " << answer() << endl;
	return EXIT_SUCCESS;
}

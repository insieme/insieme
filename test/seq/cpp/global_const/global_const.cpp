#include <iostream>

const float var = 1.0f;
static const float var1 = 1.0f;
const float arr[1] = { 1.0f};
static const float arr1[1] = { 1.0f};


// check init of globals which call ctors
struct T {
	int i;
	// note, no default ctor. this will ensure that the generated code can not use in-place init
	T(int i) : i(i) {}
};
T t = { 4 };
const T ct = { 5 };

int main() {
	std::cout << var;
	std::cout << var1;
	std::cout << arr[0];
	std::cout << arr1[0];
	std::cout << t.i;
	std::cout << ct.i;
	return 0;
}


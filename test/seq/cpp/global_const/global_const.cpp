#include <iostream>

const float var = 1.0f;
static const float var1 = 1.0f;
const float arr[1] = { 1.0f};
static const float arr1[1] = { 1.0f};

int main() {
	std::cout << var;
	std::cout << var;
	std::cout << arr[0];
	std::cout << arr1[0];
	return 0;
}


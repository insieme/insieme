#include <future>
#include <iostream>

namespace test {
	enum class A { A=1, B=2 };
}

int main() {
	//test with an intercepted scoped enum type
	if(std::launch::async == std::launch::deferred) {
		std::cout << "enum elements match\n";
	} else {
		std::cout << "enum elements do not match\n";
	}

	//test with namespaced scoped enum type
	if(test::A::B == test::A::A) {
		std::cout << "elements match\n";
	} else {
		std::cout << "elements do not match\n";
	}

	return 0;
}

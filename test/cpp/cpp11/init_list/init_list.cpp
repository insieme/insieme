#include <iostream>
#include <initializer_list>

//simple cases
struct T {
	T(std::initializer_list<int> l) {
		for(auto e : l) {
			std::cout << "T: " << e << std::endl;
		}
	}
};

void f(std::initializer_list<int> l) {
	for(auto e : l) {
		std::cout << "f: " << e << std::endl;
	}
}

//more complicated semantics
struct TestObj {
	TestObj() {
		std::cout << "Construct TestObj" << std::endl;
	}
	TestObj(const TestObj& other) {
		std::cout << "Copy construct TestObj" << std::endl;
	}
	TestObj(TestObj&& other) {
		std::cout << "Move construct TestObj" << std::endl;
	}
	~TestObj() {
		std::cout << "Destroy TestObj" << std::endl;
	}
};

struct T2 {
	T2(std::initializer_list<TestObj> l) {}
};

void f2(std::initializer_list<TestObj> l) {}

int main (){

	//simple cases
	T({ 5 });
	T{42,43};
	f({ 5 });

	//more complicated semantics
	TestObj obj;
	std::cout << 1 << std::endl;
	T2({ obj, obj });
	std::cout << 2 << std::endl;
	std::initializer_list<TestObj> a{ obj };
	std::cout << 3 << std::endl;
	auto b = a;
	std::cout << 4 << std::endl;
	T2 t2(a);
	std::cout << 5 << std::endl;
	f2(b);
	std::cout << 6 << std::endl;
	std::initializer_list<TestObj> d;
	{
		std::initializer_list<TestObj> c{ obj };
		std::cout << 7 << std::endl;
		d = c;
		std::cout << 8 << std::endl;
		std::initializer_list<TestObj> e(c);
		std::cout << 9 << std::endl;
	}
	std::cout << 10 << std::endl;

	return 0;
}


#include <string>
#include <iostream>
#include <typeinfo>

using namespace std;

class A {};

template<class T>
struct X {
	enum { value = 1 };
};

template<>
struct X<A> {
	enum { value = 2 };
};

template<>
struct X<const A> {
	enum { value = 3 };
};

template<>
struct X<char> {
	enum { value = 4 };
};

template<>
struct X<const char> {
	enum { value = 5 };
};



int main() {

	std::cout << (typeid(A) == typeid(const A)) << "\n";
	std::cout << (typeid(A*) == typeid(const A*)) << "\n";

	std::cout << X<std::string>::value << "\n";
	std::cout << X<A>::value << "\n";
	std::cout << X<const A>::value << "\n";
	std::cout << X<char>::value << "\n";
	std::cout << X<const char>::value << "\n";


	typedef string::const_iterator A;
	typedef string::iterator B;


	std::cout << (typeid(A) == typeid(A)) << "\n";
	std::cout << (typeid(A) == typeid(B)) << "\n";
	std::cout << (typeid(B) == typeid(B)) << "\n";

	return 0;
}


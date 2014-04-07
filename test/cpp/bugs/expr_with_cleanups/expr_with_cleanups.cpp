#include <iostream>
#include <string>
//
//class B {
//private:
//	B(const B& a) {}
//public:
//	B(std::string s) {}
//	std::string s;
//};

class A {
private:
//	B mem;
	std::string mem;
public:
	A(std::string s) : mem(s) { }
	void print() const {
		std::cout << "member: " << mem << std::endl;
	}
};


int main() {
	A a(std::string("hallo"));
	a.print();
	return 0;
}

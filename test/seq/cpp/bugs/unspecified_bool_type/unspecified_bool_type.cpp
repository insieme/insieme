#include <iostream>
class Testable {
private:
	bool * ptr_;
	struct Tester {
		Tester(int) {}  // No default constructor
		void dummy() {}
	};

	typedef void (Tester::*unspecified_bool_type)();

public:
	operator unspecified_bool_type() const {
		return !ptr_ ? 0 : &Tester::dummy;
	}
};

int main() {
	Testable a;
	if(a) {
		std::cout << " hello " << std::endl;
	}
	std::cout << " bye " << std::endl;
	return 0;
}

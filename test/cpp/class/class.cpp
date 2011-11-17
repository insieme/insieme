#include <iostream>

class SimpleClass {
	private:
		int mPrivateInt;
		double mPrivateDouble;
		int mThisTest;
	protected:
		int mProtectedInt;
	public:
		SimpleClass() : mPrivateInt(10),
						mPrivateDouble(20.0),
						mThisTest(30),
						mProtectedInt(40),
						mPublicInt(50) {}

		int mPublicInt;

		int privateInt() { return mPrivateInt; }
		double privateDouble();
		int protectedInt() { return mProtectedInt; }
		int thisTest() { return this->mThisTest; }

		int inlineFunc();

		int funcOverloading(int i) { return i; };
		double funcOverloading(double d) { return d; };
};

double SimpleClass::privateDouble() {
	return mPrivateDouble;
}

inline int SimpleClass::inlineFunc() {
	return 0;
}

int main() {
	SimpleClass c;

	// test public member access
	std::cout << "50 == " << c.mPublicInt;

	// test public member function access
	std::cout << "10 == " << c.privateInt();
	std::cout << "20.0 == " << c.privateDouble();
	std::cout << "40 == " << c.protectedInt();

	// test this-pointer
	std::cout << "30 == " << c.thisTest();

	// test inline-keyword
	std::cout << "0 == " << c.inlineFunc();

	// test member-func-overloading
	std::cout << "1.5 == " << c.funcOverloading(1.5);	//double
	std::cout << "1 == " << c.funcOverloading(1);		//int

	return 0;
}

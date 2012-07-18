#include <stdio.h>

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

		int funcOverloading(int i) { return mPrivateInt; };
		double funcOverloading(double d) { return mPrivateDouble; };

		int constFunc() const { return mPrivateInt; };
};

double SimpleClass::privateDouble() {
	return mPrivateDouble;
}

inline int SimpleClass::inlineFunc() {
	return 0;
}

int main() {
	// test user defined Ctor with init-list for members
	SimpleClass c;

	// test public member access
	printf("50 == %d\n",c.mPublicInt);

	// test public member function access
	printf("10 == %d\n", c.privateInt());
	printf("20.0 == %f\n", c.privateDouble());
	printf("40 == %d\n", c.protectedInt());

	// test this-pointer
	printf("30 == %d\n", c.thisTest());

	/*Member function tests*/
	// test inlined member function
	printf("0 == %d\n", c.inlineFunc());

	// test member-func-overloading
	printf("1.5 == %f\n", c.funcOverloading(1.5));	//double
	printf("1 == %d\n", c.funcOverloading(1));		//int

	// test const-func
	printf("%d == %d\n", c.privateInt(), c.constFunc());

	return 0;
}

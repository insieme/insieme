#include <iostream>

//nested class
class Outer {
	class Inner {
	public:
		int inner;
	};
	Inner i;
public:
	int outer;
};

int main() {
	//nested class
	Outer o;
	return 0;
}

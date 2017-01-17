#include <stdio.h>


class A{
	int a;
	int b;

	public:
	A()
	:a(0), b(0)
	{}

	void mF(){
		printf("this is the functionCall of A\n");
	}
};

class B{
	int a;
	int b;

	public:
	B()
	:a(0), b(0)
	{}

	void mF(){
		printf("this is the functionCall of B\n");
	}
};



int main(){
	A* a = new A();
	B* b = new B();
	a->mF();
	b->mF();

	delete(a);
	delete(b);
	return 0;
}

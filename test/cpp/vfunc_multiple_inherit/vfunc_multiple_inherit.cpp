#include <stdio.h>

class A {
	int aA;
	public:
	virtual void const f() = 0;
	virtual int g(int x) = 0;
};

class X {
	int xX;
	public:
	virtual void x(int x) { this->xX=x; printf("X::x\n"); }
};

class B : public A, public X {
	int bB;
	public:
	void const f() { printf("B::f\n"); } 
	int g(int x) { printf("B::g\n"); return x; }
	virtual void b() { printf("B::b\n"); }
};

class Y {
	int yY;
	public:
	virtual void y() { printf("Y::y\n"); }
};

class C : public B, public Y {
	int cC;
	public:
	C() {}
	
	virtual void c() { printf("C::c\n"); }
	int g(int x) { printf("C::g\n"); this->cC; return x;}
	void x(int x) { printf("C::x\n"); }
	void y() { printf("C::y\n"); }
};

int call_vfunc_ptr() {
	B* pbC = new C();
	C* pcC = new C();
	X* pxB = new B();
	Y* pyC = new C();

	pxB->x(1);	//virtual call: X::x
	pbC->x(1);	//virtual call: C::x
	pcC->x(1);	//virtual call: C::x
	
	pyC->y();	//virtual call: Y::Y
	pcC->y();	//virtual call: C::y
};

int call_vfunc_ref() {
	C c;	
	B b;
	
	B& rbC = c;
	C& rcC = c;
	X& rxB = b;
	Y& ryC = c;

	rxB.x(1);	//virtual call: X::x
	rbC.x(1);	//virtual call: C::x
	rcC.x(1);	//virtual call: C::x
	
	ryC.y();	//virtual call: Y::Y
	rcC.y();	//virtual call: C::y
};

int main() {
	call_vfunc_ptr();
	call_vfunc_ref();

	C c;		
	c.y();	//non-virtual call: C::y
	c.x(1);	//non-virtual call: C::x

	Y& ryC = c;
	ryC.y();	//virtual call: C::y

	X& rxC = c;
	rxC.x(1);	//virtual call: C::x
	
	return 0;
}

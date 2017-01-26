#include <stdio.h>

class A {
	int aA;
	public:
	A() { printf("A()\n"); }
	virtual ~A() { printf("~A()\n"); }

	virtual void const f() = 0;
	virtual int g(int x) = 0;
};

class X {
	int xX;
	public:
	X() { printf("X()\n"); }

	virtual ~X() { printf("~X()\n"); }
	virtual void x(int x) { this->xX=x; printf("X::x\n"); }
};

class B : public A, public X {
	int bB;
	public:
	B() { printf("B()\n"); }
	virtual ~B() { printf("~B()\n"); }

	void const f() { printf("B::f\n"); } 
	int g(int x) { printf("B::g\n"); return x; }
	virtual void b() { printf("B::b\n"); }
};

class Y {
	int yY;
	public:
	Y() { printf("Y()\n"); }
	virtual ~Y() { printf("~Y()\n"); }

	virtual void y() { printf("Y::y\n"); }
};

class C : public B, public Y {
	int cC;
	public:
	C() { printf("C()\n"); }
	virtual ~C() { printf("~C()\n"); }
	
	virtual void c() { printf("C::c\n"); }
	int g(int x) { printf("C::g\n"); this->cC; return x;}
	void x(int x) { printf("C::x\n"); }
	void y() { printf("C::y\n"); }
};

void call_vfunc_ptr() {
	C c;
	B b;

	A* paB = &b;

	B* pbC = &c;
	C* pcC = &c;
	X* pxB = &b;
	Y* pyC = &c;
	
	paB->g(1);	//virtual call: B::g
	paB->f();	//virtual call: B::f

	pxB->x(1);	//virtual call: X::x
	pbC->x(1);	//virtual call: C::x
	pcC->x(1);	//virtual call: C::x
	
	pyC->y();	//virtual call: Y::Y
	pcC->y();	//virtual call: C::y
}

void call_vfunc_ref() {
	C c;	
	B b;

	A& raB = b;
	
	B& rbC = c;
	C& rcC = c;
	X& rxB = b;
	Y& ryC = c;

	raB.g(1);	//virtual call: B::g
	raB.f();	//virtual call: B::f

	rxB.x(1);	//virtual call: X::x
	rbC.x(1);	//virtual call: C::x
	rcC.x(1);	//virtual call: C::x
	
	ryC.y();	//virtual call: Y::Y
	rcC.y();	//virtual call: C::y
}

int main() {
	call_vfunc_ptr();
	call_vfunc_ref();

	C c;		
	
	c.c();		//non-virtual call: C::c
	c.f();		//non-virtual call: B::f
	c.g(1);		//non-virtual call: C::g
	
	c.y();	//non-virtual call: C::y
	c.x(1);	//non-virtual call: C::x

	A& raC = c;
	
	raC.f();		//virtual call: B::f
	raC.g(1);		//virtual call: C::g

	Y& ryC = c;
	ryC.y();	//virtual call: C::y

	X& rxC = c;
	rxC.x(1);	//virtual call: C::x
	
	return 0;
}

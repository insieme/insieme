#include <stdio.h>

class A {
	int aA;
	public:
	virtual ~A() { printf("~A()\n"); }

	virtual void const f() = 0;
	virtual int g(int x) = 0;
};

class B : public A {
	int bB;
	public:
	B() { printf("B()\n"); }
	virtual ~B() { printf("~B()\n"); }

	void const f() { printf("B::f\n"); } 
	int g(int x) { printf("B::g\n"); return x; }
	virtual void b() { printf("B::b\n"); }
};

class C : public B {
	int cC;
	public:
	virtual ~C() { printf("~C()\n"); }
	
	virtual void c() { printf("C::c\n"); }
	int g(int x) { printf("C::g\n"); this->cC = x; return x;}
};

void call_vfunc_ptr() {
	C c;	
	B b;
	A* paB = &b;
	B* pbC = &c;
	C* pcC = &c;

	paB->g(1);	//virtual call: B::g
	pbC->g(1);	//virtual call: C::g
	pcC->g(1);	//virtual call: C::g
	
	paB->f();	//virtual call: B::f
	pbC->f();	//virtual call: B::f
	pcC->f();	//virtual call: B::f

	pbC->b();	//virtual call: B::b
	pcC->b();	//virtual call: B::b
	
}

void call_vfunc_ref() {
	C c;	
	B b;
	A& raB = b;
	B& rbC = c;
	C& rcC = c;

	raB.g(1);	//virtual call: B::g
	rbC.g(1);	//virtual call: C::g
	rcC.g(1);	//virtual call: C::g
	
	raB.f();	//virtual call: B::f
	rbC.f();	//virtual call: B::f
	rcC.f();	//virtual call: B::f

	rbC.b();	//virtual call: B::b
	rcC.b();	//virtual call: B::b
}

int main() {
	call_vfunc_ptr();
	call_vfunc_ref();

	C c;		

	c.c();		//non-virtual call: C::c
	c.f();		//non-virtual call: B::f
	c.g(1);		//non-virtual call: C::g
	
	c.b();
	
	B* pbC = &c;
	pbC->b();
	
	B& rbC = c;
	rbC.b();

	A& raC = c;
	
	raC.f();		//virtual call: B::f
	raC.g(1);		//virtual call: C::g
	
	return 0;
	
}

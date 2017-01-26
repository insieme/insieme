#include <stdio.h>

class A {
	int aA;
	public:
	A() { printf("A()\n"); }
	virtual ~A() { printf("~A()\n"); }

	int m_a;

	virtual void const f() = 0;
	virtual int g(int x) = 0;
	
	virtual bool operator==(const A& o) = 0;

	virtual bool operator<=(const A& o) = 0;
	
	virtual void operator()(int x, int y, int z) = 0;
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
	~B() { printf("~B()\n"); }

	void const f() { printf("B::f\n"); } 
	int g(int x) { printf("B::g\n"); return x; }
	virtual void b() { printf("B::b\n"); }

	bool operator==(const A& o) { printf("B::operator==\n"); return false; }
	
	bool operator<=(const A& o) { printf("B::operator<=\n"); return (this->m_a <= o.m_a); }

	void operator()(int x, int y, int z) { x = x + y + z + this->bB; printf("B::operator()\n"); }
};

class Y {
	int yY;
	public:
	virtual void y() { printf("Y::y\n"); }

	Y() { printf("~Y()\n"); }
	virtual ~Y() { printf("~Y()\n"); }
};

class C : public B, public Y {
	int cC;
	public:
	C() { printf("C()\n"); }
	virtual ~C() { printf("~C()\n"); };
	
	virtual void c() { printf("C::c\n"); }
	int g(int x) { printf("C::g\n"); this->cC; return x;}
	void x(int x) { printf("C::x\n"); }
	void y() { printf("C::y\n"); }
	bool operator==(const A& o) { printf("C::operator==\n"); return false; }
	
	bool operator<=(const A& o) { printf("C::operator<=\n"); return (this->m_a <= o.m_a); }

	void operator()(int x, int y, int z) { x = x + y + z + this->cC; printf("C::operator()\n");}
};

void call_vfunc_ptr() {
	C c;
	B b;

	B* pbC = &c;
	C* pcC = &c;
	A* paB = &b;
	A* paC = &c;

	*pbC == *pbC;	//virtual call: C::operator()
	*pcC == *pcC;	//virtual call: C::operator()
	*paB == *paB;	//virtual call: B::operator()
	*paC == *paC;	//virtual call: C::operator()

	*pbC <= *pbC;	//virtual call: C::operator()
	*pcC <= *pcC;	//virtual call: C::operator()
	*paB <= *paB;	//virtual call: B::operator()
	*paC <= *paC;	//virtual call: C::operator()

	(*pbC)(1, 2, 3);	//virtual call: C::operator() 
	(*pcC)(1, 2, 3);	//virtual call: C::operator()
	(*paB)(1, 2, 3);	//virtual call: B::operator()
	(*paC)(1, 2, 3);	//virtual call: C::operator()
}

void call_vfunc_ref() {
	C c;	
	B b;
	
	B& rbC = c;
	C& rcC = c;
	A& raB = b;
	A& raC = c;

	rbC == rbC;	//virtual call: C::operator==
	rcC == rcC;	//virtual call: C::operator==
	raB == raB;	//virtual call: B::operator==
	raC == raC;	//virtual call: C::operator==

	rbC <= rbC;	//virtual call: C::operator<=
	rcC <= rcC;	//virtual call: C::operator<=
	raB <= raB;	//virtual call: B::operator<=
	raC <= raC;	//virtual call: C::operator<=

	rbC(1, 2, 3);	//virtual call: C::operator()
	rcC(1, 2, 3);	//virtual call: C::operator()
	raB(1, 2, 3);	//virtual call: B::operator()
	raC(1, 2, 3);	//virtual call: C::operator()
}

int main() {
	C c;		
	A& raC = c;
	B& rbC = c;
	C& rcC = c;
	C* pcC = &c;

	c.operator==(raC);		//non-virtual call: C::operator==

	rbC.operator==(raC);	//virtual call: C::operator==

	raC==raC;	//virtual call C::operator==
	
	rbC==raC;	//virtual call C::operator==
	
	c<=c;		//non-virtual call: C::operator<=
	
	c(0,1,2);	//non-virtual call with more than 2 args: C::operator()

	call_vfunc_ptr();
	call_vfunc_ref();

	return 0;
}

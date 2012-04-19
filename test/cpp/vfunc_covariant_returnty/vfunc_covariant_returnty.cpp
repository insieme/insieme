#include <stdio.h>
struct T {
	int tT;
	T() { printf("T()"); }
	~T() { printf("~T()"); }
	virtual void t() { printf("T::t\n"); }
};

struct U {
	int	uU;
	U() { printf("U()"); }
	~U() { printf("~U()"); }
	virtual void u() { printf("U::u\n"); }
};

struct V : public U {
	int vV;
	V() { printf("V()"); }
	~V() { printf("~V()"); }
};

struct W : public V {
	int wW;
	W() { printf("W()"); }
	~W() { printf("~W()"); }
};

struct S : public T, public W {
	int sS;

	S() { printf("S()"); }
	~S() { printf("~S()"); }

	virtual void s() { printf("S::s\n"); }
	virtual void u() { printf("S::u\n"); }
	virtual void t() { printf("S::t\n"); }
};

class A {
	int aA;
	public:

	A() { printf("A()"); }
	~A() { printf("~A()"); }

	virtual U*	w() = 0;
};

class B : public A {
	int bB;
	public:

	B() { printf("B()"); }
	~B() { printf("~B()"); }

	virtual void b() { printf("B::b\n"); }
	virtual U*	w() { printf("B::w\n"); return new U(); }
};

class C : public B {
	int cC;
	public:
	C() { printf("C()"); }
	~C() { printf("~C()"); }
	
	virtual void c() { printf("C::c\n"); }
	virtual S*	w() { printf("C::w"); return new S(); }
};

int call_vfunc_ptr() {
	A* paB = new B();
	B* pbC = new C();
	C* pcC = new C();
	
	U* pu;
	
	pu = paB->w();	//virtual call: B::w, returns U
	pu->u();		//virtual call: U::u
	
	pu = pbC->w();	//virtual call: C::w, returns S
	pu->u();		//virtual call: S::u
	
	pu = pcC->w();	//virtual call: C::w, returns S
	pu->u();		//virtual call: S::u

};

int call_vfunc_ref() {
	C c;	
	B b;	
	A& raB = b;
	B& rbC = c;
	C& rcC = c;

	U* pu;
	
	pu = raB.w();	//virtual call: B::w, returns U
	pu->u();		//virtual call: U::u
	
	pu = rbC.w();	//virtual call: C::w, returns S
	pu->u();		//virtual call: S::u
	
	pu = rcC.w();	//virtual call: C::w, returns S
	pu->u();		//virtual call: S::u

};

int main() {
	call_vfunc_ptr();
	call_vfunc_ref();

	C c;
	B b;

	A* paC = &c;
	A& raB = b;

	U* pu = paC->w();	//virtual call: C::w, returns S
	pu->u();			//virtual call: S::u

	pu = raB.w();	//virtual call: B::w, returns U
	pu->u();		//virtual call: U::u


	return 0;
}

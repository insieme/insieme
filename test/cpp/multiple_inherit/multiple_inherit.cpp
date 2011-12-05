//#include <iostream>

//virtual inheritance
class Animal {
public:
	virtual void eat();
};

// Two classes virtually inheriting Animal:
class Mammal : public virtual Animal {
public:
	virtual void walk();
};

class WingedAnimal : public virtual Animal {
public:
	virtual void flap();
};

// A bat is still a winged mammal
class Bat : public Mammal, public WingedAnimal {
public:
	void eat();
	void walk();
	void flap();
};

//multiple inheritance
class A {
public:
	int aA;
	int X;
};

class B {
public:
	int aB;
	int X;
};

class C : public A, public B {
public:
	C() {}
	int aC;
};

class Abstract1 {
public:
	virtual void func1() = 0;
};

class Abstract2 {
public:
	virtual void func2() = 0;
};

class Class : public Abstract1, public Abstract2 {
public:
	void func1() { }
	void func2() { }
};


int main() {
	{
		//virtual inheritance
		Animal a;
		Mammal m;
		WingedAnimal wA;
		Bat b;

		Mammal& mammal = b;
		Mammal* pMammal = new Bat();

		mammal.eat();
		pMammal->eat();
		mammal.walk();
		pMammal->walk();

		WingedAnimal& wingedAnimal = b;
		WingedAnimal* pWingedAnimal = new Bat();

		wingedAnimal.eat();
		pWingedAnimal->eat();
		wingedAnimal.flap();
		pWingedAnimal->flap();

		b.eat();
		b.walk();
		b.flap();
	}
	{
		C c;
		c.A::X;
		c.B::X;
	}
	{
		Class c;
		c.func1();
		c.func2();

		Abstract1* a1 = new Class();
		a1->func1();

		Abstract2& a2 = c;
		a2.func2();
	}
	return 0;
}

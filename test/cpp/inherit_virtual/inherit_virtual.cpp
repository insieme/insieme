#include <stdio.h>

/*
 * Virtual inheritance
 *
 * NOT WORKING
 *
 * */

class Animal {
public:
	virtual void eat() { printf("Animal::eat()\n"); }
	virtual ~Animal() { printf("~Animal\n"); }
};

// Two classes virtually inheriting Animal:
class Mammal : public virtual Animal {
public:
	virtual void walk() { printf("Mammal::walk()\n"); }
	virtual ~Mammal() {printf("~Mammal\n"); } 
};

class WingedAnimal : public virtual Animal {
public:
	virtual void flap() { printf("WingedAnimal::flap()\n"); }
	virtual ~WingedAnimal() {printf("~WingedAnimal\n"); }
};

// A bat is still a winged mammal
class Bat : public Mammal, public WingedAnimal {
public:
	void eat() { printf("Bat::eat()\n"); }
	void walk() { printf("Bat::walk()\n"); }
	void flap() { printf("Bat::flap()\n"); }
	~Bat() {printf("~Bat\n"); }
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

			delete pWingedAnimal;
			delete pMammal;
		}
}

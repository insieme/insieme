#include <iostream>

//the problem was related with the "this"literal --- which is now gone
struct X {
	int x;
	X() : x(0) {};
	bool boolMFunc() { return false; }
	bool boolMFunc(int _x) { return (x!=_x); }
	
	void loopI() {
		x = 0;
		do {
			x++;
		}while( boolMFunc(10) );
		std::cout << x << std::endl;
	}
	void loopE() {
		x = 0;
		do {
			x++;
		}while( this->boolMFunc(10) );
		std::cout << x << std::endl;
	}

	void loop2I() {
		x = 0;
		do {
			x++;
		}while( boolMFunc() );
		std::cout << x << std::endl;
	}

	void loop2E() {
		x = 0;
		do {
			x++;
		}while( this->boolMFunc() );
		std::cout << x << std::endl;
	}
};

int main () {
	X x;
	x.loopI();
	x.loopE();
	x.loop2I();
	x.loop2E();

	x.x = 0;
	do {
		x.x++;
	}while(x.boolMFunc(10));
	std::cout << x.x << std::endl;

	x.x = 0;
	do {
		x.x++;
	}while(x.boolMFunc());
	std::cout << x.x << std::endl;
}

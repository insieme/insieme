#include <stdio.h>

class C {
	int mA;

public:
	
	C(int a) : mA(a){}
	C(const C& c1 ){
		this->mA = c1.mA;	
	}		
	~C() {
		printf("~C(%d)",mA);
		mA = 0;
	}
};

void f(C c) {
	//copy of c is created and destroyed when leaving the function scope
	printf("-entering f scope-");
	C local(10);
	printf("-exiting f scope-");
}

void p(const C& c) {
        //copy of c is created and destroyed when leaving the function scope
        printf("-entering p scope-");
        C local(10);
        printf("-exiting p scope-");
}

C q(C c) {
        //copy of c is created and destroyed when leaving the function scope
        printf("-entering q scope-");
        C local(10);
        printf("-exiting q scope-");
	return local;
}

C r(const C& c) {
        //copy of c is created and destroyed when leaving the function scope
        printf("-entering r scope-");
        C local(10);
        printf("-exiting r scope-");
	return local;
}


int main() {
	C c1(1);
	f(c1);
	f(C(2));
	C c3(3);
	p(c3);
	p(C(4));
	C c5(5);
	q(c5);
	q(C(6));
	C c7(7);
	r(c7);
	r(C(8));
	return 0;
}

#include <stdio.h>

class Obj{
private:
	void one(){
		printf("one\n");
	}

public:

	int f(){
		printf("f\n");

	}
	int f2(int v){
		printf("f2 %d\n", v);
	}

	void g(){
		printf("g\n");
	}
	void g2(int x){
		printf("g2 %d\n", x);
	}


	void two(){
		printf("two\n");
		one();
	}
};

int main(){

	Obj o;

	o.f();
	o.f2(1);
	o.g();
	o.g2(2);
	o.two();


	return 0;
}

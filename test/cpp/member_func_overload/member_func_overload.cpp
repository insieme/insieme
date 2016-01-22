#include <stdio.h>

class Obj{
private:

public:

	int f(){
		printf("f\n");
		return 0;
	}
	int f(int v){
		printf("f2 %d\n", v);
		return 0;
	}

	void g(){
		printf("g\n");
	}
	void g(int x){
		printf("g2 %d\n", x);
	}

};

int main(){

	Obj o;

	o.f();
	o.f(1);
	o.g();
	o.g(2);


	return 0;
}

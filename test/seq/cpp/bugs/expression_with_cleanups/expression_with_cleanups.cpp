#include <stdio.h>


class Obj{
public:
	~Obj() { } 
};


class Papa{
	int x;
public:
	Papa( Obj a, Obj b) :x(15){ } 
	~Papa () {}
	int get() { return x;}
};


void f(Papa a){
		printf("%d\n", a.get() );
}
int g(Papa a){
	return a.get();
}

int main (){
	Obj a;
	Obj b;
	f(Papa(a,b));


	int x = 0;
	x = g(Papa(a,b));

	printf("%d\n", x );
	
	for (int i = g(Papa(a,b)); i> 0; --i){
		printf("%d\n", i );
	}

	return 0;
}

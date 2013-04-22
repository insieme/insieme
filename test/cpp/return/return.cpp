#include <stdio.h>


class Obj {

public:
	Obj(){
		printf ("CTOR\n");
	}

};


Obj f(){
	Obj o;
	return o;
}

Obj g(){
	return Obj();
}

Obj h(){
	Obj o;
	Obj& ref = o;
	return ref;
}


int main(){

	{
		f();
		g();
		h();
	}

	{
		Obj a =	f();
		Obj b = g();
		Obj c = h();
	}
	{
		const Obj& a =f();
		const Obj& b = g();
		const Obj& c = h();
	}
}

#include <iostream>

struct Obj {
	Obj() : a(10) {}
	int a;
};

int main() {

	/*
	//this block works
	{
		Obj a;
		Obj();
	}
	*/
	
	//this block doesn't work
	{
		Obj();
	}

	return 0;
}


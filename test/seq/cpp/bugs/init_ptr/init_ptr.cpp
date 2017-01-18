#include <iostream>

struct Obj{

	int value;
	char* charPtr;

	void initThing(){
		value = 0;
		charPtr = NULL;

		std::cout << "init the thing" << std::endl;
	}
};


int main (){

	Obj a;
	a.initThing();
	return 0;
}


#include <iostream>


class Obj{
	int field;
public:
	Obj(int o):field(o){
	}
	int get(){
		return field;
	}
};

Obj global(1);


int main (){

	std::cout << "obj: " << global.get() << std::endl;
	

	return 0;
}


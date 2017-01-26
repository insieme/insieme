#include <iostream>

class Obj{
	Obj& operator=(const Obj&){
		return *this;
	}

	int val;
public:
	Obj(int par = 10)
		: val(par)
	{ }

	void method(){
		std::cout << "val: " << val<< std::endl;
	}
};

Obj  obj;
int main(){
	obj.method();
	return 0;
}

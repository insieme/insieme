#include <iostream>

class Obj{
public:
	int a;

	Obj(int a) : a(a) {
	}

	Obj(Obj& o) : a(o.a) {
	}
};


int main(){
	Obj a(4);
	Obj b(a);
	
	std::cout << b.a << std::endl;
}

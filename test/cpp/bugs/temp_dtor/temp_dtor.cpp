#include <iostream>


class Obj{
	int a;
public: 
	Obj(int a = 0) : a(a) { std::cout << "ctor" << a << std::endl;}
	Obj(const Obj& o) : a(o.a) { std::cout << "copy ctor" << a<< std::endl;}
	~Obj()  {std::cout << "dtor" << a << std::endl; }
	int getA() {return a;}
};

void f(Obj o){
	std::cout << "func call" << o.getA() << std::endl;
}

int main (){

//	std::cout << " === "<< std::endl;
	Obj a;
	f(a);
//	std::cout << " === "<< std::endl;
//	f(Obj(1));
//	std::cout << " === "<< std::endl;

	return 0;
}

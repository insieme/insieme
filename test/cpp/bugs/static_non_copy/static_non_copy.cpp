#include <iostream>

class Obj{

	int val;

	Obj(const Obj& obj) = delete;
	Obj operator=(const Obj& obj) = delete;

public:
	Obj(): val(13){
		std::cout << "ctor" << std::endl;
	}

	int getVal(){
		return val;
	}
};



////////////////////////////////////////////////////
//int f(int a){
//	static int x = a;
//	return x;
//}

////////////////////////////////////////////////////
int g(int a){
	static Obj x;
	return x.getVal();
}


int main (){

	{
		std::cout << g(1) << std::endl;
		std::cout << g(2) << std::endl;
	}


	return 0;

}

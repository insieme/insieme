#include <iostream>

class Obj{

	int val;

	Obj(const Obj& obj){
	}
	Obj operator=(const Obj& obj){
	}

public:
	Obj(): val(0){
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

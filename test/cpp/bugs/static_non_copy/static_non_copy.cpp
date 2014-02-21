#include <iostream>

class Obj{

	int val;

public:
	Obj(int a): val(a){
	}

	int getVal(){
		return val;
	}
};



////////////////////////////////////////////////////
int f(int a){
	static int x = a;
	return x;
}

////////////////////////////////////////////////////
//int g(int a){
//	static Obj x(a);
//	return x.getVal();
//}
//

int main (){

	{
		std::cout << f(1) << std::endl;
		std::cout << f(2) << std::endl;
	}
//	{
//		std::cout << g(1) << std::endl;
//		std::cout << g(2) << std::endl;
//	}


	return 0;

}

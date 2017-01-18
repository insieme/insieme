

#include <iostream>

void f(int a){
	std::cout << " val: " << a << std::endl;
}

template <typename ... ARGS>
void f(int a,  ARGS... args ){
	std::cout << " val: " << a << std::endl;
	f(args...);
}


int main (){
	f (1,2,3,4);
	return 0;
}

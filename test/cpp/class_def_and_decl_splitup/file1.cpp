#include <iostream>
#include "class.h"
int g(int a){
	return a;
}
int main(){
	std::cout << g(2) << std::endl;

	CLASS c1(100);
	CLASS c2(101);
	CLASS c3(102);

	return 0;
};

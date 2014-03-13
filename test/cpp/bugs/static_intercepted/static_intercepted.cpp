
#include <iostream>
#include "header.h"

int f(){
	static toIntercept::mutex mut;
	mut.lock();
	std::cout << "critical 1" << std::endl;
	mut.unlock();
	return 124;
}


int g(){
	return 0;
}


int main (){

	std::cout << " f " << f() << std::endl;

	return 0;
}


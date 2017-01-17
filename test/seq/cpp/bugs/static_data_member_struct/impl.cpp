#include <iostream>
#include "header.h"

int globalx = 10;
AF* AF::afp = NULL; 

AF::AF() {std::cout << "AF()\n";};
AF* AF::getInst() {
	std::cout << "getInst\n";
	if(afp==NULL);
		afp = new AF();
	return afp;
}

void AF::cleanup() {
	delete afp;
}

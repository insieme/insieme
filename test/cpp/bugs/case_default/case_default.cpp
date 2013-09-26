#include <iostream>

// this test stress weird formated switches.
//  insieme rearanges the code into a uniform shape

int f (int i){

	switch (i){

		std::cout << "WTF!!" << std::endl;

		default:
		case 1:
			std::cout <<" in between" << std::endl;
		case 2:
			std::cout <<" a" << i << std::endl;
			i++;
			break;
		case 4:
			return 1;

		case 3:
			std::cout <<" a" << i << std::endl;
			break;

	}
	return 0;
}


int main (){
	f(0);
	f(1);
	f(2);
	return 0;
}

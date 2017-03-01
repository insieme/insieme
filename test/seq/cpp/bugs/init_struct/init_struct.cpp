

#include <iostream>


struct obj{
	int a;
	float b;
};

obj global = {2, 2.3f};
int x = 10;


obj ptr[] = { {1,10.0}, {2,1.3}, {3,45.0}};

int main(int argc, char **argv){

	std::cout << global.b  << x<< std::endl;
	std::cout << ptr[1].b  << x<< std::endl;
	return 0;
}

#include <iostream>
#include <string>

#include <sstream>

char cad[] = "this is string";

int main(int argc, char **argv){
	std::istringstream iss(cad);

	int a;
	iss >> a;
	if (iss.fail()) std::cout << "should fail" << std::endl;

	return 0;
}


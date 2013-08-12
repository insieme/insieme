#include <iostream>
#include <string>

#include <sstream>

char str[] = "this is a string";

int main(int argc, char **argv){
	std::istringstream iss(str);

	int a;
	iss >> a;
	if (iss.fail()) std::cout << "should fail" << std::endl;

	return 0;
}


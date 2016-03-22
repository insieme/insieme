#include <map>
#include <functional>
#include <string>
#include <iostream>

struct A {
};

enum X {
    A,B,C
} xX;

int main() {
	std::map<std::string, std::pair<struct A, int> > values;
	std::map<X,std::string> enumVal;

	std::map<const std::string, std::pair<struct A, int> >::iterator x = values.find("test");
	std::map<X,std::string>::iterator y = enumVal.find(xX);
	if(x==values.end())
		std::cout << "not found";
	else
		std::cout << "found";
	return 0;
}

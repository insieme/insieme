#include <string>
#include <cstdio>

int main(int ac, char** av) {
	char** x = av;
	std::string a = x[0];
	std::string b = av[0];
	printf("%s = %s\n", a.c_str(), b.c_str());
}

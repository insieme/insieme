#include <string>

int main(int ac, char** av) {
	char** x = av;
	std::string a = x[0];
	std::string b = av[0];
	return a != b;
}

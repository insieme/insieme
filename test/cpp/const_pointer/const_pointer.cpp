#include <string.h>
#include <iostream>

void display(char* p) {
	std::cout << *p << " non const arg" << std::endl;
}

void display(const char* p) {
	std::cout << *p << " const arg" << std::endl;
}

int main() {
	char* p = strdup("string");
	const char* cp = "string";
	display(p);
	display(cp);
	return 0;
}

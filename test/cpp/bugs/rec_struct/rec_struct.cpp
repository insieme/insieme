#include <iostream>

struct marker {
	struct marker* next;
	int n;
};

int main() {
	{
		marker a;
		a.n=5;
		std::cout << a.n;
	}
	{
		marker* pa = new marker;
		pa->n = 5;
		std::cout << pa->n;

	}
	return 0;
}

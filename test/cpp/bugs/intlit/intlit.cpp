#include <cstddef>
#include <cstdio>
int main() {
	std::size_t st = 10;
	for(std::size_t i=0ul;i<0;i++);
	long l1 = 9223372036854775807;
	printf("%ld", l1);

	long l = 1000000000;
	printf("%ld", l);

	unsigned long ul = 1000000000;
	printf("%ld", ul);

	double x = 1.0e10;
	printf("%f", ul);
	return 0;
}

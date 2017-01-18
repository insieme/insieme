#include <stdio.h>

int odd(unsigned);

int even(unsigned x) {
	return (x==0)?1:odd(x-1);
}

int main(int argc, char* argv[]) {
	printf("even(%d)?: %s\n", 12, even(12) ? "true" : "false");
	printf("odd(%d)?: %s\n", 12, odd(12) ? "true" : "false");
}
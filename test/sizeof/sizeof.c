
#include <stdio.h>

int help() {
	printf("Hello world!\n");
}


int main(int argc, char** argv) {
	printf("Size of int: %d\n", sizeof(help()));
	printf("Size of long double %d\n", sizeof(long double));
	return 0;
}

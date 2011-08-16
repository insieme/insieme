#include <stdio.h>

int main(int argc, char** argv) {
	// just print the list of arguments
	printf("Number of arguments received: %d\n", argc-1);

	// first parameter is not printed (binary name may be different)
	for(int i=1; i<argc; i++) { 
		printf("  %2d. %s\n", i, argv[i]);
	}
	return 0;
}


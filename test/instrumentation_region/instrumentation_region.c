#include <stdio.h>
#include <math.h>
#include <stdlib.h>

int main(int argc, char **argv) {
	if(argc != 2) {
		printf("Need an int argument\n");
		return -1;
	}
		
	#pragma instrumentation region id(0)
	for(int i=0; i<atoi(argv[1]); ++i) {
		printf(".");
	}
	return 0;
}

#include <stdio.h>

// extern int printf(char*, ...);

void fun(void* x) {}

int main(int argc, char* argv[]) {

	{
	#pragma omp parallel for default(none)
	{
	printf("Hallo Insieme, \n\t\"the number %d in compilers!\"\n", 1);
	//printf("Name of executable: %s\n", argv[0]);
	}
	}
	//fun(NULL);
	return 0;
}

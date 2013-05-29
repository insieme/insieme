#include <stdio.h>


int bots_verbose_mode = 1;

#define bots_message(msg, ...) \
	   { int a;}

int main(int argc, char** arv) {

	#pragma omp parallel
	{
		printf("Hello World!\n");
	} // end parallel

	#pragma omp parallel
	bots_message(" completed!\n")
	return 0;
}

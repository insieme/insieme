#include <stdio.h>

#include "a.h"
#include "b.h"


int g(int a) {
	return a-1;
}

int main(int argc, char** argv) {
	printf("res=%d\n", f(0, 0));
}

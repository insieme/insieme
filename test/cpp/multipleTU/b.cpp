#include <stdio.h>

#include "a.h"

int Obj::f(){
	return 0;
}


int main(int argc, char** argv) {
	Obj o;
	printf("res=%d\n", f(o));
}

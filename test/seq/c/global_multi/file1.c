
#include "test.h"
#include <stdio.h>

int var1 = 5;

int func() {
	static int var0 = 1;

	return var0++;
}

int main(int argc, char* argv[]) {

	var1 = 6;

	static int var2 = 2;

	printf("func()=%d, func2()=%d, func3()=%d\n", func(), func2(), (var1-=2, func3()));

	return 0;

}

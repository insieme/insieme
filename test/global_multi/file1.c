
#include "test.h"
#include <stdio.h>

int var1 = 5;

static int func() {
	static int var1 = 1;

	return var1++;
}

int main(int argc, char* argv[]) {

	var1 = 6;

	static int var2 = 2;

	printf("func()=%d, func2()=%d, func3()=%d\n", func(), func2(), (var1-=2, func3()));

	return 0;

}

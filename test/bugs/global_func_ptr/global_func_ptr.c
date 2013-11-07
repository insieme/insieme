#include <stdio.h>
void func() {
	printf("func()\n");
}
void (*gfp)() = func;

int main() {
	void (*fp)() = func;

	(*gfp)();
	(*fp)();
	(gfp)();
	(fp)();
}

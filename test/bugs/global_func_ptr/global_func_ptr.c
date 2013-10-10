#include <stdio.h>
void func() {
	printf("func()\n");
}
void (*fp)() = func;

int main() {
	(*fp)();
	(fp)();
}

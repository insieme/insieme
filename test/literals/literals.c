#include <stdio.h>

//typedef int bool;
#define bool int

#define true 1
#define false 0

int main(int argc, char* argv[]) {

	printf("Hello");
	printf("Hello %s\n", "World");
	printf("Integer:   %d\n", 14);
	printf("Float:     %f\n", 0.54);
	printf("Character: %c\n", 'c');
	printf("Prime:     %c\n", '\'');
	printf("NewLine:   %c", '\n');
	printf("Signed:    %ld\n", -2l);
	printf("Unsigned:  %ld\n", 2ul);
	printf("Full:      %lld\n", 0xFull);
	char c = 'a';
	printf("Char:      %c\n", c);
	return 0;
}

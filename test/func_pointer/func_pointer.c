
#include <stdio.h>


int min(int a, int b)  { return a<b?a:b; }
int max(int a, int b) { return a>b?a:b; }

int sum(int a, int b) { return a+b; }
int avg(int a, int b) { return sum(a,b)/2; }

int main(int argc, char* argv[]) {
	
	int (*funcs[])(int, int) = { min, max, sum, avg };
	
	for(int i=0; i<4; i++) {
		printf("Applying func: %d\n", funcs[i](10, 20));
	}
	
}
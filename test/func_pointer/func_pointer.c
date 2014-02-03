
#include <stdio.h>


int min(int a, int b)  { return a<b?a:b; }
int max(int a, int b) { return a>b?a:b; }

int sum(int a, int b) { return a+b; }
int avg(int a, int b) { return sum(a,b)/2; }

int f(int* v, int size) { 
	for (int i=0; i<size; ++i) 
		printf("%d,", v[i]);

    return 0;
}

int f2(int(*(*fun)[4])(int, int)) {
	return (*fun)[1](1,2);
}

typedef void (*func1)(int a);
typedef void (*func2)(int* a, int b);

int main(int argc, char* argv[]) {
	
	int (*funcs[])(int, int) = { min, max, sum, avg };
	
	for(int i=0; i<4; i++) {
		printf("Applying func: %d\n", funcs[i](10, 20));
	}

	f((int[]){2,3,4}, 3);

	int (*x) (int, int) = &f;
	if(f)
		printf("function pointer if\n");

	if(!2) { }
	
	int(* list[4])(int,int) = {min, max, sum, avg};
	printf("Sum: %d\n", f2(&list));

    // function pointers cast
    
    func1 array[10];
    array[0] = (func1)f;
	((func2)(array[0]))((int[]){5,6,7}, 3);

    return 0;
}

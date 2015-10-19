#include <stdio.h>

int f(){
	static int v = 0;
	return v++;
}
int g(int x){
	return x+1;
}

typedef struct {
	int val;
} S;

int main (){

	{
		printf("test 1\n");
		int a;
		int b;
		while ((a = f() ) < 6){
			int c;
			c = b = a;
			printf("loop: %d %d %d \n", a, b, c);
		}
		printf("after: %d %d \n", a, b);
	}
	{
		printf("test 2\n");
		int x=0;
		int y=10;
		int z=10;
		while ((x = f() ) < (y = g(z))){
			printf("x:%d\n", x);
		}
		printf("after: %d %d %d \n", x, y, z);
	}
	{
		printf("test 3\n");
		S a = {0};
		a.val=5;
		a.val=g(a.val = 6);
		printf("val%d\n", a.val);
	}
	{
		printf("test 4\n");
		int a, b=5;
		int buff[10];

		for (int i=0; i < 10; i++){
			buff[i] = i;
		}

		a = buff[b++] = 0;
		printf("a:%d\n", a);
	}
	{
		printf("test 5\n");
		int a = 0;
		int b = 0;
		while ((a = b++) < 5){
			printf("a:%d\n", a);
			printf("b:%d\n", b);
		}
		printf("after: a %d\n", a);
		printf("after: b %d\n", b);
	}

	return 0;
}

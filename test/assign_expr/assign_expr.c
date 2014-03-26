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
		int a;
		int b;
		while ((a = f() ) < 6){
			int c;
			c = b = a;
			printf("%d\n", a);
		}
	}
	{
		int x=0;
		int y=10;
		int z=10;
		while ((x = f() ) < (y = g(z))){
			printf("%d\n", x);
		}
	}
	{
		S a = {0};
		a.val=5;
		a.val=g(a.val = 6);
		printf("%d\n", a.val);
	}
	{
		int a, b=5;
		int buff[10];

		for (int i=0; i < 10; i++)
			buff[i] = i;

		a = buff[b++] = 0;
		printf("%d/n", a);
	}
	{
		int a = 0;
		int b = 0;
		while ((a = b++) < 5){
			printf("%d\n", a);
			printf("%d\n", b);
		}
		printf("%d\n", a);
		printf("%d\n", b);
	}

	return 0;
}

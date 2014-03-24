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
		while ((a = f() ) < 6){
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

	return 0;
}

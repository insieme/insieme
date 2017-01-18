#include <stdio.h>


int f(int b){
	printf("call\n");
	return 0;
}


int main (){

	{
		int a;
		int b;
		int c;
		int d;
		int e;
		
	//	a = 0;
	//	printf("%d\n", a);
	//	printf("%d\n", a= 234);
	//
	//
		a = b = c = d = e = 1;
		printf("%d %d %d %d %d \n", a,b,c,d,e);

	  	b = c = d = e = 2;
		printf("%d %d %d %d %d \n", a,b,c,d,e);

		d = e = 3;
		printf("%d %d %d %d %d \n", a,b,c,d,e);

		e = 4;
		printf("%d %d %d %d %d \n", a,b,c,d,e);
	}

	{
		int x;
		f(x=3);
	}
	{
		int x;
		int y;
		f(y=x++);
	}
	

	return 0;
}

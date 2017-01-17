#include <stdio.h>

void refParam(int& a){
	printf("4=%d\n", a);
}
void valParam(int a){
	printf("4=%d\n", a);
}


int main (){

	// just declare
	{
		int a =1;
		int& ref = a;

		printf("1=%d\n", ref);
	}

	// assign
	{
		int a =2;
		int b =3;
		int& ref = a;

		ref = b;
		printf("2=%d\n", ref);
		a = ref;
		printf("3=%d\n", a);
	}

	// func call
	{
		int a =4;
		int& ref = a;

		refParam(a);
		valParam(a);

		refParam(ref);
		valParam(ref);
	}
	//return 0;
}

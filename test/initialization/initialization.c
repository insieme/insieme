

#include <stdio.h>

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

int getValue(){
	return 0;
}

int* getPointer(){
	return NULL;
}

const int* getPointerConst(){
	return NULL;
}

int*const getConstPointer(){
	return NULL;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

struct A{
	int a;
	int b;
};

struct A getStruct(){
	struct A a;
	return a;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void function (int a, struct A b, int * c){
}


int main(){

	// scalars
	{
		int a        = 0;
		int *b       = NULL;
		const int *c = NULL;
		int *const d = NULL;
	}

	// scalars from function
	{
		int a        = getValue();
		int *b       = getPointer();
		const int *c = getPointerConst();
		int *const d = getConstPointer();
	}

	// structs;
	{
		struct A a;
		struct A b = {0,1};
		struct A c = {.a = 0, .b = 1};
		struct A d = b;
	}

	// structs from function
	{
		struct A a = getStruct();
	}

	// arrays
	{
		int  a[]  = {0,1,2,3};
		int  b[4] = {0,1,2,3};
		int  c[4] = {0,1};
		int* d[][4] = {a,b};
	}

	// arrays from function
	{
		int a[] = {getValue()};
		int* b[][2] = {getPointer(), getPointer()};
	}

	// initialize parameters
	{	
		function (getValue(), getStruct(), getPointer());
		function (0, (struct A){0,1}, (int*){0});
	}	
	return 0;
}

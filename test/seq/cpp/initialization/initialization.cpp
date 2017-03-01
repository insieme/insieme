

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

int global =1;
int& getRef(){
	return global;
}
const int& getConstRef(){
	return global;
}

typedef int* ptr;
ptr globalPtr = NULL;
ptr& getRefPtr(){
	return globalPtr;
}

ptr& getConstRefPtr(){
	return globalPtr;
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

struct A{
	int a;
	int b;
};

A globalStruct;

A getStruct(){
	return globalStruct;
}

A& getRefStruct(){
	return globalStruct;
}
const A& getConstRefStruct(){
	return globalStruct;
}
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


void f (int a, A b, int * c){
}

void g (int& a, A& b, int *& c){
}
void h (const int& a, const A& b, const ptr& c){
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

	// refs
	{
		int a        = 0;
		int& b = a;
		const int& c = a;
	}

	// refs from functions
	{
		int a  = getRef();
		int b  = getConstRef();

		int& c = getRef();
		const int& d = getRef();
		const int& e = getConstRef();
	}

	// more complex
	{
		int a = getStruct().a;
		int b = getRefStruct().a;
		int c = getConstRefStruct().a;
	}

	// structs;
	{
		A a;
		A b = {0,1};
		A c = {.a = 0, .b = 1};
		A d = b;
	}

	// structs from function
	{
		A a = getStruct();
		A b = getRefStruct();
		A c = getConstRefStruct();

		A& d = getRefStruct();
		const A& e = getRefStruct();
		const A& f = getConstRefStruct();


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
		A a;
		f (0, a, (int*){0});
		f (getValue(), getStruct(), getPointer());
		f (0, (struct A){0,1}, (int*){0});
		
		g (getRef(), getRefStruct(), getRefPtr());

		h (getValue(), getStruct(), getPointer());
		h (getRef(), getRefStruct(), getRefPtr());
		h (getConstRef(), getConstRefStruct(), getConstRefPtr());

	}
	return 0;
}

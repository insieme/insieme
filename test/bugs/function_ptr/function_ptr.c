#include <stdio.h>

void f() { printf("function f\n"); };
void* f1() { printf("function f1\n"); return NULL; };

struct test {
      void *(*bzalloc)();
} t;


int main(int argc, char** argv)
{
	{
		void* p = 0;
		p = NULL;
		
		if(p == NULL) 
			printf("void* p == NULL\n");
		else if(p == NULL) 
			printf("void* p == NULL\n");
		else
			printf("void* p != NULL\n");
	}
	{
		int* p = 0;
		p = NULL;
		
		if(p == NULL) 
			printf("int* p == NULL\n");
		else if(p == NULL) 
			printf("int* p == NULL\n");
		else
			printf("int* p != NULL\n");
	}
	{
		t.bzalloc = (void *(*)())0;
		t.bzalloc = (void *(*)()) NULL;
		t.bzalloc = 0;
		t.bzalloc = NULL;

		if(t.bzalloc == NULL) 
			printf("t.bzalloc == NULL\n");
		else if(t.bzalloc == 0) 
			printf("t.bzalloc == NULL\n");
		else
			printf("t.bzalloc != NULL\n");
	}
	{
		void (*a)() = 0;
		void (*b)();
		void (*c)() = &f;

		b = c;
		f();
		b();
		
		if(a == NULL) 
			printf("a == NULL\n");	
		else if(a == 0) 
			printf("a == NULL\n");	
		else
			printf("a != NULL\n");	
	}
	{
		void* (*a)() = 0;
		void* (*b)();
		void* (*c)() = f1;
		
		b = c;
	
		f1();
		b();

		if(a == NULL) 
			printf("a == NULL\n");	
		else if(a == 0) 
			printf("a == NULL\n");	
		else
			printf("a != NULL\n");	
	}
}

#include <stdio.h>

class Obj{
	public:
		int val;

		Obj(int a)
		:val(a) {}

		Obj(Obj& o)
		:val(o.val) 
		{}

};


void refParam(Obj& a){
	printf("4=%d\n", a.val);
}
void valParam(Obj a){
	printf("4=%d\n", a.val);
}


int  main (){
	// just declare
	{
		Obj a (1);
		Obj& ref = a;

		printf("1=%d\n", ref.val);
	}
	// assign
	{
		Obj a (2);
		Obj b (3);
		Obj c (4);
		Obj& ref = a;
		ref = b;
		printf("3=%d\n", ref.val);
		c = ref;
		printf("3=%d\n", c.val);
	}
	// func call
	{
		Obj a (4);
		Obj& ref = a;

		refParam(a);
		valParam(a);

		refParam(ref);
		valParam(ref);
	}
	return 0;
}

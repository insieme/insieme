#include <stdio.h>

class Obj{
	public:
		int val;

		Obj(int a)
		:val(a) {}

/*		Obj(Obj& o)
		:val(o.val) 
		{}
		*/

};


void constRefParam(const Obj& a){
	printf("4=%d\n", a.val);
}
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
		const Obj& cref = a;
		const Obj& cref2 = ref;

		printf("1=%d\n", ref.val);
		printf("1=%d\n", cref.val);
		printf("1=%d\n", cref2.val);
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
		const Obj& cref = a;

  		valParam(a);
  	valParam(ref);
		valParam(cref);

		refParam(a);
		refParam(ref);

		constRefParam(a);
		constRefParam(ref);
		constRefParam(cref);
	}
	return 0;
}

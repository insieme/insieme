#include <iostream>

class Obj{
	public:
		void f1 ( void) {std::cout << "f1" << std::endl;}
		void f2 ( void) {std::cout << "f2" << std::endl;}
		void f3 ( void) {std::cout << "f3" << std::endl;}

		void g1 (int a) {std::cout << "g1" << a << std::endl;}
};


typedef void (Obj::*memb_func_ptr_t) ( void);
typedef void (Obj::*memb_func_ptr2_t) ( int);

memb_func_ptr_t getNull(){
	return  0;
}


typedef void (*func_ptr_t) ( void);
void func(){
}

func_ptr_t getNull2(){
	return 0;
}



int main (){

	memb_func_ptr_t ptr = &Obj::f1;

	{
		Obj a;
		(a.*ptr)();

		// NOTICE: this calls a member function of a non existing object
		// since it does not use any member field, no  one gave a fuck that day!
		Obj* b;
		(b->*ptr)();
	}
	{
		ptr = &Obj::f2;
		Obj a;
		(a.*ptr)();

		Obj* b;
		(b->*ptr)();
	}
	{
		ptr = &Obj::f3;
		Obj a;
		(a.*ptr)();

		Obj* b;
		(b->*ptr)();
	}
	
	{
		memb_func_ptr2_t ptr;
		ptr = &Obj::g1;
		Obj a;
		(a.*ptr)(1);

		Obj* b;
		(b->*ptr)(2);
	}

	{
		ptr = getNull();
	}

	// plain function pointer
	{
		func_ptr_t ptr2 = &func;
		ptr2 = getNull2();
	}

    {
        if(ptr) {
            Obj a;
            (a.*ptr)();
        } else {
            std::cout << "is null\n";
        }
    }

	return 0;
}

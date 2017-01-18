#include <iostream>



class Obj{
	public:
		Obj() : a(0), b(1){}

		int a;
		int b;
};

typedef int (Obj::* mem_ptr_t);


int main (){

    mem_ptr_t ptr = 0;

	Obj object;
	ptr = &Obj::a;

	std::cout << "ptr: " << object.*ptr << std::endl;
	object.*ptr = 4;
	std::cout << "ptr: " << object.*ptr << std::endl;

    if(ptr) {
        std::cout << "ptr!=0\n";
    } else {
        std::cout << "ptr==0\n";
    }

    {
        mem_ptr_t x;
        Obj o;

        x = &Obj::a;

        int i;

        i= o.*x;

    }

	return 0;
}


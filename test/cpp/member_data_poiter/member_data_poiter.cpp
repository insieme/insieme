//#include <iostream>



class Obj{
	public:
		int a;
		float b;
};

typedef (Obj::*mem_ptr_t);


int main (){

	mem_ptr_t ptr = 0;

	Obj object;
	ptr = &(object.a);





	return 0;
}


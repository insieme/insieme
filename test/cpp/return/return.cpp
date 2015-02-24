#include <iostream>

//////////////////////////////////////////////////////////////////////////////////////////////////
int value(){
	int a =1;
	return a;
}

int& ref(){
	int a = 1;
	return a;
}

const int& constRef(){
	int a = 1;
	return a;
}

int* pointer(){
	int a =1;
	return &a;
}

int recursive(int a) {
    std::cout << "recursive call: " << a << std::endl;
    if(a>0)
        return recursive(a-1);
    return a;
}

void recursive_void(int a) {
    std::cout << "recursive call void: " << a << std::endl;
    if(a>0)
        return recursive_void(a-1);
    return;
}
////////////////////////////////////////////////////////////////////////////////////////////////


class Obj {
	int a;
public:
	Obj(int v=3)
	: a(v)
	{}
	Obj value(){ return *this; }
	Obj& ref(){ return *this; }
	const Obj& constRef(){ return *this; }
	Obj* pointer(){ return this; }

	Obj value2(){ 
		Obj& o = *this;
		return o; 
	}
	Obj& ref2(){ 
		Obj& o = *this;
		return o; 
	}
	const Obj& constRef2(){ 
		Obj& o = *this;
		return o; 
	}
	Obj* pointer2(){
		Obj& o = *this;
		return &o;
	}


	Obj value3(){ 
		const Obj& o = *this;
		return o; 
	}
	const Obj& constRef3(){ 
		const Obj& o = *this;
		return o; 
	}
};

////////////////////////////////////////////////////////////
Obj Objvalue(){
	Obj a;
	return a;
}
Obj& Objref(){
	Obj a;
	return a;
}
const Obj& ObjconstRef(){
	Obj a;
	return a;
}
Obj* Objpointer(){
	Obj a;
	return &a;
}

int main(){

// primitives
	{
		value();
		ref();
		constRef();
	 	pointer();
	}

// objects
	{
		Objvalue();
		Objref();
		ObjconstRef();
		Objpointer();
	}

// members
	{
		Obj o;
		o.value();
		o.ref();
		o.constRef();
		o.pointer();
	}
	
// members 2
	{
		Obj o;
		o.value2();
		o.ref2();
		o.constRef2();
		o.pointer2();
	}

// members 3
	{
		Obj o;
		o.value3();
		o.constRef3();
	}

//recursive
    {
        recursive(3);
    }

//recursive void
    {
        recursive_void(3);
    }
	return 0;
}

#include <iostream>

struct Obj{

	int x;

	Obj(int v =0)
		: x(v)
	{}

	operator int(){
		return x;
	}

	operator float(){
		return (float)x + 0.1f;
	}

	int operator ()(){
		return x * 2;
	}
};

int main (){

	Obj o( 45);

	int x = int(o);
	float y = float(o);
	int z = o();

	std::cout << x << std::endl;
	std::cout << y << std::endl;
	std::cout << z << std::endl;

	return 0;
}

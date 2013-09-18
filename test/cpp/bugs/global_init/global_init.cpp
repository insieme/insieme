

#include <iostream>
#include <vector>



class Obj{
	Obj& operator=(const Obj&){
	}

	int val;
public:
	Obj(int par = 10)
		: val(par)
	{ }

	void method(){
		std::cout << "val: " << val<< std::endl;
	}
};

std::vector<unsigned> input;

Obj  obj;
int main(){


	input.push_back(0);
	obj.method();

	std::cout << "vector[0]:" << input[0] << std::endl;

	return 0;
}

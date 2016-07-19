#include <iostream>

template <class T>
struct S {
	S(std::initializer_list<T> l) {
		std::cout << "constructed with a " << l.size() << "-element list\n";
		for(auto i : l) {
			std::cout << i << std::endl;
		}
	}
};


int main (){

	// C++11 init list constructor
	{
		S<int> collection = {1,5,7,8};
	}

	return 0;
}

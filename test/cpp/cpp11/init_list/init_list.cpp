#include <iostream>
#include <vector>

//struct Object {
//	float first;
//	int second;
//};

template <class T>
struct S {
	std::vector<T> v;

	S(std::initializer_list<T> l) : v(l) {
		std::cout << "constructed with a " << l.size() << "-element list\n";
	}
//
//	void append(std::initializer_list<T> l) {
//		v.insert(v.end(), l.begin(), l.end());
//	}
//
//	std::pair<const T*, std::size_t> c_arr() const {
//		return {&v[0], v.size()};  // list-initialization in return statement
//	}
};


int main (){

	// C++03 standar initialization
//	{
//		Object scalar = {0.43f, 10}; //One Object, with first=0.43f and second=10
//
//		std::cout << "scalar: " << scalar.first << " " << scalar.second << std::endl;
//
//		Object anArray[] = {{13.4f, 3}, {43.28f, 29}, {5.934f, 17}}; //An array of three Objects
//
//		for (int i=0; i<3; i++)
//			std::cout << "scalar: " << anArray[i].first << " " << anArray[i].second << std::endl;
//	}

	// C++11 init list constructor
	{
		S<int> collection = {1,5,7,8};
	}

	return 0;
}

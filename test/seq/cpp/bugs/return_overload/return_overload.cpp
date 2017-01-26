

#include <iostream>



template <typename T>
T function(){
	std::cout << "overload with size: " << sizeof(T) << std::endl;
	return T();
}



template <typename T>
struct Obj{
	T field;

	Obj(T init = T()) : field(init) {}

	T getField(){
		std::cout << "membFunc with size: " << sizeof(T) << std::endl;
		return field;
	}	
};

struct Obj2{
	
	template <typename T>
	T member(){
		std::cout << "membFunc with size: " << sizeof(T) << std::endl;
		return T();
	}	

	template <typename T>
	T operator () (T param){
		std::cout << "membFunc with size: " << sizeof(T) << std::endl;
		return T();
	}

	template<typename T>
	T operator () () {
		std::cout << "operator no args with size" << sizeof(T) << std::endl;
		return T();
	}
};


int main (){

//{
//	int    a = function<int> ();
//	double b = function<double> ();
//	function<Obj<int> > ();
//}
//
//{
//	Obj<int> x;
//	Obj<double> y;
//	x.getField();
//	y.getField();
//}

{
	Obj2 o;
	o.member<int>();
	o.member<double>();
	int x;
	o(x);
	double y;
	o(y);

	o.operator()<int>();
}


	return 0;
}

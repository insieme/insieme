#include <tr1/array>
#include <iostream>


struct Obj{

	int val;

	Obj(int a):val(a) {}
	~Obj() {val =0;}
};




//template < typename T > inline
//std::tr1::array<T, 3>
//makeArray ( const T& a, const T& b, const T& c){
//	std::tr1::array<T, 3> ret = { { a, b , c } };
//	return ret;
//}
//
//
template < typename T, typename ... ARGS > inline
std::tr1::array<T, 1 + sizeof...(ARGS)>
makeArray ( const T& a, const ARGS& ... args){
	std::tr1::array<T, 1 + sizeof...(ARGS)> ret = { { a , static_cast<T>(args)... } } ;
	return ret;
}


int main (){

	//std::tr1::array<int,3> Integer = makeArray(2,3,4);


	std::tr1::array<Obj,3> Objs = makeArray(Obj(1), Obj(2), Obj(3));

	std::cout << "1:" << Objs[0].val << " 2:" << Objs[1].val << " 3:" << Objs[2].val << std::endl;

	return 0;
}

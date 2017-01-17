#include <iostream>

/////////////////////////////////////////
/////////////////////////////////////////
template <typename T>
auto func ( T x, T y) -> decltype(x+y){
	return x+y;
}
/////////////////////////////////////////
/////////////////////////////////////////

struct overload{
	int a;
	float b;

	overload (int a_, float b_)
	: a(a_), b(b_)
	{}


	overload operator+( const overload& o) const{
		overload res(a+o.a, b+o.b);
		return res;
	}
};


int main (){

	auto x = func<int> ( 1, 5);
	std::cout << " value: " << x << std::endl;

	// should have a cast in call
	auto y = func<float> ( 6.0f, 7.0);
	std::cout << " value: " << y << std::endl;

	// should have a cast in call
	auto z = func<overload> ( overload(1,0.1), overload(4, 5.0));
	std::cout << " value: " << z.a << " " << z.b << std::endl;

	return 0;
}

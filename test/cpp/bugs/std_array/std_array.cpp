//is CXX11
//#include <array>
#include <iostream>
#include <boost/array.hpp>

/*
template<typename T, std::size_t N> 
class array 
{
	public:
		T data[N];
};

int main() {
	//is CXX11
	//std::array<int, 3> a;
	array<int, 3> a;
	a.data[0];
}
*/

int main() {
	boost::array<int, 3> a;
	a[0];
}

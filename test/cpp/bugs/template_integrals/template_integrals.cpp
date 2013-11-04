
#include <iostream>

template <typename T, int N, typename R>
R f ( T val){

	R res;
	for (int i=0; i<N; i++){
		res += val;
	}
	return res;

}

int main (){

	{
		float r = f<int, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = f<long long, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = f<char, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}
	{
		float r = f<bool, 5, float> (7);
		std::cout << "r: " << r << std::endl;
	}

	return 0;
}

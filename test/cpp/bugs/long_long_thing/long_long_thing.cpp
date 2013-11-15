#include <iostream>



inline void f (long a){
	std::cout << "long" << a << std::endl;
}

inline void f (long long b){
	std::cout << "long long" << b << std::endl;
}

inline void f (unsigned long a){
	std::cout << "u long" << a << std::endl;
}

inline void f (unsigned long long b){
	std::cout << "u long long" << b << std::endl;
}

int main (){

	{
		f(1l);
		f(2ll);
		f(3ul);
		f(4ull);
	}

	{
		long long a =1;
		a = a +1;
		a ++;
		if (a < 8)
			a = a -3;
		else
			a -=3;
		a --;

		std::cout << " long long " << a << std::endl;

	}

	{
		unsigned long long a =1;
		a = a +1;
		a ++;
		if (a < 8)
			a +=3;
		a --;

		std::cout << " long long " << a << std::endl;

	}

	return 0;
}

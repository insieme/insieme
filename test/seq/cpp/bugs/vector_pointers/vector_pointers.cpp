#include <vector>

struct A{
		int a;
};


int f(int * a){
	return 0;
}

template <typename T>
int g(const T& a){
	return 0;
}

int main (){

	{
		int a = 9;
		std::vector<int *> v;
		v.push_back(&a);
	}

	{
		std::vector<int *> v;
		v.push_back(new int(6));
	}

	{
		int a = 1;
		f(&a);
	}

	{
		f(new int(1));
	}

	{
		g<int*>(new int(1));
	}

	return 0;
}

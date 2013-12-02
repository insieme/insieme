
template <typename T>
struct Obj{

	T& agregate;

	Obj(T& init)
	: agregate(init){
	}
};


Obj<int> f(int& v){
	return Obj<int>(v);
}


template <typename T>
struct A{

	T value;

	A(T init) : value(init) {}

};

template <typename T>
struct B{

	A<T> g() const{
		return A<T>(T());
	}

};


int main (){

	{
	int x = 8;
	const Obj<int>& o = f(x);
	}

	{
		B<int> b;
		const A<int>& ref = b.g();
	}
	{
		typedef A<float> aliasA;
		typedef B<float> aliasB;

		aliasB b;
		const aliasA& ref = b.g();
	}

	return 0;
}




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



int main (){

	int x = 8;
	const Obj<int>& o = f(x);

	return 0;
}



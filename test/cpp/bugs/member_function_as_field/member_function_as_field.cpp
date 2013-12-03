struct Obj{
	int i;
	static int f();
};


struct Obj2 {
	int operator()() const { return 0;}
};

struct Obj3{
	Obj2 f;
};


template <class T>
int function(const T& o){
	int a = o.f();
	return a;
}


int main (){
	{
		Obj o;
		function ( o) ;
	}
	{
		Obj3 o;
		function ( o) ;
	}
	return 0;
}

 int Obj::f() {
	return 0;
}

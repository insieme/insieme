
// type decl					
class Obj{													// TYPE
	// not a var
	int val;
	public:
		// method decl
		int method(){return 0; }			// FUNC
		Obj(){}								// FUNC
		~Obj(){}							// FUNC
};

// func decl								
int func() {								// FUNC
	return 1;
}
// global var
int g;												// VAR

// template thing
template <typename T>										// TYPE  x2?
struct Tmp{
	T val;
	Tmp (T init) 							// FUNC   VAR
		: val(init){}					
	Tmp (const Tmp<T>& o) 					// FUNC   VAR
		: val(o.val){}
	Tmp () {val =0;}						// FUNC

	T getVal()								// FUNC
		{return val;}
	~Tmp(){									// FUNC
		g += val;
	}

};

template <class T> T& templFunc ( T& a) 
{ return a; }

template <>
int& templFunc<int> ( int& a) 
{ return a; }

int main (){								// FUNC

	// variable decl
	int a;											// VAR

	// usage of var
	a = a+1;

	Obj obj;										// VAR

	// call of function
	// and use of global
	g = func();

	Tmp<int> T1 (1);								// VAR
	Tmp<float> T2 (2.5f);							// VAR

	T1.getVal();
	T2.getVal();

	templFunc(a);							// FUNC    VAR
	float b;										// VAR
	templFunc(b);							// FUNC    VAR

}

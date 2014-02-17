
// type decl					
class Obj{													// TYPE
	// not a var
	int val;
	public:
		// method decl
		int method(){return 0; }			// FUNC I
		Obj(){}								// FUNC	I
		~Obj(){}							// FUNC I
							// + Synthetised copy ctor I
};

// func decl								
int func() {								// FUNC  I
	return 1;
}
// global var
int g;												// VAR

// template thing
template <typename T>										// TYPE  
struct Tmp{
	T val;
	Tmp (T init) 							// FUNC  II  VAR
		: val(init){}					
	Tmp (const Tmp<T>& o) 					// FUNC  II VAR
		: val(o.val){}
	Tmp () {val =0;}						// FUNC II

	T getVal()								// FUNC II
		{return val;}
	~Tmp(){									// FUNC II
		g += val;
	}

};

template <class T> T& templFunc ( T& a) 	// FUNC	I
{ return a; }

template <>
int& templFunc<int> ( int& a) 				// FUNC	I
{ return a; }

int main (){								// FUNC I

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

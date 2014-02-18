


#include <vector>
#include <set>
#include <algorithm>




struct TheType{
	int a;
	bool operator< (const TheType& t){
		return this->a < t.a;
	}
};


////////////////////////////////////////////////////////////////////////////////////
//  looking for a problem on the constructor of this little object here

template< class T_PTR> 
struct func_ltDerefPtr {

	bool operator() (T_PTR a, T_PTR b) {
		return *a < *b;
	}

};


///////////////////////////////////////////////////////////////////////////////////
//			use 1, the FSM (seems to work)

//template <class T_DERIVED, class R>
//struct WhatThing {
//
//	typedef R(T_DERIVED::*mfunPtr) ();
//	typedef std::vector<mfunPtr> funcList;
//	std::vector<funcList> funcs;
//
//	typedef std::vector<unsigned> uv;
//	std::vector<uv> matrix;
//
//	WhatThing(int X, int Y){
//		for (unsigned i=0; i< X; i++){
//			funcs.push_back(funcList(Y, &T_DERIVED::f));
//			matrix.push_back(uv(Y, 0));
//		}
//	}
//
//
//	void doIt(){
//		
//	}
//
//};
//
//
//
//template <class T_HANDLE>
//struct DoTheThing : public WhatThing<DoTheThing<T_HANDLE>,  void>{
//
//	DoTheThing  ()
//	:WhatThing<DoTheThing<T_HANDLE>,  void>(10,10){
//
//
//	}
//
//	void soDoIt(std::vector<T_HANDLE*>& handles){
//
//		std::sort (handles.begin(), handles.end(), func_ltDerefPtr<T_HANDLE*>());
//	}
//
//
//	void f(){
//	}
//};

///////////////////////////////////////////////////////////////////////////////////
//			use 2, the  Border List

class TheClass{

public:
	void f(){
		it i = theSet.begin();

	}


	protected:



		typedef std::set< TheType*, func_ltDerefPtr<TheType> > t_set;
		typedef t_set::iterator it;
		t_set  theSet;

};


///////////////////////////////////////////////////////////////////////////////////
//

int main (){

	{  // case 1

//		std::vector<TheType*> v;
//		DoTheThing<TheType> thisThing;
//		thisThing.soDoIt(v);
	}

	{
		TheClass c;
		c.f();

	}

	return 0;
}

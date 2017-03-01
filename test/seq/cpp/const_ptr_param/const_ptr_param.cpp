
struct A{
	int val;
};


struct Obj{
	int operator() ( A* const a, A*const b) const{
		return a->val - b->val;
	}
//	int operator() ( A*  a, A* b) const{
//		return a->val - b->val;
//	}
};



int main (){

	A a= {1};
	A b= {1};


	A* x = &a;
	A* y = &b;
	

	int dkfsal = x->val + y->val;


	Obj c;

	A*const z = &a;
	
	return  c(&a,&b);
}

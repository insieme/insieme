

#include <iostream>

// this test checks for recursion inside of 
// templated members and relations between templates


//////////////////////////////////////////////////////
//   dependence between templates
template <class A, class B>
A assignValue(B b){
	return A(b);
}


template <class A>
class Obj{

	const A& c_ref;
	A& ref;
	A  value;


	public:

		Obj (A& a) : 
		 ref(a), c_ref(a), value(a)
		{}

		Obj (const Obj<A>& o):
			ref(o.ref), c_ref(o.c_ref), value(o.value)
		{}

		template <class B>
		Obj createCopy(B b){
			return assignValue<Obj<A>, B> (b);
		}

		A getValue(){
			return value;
		}
		A getRefValue(){
			return ref;
		}
};

///////////////////////////////////////////////////////////
//     recursive templated functions call


int main (){
	{
		int value =4;
		Obj<int> a(value);
		Obj<int> b(a);
		value++;
		std::cout << value << ":" << b.getRefValue() << ":" << b.getRefValue() << ":" << b.getValue() << std::endl;

	}
	{
		float value =4;
		Obj<float> a(value);
		Obj<float> b(a);
		value++;
		std::cout << value << ":" << b.getRefValue() << ":" << b.getRefValue() << ":" << b.getValue() << std::endl;
	}
	{
		int  value = 4;
		int* ptr = &value;
		Obj<int *> a(ptr);
		Obj<int *> b(a);
		value++;
		std::cout << value << ":" << *b.getRefValue() << ":" << *b.getRefValue() << ":" << *b.getValue() << std::endl;
	}
}

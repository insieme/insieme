class B {
	int intB;
};

template<class X>
class TemplatedClass {
public:
	X templatedMember;
};

//default template arg
template < class Y = B >
class A {
public:
	int intA;
	Y b;
};

template<class T> T min(T a, T b) { return a<b ? a:b; }

//templated function
template<class T> T& f1(T& a) { return a; }
template<class T> int f2(A<T>& a) { return a.intA; }
//returns reference
template<class T> T& f3(A<T>& a) { return a.b; }

//returns object
template<class T> T f4(A<T>& a) { return a.b; }

//specialized template-function
A<double>& f1(A<double> & a) { return a; }

int main() {
	A<> a;			//default template arg
	A<int> aInt;
	A<int>& rAInt = aInt;
	A<double> aDouble;
	A<B> aB;

	f1<A<int> > (aInt);
	f1(aDouble);

	//only A<templateType> objects for f2
	f2(a);
	f2<int> (aInt);
	f2<double> (aDouble);
	f2<B> (aB);

	f3(a);					//returns B
	f3<int>(aInt);			//returns int
	f3<double>(aDouble);	//returns double

	//f4(a);					//returns B
	f4<int>(aInt);			//returns int
	f4<double>(aDouble);	//returns double

	TemplatedClass<B> tCB;
	TemplatedClass<A<B> > tCAB;

	return 0;
}

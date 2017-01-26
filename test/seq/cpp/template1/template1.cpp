#include <iostream>




template <class T>
class myTempl{
	private :
		T& elem;

	public:

		myTempl(T& e)
			: elem (e) {}

		T returnCopy(){
			return elem;
		}

		void plusOne(){
			elem +=1;
		}

		myTempl<T>& operator= (const myTempl<T>& o){
			elem = o.elem;
			return *this;
		}
};



int main (){

	{ //int
		int a = 0;
		myTempl<int> obj(a);
		obj.plusOne();
		std::cout << " int a: " << a << std::endl;
	}
	{ // float
		float a = 0.0f;
		myTempl<float> obj(a);
		obj.plusOne();
		std::cout << " float a: " << a << std::endl;
	}
	{ // double
		double a = 0.0;
		myTempl<double> obj(a);
		obj.plusOne();
		std::cout << " double a: " << a << std::endl;
	}
	return 0;
}

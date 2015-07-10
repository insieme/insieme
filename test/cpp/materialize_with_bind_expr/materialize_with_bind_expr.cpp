#include <iostream>

//base class non user defined ctor
class Base {
public:
	Base() {};
	~Base() {};
	int i;
	int j;
	const Base operator() (int k, int x) { this->i=k; this->j=-1; return *this; }
	Base * test() {
		std::cout << "Base test call\n";
		Base * d = new Base((*this)(0,0));
		//after operator call this->i should be set
		std::cout << this->i << std::endl;
		std::cout << this->j << std::endl;
		d->j = 2;
		//d->j should be set to 2
		std::cout << d->j << std::endl;
		return d;
	}

};

//base class non user defined ctor
class BaseUD {
public:
	BaseUD() { std::cout << "ctor call\n"; };
	~BaseUD() { };
	int i;
	int j;
	const BaseUD operator() (int k, int x) { this->i=k; this->j=-1; return *this; }
	BaseUD * test() {
		std::cout << "BaseUD test call\n";
		BaseUD * d = new BaseUD((*this)(0,0));
		//after operator call this->i should be set
		std::cout << this->i << std::endl;
		std::cout << this->j << std::endl;
		d->j = 2;
		//d->j should be set to 2
		std::cout << d->j << std::endl;
		return d;
	}

};



int main() {
	Base l;
	Base * a = l.test();
	BaseUD m;
	BaseUD * b = m.test();
	std::cout << a->j << std::endl;
	std::cout << b->j << std::endl;
	return 0;
}

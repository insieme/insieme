#include <cassert>
#include <iostream>

class Base {
public:
	virtual void dummy() { }
	Base() {  }
	//~Base() {  }
};

class Derived: public Base {
public:
	Derived() {  }
	int a;
	//~Derived() {  }
};

int f() { return 42; }

void g(unsigned int d) {
	std::cout << "val d=" << d << "\n";
}

void y(unsigned int& d) {
	std::cout << "ref d=" << d << "\n";
}

int main()
{
    int i = 7;
 
    // pointer to integer and back
    int *v1 = &i;
    //std::cout << "The value of &i is 0x" << std::hex << v1 << '\n';
    int* p1 = reinterpret_cast<int*>(v1);
    assert(p1 == &i);
    std::cout << *p1 << " == " << i << std::endl; 
 
    // type aliasing through pointer
    char* p2 = reinterpret_cast<char*>(&i);
    if(p2[0] == '\x7')
        std::cout << "This system is little-endian\n";
    else
        std::cout << "This system is big-endian\n";
 
    // type aliasing through reference (builtin)
    reinterpret_cast<unsigned int&>(i) = 42;
    std::cout << i << '\n';

    // type aliasing through reference (object)
    Derived d;
    d.a=5;
    Derived& dd = d;
    std::cout << d.a << " " << dd.a << std::endl;
    reinterpret_cast<Base&>(d) = dd;
    std::cout << d.a << " " << dd.a << std::endl;

    // function calls
    int a = 2;
    unsigned int aa=3;
    unsigned int &b = aa;
    y(reinterpret_cast<unsigned int&>(a));
    y(reinterpret_cast<unsigned int&>(aa));
    y(b);
    g(reinterpret_cast<unsigned int&>(a));
    g(reinterpret_cast<unsigned int&>(aa));
    g(b);
}

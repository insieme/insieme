#include <string>
#include <iostream>
 
struct A {
    std::string s;
    A() : s("test") {}
    A(const A& o) : s(o.s) { std::cout << "move failed!\n";}
    A(A&& o) : s(std::move(o.s)) {}
};

A f(A a) {
    return a;
}
 
struct B : A {
    std::string s2; 
    int n;
    // implicit move contructor B::(B&&)
    // calls A's move constructor
    // calls s2's move constructor
    // and makes a bitwise copy of n
};
                                    
struct C : B {
    ~C() {}; // destructor prevents implicit move ctor C::(C&&)
};
                      
struct D : B {
    D() {}
    ~D() {}; // destructor would prevent implicit move ctor D::(D&&)
//    D(D&&); // force a move ctor anyway
};

//use rval ref as argument
int add(int&& a, int&& b) {
    int x = a+b;
    return x;
}

int add(int a, int b) {
    return a+b;
}

int main() {
    //explicit
    {
        int&& a = 5;
        int&& b = 10;
        int ra = add(a, b);
        std::cout << ra << std::endl;
    }
    //mixed
    {
        int&& a = 5;
        int ra = add(a, 5);
        std::cout << ra << std::endl;
    }
    //other things
    {
        int a = 5;
        int ra = add(a, 5);
        std::cout << ra << std::endl;
    }

    //move ctor tests
    {
        std::cout << "######\nTrying to move A\n";
        std::cout << "test move from rvalue...\n";
        A a1 = f(A()); // move-construct from rvalue temporary
        std::cout << "test move from xvalue...\n";
        A a2 = std::move(a1); // move-construct from xvalue
             
        std::cout << "Trying to move B\n";
        B b1;
        std::cout << "Before move, b1.s = \"" << b1.s << "\"\n";
        B b2 = std::move(b1); // calls implicit move ctor
        std::cout << "After move, b1.s = \"" << b1.s << "\"\n";
                                 
        std::cout << "Trying to move C\n";
        C c1;
        C c2 = std::move(c1); // calls the copy constructor
                                             
        std::cout << "Trying to move D\n";
        D d1;
        D d2 = std::move(d1);
    }
    return 0;
}

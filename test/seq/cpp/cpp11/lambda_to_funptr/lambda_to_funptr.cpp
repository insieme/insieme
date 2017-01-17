#include <iostream>

int b() {
    return 5;
}

typedef int (*fptr)(); 
typedef int (*fptr2)(int); 

int d(fptr&& x) {
    return x();
}

int e(fptr2&& x, int arg) {
    return x(arg);
}

int main() {
    int a = d([]() { int k=5+8; return k; });
    int b = e([](int c) { int k=c+8; return k; }, 10);
    std::cout << "a: " << a << "\n";
    std::cout << "b: " << b << "\n";
    return 0;
}

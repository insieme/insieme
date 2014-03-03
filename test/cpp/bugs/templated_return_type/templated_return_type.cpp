#include <iostream>


class A {
    public:
        template<typename T>
        T getSizeOf() {
            std::cout << "Size of given type: " << sizeof(T) << std::endl;
            return T();
        }
};

int main() {
    A a;
    int i = a.getSizeOf<int>();
    double d = a.getSizeOf<double>();
    A b = a.getSizeOf<A>();
    return 0;
}

#include <functional>

namespace ns {
template <typename T>
class A{
public:
    T t;
};

void benchmark(A<std::function<void()> >& initializer) {
    initializer.t();
    return;
}
} 

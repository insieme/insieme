#include "function_type_template_arg.h"
#include <functional>
#include <iostream>

int main() {
    ns::A<std::function<void()> > a;
    a.t = []() { std::cout << "lambda call\n"; };
    ns::benchmark(a);
    return 0;
}

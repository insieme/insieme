#include <iostream>
#include <vector>
#include <string>

int main() {
    //crazy declarations
    //builtin types
    int i=i;
    i=1;
    std::cout << "i: " << i << std::endl;
    //class
    std::string s = s;
    s = "test";
    std::cout << "s: " << s << std::endl;
    //templated types
    std::vector<int> v=v;
    v.clear();
    std::cout << "v.size(): " << v.size() << std::endl;
    return 0;
}

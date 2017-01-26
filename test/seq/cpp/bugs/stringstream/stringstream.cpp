#include <sstream>
#include <iostream>

int main() {
    std::stringstream ss;
    ss << "blaa";
    std::cout << ss.str() << std::endl;
    return 0;
}

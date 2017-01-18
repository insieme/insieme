#include <iostream>
#include <vector>
#include <algorithm>
#include "adv_op_overl.h" 


int main() {
    X::A a;
    X::A b;	
    X::A c = (a+=a);
	a+=b;
	a-=b;
	a/=b;
    a-b;
    a+b;
    a*b;
    a/b;
    std::vector<X::A> vec;
    vec.push_back(a);
    vec.push_back(b);
    vec.push_back(c);
    std::cout << "vec_size: " << vec.size() << std::endl;
    // using default comparison:
    bool z = (a==b);
    std::vector<X::A>::iterator it;
    it = std::unique(vec.begin(), vec.end());
    std::cout << "vec_size: " << vec.size() << std::endl;
	return 0;
}

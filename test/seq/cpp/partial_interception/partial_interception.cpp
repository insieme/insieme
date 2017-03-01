#include <string>
#include <memory>

template<typename T>
void takeT(T t) {}

struct C {		
	template<typename T>
	void takeT(T t) {}
};	
	
int main() {
	std::string a;
	a == a;
	
	std::basic_string<char, struct std::char_traits<char >, std::allocator<char > > var_12;
	std::operator==(var_12, var_12);
	
	takeT(1);
	takeT<int>(1);

	C c;
	c.takeT(1);
	c.takeT<int>(1);
	
	auto b = std::make_shared<int>(5ll);
}

#include <unistd.h>
#include <iostream>

enum Kind { Linear=0, Magnet=1 };
struct Source;
typedef struct Source Source;

struct Source {
    Kind type;
};

enum {ZERO, ONE, TWO} var1;

enum Clash { CLASHX=10};

namespace ns {
	enum ScopedEnum { X=0, Y };
	ScopedEnum sc = X;
	Kind k = Linear;

	enum NSClash { CLASHX=100};
}

enum Shadowing { SHADOW=10, LIGHT=100};
int f() {
	enum Shadowing { SHADOW=100, LIGHT=10};
	Shadowing s = SHADOW;
	return s;
}
int main() {
	//enum used in struct
	Source s1;
	s1.type = Magnet;
	std::cout << s1.type << std::endl;
	if(s1.type == Magnet) {
		s1.type = Linear;
	}
	std::cout << s1.type << std::endl;
	
	//extern enum
	int x = sysconf(_SC_NPROCESSORS_CONF);
	std::cout << x << std::endl;
	
	//normal anon. enum
	Kind k1 = Linear;
	var1 = ONE;
	std::cout << k1 << std::endl;
	std::cout << var1 << std::endl;	

	//scoped in namespace
	std::cout << ns::X << std::endl;	
	
	Kind Y;
	ns::ScopedEnum y = ns::Y; 
	std::cout << y << std::endl;	
	std::cout << ns::k << std::endl;	
	std::cout << ns::sc << std::endl;	

	//enum clash
	Clash clash = CLASHX;
	ns::NSClash nsclash = ns::CLASHX;

	//enum shadowing
	int shadow = SHADOW;
	std::cout << shadow << std::endl;
	shadow = f();
	std::cout << shadow << std::endl;

	return 0;
}

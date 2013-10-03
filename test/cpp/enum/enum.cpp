#include <unistd.h>
#include <iostream>

enum Kind { Linear=0, Magnet=1 };
struct Source;
typedef struct Source Source;

struct Source {
    Kind type;
};

enum {ZERO, ONE, TWO} var1;

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
	return 0;
}

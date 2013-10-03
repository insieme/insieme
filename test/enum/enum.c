#include <unistd.h>
#include <stdio.h>

enum Kind { Linear=0, Magnet=1 };
typedef enum Kind Kind;

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
	printf("%i\n", s1.type);
	if(s1.type == Magnet) {
		s1.type = Linear;
	}
    printf("%i\n", s1.type);
	
	//extern enum
	int x = sysconf(_SC_NPROCESSORS_CONF);
	printf("%i\n", x);
	
	//normal anon. enum
	Kind k1 = Linear;
	var1 = ONE;
	printf("%i\n", k1);
	printf("%i\n", var1);	
	return 0;
}

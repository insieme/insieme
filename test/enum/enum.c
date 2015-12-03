#include <unistd.h>
#include <stdio.h>

enum Kind { Linear=0, Magnet=1 };
typedef enum Kind Kind;

struct Source;
typedef struct Source Source;

struct Source {
    Kind type;
};
struct EnumStruct;
typedef struct EnumStruct EnumStruct;

enum ThingWithE { E=1 }; 
void f() {
	//Shadows the outside enum
	enum ThingWithE { E=10 };
	enum ThingWithE e = E;
	printf("f %i\n",e);
}

struct EnumStruct {
	enum ThingWithE e;
};

// two anonymous enums to test names aliasing
enum {ZERO, ONE, TWO} var1;
enum {GREEN, BLUE, RED} var2;

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
	printf("%i\n", k1);

	// globals
	var1 = ONE;
	printf("%i\n", var1);	

	var2 = GREEN;
	printf("%i\n", var2);	

	//enum in struct
	EnumStruct es;
	es.e = E;
	printf("%i\n", es.e);
	
	//enum shadowing
	enum ThingWithE e = E;
	printf("%i\n", e);
	f();

    //enum casts
    Kind k2;
    k2 = Linear;
    if(k2) {
        printf("k2 is true\n");
    } else {
        printf("k2 is false\n");
    }
    
    k2 = Magnet;
    if(k2) {
        printf("k2 is true\n");
    } else {
        printf("k2 is false\n");
    }
    
	return 0;
}

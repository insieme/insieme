

//typedef int bool;
#define bool int

#define true 1
#define false 0

extern int printf(const char *, ...);

bool even(unsigned x);
bool odd(unsigned x);

bool even(unsigned x) {
	return (x==0)?true:odd(x-1);
}

bool odd(unsigned x) {
	return (x==0)?false:even(x-1);
}

int main(int argc, char* argv[]) {
	int x = 10;
	printf("x=%d\n", x);
	printf("even(x)=%s\n", (even(x))?"true":"false");
	printf("odd(x)=%s\n", (odd(x))?"true":"false");
	return 0;
}

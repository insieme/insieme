#include <stdio.h>

int f1(int);
int g1(int);

int f2(int);
int g2(int);

int f2(int v) {
	if ( v < 10 )
		return 10 + g2(v);
	return v;
}

int g2(int v) {
	if (v > 10 )
		return 0;
	return f2(v+1);
}

int f1(int v) {
	if ( v > 0 )
		return g1(v-1);
	return -1;
}

int g1(int v) {
	if ( v > 0 ) {
		int a = f1(v-1);
		return a + f2(v+1);
	}
	return 10;
}

int main(int argc, char* argv[]) {

	printf("%d\n", f1(3));


}

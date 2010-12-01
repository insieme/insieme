
#include <stdio.h>

int N = 10;

int main(int argc, char* argv[]) {
	
	int a = 10;
	
	for(int idx = 0; idx < a; idx++) {
		printf("idx=%d", idx);
	}
	
	for(int idx=a; idx>=0; --idx) {
		printf("idx=%d", idx);
	}
	
	for(int idx=N; idx>0; idx--) {
		printf("idx=%d", idx);
	}
	
	for(int idx=a; N>0; N--) {
		printf("idx=%d", a);
	}
	
	for(a=1;a<15;a+=3) {
		printf("a=%d\n", a);
	}
	
	printf("a=%d\n", a);
	
}
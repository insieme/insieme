
#include <stdio.h>

int main(int argc, char* argv[]) {
	
	int a = 10;
	
	for(int idx = 0; idx < a; idx++) {
		printf("idx=%d", idx);
	}
	
	for(int idx=a; idx>=0; --idx) {
		printf("idx=%d", idx);
	}
	
}
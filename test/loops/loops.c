
#include <stdio.h>

int N = 10;
int i;

int func(int k) {

	for(k; k<10; k++) {
		printf("k=%d\n", k);
	}

	for(k=10; k>=5; k--) {
		printf("k=%d\n", k);
	}
	printf("%d\n", k);

}

int i;
void func2() {
	for(i=10; i>=0; i--) {
		printf("k=%d\n", i);
	}
}

int main(int argc, char* argv[]) {

	int a = 10;
	{
	for(int idx = 0; idx < a; idx++) {
		printf("idx=%d\n", idx);
	}
	for(int idx = 0; idx < a; idx++) {
		idx++;
		printf("idx=%d\n", idx);
	}
	for(int idx=a; idx>=0; --idx) {
		printf("idx=%d\n", idx);
	}

	for(int idx=N; idx>0; idx--) {
		printf("idx=%d\n", idx);
	}

	for(int idx=a; N>0; N--) {
		printf("idx=%d\n", a);
	}

	for(a=15;a>=0;a-=3) {
		printf("a=%d\n", a);
	}

	for(a=1;a<15;a+=3) {
		printf("a=%d\n", a);
	}
	}

	// example of loop with a complex condition expression
	// in this case the loop is rewritten as a while loop
	for(a=1;a < 10 && a > 0; a+=2) {
		printf("a=%d\n", a);
	}

	// example of for loop with missing increment expression
	for(a=0; a!=0; ) {
		printf("a=%d\n", a);
	}

	printf("a=%d\n", a);

	func(a);
	func2();

}

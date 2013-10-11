
#include <stdio.h>

int N = 10;
int i;

int func(int k) {

	for(k; k<10; k++) {
		printf("1: k=%d\n", k);
	}

	for(k=10; k>=5; k--) {
		printf("2: k=%d\n", k);
	}
	printf("%d\n", k);

}

void func2() {
	for(i=10; i>=0; i--) {
		printf("3: k=%d\n", i);
	}
}


void func3() {
	int x;

	for(x=1; x<5; x++)
	{
		printf("4: x = %d\n", x);

		if( x == 2)
		{
			x++;
		}
		else
			continue;
	}
}

void func4() {
	int x;

	for(x=0; x<2; x++)
	{
		int i = 0;

		printf("5: x = %d\n", x);

		do {
			printf("6: i = %d\n", i);

			i++;
			if(i==1)
				continue;
		} while(i<2);
	}
}


int main(int argc, char* argv[]) {

	int a = 10;
	{
	for(int idx = 0; idx < a; idx++) {
		printf("7: idx=%d\n", idx);
	}
	for(int idx = 0; idx < a; idx++) {
		idx++;
		printf("8: idx=%d\n", idx);
	}
	for(int idx=a; idx>=0; --idx) {
		printf("9: idx=%d\n", idx);
	}

	for(int idx=N; idx>0; idx--) {
		printf("10: idx=%d\n", idx);
	}

	for(int idx=a; N>0; N--) {
		printf("11: idx=%d\n", a);
	}

	for(a=15;a>=0;a-=3) {
		printf("12: a=%d\n", a);
	}

	for(a=1;a<15;a+=3) {
		printf("13: a=%d\n", a);
	}
	}

	// example of loop with a complex condition expression
	// in this case the loop is rewritten as a while loop
	for(a=1;a < 10 && a > 0; a+=2) {
		printf("14: a=%d\n", a);
	}

	// example of for loop with missing increment expression
	for(a=0; a!=0; ) {
		printf("15: a=%d\n", a);
	}

	printf("16: a=%d\n", a);

	func(a);
	func2();
	func3();
	func4();
}


#include <stdio.h>

int N = 10;
int i;

void func(int k) {

	for(k; k<10; k++) {
		printf("1a: k=%d\n", k);
	}
	printf("%d\n", k);

	for(k; k<=10; k++) {
		printf("1b: k=%d\n", k);
	}
	printf("%d\n", k);

	for(k=10; k>5; k--) {
		printf("2a: k=%d\n", k);
	}
	printf("%d\n", k);

	for(k=10; k>=5; k--) {
		printf("2b: k=%d\n", k);
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

void func5(int s) {
	int x = s;

	for(int i = x - 1; i >= 0; i--) {
		printf("17: i = %d\n", i);
	}

	int y = 20;

	for(int i = y - x; i <= y+x; i++) {
		printf("18: i = %d\n", i);
	}
}

void func6() {
	for(unsigned i = 20; i > 10; --i) {
		printf("19: i = %d\n", i);
	}
}

void func7() {
   int i = 14;
 
   for (; i >= 3; i -= 4) {
           printf("%d: %d\n",__LINE__, i);
   }
   printf("%d: %d\n",__LINE__, i);

   for (; i >= 0; i--) {
           printf("%d: %d\n", __LINE__, i);
   }
   printf("%d: %d\n",__LINE__, i);

   i = 11;
   for (; i >= 3; i -= 4) {
           printf("%d: %d\n", __LINE__, i);
   }
   printf("%d: %d\n",__LINE__, i);
}

void func8() {
    int n=0;
    int x=1;
    //loop should be converted to while
    for(int i=1; i<10; i+=i) {
        n+=i;
    }
    printf("21: %i\n", n);
    n=1;
    //loop should be converted to while
    for(int i=1; i<10; i+=i) {
        n+=n;
    }
    printf("22: %i\n", n);
    n=0;
    //loop should not be converted to while
    for(int i=1; i<10; i+=x) {
        n+=i;
    }
    printf("23: %i\n", n);
    n=1;
    //loop should not be converted to while
    for(int i=1; i<10; i+=x) {
        n+=n;
    }
    printf("24: %i\n", n);
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

    {
        int end = 20;
        int count;

        for(a=0; a < end; a++) {
                if(a > 10)
                        break;
        }

        count = a;

        printf("17: count = %d\n", count);

        for(a=end; a > 0 ; a--) {
                if(a < 10)
                        break;
        }
        printf("18: a = %d\n", a);
    }

	func(a);
	func2();
	func3();
	func4();
	func5(a);
	func6();
    func7();
    func8();
}

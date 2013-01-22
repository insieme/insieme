// this test is written in an insane way on purpose

#define bool int

#define true 1
#define false 0

extern int printf(char *, ...);

bool even(int x);
bool odd(int x);

bool even(int x) {
	if(x==0) return true;
	if(x==-1) return false;
	bool a = false, b = false;
	#pragma omp task shared(a)
	a = odd(x-1);
	#pragma omp task shared(b)
	b = even(x-2);
	#pragma omp taskwait
	return a && b;
}

bool odd(int x) {
	if(x==0) return false;
	if(x==-1) return true;
	bool c = false, d = false;
	#pragma omp task shared(c)
	c = even(x-1);
	#pragma omp task shared(d)
	d = odd(x-2);
	#pragma omp taskwait
	return c && d;
}

int main(int argc, char* argv[]) {
	int x = 10;
	#pragma omp parallel
	{
		#pragma omp single
		{
			printf("x=%d\n", x);
			printf("even(x)=%s\n", (even(x))?"true":"false");
			printf("odd(x)=%s\n", (odd(x))?"true":"false");
		}
	}
	return 0;
}

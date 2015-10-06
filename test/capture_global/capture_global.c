

//typedef int bool;
#define bool int

#define true 1
#define false 0

extern int printf(const char *, ...);

int count = 0;
int a[50];


int inc(int step) {
	a[step]++;
	count += step;
	return count;
}

int dec(int step) {
	a[step]--;
	count -= step;
	return count;
}

int reset() {
	static int reset_count=1;
	count = 0;
	reset_count += 1;
	return count;
}

int main(int argc, char* argv[]) {
	for (int i=0; i<10; i++) {
		printf("count=%2d\n", inc(2));
		printf("count=%2d\n", dec(1));
		if (i==5) {
			reset();
		}
	}
	return 0;
}


#define N 10

extern int printf(char*, ...);

int a[N];
//char(* b)[N];

void init() {
	for (int i=0; i<N; i++) {
		a[i] = i;
	}
}

int sum() {
	int res = 0;
	for (int i=0; i<N; i++) {
		res += a[i];
	}
	return res;
}

int main(int argc, char* argv[]) {
//	b = argv;
	init();
	printf("sum=%d\n", sum());
}

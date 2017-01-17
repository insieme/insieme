
#include <stdio.h>

int N = 10;
int i;

struct data {
	int x,y;
};

int f1(int x) {
	return x;
}

int f2(int* x) {
	return *x;
}

int f3(int* x) {
	return i++;
}

int f4(struct data data) {
	return data.x;
}

int f5(struct data* data) {
	return data->x;
}

int main(int argc, char* argv[]) {
	int a = 10;
	struct data data = (struct data) {1,1};
	f1(a);
	f2(&a);
	f3(&a);
	f4(data);
	f5(&data);
	printf("a=%d\n", a);
	return 0;
}

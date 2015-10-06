
int printf(const char* arg, ...);

int f(int* a) {
	return (*a)++;
}

int main(int argc, char* argv[]) {
	int a=0;
	argc = 0;

	f(&a); // solve this problem in the backend
	f(&argc);

	printf("%d\n", argc);

	argc = a;

	printf("%d\n", argc);
}

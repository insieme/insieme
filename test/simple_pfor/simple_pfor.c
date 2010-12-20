extern void printf(const char*, ...);

int main() {

	int i;
	int a[10];

	#pragma omp parallel
	{
		#pragma omp for
		for (i=0; i<10; i++) {
			a[i] = i;
		}
	}

	printf("After Loop: %d\n", i);

	for (i=0; i<10; i++) {
		printf("a[%d]=%d\n", i, a[i]);
	}
}

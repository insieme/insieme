extern void printf(const char*, ...);

int main() {

	#pragma omp parallel
	{
		printf("hello world\n");
	}
}

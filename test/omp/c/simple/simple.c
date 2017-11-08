extern int printf(const char *, ...);

int main() {

	int x = 0;

	#pragma omp parallel
	{
		x = 1; // i know, it isn't save, but it works ...		
	}

	if(x>0) {
		printf("hello world\n");
	}
}

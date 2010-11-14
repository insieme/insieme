
extern int printf(char*, ...);

int main(int argc, char* argv[]) {
	#pragma omp parallel for default(none)
	{
	printf("Hallo Insieme, \n\t\"the number %d in compilers!\"", 1);
	}
	return 0;
}

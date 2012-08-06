
int main(int argc, char **argv) {
	
	if(argc == 1) return 0;
	
	#pragma omp task
	return 1;
}
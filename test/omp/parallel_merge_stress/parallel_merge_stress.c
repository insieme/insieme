
int main() {

	int x = 0;

	for (int k=0; k<10000; ++k) {
		#pragma omp parallel
		{
			#pragma omp master
			x++;	
		}
	}

	if(x!=10000) {
		printf("FAIL\n");
	}
}

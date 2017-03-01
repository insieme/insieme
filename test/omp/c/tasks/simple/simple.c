int N = 100;

int main() {
	
	int i = 0;
	
	#pragma omp parallel
	{
		#pragma omp single 
		{
			for(int i=0; i<N; ++i) {
				#pragma omp task
				{
					#pragma omp atomic
					i++;
				}
			}
		}
	}
	
	return i != N;
}

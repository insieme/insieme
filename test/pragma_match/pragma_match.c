
int main(int argc, char **argv) {
	
	if(argc == 1) return 0;
		
	if(argc ==2 ) 
		#pragma omp task
		return 2;
	else 
		#pragma omp barrier	


	#pragma omp task
	return 1;
}

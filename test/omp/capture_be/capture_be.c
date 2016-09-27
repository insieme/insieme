
void testParam(int* a) {
	int magic;

	#pragma omp parallel
	#pragma omp single
	#pragma omp task untied
	{
		*a = 0;
	}
}

int main(int argc, char** argv) {
	int amem;
	int* a = &amem;
	
	testParam(a);
	
	return *a;
 }

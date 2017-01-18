// this test case is used by the meta info integration test!

int main(int argc, char** argv) {

	int i = 0;

	#pragma omp parallel
	{
		i++;
	}

	return (i > 10000000) ? 1 : 0;
}


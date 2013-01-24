
int main(int argc, char** argv) {

	int iterations = 0;
	
	for(unsigned char i = 8; i>0; --i) {
		iterations++;
	}
	
	printf("Iterations: %d\n", iterations);
	
	return 0;
}
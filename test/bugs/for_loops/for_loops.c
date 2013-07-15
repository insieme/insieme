
int main(int argc, char** argv) {

	int iterations = 0;
	int j, cnt = 0;
        int vec[15];

	for(unsigned char i = 8; i>0; --i) {
		iterations++;
	}
	
	printf("Iterations: %d\n", iterations);
	
        vec[10] = 15;
 
        for(j=0; j<vec[10]; j++)
        {
                cnt ++;
                if(j == 10)
                        vec[10] ++;
        }
 
	printf("Iterations: %d\n" cnt);

	return 0;
}

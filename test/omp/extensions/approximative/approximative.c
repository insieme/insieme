
#include <stdlib.h> 

float calc(float *in) {
	return ( 1*in[-3] + 3*in[-2] + 4*in[-1] + 4*in[0] + 4*in[1] + 3*in[2] + 1*in[3] ) / 20.0f;
}

float approx_calc(float *in) {
	return in[0];
}


int main(int argc, char** argv) {

	int N = 10000;
	
	float* in = (float*)calloc(N, sizeof(float));
	float* out = (float*)calloc(N, sizeof(float));
	
	for(int i = 3; i<N-3; ++i) {
		#pragma omp task approximate(calc:approx_calc)
		out[i] = calc(in+i);
	}
	#pragma omp taskwait
	return 0;
}
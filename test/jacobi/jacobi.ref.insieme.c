// --- Generated Inspire Code ---
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: fundef_codefragment_clock //
long clock();

// start code fragment :: fundef_codefragment_memset //
void* memset(void*, int, unsigned int);

// start code fragment :: fundef_codefragment_sin //
double sin(double);

// start code fragment :: fundef_codefragment_init_func //
double init_func(int x, int y){
	return (((double)(40)) * sin(((double)(((16 * ((2 * x) - 1)) * y)))));;
}

// start code fragment :: fundef_codefragment_pow //
double pow(double, double);

// start code fragment :: fundef_codefragment_memcpy //
void* memcpy(void*, void*, unsigned int);

// start code fragment :: fundef_codefragment_sqrt //
double sqrt(double);

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	long start_t = 0;
	long end_t = 0;
	double setup_time = 0.0;
	double elapsed_time = 0.0;
	start_t = clock();
	float u[650][650];
	float tmp[650][650];
	float f[650][650];
	float res[650][650];
	memset(u, 0, ((unsigned int)((650 * 650))));
	memset(f, 0, ((unsigned int)((650 * 650))));
	for(int i = 0; i < 650; i += 1) for(int j = 0; j < 650; j += 1) f[((unsigned int)(i))][((unsigned int)(j))] = ((float)(init_func(i, j)));
	double comm_time = ((double)(0));
	double comp_time = ((double)(0));
	double timer = ((double)(0));
	double resv = 0.0;
	double factor = pow((((double)(1)) / ((double)(650))), ((double)(2)));
	end_t = clock();
	setup_time = (((double)((end_t - start_t))) / ((double)(1000000l)));
	start_t = clock();
	for(int it = 0; it < 10; it += 1) {
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) tmp[((unsigned int)(i))][((unsigned int)(j))] = ((float)(((((double)(1)) / ((double)(4))) * (((double)((((u[((unsigned int)((i - 1)))][((unsigned int)(j))] + u[((unsigned int)(i))][((unsigned int)((j + 1)))]) + u[((unsigned int)(i))][((unsigned int)((j - 1)))]) + u[((unsigned int)((i + 1)))][((unsigned int)(j))]))) - (factor * ((double)(f[((unsigned int)(i))][((unsigned int)(j))])))))));
		};
		memcpy(u, tmp, ((unsigned int)((650 * 650))));
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) res[((unsigned int)(i))][((unsigned int)(j))] = (((((f[((unsigned int)(i))][((unsigned int)(j))] - (((float)(4)) * u[((unsigned int)(i))][((unsigned int)(j))])) + u[((unsigned int)((i - 1)))][((unsigned int)(j))]) + u[((unsigned int)((i + 1)))][((unsigned int)(j))]) + u[((unsigned int)(i))][((unsigned int)((j - 1)))]) + u[((unsigned int)(i))][((unsigned int)((j + 1)))]);
		};
		double norm = ((double)(0));
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) norm = (norm + pow(((double)(res[((unsigned int)(i))][((unsigned int)(j))])), ((double)(2))));
		};
		resv = (sqrt(norm) / ((double)((650 - 1))));
	};
	end_t = clock();
	elapsed_time = (((double)((end_t - start_t))) / ((double)(1000000l)));
}

// start code fragment :: unnamed //

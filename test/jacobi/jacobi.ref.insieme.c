// --- Generated Inspire Code ---
#include <stddef.h>
#define bool int
#define true 1
#define false 0
// --- Entry Point ---

// start code fragment :: lambda_struct___insieme_fun_type_2 //
struct __insieme_fun_type_2 { 
    const double(*fun)(*(struct __insieme_fun_type_2));
    const size_t size;
};

// start code fragment :: fundef_codefragment_init_func //
double init_func(int x, int y){
	return (((double)(40)) * __insieme_expr_3(((double)(((16 * ((2 * x) - 1)) * y)))));;
}

// start code fragment :: fundef_codefragment_main //
int main(int argc, char** argv){
	long start_t = 0;
	long end_t = 0;
	double setup_time = 0.0;
	double elapsed_time = 0.0;
	start_t = __insieme_expr_0();
	float u[650][650];
	float tmp[650][650];
	float f[650][650];
	float res[650][650];
	__insieme_expr_1(u, 0, ((unsigned long)((650 * 650))));
	__insieme_expr_1(f, 0, ((unsigned long)((650 * 650))));
	for(int i = 0; i < 650; i += 1) for(int j = 0; j < 650; j += 1) f[((unsigned int)(i))][((unsigned int)(j))] = ((float)(((*(struct __insieme_fun_type_2))())->fun(i, j)));
	double comm_time = ((double)(0));
	double comp_time = ((double)(0));
	double timer = ((double)(0));
	double resv = 0.0;
	double factor = __insieme_expr_4((((double)(1)) / ((double)(650))), ((double)(2)));
	end_t = __insieme_expr_0();
	setup_time = (((double)((end_t - start_t))) / ((double)(1000000l)));
	start_t = __insieme_expr_0();
	for(int it = 0; it < 10; it += 1) {
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) tmp[((unsigned int)(i))][((unsigned int)(j))] = ((float)(((((double)(1)) / ((double)(4))) * (((double)((((u[((unsigned int)((i - 1)))][((unsigned int)(j))] + u[((unsigned int)(i))][((unsigned int)((j + 1)))]) + u[((unsigned int)(i))][((unsigned int)((j - 1)))]) + u[((unsigned int)((i + 1)))][((unsigned int)(j))]))) - (factor * ((double)(f[((unsigned int)(i))][((unsigned int)(j))])))))));
		};
		__insieme_expr_5(u, tmp, ((unsigned long)((650 * 650))));
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) res[((unsigned int)(i))][((unsigned int)(j))] = (((((f[((unsigned int)(i))][((unsigned int)(j))] - (((float)(4)) * u[((unsigned int)(i))][((unsigned int)(j))])) + u[((unsigned int)((i - 1)))][((unsigned int)(j))]) + u[((unsigned int)((i + 1)))][((unsigned int)(j))]) + u[((unsigned int)(i))][((unsigned int)((j - 1)))]) + u[((unsigned int)(i))][((unsigned int)((j + 1)))]);
		};
		double norm = ((double)(0));
		for(int i = 1; i < (650 - 1); i += 1) {
			for(int j = 1; j < (650 - 1); j += 1) norm = (norm + __insieme_expr_4(((double)(res[((unsigned int)(i))][((unsigned int)(j))])), ((double)(2))));
		};
		resv = (__insieme_expr_6(norm) / ((double)((650 - 1))));
	};
	end_t = __insieme_expr_0();
	elapsed_time = (((double)((end_t - start_t))) / ((double)(1000000l)));
}

// start code fragment :: unnamed //

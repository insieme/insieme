#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "lib_icl.h"

#define SIZE 6400000
#define FLOAT float

#define VECTOR_WIDTH 1
#define CHECK_RESULT 1

/* =================================================================================================== */
/*  N validation function. Here are the two reference routines used by validation */
/*  (the second one, bsop_reference, calls the first one, N, in two places). */
/*  These are used at the end of the main routine to verify the values in the returned array. */
/*  This first routine estimates the cumulative distribution function using a quintic polynomial. */
/* =================================================================================================== */

double N(double x)
{
	double A1 = 0.319381530;
	double A2 = -0.356563782;
	double A3 = 1.781477937;
	double A4 = -1.821255978;
	double A5 = 1.330274429;
	double INV_ROOT2PI = 0.39894228;
	double k, n;
	double accum;
	double candidate_answer;
	int flag;
	flag = (x < 0);
	x = (x < 0) ? -x : x;
	k = 1.0 / (1.0 + 0.2316419 * x);
	accum = A4 + A5 * k;
	accum = k * accum + A3;
	accum = k * accum + A2;
	accum = k * accum + A1;
	accum = k * accum;
	n = exp(-0.5 * x * x);
	n *= INV_ROOT2PI;
	candidate_answer = 1.0 - n * accum;
	return (flag ? 1.0 - candidate_answer : candidate_answer);
}

/* =================================================================================================== */
/*  BSOP Reference validation function. This function invokes N in two places. */
/* =================================================================================================== */

double bsop_reference(int cpflag, double S0, double K, double r,
                      double sigma, double T) {
	double d1, d2, c, p, Nd1, Nd2, expval, answer;
	d1 = log(S0 / K) + (r + 0.5 * sigma * sigma) * T;
	d1 /= (sigma * sqrt(T));
	expval = exp(-r * T);
	d2 = d1 - sigma * sqrt(T);
	Nd1 = N(d1);
	Nd2 = N(d2);
	c = S0 * Nd1 - K * expval * Nd2;
	p = K * expval * (1.0 - Nd2) - S0 * (1.0 - Nd1);
	answer = cpflag ? c : p;
	return answer;
}

void validate(FLOAT *S0_fptr, FLOAT *K_fptr, FLOAT *r_fptr,
                                        FLOAT *sigma_fptr, FLOAT *T_fptr, FLOAT *answer_fptr,
                                        int *cpflag_fptr, unsigned long size,
                                        double *maxouterr, int *maxouterrindex)
{
	printf("ArraySize: %ld\n", size);

	*maxouterr = -1.0;
	*maxouterrindex = -1;
	unsigned long i;
	for (i = 0; i < size; i += 1) {
		cl_double a, b, absb, del, abserr, relerr, outerr;
		int *temp_int;
		a = (cl_double) answer_fptr[i];
		temp_int = (int *) &cpflag_fptr[i];
		b = bsop_reference(*temp_int, (cl_double) S0_fptr[i],
						   (cl_double) K_fptr[i], (cl_double) r_fptr[i],
						   (cl_double) sigma_fptr[i], (cl_double) T_fptr[i]);
		del = a - b;
		abserr = del;
		del = (del < 0.0f) ? -del : del;
		absb = (b < 0.0f) ? -b : b;
		relerr = del / absb;
		outerr = (del > relerr) ? relerr : del;
		if (outerr > *maxouterr) {
			*maxouterr = outerr;
			*maxouterrindex = i;
		}
	}
}


int main() {
	icl_buffer *cpflag_buf, *S0_buf, *K_buf, *r_buf, *sigma_buf, *T_buf, *answer_buf;

	/* declare some variables for intializing data */
	int idx;
	int S0Kdex, rdex, sigdex, Tdex;
	FLOAT S0_array[4] = { 42.0, 30.0, 54.0, 66.0 };
	FLOAT K_array[16] = { 	40.0, 36.0, 44.0, 48.0,
				24.0, 28.0, 32.0, 36.0,
				48.0, 52.0, 56.0, 60.0,
				60.0, 64.0, 68.0, 72.0
			    };
	FLOAT r_array[4] = { 0.1, 0.09, 0.11, 0.12 };
	FLOAT sigma_array[4] = { 0.2, 0.15, 0.25, 0.30 };
	FLOAT T_array[4] = { 0.5, 0.25, 0.75, 1.0 };
	idx = 0;

	/* Pointers used to allocate memory and split that memory into input and output arrays */
	/* These pointers point to the data buffers needed for Black Scholes computation */
	int *cpflag;
	FLOAT *S0, *K, *r, *sigma, *T, *answer;

	cpflag = (int*)malloc(SIZE * sizeof(int));
	S0 = (FLOAT*)malloc(SIZE * sizeof(FLOAT));
	K = (FLOAT*)malloc(SIZE * sizeof(FLOAT));
	r = (FLOAT*)malloc(SIZE * sizeof(FLOAT));
	sigma = (FLOAT*)malloc(SIZE * sizeof(FLOAT));
	T = (FLOAT*)malloc(SIZE * sizeof(FLOAT));
	answer = (FLOAT*)malloc(SIZE * sizeof(FLOAT));

	cl_ulong size = SIZE;
	int memsize = (size * sizeof(FLOAT));


	icl_init_devices(ICL_GPU);
	
	if (icl_get_num_devices() != 0) {
		icl_device* dev = icl_get_device(0);
		icl_print_device_short_info(dev);
		
		cpflag_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(unsigned int) * memsize);
		S0_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(FLOAT) * memsize);
		K_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(FLOAT) * memsize);
		r_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(FLOAT) * memsize);
		sigma_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(FLOAT) * memsize);
		T_buf = icl_create_buffer(dev, CL_MEM_READ_ONLY, sizeof(FLOAT) * memsize);
		answer_buf = icl_create_buffer(dev, CL_MEM_READ_WRITE, sizeof(FLOAT) * memsize);

		/* Here we load some values to simulate real-world options parameters.
		* Users who wish to provide live data would replace this clause
		* with their own initialization of the arrays. */
		for (int k = 0; k < size; ++k) {
			int *temp_int;
			Tdex = (idx >> 1) & 0x3;
			sigdex = (idx >> 3) & 0x3;
			rdex = (idx >> 5) & 0x3;
			S0Kdex = (idx >> 7) & 0xf;

			temp_int = (int *) &cpflag[k];
			temp_int[0] = (idx & 1) ? 0xffffffff : 0;
			if (sizeof(FLOAT) == 8) temp_int[1] = (idx & 1) ? 0xffffffff : 0;

			S0[k] = S0_array[S0Kdex >> 2];
			K[k] = K_array[S0Kdex];
			r[k] = r_array[rdex];
			sigma[k] = sigma_array[sigdex];
			T[k] = T_array[Tdex];
			answer[k] = 0.0f;
			idx++;
		}
	
		// write data to ocl buffers
		icl_write_buffer(cpflag_buf, CL_TRUE, SIZE * sizeof(int), cpflag, NULL, NULL);
		icl_write_buffer(S0_buf, CL_TRUE, SIZE * sizeof(float), S0, NULL, NULL);
		icl_write_buffer(K_buf, CL_TRUE, SIZE * sizeof(float), K, NULL, NULL);
		icl_write_buffer(r_buf, CL_TRUE, SIZE * sizeof(float), r, NULL, NULL);
		icl_write_buffer(sigma_buf, CL_TRUE, SIZE * sizeof(float), sigma, NULL, NULL);
		icl_write_buffer(T_buf, CL_TRUE, SIZE * sizeof(float), T, NULL, NULL);
		icl_write_buffer(answer_buf, CL_TRUE, SIZE * sizeof(float), answer, NULL, NULL);

		icl_kernel* kernel = icl_create_kernel(dev, "ocl_blackscholes.cl", "bsop_kernel", "", ICL_SOURCE);

		size_t szLocalWorkSize = 256;

		/* Compute the number of work groups needed to handle the array (only used when kernel is not a Task) */
		size_t num_workgroups = size / (VECTOR_WIDTH * szLocalWorkSize);

		size_t szGlobalWorkSize = num_workgroups * szLocalWorkSize;

		icl_run_kernel(kernel, 1, &szGlobalWorkSize, &szLocalWorkSize, NULL, NULL, 7,
											(size_t)0, (void *)cpflag_buf,
											(size_t)0, (void *)S0_buf,
											(size_t)0, (void *)K_buf,
											(size_t)0, (void *)r_buf,
											(size_t)0, (void *)sigma_buf,
											(size_t)0, (void *)T_buf,
											(size_t)0, (void *)answer_buf);

		icl_read_buffer(answer_buf, CL_TRUE, sizeof(int) * SIZE, &answer[0], NULL, NULL);
		
#if CHECK_RESULT
		double maxouterr = 0;
		int maxouterrindex = 0;
		/* Verify answers using single precision validation function */
		validate(S0, K, r, sigma, T, answer, cpflag, size, &maxouterr, &maxouterrindex);

		/* Is maximum error outside the acceptable range, if so, flag it */
		printf("BlackScholes workload: max error is %e at index %d\n", maxouterr, maxouterrindex);
		if (maxouterr > 0.00002) {
			printf("Max error check: FAIL\n");
			exit (EXIT_FAILURE);
		} else {
			printf("Max error check: OK\n");
		}
#endif

		icl_release_buffers(6, cpflag_buf, S0_buf, K_buf, r_buf, sigma_buf, T_buf, answer_buf);
		icl_release_kernel(kernel);

		free(cpflag);
		free(S0);
		free(K);
		free(r);
		free(sigma);
		free(T);
		free(answer);
	} else
		printf("ERROR: No devices found\n");
	
	icl_release_devices();
	return 0;	
}

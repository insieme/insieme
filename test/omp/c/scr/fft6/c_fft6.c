/* ***********************************************************************
  This program is part of the
	OpenMP Source Code Repository

	http://www.pcg.ull.es/ompscr/
	e-mail: ompscr@etsii.ull.es

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  (LICENSE file) along with this program; if not, write to
  the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
  Boston, MA  02111-1307  USA

	FILE:              c_fft6.c
  VERSION:           1.0
  DATE:              May 2004
  AUTHOR:            F. de Sande
  COMMENTS TO:       sande@csi.ull.es
  DESCRIPTION:       Bailey's 6-step 1D Fast Fourier Transform
	                   Computes the 1D FFT of an input signal (complex array)
  COMMENTS:          The code performs a number (ITERS) of iterations of the
	                   Bailey's 6-step FFT algorithm (following the ideas in the
										 CMU Task parallel suite).
                        1.- Generates an input signal vector (dgen) with size
												    n=n1xn2 stored in row major order
                     	      In this code the size of the input signal
														is NN=NxN (n=NN, n1=n2=N)
                     	  2.- Transpose (tpose) A to have it stored in column

												    major order
                     	  3.- Perform independent FFTs on the rows (cffts)
                     	  4.- Scale each element of the resulting array by a
												    factor of w[n]**(p*q)
                     	  5.- Transpose (tpose) to prepair it for the next step
                     	  6.- Perform independent FFTs on the rows (cffts)
                     	  7.- Transpose the resulting matrix
										 The code requires nested Parallelism.
  REFERENCES:        David H. Bailey
                     FFTs in External or Hierarchical Memory,
		                 The Journal of Supercomputing, vol. 4, no. 1, pg. 23-35. Mar 1990,
                     vol. 4, no. 7, pg. 321. Jul 1961
                     http://www-2.cs.cmu.edu/~fx/tpsuite.html
                     http://en.wikipedia.org/wiki/Cooley-Tukey_FFT_algorithm
  BASIC PRAGMAS:     parallel for
  USAGE:             ./c_fft6.par 1024 10
  INPUT:             The size of the input signal.
	                   The number of iterations to perform
  OUTPUT:            The code tests the correctness of the output signal
	FILE FORMATS:      -
	RESTRICTIONS:      The size of the input signal MUST BE a power of 2
	REVISION HISTORY:
**************************************************************************/
#include "OmpSCR.h"
#include <math.h>

#define PI (3.141592653589793f)
#define	NUM_ARGS		2
#define NUM_TIMERS	4
typedef double real_type;

/* Input values (CONSTANTS) */
unsigned NN;                 /* input vector is NN complex values */
unsigned LOGNN;              /* log base 2 of NN */
unsigned N;                  /* square root of NN */
unsigned LOGN;               /* log base 2 of N   */
int      ITERS;              /* number of input vectors */

typedef struct { real_type re, im; } complex;
/* -----------------------------------------------------------------------
                             PROTOTYPES
 * ----------------------------------------------------------------------- */
int prperf(double tpose_time, double scale_time, double ffts_time, int nn, int lognn, int iters);
int gen_bit_reverse_table(int *brt, int n);
int gen_w_table(complex *w, int n, int logn, int ndv2);
int gen_v_table(complex *v, int n);
int dgen(complex *xin, int n);
int tpose(complex *a, complex *b, int n);
int tpose_seq(complex *a, complex *b, const int n);
int cffts(complex *a, int *brt, complex *w, int n, int logn, int ndv2);
int cffts_seq(complex *a, int *brt, complex *w, const int n, int logn, int ndv2);
int fft(complex *a, int *brt, complex *w, int n, int logn, int ndv2);
int scale(complex *a, complex *v, int n);
int scale_seq(complex *a, complex *v, const int n);
int chkmat(complex *a, int n);
complex complex_pow(complex z, int n);
int log2_int(int n);
int print_mat(complex *a, int n);
int print_vec(complex *a, int n);
/* -----------------------------------------------------------------
                         IMPLEMENTATION
 * ----------------------------------------------------------------- */
/* -----------------------------------------------------------------
       gen_bit_reverse_table - initialize bit reverse table
       Postcondition: br(i) = bit-reverse(i-1) + 1
   ----------------------------------------------------------------- */
int gen_bit_reverse_table(int *brt, int n) {
  register int i, j, k, nv2;

  nv2 = n / 2;
  j = 1;
  brt[0] = j;
  for (i = 1; i < n; ++i) {
	  k = nv2;
    while (k < j) {
			j -= k;
			k /= 2;
		}
	  j += k;
	  brt[i] = j;
  }
  return 0;
}
/* -----------------------------------------------------------------------
   gen_w_table: generate powers of w.
   postcondition: w(i) = w**(i-1)
 * ----------------------------------------------------------------------- */
int gen_w_table(complex *w, int n, int logn, int ndv2) {
    register int i;
		 complex ww, pt;

    ww.re = cos(PI / ndv2);
    ww.im = -sin(PI / ndv2);
		w[0].re = 1.f;
		w[0].im = 0.f;
    pt.re = 1.f;
    pt.im = 0.f;
    for (i = 1; i < ndv2; ++i) {
			w[i].re = pt.re * ww.re - pt.im * ww.im;
			w[i].im = pt.re * ww.im + pt.im * ww.re;
			pt = w[i];
    }
    return 0;
}
/* -----------------------------------------------------------------------
       gen_v_table - gen 2d twiddle factors
 * ----------------------------------------------------------------------- */
int gen_v_table(complex *v, int n) {
  register int j, k;
   complex wn;

  wn.re = cos(PI * 2.f / (n * n));
  wn.im = -sin(PI * 2.f / (n * n));
	for (j = 0; j < n; ++j) {
    for (k = 0; k < n; ++k) {
			v[j * n + k] = complex_pow(wn, j * k);
	  }
  }
  return 0;
}
/* -----------------------------------------------------------------------
           z^n = modulo^n (cos(n * alfa) + i sin(n * alfa)
 * ----------------------------------------------------------------------- */
complex complex_pow(complex z, int n) {
	 complex temp;
	 real_type pot, nangulo, modulo, angulo;

	modulo = sqrt(z.re * z.re + z.im * z.im);
	angulo = atan(z.im / z.re);
  pot = pow(modulo, n);
	nangulo = n * angulo;

	temp.re = pot * cos(nangulo);
	temp.im = pot * sin(nangulo);
  return(temp);
}
/* -----------------------------------------------------------------------
   A: Para una se�al de entrada de tama�o N y de forma
                                  (1,0), (1,0), ..., (1,0)
   la salida debe ser de la forma
                                  (N,0), (0,0), ..., (0,0)

   B: Para una se�al de entrada de tama�o N y de forma
                                  (0,0), (0,0), ..., (N*N, 0), ..., (0,0)
   la salida debe ser de la forma (N*N, 0), (0,0), ..., (0,0)

   Propiedad �til: C: FFT(s1 + s2) = FFT(s1) + FFT(s2)

 * ----------------------------------------------------------------------- */
int dgen(complex *xin, int n) {
  register int i;
	int nn = n * n;
	/* Se�al de forma B */
  for (i = 0; i < nn; ++i) {
		xin[i].re = 0.f;
		xin[i].im = 0.f;
  }
  xin[nn / 2].re = (real_type)nn;

  /* Se�al de forma A */
	/*
  for (i = 0; i < nn; ++i) {
		xin[i].re = 1.0;
		xin[i].im = 0.f;
  }
  */
  return 0;
}
/* ----------------------------------------------------------------- */
int tpose(complex *a, complex *b, const int n) {
  register int i, j;

#pragma omp parallel for private(i, j)
  for (i = 0; i < n; ++i) {
    for (j = i; j < n; ++j) {
      b[i * n + j] = a[j * n + i];
      b[j * n + i] = a[i * n + j];
    }

  }
  return 0;
}

/* ----------------------------------------------------------------- */
int tpose_seq(complex *a, complex *b, const int n) {
  register int i, j;

  for (i = 0; i < n; ++i) {
    for (j = i; j < n; ++j) {
      b[i * n + j] = a[j * n + i];
      b[j * n + i] = a[i * n + j];
    }
  }
  return 0;
}
/* ----------------------------------------------------------------- */
int cffts(complex *a, int *brt, complex *w, const int n, int logn, int ndv2) {
  register int i;

#pragma omp parallel for private(i)
  for (i = 0; i < n; ++i){
    fft(&a[i * n], brt, w, n, logn, ndv2);
  }
    /* fft(a + i * n, brt, w, n, logn, ndv2); */
  return 0;
}
/* ----------------------------------------------------------------- */
int cffts_seq(complex *a, int *brt, complex *w, const int n, int logn, int ndv2) {
  register int i;

  for (i = 0; i < n; ++i)
    fft(&a[i * n], brt, w, n, logn, ndv2);
  return 0;
}
/* -----------------------------------------------------------------------
       Fast Fourier Transform
       1D in-place complex-complex decimation-in-time Cooley-Tukey
 * ----------------------------------------------------------------------- */
int fft(complex *a, int *brt, complex *w, int n, int logn, int ndv2) {
	  register int i, j;
     int powerOfW, stage, first, spowerOfW;
     int ijDiff;
     int stride;
     complex ii, jj, temp, pw;

  /* bit reverse step */
  for (i = 0; i < n; ++i) {
    j = brt[i];
    if (i < (j-1)) {
			temp = a[j - 1];
      a[j - 1] = a[i];
      a[i] = temp;
    }
  }

  /* butterfly computations */
  ijDiff = 1;
  stride = 2;
  spowerOfW = ndv2;
  for (stage = 0; stage < logn; ++stage) {
    /* Invariant: stride = 2 ** stage
       Invariant: ijDiff = 2 ** (stage - 1) */
    first = 0;
    for (powerOfW = 0; powerOfW < ndv2; powerOfW += spowerOfW) {
			pw = w[powerOfW];
      /* Invariant: pwr + sqrt(-1)*pwi = W**(powerOfW - 1) */
      for (i = first; i < n; i += stride) {
        j = i + ijDiff;
				jj = a[j];
        ii = a[i];
        temp.re = jj.re * pw.re - jj.im * pw.im;
        temp.im = jj.re * pw.im + jj.im * pw.re;
        a[j].re = ii.re - temp.re;
        a[j].im = ii.im - temp.im;
        a[i].re = ii.re + temp.re;
				a[i].im = ii.im + temp.im;
      }
      ++first;
    }
    ijDiff = stride;
    stride <<= 1;
    spowerOfW /= 2;
  }
  return 0;
} /* fft */
/* ----------------------------------------------------------------- */
int scale(complex *a, complex *v, const int n) {
  register int i, j, index;
	complex aa, vv;

#pragma omp parallel for private(i, j, index, aa, vv)
  for (i = 0; i < n; ++i) {
    for (j = 0; j < n; ++j) {
			index = i * n + j;
      aa = a[index];
			vv = v[index];
			a[index].re = aa.re * vv.re - aa.im * vv.im;
			a[index].im = aa.re * vv.im + aa.im * vv.re;
    }

  }
  return 0;
}
/* ----------------------------------------------------------------- */
int scale_seq(complex *a, complex *v, const int n) {
  register int i, j, index;
	complex aa, vv;

  for (i = 0; i < n; ++i) {
    for (j = 0; j < n; ++j) {
			index = i * n + j;
      aa = a[index];
			vv = v[index];
			a[index].re = aa.re * vv.re - aa.im * vv.im;
			a[index].im = aa.re * vv.im + aa.im * vv.re;
    }
  }
  return 0;
}
/* -----------------------------------------------------------------------
       chkmat - check the output matrix for correctness
 * ----------------------------------------------------------------------- */
int chkmat(complex *a, int n) {
  int sign, i, j, nn, errors;
  real_type EPSILON = 1e-4f;

  errors = 0;
	nn = n * n;
  for (i = 0; i < n; ++i) {
    sign = 1;
    for (j = 0; j < n; ++j) {
      if (a[i * n + j].re > nn * sign + EPSILON)
        ++errors;
      if (a[i * n + j].re < nn * sign - EPSILON)
        ++errors;
      sign = -sign;
    }
  }
  if (errors > 0) {
		printf("Errors = %d\n", errors);
		exit(-1);
  }
  return 0;
}
/* ----------------------------------------------------------------- */
int prperf(double tpose_time, double scale_time, double ffts_time, int nn, int lognn, int iters) {
   double secs;
   double fpercent, spercent, tpercent;
   double flops, mflops;

  tpose_time /= iters;
  scale_time /= iters;
  ffts_time /= iters;
  secs = tpose_time + scale_time + ffts_time;
  tpercent = tpose_time / secs * 100;
  spercent = scale_time / secs * 100;
  fpercent = ffts_time / secs * 100;
  flops = (real_type) (nn * 5 * lognn);
  mflops = flops / 1e6f / secs;
  printf("***********************\n");
  printf("1D FFT %d points\n" ,nn);
  printf("***********************\n");
  printf("Time per input vector:\n");
  printf("tpose    : %lf %lf percent\n", tpose_time, tpercent);
  printf("scale    : %lf %lf percent\n", scale_time, spercent);
  printf("ffts     : %lf %lf percent\n", ffts_time, fpercent);
  printf("total(s) : %lf\n", secs);
  printf("mflop/s  : %lf\n", mflops);
  return 0;
} /* prperf */
/* -----------------------------------------------------------------------
    Base 2 logarithm
 * ----------------------------------------------------------------------- */
int log2_int(int n) {
		register int i, aux;

		aux = 1;
		for (i = 0; i <= 128; i++) {
				if (aux > n)
					 return (i - 1);
				aux <<= 1;
		}
  return -1;
}
/* ----------------------------------------------------------------- */
int print_mat(complex *a, int n) {
  register int i, j;

  for (i = 0; i < n; ++i) {
    for (j = 0; j < n; ++j) {
			printf(" (%.0f, %.0f)", a[i * n + j].re, a[i * n + j].im);
    }
		printf("\n");
  }
	printf("\n");
  return 0;
}
/* ----------------------------------------------------------------- */
int print_vec(complex *a, int n) {
  register int i;

  for (i = 0; i < n*n; ++i)
		printf("  (%.0f, %.0f)", a[i].re, a[i].im);
	printf("\n\n");
  return 0;
}
/* ----------------------------------------------------------------- */
int main(int argc, char *argv[]) {
  complex **xin;          /* input vector                   */
  complex **aux;          /* intermediate array (same size) */
  /* precomputed vectors */
  int *brt;               /* bit reverse table for 1d fft's */
	complex *w;             /* twiddles for 1d fft's          */
	complex *v;             /* twiddles for 2d fft            */
  double total_time;      /* timing                         */
  int k;
	int NUMTHREADS, MAX_THREADS;
#define ID  omp_get_thread_num()
  float    MEMORY;             /* number of bytes required */
	char *PARAM_NAMES[NUM_ARGS] = {"Size (a power of 2).", "No. of iterations"};
	char *TIMERS_NAMES[NUM_TIMERS] = {"Total_time", "Tpose time", "Scale time", "Column FFTs time" };
  char *DEFAULT_VALUES[NUM_ARGS] = {"64", "10"};


  NUMTHREADS = omp_get_max_threads();
  OSCR_init (NUMTHREADS, "Bailey's '6-step' Fast Fourier Transform", "Use 'fft6' <size (in K)> <iters>", NUM_ARGS,
                PARAM_NAMES, DEFAULT_VALUES , NUM_TIMERS, NUM_TIMERS, TIMERS_NAMES,
                argc, argv);


  /* Command line arguments processing */
	N = OSCR_getarg_int(1);
	ITERS = OSCR_getarg_int(2);
  /*
  if(argc == 3) {
    N = atoi(argv[1]);
    ITERS = atoi(argv[2]);
	}
  else {
		printf("Usage: %s N ITERS\n", argv[0]);
		printf("N is the size of the input signal. Should be a power of two.\n");
    N = 64;
		ITERS = 10;
	}
  */

	MAX_THREADS = omp_get_max_threads();
  NN = N * N;
	LOGN = log2_int(N);
  LOGNN = log2_int(NN);
	MEMORY = N * sizeof(int) +
		       2 * (NN) * sizeof(complex) * MAX_THREADS + (NN) * sizeof(complex) +
				   (N / 2) * sizeof(complex);

	/* Memory allocation */
	xin = OSCR_calloc(MAX_THREADS, sizeof(*xin));
	aux = OSCR_calloc(MAX_THREADS, sizeof(*aux));
	brt =     (int *)OSCR_calloc(N,       sizeof(int));
	v   = (complex *)OSCR_calloc(N * N,   sizeof(complex)); /* twiddles for 2d fft */
	w   = (complex *)OSCR_calloc((N / 2), sizeof(complex)); /* twiddles for 1d fft's */

	printf("Input values:\n");
	printf("=============\n");
	printf("N           : %u\n", N);
	printf("ITERS       : %u\n", ITERS);
	printf("NN          : %u\n", NN);
	printf("LOGN        : %u\n", LOGN);
	printf("LOGNN       : %u\n", LOGNN);
	printf("Memory      : %.2f Kbytes\n", MEMORY / 1024);

  /* precompute the input array and fft constants */
  gen_bit_reverse_table(brt, N);
  gen_w_table(w, N, LOGN, N / 2);
  gen_v_table(v, N);

	for (k = 0; k < MAX_THREADS; k++) {
		xin[k] = (complex *)OSCR_calloc(N * N, sizeof(complex));
		aux[k] = (complex *)OSCR_calloc(N * N, sizeof(complex));
	}
#define TOTAL 0

OSCR_timer_start(TOTAL);
#pragma omp parallel for private(k)
  for(k = 0; k < ITERS; k++) {
    dgen(xin[ID], N);

		tpose(xin[ID], aux[ID], N);

		cffts(aux[ID], brt, w, N, LOGN, N / 2);

		scale(aux[ID], v, N);

		tpose(aux[ID], xin[ID], N);

		cffts(xin[ID], brt, w, N, LOGN, N / 2);

		tpose(xin[ID], aux[ID], N);

		chkmat(aux[ID], N);
	}
	OSCR_timer_stop(TOTAL);
	total_time = OSCR_timer_read(TOTAL);

	/* Display results and time */
	OSCR_report(1, TIMERS_NAMES);

	printf("============================");
	printf("\n# THREADS : %d\n", NUMTHREADS);
	printf("N         : %d\n", N);
	printf("ITERS     : %d\n", ITERS);
//	printf("total TIME: %.6lf secs.\n", total_time);

#undef ID
#undef MAX_THREADS
	return 0;
}
/* ----------------------------------------------------------------- */

/*
 * vim:ts=2:sw=2:set nonu:
 */

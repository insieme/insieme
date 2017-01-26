/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/
 
#include "kernel_def.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* Defines ****************************************************************/
#define SFIXED_MAX 1
#define max( a, b ) ( ((a) > (b)) ? (a) : (b) )
/* NrOf bits in the scaled 16+16b number that the scaling function tries to make utilized */
#define SCALING_CENTER_POINT 9 


/* Function Declarations **************************************************/

/* A*B=C, square 4xX complex matrixes only */
void matrix_mult_4xX_complex(int X, complexMatrix_t A,  complexMatrix_t B,  complexMatrix_t C) {
  int i,j;
  complex temp;

  for (i=0; i<4; i++) {
    for (j=0; j<X; j++) {
      temp=cmul(A[i][0],B[0][j]);
      temp=cadd(temp,cmul(A[i][1],B[1][j]));
      temp=cadd(temp,cmul(A[i][2],B[2][j]));
      C[i][j]=cadd(temp,cmul(A[i][3],B[3][j]));
    }
  }
}

/* A*B=C, square 4x1 A, B is a __fixed column vector complex matrixes only */
void matrix_scale_4xX_complex_fixed(int X, complexMatrix_t A, __fixed B,  complexMatrix_t C) {
  int i;

  for  (i=0; i<X; i++) {
    C[i][0]=cscale(B,0,A[i][0]);
    C[i][1]=cscale(B,0,A[i][1]);
    C[i][2]=cscale(B,0,A[i][2]);
    C[i][3]=cscale(B,0,A[i][3]);
  }
}

/******************************************************************************
 * @brief Calculates C=conj(A')*A+B for 4x1 complex matrixes A & B. 
 * (Result is placed in matrix C). C used for debugging of function...
 * The result matrix is scaled with the no of bits nearest lower to
 * SCALING_CENTER_POINT-log2(max(C)). This is primarily in order to avoid
 * overflow when calculating W (~H/C) in cholsolve.
 * scaling_no_of_bits is the number of bits to shift the values of C with in 
 * order to get the true values of C
 * @return      void
 *****************************************************************************/
void matrix_a_a_hermite_plus_b_4xX_complex(int X, complexMatrix_t A, complexMatrix_t B, complexMatrix_t C, int *scaling_no_of_bits) {
  int i, j, max_element = 0, scale_limit = 2, size_no_of_bits = 0, neg_scaling_no_of_bits;
  complex A_temp, A_temp_conj, AA_conj_temp, temp_complex;
  for (i=0; i<4; i++) {
    for (j=0; j<X; j++) {
      temp_complex = cmake(1,0); /* reset accumulator */
      /* unrolled inner loop:
         round 0: */
      A_temp_conj = cconj(A[j][0]);
      A_temp = A[i][0];
      AA_conj_temp = cmul(A_temp, A_temp_conj);
      temp_complex = cadd(temp_complex, AA_conj_temp);
      /* round 1: */
      A_temp_conj = cconj(A[j][1]);
      A_temp = A[i][1];
      AA_conj_temp = cmul(A_temp, A_temp_conj);
      temp_complex = cadd(temp_complex, AA_conj_temp);
      /* round 2: */
      A_temp_conj = cconj(A[j][2]);
      A_temp = A[i][2];
      AA_conj_temp = cmul(A_temp, A_temp_conj);
      temp_complex = cadd(temp_complex, AA_conj_temp);
      /*  round 3: */
      A_temp_conj = cconj(A[j][3]);
      A_temp = A[i][3];
      AA_conj_temp = cmul(A_temp, A_temp_conj);
      temp_complex = cadd(temp_complex, AA_conj_temp);
      C[i][j] = cadd(B[i][j], temp_complex);
      max_element = max(max_element, max(abs(C[i][j].re),abs(C[i][j].im))); /* find maximum size of real or im of elements */
    }
  }
  /* calculate scaling factor */
  while (abs(scale_limit) < max_element) {
    scale_limit = scale_limit * 2;
    size_no_of_bits = size_no_of_bits + 1;
  }
  *scaling_no_of_bits = size_no_of_bits - SCALING_CENTER_POINT;
  neg_scaling_no_of_bits = -(*scaling_no_of_bits);

  /* perform the very scaling of C */
  for (i=0; i<4; i++)
    for (j=0; j<X; j++)
      C[i][j] = cscale(16000, neg_scaling_no_of_bits + 1, C[i][j]);
}

#define TOOSMALL 10
void cholsolve_4xX_complex(int X, complexMatrix_t W, complexMatrix_t U, complexMatrix_t H) {
  int layer, j, k;
  complex sumc;
  int sum;

  for (layer=0; layer< X; layer++)
    for (j=0; j< 4; j++)
      W[layer][j] = cmake(0,0);

  for (layer=0; layer<X; layer++) {
    /* First compute U[layer][layer] */
    sum = cabs2(U[layer][layer]);
    for (j=1; j<=(layer-1); j++) 
      sum -= cabs2(U[j][layer]);
    if (sum > TOOSMALL) {
      W[layer][layer] = cmake((short int)sqrt(sum),0);
      /* Now find elements U[row][k], k > row. */
      for (k=layer; k < 4; k++) {
	sumc = H[layer][k];
	for (j=1; j<=(layer-1); j++) 
	  sumc = csub(sumc, cmul(H[j][layer],H[j][k]));
	W[layer][k] = cscale(W[layer][layer].re, 0, sumc);
      }
    } else {
      /* blast off the entire row. */
      for (k=layer; k<X; k++) 
	W[layer][k] = cmake(0,0);
    }
  }
}

void mmse_by_cholsolve_4xX_complex(int X, complexMatrix_t *W_p, complexMatrix_t R_p, scData_t in[RX_ANT][MAX_LAYERS], int no_of_sc, int rho, int beta, int alpha) {
  /* local matrixes used for cholsolve etc */
  complex c0 = cmake(0,0);
  complex c1 = cmake(1,1);
  complexMatrix_t F                = { {c1, c1, c1, c1}, {c1, c1, c1, c1}, {c1, c1, c1, c1}, {c1, c1, c1, c1} };
  complexMatrix_t H                = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t R                = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t Ri               = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t Hrho             = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t Hprim            = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t Ri_regularized   = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };
  complexMatrix_t Hprim_transposed = { {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0}, {c0, c0, c0, c0} };

  int scaling_Ri;
  __fixed alpha_temp;
  complex complex_beta;
  int current_sc;
  int i,j;

  /* call/spawn some instances for parts of the total number of RBs */
  for (current_sc = 0; current_sc < no_of_sc; current_sc++) {

    /* Put the data in a compact matrix form */
    for (i=0; i<4; i++) 
      for (j=0; j<X; j++) 
	H[i][j]=in[j][i][current_sc];

    /* Scale H with rho:
       calculate Hrho=rho*H as matrix_scale_4x1_complex_fixed(H,rho,Hrho) */
    matrix_scale_4xX_complex_fixed(X, H, rho, Hrho);

    /* Take care of precoding:
       calculate H'=F*Hrho as matrix_mult_4x1_complex(Hrho,F,Hprim) */
    matrix_mult_4xX_complex(X, Hrho, F, Hprim_transposed);

    for (i=0; i<4; i++)
      for (j=0; j<X; j++)
	Hprim[i][j]=Hprim_transposed[j][i];
    
    /* Form Ri=H*H'+R */
    matrix_a_a_hermite_plus_b_4xX_complex(X, Hprim, R, Ri, &scaling_Ri);

    /* Regularize Ri */
    complex_beta=cscale(SFIXED_MAX,-scaling_Ri, cmake(beta,0));
    for  (i=0; i<X; i++)
      Ri[i][i]=cadd(Ri[i][i],complex_beta);

    alpha_temp=16000+alpha; /* to be able to scale up at all */
    for  (i=0; i<X; i++) {
      Ri_regularized[i][0]=cscale(alpha_temp, 1, Ri[i][0]);
      Ri_regularized[i][1]=cscale(alpha_temp, 1, Ri[i][1]);
      Ri_regularized[i][2]=cscale(alpha_temp, 1, Ri[i][2]);
      Ri_regularized[i][3]=cscale(alpha_temp, 1, Ri[i][3]);
    }

    /* calculate W as cholsolve_4x1_complex(W,Ri,Hprim) */
    cholsolve_4xX_complex(X, W_p[current_sc], Ri_regularized, Hprim);
  }
}

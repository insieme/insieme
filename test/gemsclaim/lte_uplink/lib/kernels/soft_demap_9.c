/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include <stdio.h>
#include "soft_demap_9.h"

void soft_demap(complex* in, int scaling_factor, int mod, int n, char* out) {
  int L;
  int i,k;
	
  switch (mod) {
  case MOD_PSK:
    {
      int ind = 0;
      for (i=0; i<n; i++) {
	out[ind++] = in[i].re * scaling_factor;
      }
    }
    break;
  case MOD_QPSK:
    {
      int ind = 0;
      complex exp_pi4, temp; 
      exp_pi4.re = 4096;
      exp_pi4.im = -4096;
      for (i=0; i<n; i++) {
	temp = cmul(in[i], exp_pi4);
	out[ind++] = (temp.re * scaling_factor)%255;
	out[ind++] = (temp.im * scaling_factor)%255;
      }
    }
    break;
  case MOD_16QAM:
    {
      int temp_real, temp_imag;
      int ind = 0;
      
      for (i=0; i<n; i++) {
	for (k=0; k<4; k++) {
	  L = k*4096;
	  temp_real = (int)(((L - 1) - ((in[i].re * scaling_factor)>>8) + (L - 1)) / 2.0);
	  temp_imag = (int)(((L - 1) - ((in[i].im * scaling_factor)>>8) + (L - 1)) / 2.0);
	  if (temp_real < 0)
	    temp_real = 0;
	  else if (temp_real > (L - 1))
	    temp_real = (L - 1);
	  if (temp_imag < 0)
	    temp_imag = 0;
	  else if (temp_imag > (L - 1))
	    temp_imag = (L - 1);
	  out[ind++] = temp_real%255;
	  out[ind++] = temp_imag%255;
	}
      }
    }
    break;
  case MOD_64QAM:
    {
      int temp_real, temp_imag;
      int ind = 0;
      
      for (i=0; i<n; i++) {
	for (k=0; k<6; k++) {
	  L = k*4096;
	  temp_real = (int)(((L - 1) - ((in[i].re * scaling_factor)>>8) + (L - 1)) / 2.0);
	  temp_imag = (int)(((L - 1) - ((in[i].im * scaling_factor)>>8) + (L - 1)) / 2.0);
	  if (temp_real < 0)
	    temp_real = 0;
	  else if (temp_real > (L - 1))
	    temp_real = (L - 1);
	  if (temp_imag < 0)
	    temp_imag = 0;
	  else if (temp_imag > (L - 1))
	    temp_imag = (L - 1);
	  out[ind++] = temp_real%255;
	  out[ind++] = temp_imag%255;
	}
      }
    }
    break;
  default:
    printf("Modulation not supported: %i\n", mod);
  }
}

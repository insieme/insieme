/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "stdlib.h"
#include "stdio.h"
#include "def.h"
#include "uplink.h"
#include "uplink_verify.h"
#include "crc_13.h"

#if PARAMETER_MODEL_VERIFICATION
FILE *file;
#if !CREATE_VERIFICATION_DATA && DETAILED_VERIFICATION_DATA
#include "verification_data_detailed.h"
#elif !CREATE_VERIFICATION_DATA && !DETAILED_VERIFICATION_DATA
#include "verification_data.h"
#endif
#endif

void _uplink_verify(int subframe, char *data, int length) {
#if PARAMETER_MODEL_VERIFICATION && CREATE_VERIFICATION_DATA
  fprintf(file, "short ver%i = %i;\n", subframe, crcFast((unsigned char *)data, length));
#elif PARAMETER_MODEL_VERIFICATION
				//for(int i=0; i<length;++i) printf("%d, ", data[i]);
				//printf("\n");
  unsigned char crc = crcFast((unsigned char *)data, length);
  if (crc != ver_data[subframe]) {
    printf("Final data is not same as verification data.\n");
    printf("Subframe: %i, %hu != %hu\n", subframe, crc, ver_data[subframe]);
    verification_errors = 1;
  } else {
    printf("Subframe %i passed complete test\n", subframe);
  }
#endif
}

void _uplink_layer_verify(int subframe, scData_t data[][RX_ANT], complexMatrix_t R, int length, int layer, int slot) {
#if PARAMETER_MODEL_VERIFICATION && DETAILED_VERIFICATION_DATA
  int i, j, k;
#if CREATE_VERIFICATION_DATA
  fprintf(file, "const short int ldata%i_slot%i[] = {", subframe, slot);
  for (i=0; i<layer; i++)
    for (j=0; j<4; j++)  /* RX */
      for (k=0; k<length; k++)
	if (i==0 && j==0 && k==0)
	  fprintf(file, "%i, %i", (short int)data[0][0][0].re, (short int)data[0][0][0].im);
	else
	  fprintf(file, ", %i, %i", (short int)data[i][j][k].re, (short int)data[i][j][k].im);
  fprintf(file, "};\n");

  fprintf(file, "const short int rdata%i_slot%i[] = {", subframe, slot);
  for (i=0; i<layer; i++)
    for (j=0; j<RX_ANT; j++)
      if (i==0 && j==0)
	fprintf(file, "%i, %i", (short int)R[0][0].re, (short int)R[0][0].im);
      else
	fprintf(file, ", %i, %i", (short int)R[i][j].re, (short int)R[i][j].im);
  fprintf(file, "};\n");
#else
  int l = 0;
  int passed = 1;

  for (i=0; i<layer; i++)
    for (j=0; j<4; j++)  /* RX */
      for (k=0; k<length; k++)
	if (data[i][j][k].re != verification_layer_data[subframe][slot][l++]) {
	  printf("Re layer data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, k=%i, l=%i, %i != %i\n", subframe, slot, i, j, k, l-1, data[i][j][k].re, verification_layer_data[subframe][slot][l-1]);
	  passed = 0;
	  break;
	} else if (data[i][j][k].im != verification_layer_data[subframe][slot][l++]) {
	  printf("Im layer data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, k=%i, l=%i, %i != %i\n", subframe, slot, i, j, k, l-1, data[i][j][k].im, verification_layer_data[subframe][slot][l-1]);
	  passed = 0;
	  break;
	}
  l = 0;
  for (i=0; i<layer; i++)
    for (j=0; j<RX_ANT; j++)
	if (R[i][j].re != verification_r_data[subframe][slot][l++]) {
	  printf("Re R data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, l=%i, %i != %i\n", subframe, slot, i, j, l-1, R[i][j].re, verification_r_data[subframe][slot][l-1]);
	  passed = 0;
	  break;
	} else if (R[i][j].im != verification_r_data[subframe][slot][l++]) {
	  printf("Im R data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, l=%i, %i != %i\n", subframe, slot, i, j, l-1, R[i][j].im, verification_r_data[subframe][slot][l-1]);
	  passed = 0;
	  break;
	}
  
  if (passed)
    printf("Subframe %i, slot %i passed layer test\n", subframe, slot);
  else
    verification_errors = 1;
#endif
#endif
}

void _uplink_weight_verify(int subframe, weightSC_t *data, int length, int layer, int slot) {
#if PARAMETER_MODEL_VERIFICATION && DETAILED_VERIFICATION_DATA
  int i, j, k;
#if CREATE_VERIFICATION_DATA
  fprintf(file, "const short int wdata%i_slot%i[] = {", subframe, slot);
  for (i=0; i<layer; i++)
    for (j=0; j<length; j++)
      for (k=0; k<4; k++) /* RX */
	if (i==0 && j==0 && k==0)
	  fprintf(file, "%i, %i", (short int)data[0][0][0].re, (short int)data[0][0][0].im);
	else
	  fprintf(file, ", %i, %i", (short int)data[i][j][k].re, (short int)data[i][j][k].im);
  fprintf(file, "};\n");
#else
  int l = 0;
  int passed = 1;

  for (i=0; i<layer; i++)
    for (j=0; j<length; j++)
      for (k=0; k<4; k++) {
	if (data[i][j][k].re != verification_weight_data[subframe][slot][l++]) {
	  printf("Re weight data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, k=%i, l=%i, %i != %i\n", subframe, slot, i, j, k, l-1, data[i][j][k].re, verification_weight_data[subframe][slot][l-1]);
	  passed = 0;
	}
        if (data[i][j][k].im != verification_weight_data[subframe][slot][l++]) {
	  printf("Im weight data is not same as verification data.\n");
	  printf("Subframe: %i, slot: %i, i=%i, j=%i, k=%i, l=%i, %i != %i\n", subframe, slot, i, j, k, l-1, data[i][j][k].im, verification_weight_data[subframe][slot][l-1]);
	  passed = 0;
	}
      }
  if (passed)
    printf("Subframe %i, slot %i passed weight test\n", subframe, slot);
  else
    verification_errors = 1;
#endif
#endif
}

void _uplink_symbol_verify(int subframe, complex *data, int length) {
#if PARAMETER_MODEL_VERIFICATION && DETAILED_VERIFICATION_DATA
  int i;

#if CREATE_VERIFICATION_DATA
  fprintf(file, "const unsigned short sdata%i[] = {", subframe);
  fprintf(file, "%hu, %hu", (unsigned short)data[0].re, (unsigned short)data[0].im);
  for (i=1; i<length; i++)
    fprintf(file, ", %hu, %hu", (unsigned short)data[i].re, (unsigned short)data[i].im);
  fprintf(file, "};\n");
#else
  for (i=0; i<length; i++) {
    if ((unsigned short)data[i].re != (unsigned short)verification_symbol_data[subframe][2*i]) {
      printf("Re symbol data is not same as verification data.\n");
      printf("Subframe: %i, i=%i, %hu != %hu\n", subframe, i, (unsigned short)data[i].re, (unsigned short)verification_symbol_data[subframe][2*i]);
      break;
    } else if ((unsigned short)data[i].im != (unsigned short)verification_symbol_data[subframe][2*i+1]) {
      printf("Im symbol data is not same as verification data.\n");
      printf("Subframe: %i, i=%i, %hu != %hu\n", subframe, i, (unsigned short)data[i].im, (unsigned short)verification_symbol_data[subframe][2*i+1]);
      break;
    }
  }
  if (i==length)
    printf("Subframe %i passed symbol test\n", subframe);
  else
    verification_errors = 1;
#endif
#endif
}

void _uplink_interleave_verify(int subframe, complex *data, int length) {
#if PARAMETER_MODEL_VERIFICATION && DETAILED_VERIFICATION_DATA
  int i;
#if CREATE_VERIFICATION_DATA
  fprintf(file, "const unsigned short idata%i[] = {", subframe);
  fprintf(file, "%hu, %hu", (unsigned short)data[0].re, (unsigned short)data[0].im);
  for (i=1; i<length; i++)
    fprintf(file, ", %hu, %hu", (unsigned short)data[i].re, (unsigned short)data[i].im);
  fprintf(file, "};\n");
#else
  for (i=0; i<length; i++) {
    if ((unsigned short)data[i].re != (unsigned short)verification_interleave_data[subframe][2*i]) {
      printf("Re interleave data is not same as verification data.\n");
      printf("Subframe: %i, i=%i, %hu != %hu\n", subframe, i, (unsigned short)data[i].re, (unsigned short)verification_interleave_data[subframe][2*i]);
      break;
    } else if ((unsigned short)data[i].im != (unsigned short)verification_interleave_data[subframe][2*i+1]) {
      printf("Im interleave data is not same as verification data.\n");
      printf("Subframe: %i, i=%i, %hu != %hu\n", subframe, i, (unsigned short)data[i].im, (unsigned short)verification_interleave_data[subframe][2*i+1]);
      break;
    }
  }
  if (i==length)
    printf("Subframe %i passed interleave test\n", subframe);
  else
    verification_errors = 1;
#endif
#endif
}

void _init_verify(void) {
#if PARAMETER_MODEL_VERIFICATION
#if CREATE_VERIFICATION_DATA && DETAILED_VERIFICATION_DATA
  file = fopen("uplink/verification_data_detailed.h","w");
#elif CREATE_VERIFICATION_DATA
  file = fopen("uplink/verification_data.h","w");
#else
  __init_verify();
  verification_errors = 0;
#endif
#endif
}

void _uplink_write_verify_data(int subframes) {
#if PARAMETER_MODEL_VERIFICATION && CREATE_VERIFICATION_DATA
  int i;

  fprintf(file, "\nstatic short ver_data[%i];\n", subframes-1);
#if DETAILED_VERIFICATION_DATA
  fprintf(file, "\nstatic const short int *verification_layer_data[%i][2];\n", subframes-1);
  fprintf(file, "\nstatic const short int *verification_r_data[%i][2];\n", subframes-1);
  fprintf(file, "\nstatic const short int *verification_weight_data[%i][2];\n", subframes-1);
  fprintf(file, "\nstatic const unsigned short *verification_symbol_data[%i];\n", subframes-1);
  fprintf(file, "\nstatic const unsigned short *verification_interleave_data[%i];\n", subframes-1);
#endif

  fprintf(file, "\nvoid __init_verify(void) {\n");
  for (i=0; i<subframes-1; i++) {
    fprintf(file, "ver_data[%i] = ver%i;\n", i, i);
#if DETAILED_VERIFICATION_DATA
    fprintf(file, "verification_layer_data[%i][0]   = ldata%i_slot0;\n", i, i);
    fprintf(file, "verification_layer_data[%i][1]   = ldata%i_slot1;\n", i, i);
    fprintf(file, "verification_r_data[%i][0]       = rdata%i_slot0;\n", i, i);
    fprintf(file, "verification_r_data[%i][1]       = rdata%i_slot1;\n", i, i);
    fprintf(file, "verification_weight_data[%i][0]  = wdata%i_slot0;\n", i, i);
    fprintf(file, "verification_weight_data[%i][1]  = wdata%i_slot1;\n", i, i);
    fprintf(file, "verification_symbol_data[%i]     = sdata%i;\n", i, i);
    fprintf(file, "verification_interleave_data[%i] = idata%i;\n", i, i);
#endif
  }
  fprintf(file, "}\n");

#else
    if (verification_errors)
      printf("Errors were encountered\n");
    else
      printf("Completed test without errors\n");
#endif
}

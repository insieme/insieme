/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "feldspar.h"
#include "r_alpha.h"

/* >> Defines ****************************************************************/
#define PRINT 0 /* set to 1 to print the result to stdout */

#define _3GPP_SUBC_PER_RB 12
#define MAX_NO_OF_RB 100
#define NO_OF_TX_ANTENNAS 2

/* >> Local Function Declarations ********************************************/
void alpha__n__prs(signed int var0_0_0, signed int var0_0_1, signed int var0_1_0, signed int var0_1_1, signed int * out_0, RealNum * out_1);

void RsGen(signed int n_rb, complex *result) {
  signed int tmp;
  int this_antenna = 0;
#if PRINT
  int i;
#endif

  /* Calculate alpha for all antennas: */
  alpha__n__prs(10, NO_OF_TX_ANTENNAS, 2, 3, NULL, NULL);

  /* Calculate result for all antennas: */
  for (this_antenna = 0; this_antenna < NO_OF_TX_ANTENNAS; this_antenna++) {
    /* generate alphaValue shifted RS sequence r: */
    r__alpha(12,
	     (n_rb*_3GPP_SUBC_PER_RB),
	     0,
	     n_rb*_3GPP_SUBC_PER_RB,
	     0,
	     &tmp,
	     result);

#if PRINT
  printf("Result of reference sequence genereation:\n");
  printf("[");
  for (i=0; i < _3GPP_SUBC_PER_RB*MAX_NO_OF_RB; i++) {
#if FLUNK
    /* Print the complex value somehow */
#else
    printf("%d", result[i].re);
    printf("+j%d, ", result[i].im);
#endif
  }
  printf("]\n");
#endif
  }
}

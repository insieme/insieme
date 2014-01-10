/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>

#include "typedefs.h"
#include "n_prs_all_ns.h"
#include "feldspar.h"

#include "ulRsGen.h"

/* >> Defines ****************************************************************/
#define _3GPP_SUBC_PER_RB 12
#define MAX_NO_OF_RB 100
#define NO_OF_TX_ANTENNAS 2
#define SLOTS_PER_RADIO_FRAME 20
#define NO_OF_TEST_RS_GENERATION_RUNS 20

/* >> Global Variable Definitions ********************************************/

/* Allocate buffer for output data */
static complex r[_3GPP_SUBC_PER_RB*MAX_NO_OF_RB];
static U16 n_prs[SLOTS_PER_RADIO_FRAME];

void testRsGen() {
  U16 i,j;
  signed int n_rb;

  for (i = 0; i < NO_OF_TEST_RS_GENERATION_RUNS; i++) {
    /* simple pseudo random generator */
    n_rb=(271*i)%MAX_NO_OF_RB;

    /* cyclic shift generation for a radio frame */
    n__prs__all__ns(15, NULL, (signed int*)n_prs);
    
    /* pilot symbol generation for a radio frame */
    for (j = 0; j < SLOTS_PER_RADIO_FRAME; j++)	{
      RsGen(n_rb,r);
    }
  }
}

int main(void) {
  testRsGen();

  return 0;
}

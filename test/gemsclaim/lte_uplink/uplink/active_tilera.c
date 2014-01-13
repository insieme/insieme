/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 *******************************************************************************
 * Author: Magnus Sjalander                                                    *
 ******************************************************************************/

#include "active.h"

void deactivate_cpu(void) {
  asm("nap");
}

void spin_cpu(void) {
  asm ("{moveli r2, 1; moveli r3, 18;}
        {shl r2, r2, r3; move r1, zero;}
        1:;
        {addi r1, r1, 1; seq r3, r1, r2;}
        bz r3, 1b;"
        : 
        :
        : "r1", "r2", "r3"
      );
}

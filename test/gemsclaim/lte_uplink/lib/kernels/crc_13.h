/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef _CRC_H
#define _CRC_H

#include "complex_def.h"

void crcInit(void);
unsigned char crcFast(unsigned char const message[], int nBytes);

#endif

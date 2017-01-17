/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef MISSINGFUNC_H
#define MISSINGFUNC_H

#include "typedefs.h"
#include "feldspar_intel_types.h"


#ifdef __cplusplus
extern "C" {
#endif

void intToU32(signed int in, U32 *out);
void u32ToInt(unsigned long int in, signed int *out);
void intToReal(signed int in, RealNum *out);
void cos_feldspar(RealNum a, RealNum *b);
void sin_feldspar(RealNum a, RealNum *b);

#define pi 3.14159265f

#ifdef __cplusplus
}
#endif

#endif

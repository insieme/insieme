/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "missingFunc.h"
#include <math.h>

void intToU32(signed int in, U32 *out) {
  *out=(unsigned long int)in;
}
void u32ToInt(unsigned long int in, signed int *out) {
  *out=(signed int)in;
}
void intToReal(signed int in, RealNum *out) {
#if FLUNK
  *out=in;
#else
  if (out!=0)
    *out=(int)in;
#endif
}

/* cosinus
   cos :: Data Float -> Data Float
   cos = function "cos" (\_ -> universal) (Prelude.cos) */
void cos_feldspar(RealNum a, RealNum *b) {
#if FLUNK
  *b=__cos_i16(a)>>16;
#else
  *b=32768*cos(2*3.14159265*(float)a/65536);
#endif
}

/* sinus
   sin :: Data Float -> Data Float
   sin = function "sin" (\_ -> universal) (Prelude.sin) */
void sin_feldspar(RealNum a, RealNum *b) {
#if FLUNK
  *b=__sin_i16(a)>>16;
#else
  *b=32768*sin(2*pi*(float)a/65536);
#endif
}

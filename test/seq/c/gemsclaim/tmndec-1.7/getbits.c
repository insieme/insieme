/************************************************************************
 *
 *  getbits.c, bit level routines for tmndecode (H.263 decoder)
 *  Copyright (C) 1996  Telenor R&D, Norway
 *        Karl Olav Lillevold <Karl.Lillevold@nta.no>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Karl Olav Lillevold               <Karl.Lillevold@nta.no>
 *  Telenor Research and Development
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *
 *  Robert Danielsen                  e-mail: Robert.Danielsen@nta.no
 *  Telenor Research and Development  www:    http://www.nta.no/brukere/DVC/
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *  
 ************************************************************************/

/*
 * based on mpeg2decode, (C) 1994, MPEG Software Simulation Group
 * and mpeg2play, (C) 1994 Stefan Eckart
 *                         <stefan@lis.e-technik.tu-muenchen.de>
 *
 */


#include <stdlib.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"
#ifdef WIN32
#include <io.h>
#endif


/* to mask the n least significant bits of an integer */

static unsigned int msk[33] =
{
  0x00000000,0x00000001,0x00000003,0x00000007,
  0x0000000f,0x0000001f,0x0000003f,0x0000007f,
  0x000000ff,0x000001ff,0x000003ff,0x000007ff,
  0x00000fff,0x00001fff,0x00003fff,0x00007fff,
  0x0000ffff,0x0001ffff,0x0003ffff,0x0007ffff,
  0x000fffff,0x001fffff,0x003fffff,0x007fffff,
  0x00ffffff,0x01ffffff,0x03ffffff,0x07ffffff,
  0x0fffffff,0x1fffffff,0x3fffffff,0x7fffffff,
  0xffffffff
};


/* initialize buffer, call once before first getbits or showbits */

void initbits()
{
  ld->incnt = 0;
  ld->rdptr = ld->rdbfr + 2048;
  ld->bitcnt = 0;
}

void fillbfr()
{
  int l;

  ld->inbfr[0] = ld->inbfr[8];
  ld->inbfr[1] = ld->inbfr[9];
  ld->inbfr[2] = ld->inbfr[10];
  ld->inbfr[3] = ld->inbfr[11];

  if (ld->rdptr>=ld->rdbfr+2048)
  {
    l = read(ld->infile,ld->rdbfr,2048);
    ld->rdptr = ld->rdbfr;
    if (l<2048)
    {
      if (l<0)
        l = 0;

      while (l<2048)   /* Add recognizable sequence end code */
      {
        ld->rdbfr[l++] = 0;
        ld->rdbfr[l++] = 0;
        ld->rdbfr[l++] = (1<<7) | (SE_CODE<<2);
      }
    }
  }

  for (l=0; l<8; l++)
    ld->inbfr[l+4] = ld->rdptr[l];

  ld->rdptr+= 8;
  ld->incnt+= 64;
}


/* return next n bits (right adjusted) without advancing */

unsigned int showbits(n)
int n;
{
  unsigned char *v;
  unsigned int b;
  int c;

  if (ld->incnt<n)
    fillbfr();

  v = ld->inbfr + ((96 - ld->incnt)>>3);
  b = (v[0]<<24) | (v[1]<<16) | (v[2]<<8) | v[3];
  c = ((ld->incnt-1) & 7) + 25;
  return (b>>(c-n)) & msk[n];
}


/* return next bit (could be made faster than getbits(1)) */

unsigned int getbits1()
{
  return getbits(1);
}


/* advance by n bits */

void flushbits(n)
int n;
{

  ld->bitcnt+= n;
  ld->incnt-= n;
  if (ld->incnt < 0)
    fillbfr();
}


/* return next n bits (right adjusted) */

unsigned int getbits(n)
int n;
{
  unsigned int l;

  l = showbits(n);
  flushbits(n);

  return l;
}

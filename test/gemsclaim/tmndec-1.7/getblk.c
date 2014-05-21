/************************************************************************
 *
 *  getblk.c, DCT block decoding for tmndecode (H.263 decoder)
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


#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"

#define INDICES
#include "indices.h"

#define SACTABLES
#include "sactbls.h"


typedef struct {
  char run, level, len;
} DCTtab;
typedef struct {
  int val, len;
} VLCtabI;
typedef struct {
  int val, run, sign;
} RunCoef;

/* local prototypes */
RunCoef vlc_word_decode _ANSI_ARGS_((int symbol_word, int *last)); 
RunCoef Decode_Escape_Char _ANSI_ARGS_((int intra, int *last));
int DecodeTCoef _ANSI_ARGS_((int position, int intra));



extern VLCtabI DCT3Dtab0[],DCT3Dtab1[],DCT3Dtab2[];



void getblock(comp,mode)
int comp;
int mode;
{
  int val, i, j, sign;
  unsigned int code;
  VLCtabI *tab;
  short *bp;
  int run, last, level, QP;
  short *qval;

  bp = ld->block[comp];   

  if(mode == 0)
	i = 1;
  else
	i = 0;

  /* decode AC coefficients */
  while(1) {
    code = showbits(12);
    if (code>=512)
      tab = &DCT3Dtab0[(code>>5)-16];
    else if (code>=128)
      tab = &DCT3Dtab1[(code>>2)-32];
    else if (code>=8)
      tab = &DCT3Dtab2[(code>>0)-8];
    else {
      if (!quiet)
        fprintf(stderr,"invalid Huffman code in getblock()\n");
      fault = 1;
      return;
    }

    flushbits(tab->len);

    run = (tab->val >> 4) & 255;
    level = tab->val & 15;
    last = (tab->val >> 12) & 1;

    if (trace) {
      printf(" (");
      printbits(code,12,tab->len);
    }

   if (tab->val==ESCAPE) { /* escape */
      if (trace) {
        putchar(' ');
        printbits(showbits(1),1,1);
      }
      last = getbits1();
      if (trace) {
        putchar(' ');
        printbits(showbits(6),6,6);
      }
      i += run = getbits(6);
      if (trace) {
        putchar(' ');
        printbits(showbits(8),8,8);
      }
      level = getbits(8);

      if ((sign = (level>=128)))
        val = 256 - level;
      else 
        val = level;
    }
    else {
      i+= run;
      val = level;
      sign = getbits(1);
      if (trace)
        printf("%d",sign);
    }

    if (i >= 64)
    {
      if (!quiet)
        fprintf(stderr,"DCT coeff index (i) out of bounds\n");
      fault = 1;
      return;
    }

    if (trace)
      printf("): %d/%d\n",run,sign ? -val : val);


    j = zig_zag_scan[i];
    qval = &bp[j];
    if (comp >= 6)
      QP = mmax (1, mmin( 31, ( bquant_tab[bquant] * quant ) >> 2 ));
    else 
      QP = quant;
      
    /* TMN3 dequantization */
    if ((QP % 2) == 1)
      *qval = ( sign ? -(QP * (2* val+1))  : QP * (2* val+1) );
    else
      *qval = ( sign ? -(QP * (2* val+1)-1): QP * (2* val+1)-1 );

    if (last) { /* That's it */
      if (trace)
        printf("last\n");
      return;
    }
    i++;
  }
}



/*********************************************************************
 *
 *        Name:        get_sac_block
 *
 *	Description:	Decodes blocks of Arithmetic Encoded DCT Coeffs.
 *        and performs Run Length Decoding and Coefficient        
 *        Dequantisation.
 *
 *	Input:        Picture block type and number.
 *
 *	Returns:	Nothing.
 *
 *	Side Effects:	
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/


void get_sac_block(int comp, int ptype)
{
  int position=0;
  int TCOEF_index, symbol_word;
  int last=0, QP, i, j;
  short *qval, *bp;
  RunCoef DCTcoef;

  bp = ld->block[comp];

  i = (ptype==0);

  while (!last) {	/* while there are DCT coefficients remaining */
    position++;	/* coefficient counter relates to Coeff. model */
    TCOEF_index = DecodeTCoef(position, !ptype);

    if (TCOEF_index == ESCAPE_INDEX) {        /* ESCAPE code encountered */
      DCTcoef = Decode_Escape_Char(!ptype, &last);
      if (trace)
        printf("ESC: ");
    }
    else {
      symbol_word = tcoeftab[TCOEF_index];

      DCTcoef = vlc_word_decode(symbol_word,&last);
    }

    if (trace) {
      printf("val: %d, run: %d, sign: %d, last: %d\n", 
             DCTcoef.val, DCTcoef.run, DCTcoef.sign, last);
    }

    i += DCTcoef.run;

    j = zig_zag_scan[i];

    qval = &bp[j];

    i++;

    if (comp >= 6)
      QP = mmax (1, mmin( 31, ( bquant_tab[bquant] * quant ) >> 2 ));
    else 
      QP = quant;

    if ((QP % 2) == 1)
      *qval = ( (DCTcoef.sign) ? -(QP * (2* (DCTcoef.val)+1))  : 
        QP * (2* (DCTcoef.val)+1) );
    else
      *qval = ( (DCTcoef.sign) ? -(QP * (2* (DCTcoef.val)+1)-1): 
        QP * (2* (DCTcoef.val)+1)-1 );
        
  }	
  return;
}

/*********************************************************************
 *
 *        Name:        vlc_word_decode
 *
 *	Description:	Fills Decoder FIFO after a fixed word length
 *        string has been detected.
 *
 *	Input:        Symbol to be decoded, last data flag.
 *
 *	Returns:	Decoded Symbol via the structure DCTcoeff.
 *
 *	Side Effects:	Updates last flag.
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

RunCoef vlc_word_decode(int symbol_word, int *last)
{
  int sign_index;
  RunCoef DCTcoef;

  *last = (symbol_word >> 12) & 01;
 
  DCTcoef.run = (symbol_word >> 4) & 255; 

  DCTcoef.val = (symbol_word) & 15;

  sign_index = decode_a_symbol(cumf_SIGN);	

  DCTcoef.sign = signtab[sign_index];
        
  return (DCTcoef);
} 

/*********************************************************************
 *
 *        Name:        Decode_Escape_Char
 *
 *	Description:	Decodes all components for a Symbol when an 
 *        ESCAPE character has been detected.
 *
 *	Input:        Picture Type and last data flag.
 *
 *	Returns:	Decoded Symbol via the structure DCTcoeff.
 *
 *	Side Effects:	Modifies last data flag.
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

RunCoef Decode_Escape_Char(int intra, int *last)
{
  int last_index, run, run_index, level, level_index;
  RunCoef DCTcoef;

  if (intra) {
    last_index = decode_a_symbol(cumf_LAST_intra);
    *last = last_intratab[last_index];
  }
  else {
    last_index = decode_a_symbol(cumf_LAST);
    *last = lasttab[last_index];
  }

  if (intra) 
    run_index = decode_a_symbol(cumf_RUN_intra);
  else
    run_index = decode_a_symbol(cumf_RUN);

  run = runtab[run_index];

  /*$if (mrun) run|=64;$*/

  DCTcoef.run = run;

  if (intra)
    level_index = decode_a_symbol(cumf_LEVEL_intra);
  else
    level_index = decode_a_symbol(cumf_LEVEL);

  if (trace)
    printf("level_idx: %d ",level_index);

  level = leveltab[level_index];

  if (level >128) 
    level -=256;

  if (level < 0) {
    DCTcoef.sign = 1;
    DCTcoef.val = abs(level);
  }

  else {	
    DCTcoef.sign = 0;
    DCTcoef.val = level;
  }

  return (DCTcoef);
        
}
/*********************************************************************
 *
 *        Name:        DecodeTCoef
 *
 *	Description:	Decodes a.c DCT Coefficients using the        
 *        relevant arithmetic decoding model.
 *
 *	Input:        DCT Coeff count and Picture Type.	
 *
 *	Returns:	Index to LUT
 *
 *	Side Effects:	None
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

int DecodeTCoef(int position, int intra)
{
  int index;

  switch (position) {
  case 1:
    {
      if (intra) 
        index = decode_a_symbol(cumf_TCOEF1_intra);
      else 
        index = decode_a_symbol(cumf_TCOEF1); 
      break; 
    }
  case 2:
    {
      if (intra) 
        index = decode_a_symbol(cumf_TCOEF2_intra);
      else
        index = decode_a_symbol(cumf_TCOEF2);
      break; 
    }
  case 3:
    {
      if (intra) 
        index = decode_a_symbol(cumf_TCOEF3_intra);
      else
        index = decode_a_symbol(cumf_TCOEF3);
      break; 
    }
  default: 
    {
      if (intra) 
        index = decode_a_symbol(cumf_TCOEFr_intra);
      else
        index = decode_a_symbol(cumf_TCOEFr);
      break; 
    }
  }

  return (index);
}

/************************************************************************
 *
 *  sac.c, part of tmndecode (H.263 decoder)
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

/*********************************************************************
 *        SAC Decoder Module
 *        Algorithm as Specified in H26P Annex -E
 *              (c) 1995 BT Labs
 *
 *	Author:	Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

#include <stdio.h>
#include <string.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"

#define   q1    16384
#define   q2    32768
#define   q3    49152
#define   top   65535

/* local prototypes */
void bit_out_psc_layer();

/*********************************************************************
 *        SAC Decoder Algorithm as Specified in H26P Annex -E
 *
 *        Name:        decode_a_symbol
 *
 *	Description:	Decodes an Aritmetically Encoded Symbol
 *
 *	Input:        array holding cumulative freq. data
 *        also uses static data for decoding endpoints
 *        and code_value variable
 *
 *	Returns:	Index to relevant symbol model
 *
 *	Side Effects:	Modifies low, high, length, cum and code_value
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/
 
static long low, high, code_value, bit, length, sacindex, cum, zerorun=0;

int decode_a_symbol(int cumul_freq[ ])
{

  length = high - low + 1;
  cum = (-1 + (code_value - low + 1) * cumul_freq[0]) / length;
  for (sacindex = 1; cumul_freq[sacindex] > cum; sacindex++);
  high = low - 1 + (length * cumul_freq[sacindex-1]) / cumul_freq[0];
  low += (length * cumul_freq[sacindex]) / cumul_freq[0];

  for ( ; ; ) {  
    if (high < q2) ;
    else if (low >= q2) {
      code_value -= q2; 
      low -= q2; 
      high -= q2;
    }
    else if (low >= q1 && high < q3) {
      code_value -= q1; 
      low -= q1; 
      high -= q1;
    }
    else {
      break;
    }
 
    low *= 2; 
    high = 2*high + 1;
    bit_out_psc_layer(); 
    code_value = 2*code_value + bit;
  }

  return (sacindex-1);
}
 
/*********************************************************************
 *
 *        Name:        decoder_reset
 *
 *	Description:	Fills Decoder FIFO after a fixed word length
 *        string has been detected.
 *
 *	Input:        None
 *
 *	Returns:	Nothing
 *
 *	Side Effects:	Fills Arithmetic Decoder FIFO
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

void decoder_reset( )
{
  int i;
  zerorun = 0;        /* clear consecutive zero's counter */
  code_value = 0;
  low = 0;
  high = top;
  for (i = 1;   i <= 16;   i++) {
    bit_out_psc_layer(); 
    code_value = 2 * code_value + bit;
  }
  if (trace)
    printf("Arithmetic Decoder Reset \n");
}

/*********************************************************************
 *
 *        Name:        bit_out_psc_layer
 *
 *	Description:	Gets a bit from the Encoded Stream, Checks for
 *        and removes any PSC emulation prevention bits
 *        inserted at the decoder, provides 'zeros' to the
 *        Arithmetic Decoder FIFO to allow it to finish 
 *        data prior to the next PSC. (Garbage bits)
 *
 *	Input:        None
 *
 *	Returns:	Nothing
 *
 *	Side Effects:	Gets a bit from the Input Data Stream
 *
 *	Author:        Wayne Ellis <ellis_w_wayne@bt-web.bt.co.uk>
 *
 *********************************************************************/

void bit_out_psc_layer()
{
  if (showbits(17)!=1) { /* check for startcode in Arithmetic Decoder FIFO */

    bit = getbits(1);

    if(zerorun > 13) {	/* if number of consecutive zeros = 14 */	 
      if (!bit) {
        if (trace)
          printf("PSC/GBSC, Header Data, or Encoded Stream Error \n");
        zerorun = 1;        
      }
      else { /* if there is a 'stuffing bit present */
        if (trace)
          printf("Removing Startcode Emulation Prevention bit \n");
        bit = getbits(1);        /* overwrite the last bit */	
        zerorun = !bit;        /* zerorun=1 if bit is a '0' */
      }
    }

    else { /* if consecutive zero's not exceeded 14 */

      if (!bit)
        zerorun++;
      else
        zerorun = 0;
    }

  } /* end of if !(showbits(17)) */

  else {
    bit = 0;
    if (trace)
      printf("Startcode Found:Finishing Arithmetic Decoding using 'Garbage bits'\n");
  }

   /*	
   printf("lastbit = %ld bit = %ld zerorun = %ld \n", lastbit, bit, zerorun); 
   lastbit = bit;
   */
        /* latent diagnostics */
        
}


/************************************************************************
 *
 *  gethdr.c, header decoding for tmndecode (H.263 decoder)
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
 */


#include <stdio.h>
#include <stdlib.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"

/* private prototypes */
static void getpicturehdr _ANSI_ARGS_((void));

/*
 * decode headers from one input stream
 * until an End of Sequence or picture start code
 * is found
 */

int getheader()
{
  unsigned int code, gob;

  /* look for startcode */
  startcode();
  code = getbits(PSC_LENGTH);
  gob = getbits(5);
  if (gob == SE_CODE) 
    return 0;
  if (gob == 0) {
    getpicturehdr();
    if (syntax_arith_coding)        /* reset decoder after receiving */
      decoder_reset();	        /* fixed length PSC string */
  }
  return gob + 1;
}


/* align to start of next startcode */

void startcode()
{
  /* search for new picture start code */
  while (showbits(PSC_LENGTH)!=1l) 
         flushbits(1);
}

/* decode picture header */

static void getpicturehdr()
{
  int pos, pei, tmp;

  pos = ld->bitcnt;
  prev_temp_ref = temp_ref;
  temp_ref = getbits(8);
  trd = temp_ref - prev_temp_ref;

  if (trd < 0)
    trd += 256;

  tmp = getbits(1); /* always "1" */
  if (!tmp)
    if (!quiet)
      printf("warning: spare in picture header should be \"1\"\n");
  tmp = getbits(1); /* always "0" */
  if (tmp)
    if (!quiet)
      printf("warning: H.261 distinction bit should be \"0\"\n");
  tmp = getbits(1); /* split_screen_indicator */
  if (tmp) {
    if (!quiet)
      printf("error: split-screen not supported in this version\n");
    exit (-1);
  }
  tmp = getbits(1); /* document_camera_indicator */
  if (tmp)
    if (!quiet)
      printf("warning: document camera indicator not supported in this version\n");

  tmp = getbits(1); /* freeze_picture_release */
  if (tmp)
    if (!quiet)
      printf("warning: frozen picture not supported in this version\n");

  source_format = getbits(3);
  pict_type = getbits(1);
  mv_outside_frame = getbits(1);
  long_vectors = (mv_outside_frame ? 1 : 0);
  syntax_arith_coding = getbits(1);
  adv_pred_mode = getbits(1);
  mv_outside_frame = (adv_pred_mode ? 1 : mv_outside_frame);
  pb_frame = getbits(1);
  quant = getbits(5);
  tmp = getbits(1);
  if (tmp) {
    if (!quiet)
      printf("error: CPM not supported in this version\n");
    exit(-1);
  }

  if (pb_frame) {
    trb = getbits(3);
    bquant = getbits(2);
  }
  else {
    trb = 0;
  }

#ifdef USE_TIME
  if (framerate > 0 && trd > 0)
    doframerate(0);
#endif	

  pei = getbits(1);
pspare:
  if (pei) {
     /* extra info for possible future backward compatible additions */
    getbits(8);  /* not used */
    pei = getbits(1);
    if (pei) goto pspare; /* keep on reading pspare until pei=0 */
  }


  if (verbose>0) {
    /*$printf("picture header (byte %d)\n",(pos>>3)-4);$*/
    if (verbose>1) {
      printf("  temp_ref=%d\n",temp_ref);
      /*$printf("  pict_type=%d\n",pict_type);
      printf("  source_format=%d\n", source_format);
      printf("  quant=%d\n",quant);
      if (syntax_arith_coding) 
        printf("  SAC coding mode used \n");
      if (mv_outside_frame)
        printf("  unrestricted motion vector mode used\n");
      if (adv_pred_mode)
        printf("  advanced prediction mode used\n");$*/
      if (pb_frame) {
        /*$printf("  pb-frames mode used\n");$*/
        printf("  trb=%d\n",trb);
        /*$printf("  bquant=%d\n", bquant);$*/
      }
    }
  }
}



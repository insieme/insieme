/************************************************************************
 *
 *  getpic.c, picture decoding for tmndecode (H.263 decoder)
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
 * modified by Wayne Ellis BT Labs to run Annex E Arithmetic Decoding
 *           <ellis_w_wayne@bt-web.bt.co.uk>
 *
 * based on mpeg2decode, (C) 1994, MPEG Software Simulation Group
 * and mpeg2play, (C) 1994 Stefan Eckart
 *                         <stefan@lis.e-technik.tu-muenchen.de>
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "config.h"
#include "tmndec.h"
#include "global.h"

#include "indices.h" 
#include "sactbls.h"

/* private prototypes*/
static void getMBs _ANSI_ARGS_((int framenum));
static void clearblock _ANSI_ARGS_((int comp));
static int motion_decode _ANSI_ARGS_((int vec,int pmv));
static int find_pmv _ANSI_ARGS_((int x, int y, int block, int comp));
static void addblock _ANSI_ARGS_((int comp, int bx, int by,int addflag));
static void reconblock_b _ANSI_ARGS_((int comp,int bx,int by,int mode,int bdx, int bdy));
static void find_bidir_limits _ANSI_ARGS_((int vec, int *start, int*stop, int nhv));
static void find_bidir_chroma_limits _ANSI_ARGS_((int vec, int *start, int*stop));
static void make_edge_image _ANSI_ARGS_ ((unsigned char *src, unsigned char *dst, int width, int height, int edge));
void interpolate_image _ANSI_ARGS_((unsigned char *in, unsigned char *out, int width, int height));




/* decode one frame or field picture */

void getpicture(framenum)
int *framenum;
{
  int i;
  unsigned char *tmp;

  for (i=0; i<3; i++) {
    tmp = oldrefframe[i];
    oldrefframe[i] = refframe[i];
    refframe[i] = tmp;
    newframe[i] = refframe[i];
  }

  if (mv_outside_frame && *framenum > 0) {
    make_edge_image(oldrefframe[0],edgeframe[0],coded_picture_width,
            coded_picture_height,32);
    make_edge_image(oldrefframe[1],edgeframe[1],chrom_width, chrom_height,16);
    make_edge_image(oldrefframe[2],edgeframe[2],chrom_width, chrom_height,16);
  }
  getMBs(*framenum);

  if (pb_frame) {
    if (expand && outtype == T_X11) {
      interpolate_image(bframe[0], exnewframe[0],
        coded_picture_width, coded_picture_height);
      interpolate_image(bframe[1], exnewframe[1], chrom_width, chrom_height);
      interpolate_image(bframe[2], exnewframe[2], chrom_width, chrom_height);
      
      storeframe(exnewframe, *framenum);
    }
    else
      storeframe(bframe,*framenum);

    *framenum += pb_frame;

#ifdef USE_TIME
    if (framerate > 0)
      doframerate(1);
#endif
  }

  if (expand && outtype == T_X11) {
    interpolate_image(newframe[0], exnewframe[0],
              coded_picture_width, coded_picture_height);
    interpolate_image(newframe[1], exnewframe[1], chrom_width, chrom_height);
    interpolate_image(newframe[2], exnewframe[2], chrom_width, chrom_height);

    storeframe(exnewframe, *framenum);
  }
  else
    storeframe(newframe,*framenum);


}


/* decode all macroblocks of the current picture */

static void getMBs(framenum)
int framenum;
{
  int comp;
  int MBA, MBAmax;
  int bx, by;

  int COD=0,MCBPC, CBPY, CBP=0, CBPB=0, MODB=0, Mode=0, DQUANT;
  int COD_index, CBPY_index, MODB_index, DQUANT_index, MCBPC_index;
  int INTRADC_index, YCBPB_index, UVCBPB_index, mvdbx_index, mvdby_index;
  int mvx, mvy, mvy_index, mvx_index, pmv0, pmv1, xpos, ypos, gob, i,k;
  int mvdbx=0, mvdby=0, pmvdbx, pmvdby, gfid, YCBPB, UVCBPB, gobheader_read;
  int startmv,stopmv,offset,bsize,last_done=0,pCBP=0,pCBPB=0,pCOD=0;
  int DQ_tab[4] = {-1,-2,1,2};
  short *bp;

  /* number of macroblocks per picture */
  MBAmax = mb_width*mb_height;

  MBA = 0; /* macroblock address */
  newgob = 0;

  /* mark MV's above the picture */
  for (i = 1; i < mb_width+1; i++) {
    for (k = 0; k < 5; k++) {
      MV[0][k][0][i] = NO_VEC;
      MV[1][k][0][i] = NO_VEC;
    }
    modemap[0][i] = MODE_INTRA;
  }
  /* zero MV's on the sides of the picture */
  for (i = 0; i < mb_height+1; i++) {
    for (k = 0; k < 5; k++) {
      MV[0][k][i][0] = 0;
      MV[1][k][i][0] = 0;
      MV[0][k][i][mb_width+1] = 0;
      MV[1][k][i][mb_width+1] = 0;
    }
    modemap[i][0] = MODE_INTRA;
    modemap[i][mb_width+1] = MODE_INTRA;
  }

  fault = 0;
  gobheader_read = 0;
  
  while(1) {

    if (trace)
      printf("frame %d, MB %d\n",framenum,MBA);
  resync:

    /* This version of the decoder does not resync on every possible
       error, and it does not do all possible error checks. It is not
       difficult to make it much more error robust, but I do not think
       it is necessary to include this in the freely available
       version. */

    if (fault) {
      printf("Warning: A Fault Condition Has Occurred - Resyncing \n");
      startcode();  /* sync on new startcode */
      fault = 0;
    }

    if (!(showbits(22)>>6)) { /* startcode */

      startcode();  
      /* in case of byte aligned start code, ie. PSTUF, GSTUF or ESTUF
         is used */
      
      if (showbits(22) == (32|SE_CODE)) { /* end of sequence */
        if (!(syntax_arith_coding && MBA < MBAmax)) {
          return;
        }
      }
      else if ((showbits(22) == PSC<<5) ) { /* new picture */
        if (!(syntax_arith_coding && MBA < MBAmax)) {
          return;
        }
      }
      else {
        if (!(syntax_arith_coding && MBA%mb_width)) {

          if (syntax_arith_coding) {   /* SAC hack to finish GOBs which   */
            gob = (showbits(22) & 31); /* end with MBs coded with no bits */
            if (gob * mb_width != MBA) 
              goto finish_gob;
          }

          gob = getheader() - 1;
          if (gob > mb_height) {
            if (!quiet)
              printf("GN out of range\n");
            return;
          }
          
          /* GFID is not allowed to change unless PTYPE in picture header 
             changes */
          gfid = getbits(2);
          /* NB: in error-prone environments the decoder can use this
             value to determine whether a picture header where the PTYPE
             has changed, has been lost */
          
          quant = getbits(5);
          if (trace)
            printf("GQUANT: %d\n", quant);
          xpos = 0;
          ypos = gob;
          MBA = ypos * mb_width;
          
          newgob = 1;
          gobheader_read = 1;
          if (syntax_arith_coding) 
            decoder_reset();	/* init. arithmetic decoder buffer after gob */
        }
      }
    }

  finish_gob:  /* SAC specific label */

    if (!gobheader_read) {
      xpos = MBA%mb_width;
      ypos = MBA/mb_width;
      if (xpos == 0 && ypos > 0)
        newgob = 0;
    }
    else 
      gobheader_read = 0;

    if (MBA>=MBAmax) 
      return; /* all macroblocks decoded */

  read_cod:
    if (syntax_arith_coding) { 
      if (pict_type == PCT_INTER) {
        COD_index = decode_a_symbol(cumf_COD);
        COD = codtab[COD_index];
        if (trace) {
          printf("Arithmetic Decoding Debug \n");
          printf("COD Index: %d COD: %d \n", COD_index, COD);
        }
      }
      else
        COD = 0;  /* COD not used in I-pictures, set to zero */
    }
    else {
      if (pict_type == PCT_INTER) 
        COD = showbits(1);
      else
        COD = 0; /* Intra picture -> not skipped */
    }

    if (!COD) {  /* COD == 0 --> not skipped */    

      if (syntax_arith_coding)  {
        if (pict_type == PCT_INTER) {
          MCBPC_index = decode_a_symbol(cumf_MCBPC);
          MCBPC = mcbpctab[MCBPC_index];
        }	
        else {
          MCBPC_index = decode_a_symbol(cumf_MCBPC_intra);
          MCBPC = mcbpc_intratab[MCBPC_index];
        }
        if (trace) 
          printf("MCBPC Index: %d MCBPC: %d \n",MCBPC_index, MCBPC);
      }

      else {
        if (pict_type == PCT_INTER)
          flushbits(1); /* flush COD bit */
        if (pict_type == PCT_INTRA) 
          MCBPC = getMCBPCintra();
        else
          MCBPC = getMCBPC();
      }

      if (fault) goto resync;
      
      if (MCBPC == 255) { /* stuffing */
        goto read_cod;   /* read next COD without advancing MB count */
      }

      else {             /* normal MB data */

        Mode = MCBPC & 7;

        /* MODB and CBPB */
        if (pb_frame) {
          CBPB = 0;
          if (syntax_arith_coding)  {
            MODB_index = decode_a_symbol(cumf_MODB);
            MODB = modb_tab[MODB_index];
          }
          else 
            MODB = getMODB();
          if (trace)
            printf("MODB: %d\n", MODB);
          if (MODB == PBMODE_CBPB_MVDB) {
            if (syntax_arith_coding)  {
              for(i=0; i<4; i++) {
        YCBPB_index = decode_a_symbol(cumf_YCBPB);
        YCBPB = ycbpb_tab[YCBPB_index];
        CBPB |= (YCBPB << (6-1-i));
              }
 
              for(i=4; i<6; i++) {
        UVCBPB_index = decode_a_symbol(cumf_UVCBPB);
        UVCBPB = uvcbpb_tab[UVCBPB_index];
        CBPB |= (UVCBPB << (6-1-i));
              }
            }
            else
              CBPB = getbits(6);
            if (trace)
              printf("CBPB = %d\n",CBPB);
          }
        }

        if (syntax_arith_coding) {

          if (Mode == MODE_INTRA || Mode == MODE_INTRA_Q) { /* Intra */
            CBPY_index = decode_a_symbol(cumf_CBPY_intra);
            CBPY = cbpy_intratab[CBPY_index];
          }
          else {
            CBPY_index = decode_a_symbol(cumf_CBPY);
            CBPY = cbpytab[CBPY_index];
        
          }
          if (trace)
            printf("CBPY Index: %d CBPY %d \n",CBPY_index, CBPY);

        }
        else 
          CBPY = getCBPY();
 
        /* Decode Mode and CBP */
        
        
        if (Mode == MODE_INTRA || Mode == MODE_INTRA_Q)
          {/* Intra */
            if (!syntax_arith_coding)	
              CBPY = CBPY^15;        /* needed in huffman coding only */
          }

        CBP = (CBPY << 2) | (MCBPC >> 4);
      }

      if (Mode == MODE_INTER4V && !adv_pred_mode) 
        if (!quiet)
          printf("8x8 vectors not allowed in normal prediction mode\n");
          /* Could set fault-flag and resync */


      if (Mode == MODE_INTER_Q || Mode == MODE_INTRA_Q) {
        /* Read DQUANT if necessary */

        if (syntax_arith_coding) {
          DQUANT_index = decode_a_symbol(cumf_DQUANT);
          DQUANT = dquanttab[DQUANT_index] - 2; 
          quant +=DQUANT;
          if (trace)
            printf("DQUANT Index: %d DQUANT %d \n",DQUANT_index, DQUANT);
        }
        else {
          DQUANT = getbits(2);
          quant += DQ_tab[DQUANT];
          if (trace) {
            printf("DQUANT (");
            printbits(DQUANT,2,2);
            printf("): %d = %d\n",DQUANT,DQ_tab[DQUANT]);
          }
        }

        if (quant > 31 || quant < 1) {
          if (!quiet)
            printf("Quantizer out of range: clipping\n");
          quant = mmax(1,mmin(31,quant));
          /* could set fault-flag and resync here */
        }
      }

      /* motion vectors */
      if (Mode == MODE_INTER || Mode == MODE_INTER_Q || 
          Mode == MODE_INTER4V || pb_frame) {

        if (Mode == MODE_INTER4V) { startmv = 1; stopmv = 4;}
        else { startmv = 0; stopmv = 0;}

        for (k = startmv; k <= stopmv; k++) {
          if (syntax_arith_coding) {
            mvx_index = decode_a_symbol(cumf_MVD);
            mvx = mvdtab[mvx_index];
            mvy_index = decode_a_symbol(cumf_MVD);
            mvy = mvdtab[mvy_index];
            if (trace)
              printf("mvx_index: %d mvy_index: %d \n", mvy_index, mvx_index);
          }
          else {
            mvx = getTMNMV();
            mvy = getTMNMV();
          }

          pmv0 = find_pmv(xpos,ypos,k,0);
          pmv1 = find_pmv(xpos,ypos,k,1);
          mvx = motion_decode(mvx, pmv0);
          mvy = motion_decode(mvy, pmv1);
          if (trace) {
            printf("mvx: %d\n", mvx);
            printf("mvy: %d\n", mvy);
          }
          /* Check mv's to prevent seg.faults when error rate is high */
          if (!mv_outside_frame) {
            bsize = k ? 8 : 16;
            offset = k ? (((k-1)&1)<<3) : 0;
            /* checking only integer component */
            if ((xpos<<4) + (mvx/2) + offset < 0 ||
        (xpos<<4) + (mvx/2) + offset > (mb_width<<4) - bsize) {
              if (!quiet)
        printf("mvx out of range: searching for sync\n");
              fault = 1;
            }
            offset = k ? (((k-1)&2)<<2) : 0;
            if ((ypos<<4) + (mvy/2) + offset < 0 ||
        (ypos<<4) + (mvy/2) + offset > (mb_height<<4) - bsize) {
              if (!quiet)
        printf("mvy out of range: searching for sync\n");
              fault = 1;
            }
          }
          MV[0][k][ypos+1][xpos+1] = mvx;
          MV[1][k][ypos+1][xpos+1] = mvy;
        }

        /* PB frame delta vectors */

        if (pb_frame) {
          if (MODB == PBMODE_MVDB || MODB == PBMODE_CBPB_MVDB) {
            if (syntax_arith_coding) {
              mvdbx_index = decode_a_symbol(cumf_MVD);
              mvdbx = mvdtab[mvdbx_index];
              
              mvdby_index = decode_a_symbol(cumf_MVD);
              mvdby = mvdtab[mvdby_index];
            }
            else {
              mvdbx = getTMNMV();
              mvdby = getTMNMV();
            }


            mvdbx = motion_decode(mvdbx, 0);
            mvdby = motion_decode(mvdby, 0);
            /* This will not work if the PB deltas are so large they
               require the second colums of the motion vector VLC
               table to be used.  To fix this it is necessary to
               calculate the MV predictor for the PB delta: TRB*MV/TRD
               here, and use this as the second parameter to
               motion_decode(). The B vector itself will then be
               returned from motion_decode(). This will have to be
               changed to the PB delta again, since it is the PB delta
               which is used later in this program. I don't think PB
               deltas outside the range mentioned above is useful, but
               you never know... */

            if (trace) {
              printf("MVDB x: %d\n", mvdbx);
              printf("MVDB y: %d\n", mvdby);
            }
          }
          else {
            mvdbx = 0; 
            mvdby = 0;
          }
        }
      }

      if (fault) goto resync;

    }
    else { /* COD == 1 --> skipped MB */
      if (MBA>=MBAmax)
        return; /* all macroblocks decoded */
      if (!syntax_arith_coding)
        if (pict_type == PCT_INTER)
          flushbits(1);

      Mode = MODE_INTER;
      
      /* Reset CBP */
      CBP = CBPB = 0;

      /* reset motion vectors */
      MV[0][0][ypos+1][xpos+1] = 0;
      MV[1][0][ypos+1][xpos+1] = 0;
      mvdbx = 0;
      mvdby = 0;
    }

    /* Store Mode*/
    modemap[ypos+1][xpos+1] = Mode;

    if (Mode == MODE_INTRA || Mode == MODE_INTRA_Q) 
      if (!pb_frame)
        MV[0][0][ypos+1][xpos+1]=MV[1][0][ypos+1][xpos+1] = 0;


  reconstruct_mb:

    /* pixel coordinates of top left corner of current macroblock */
    /* one delayed because of OBMC */
    if (xpos > 0) {
      bx = 16*(xpos-1);
      by = 16*ypos;
    }
    else {
      bx = coded_picture_width-16;
      by = 16*(ypos-1);
    }

    if (MBA > 0) {

      Mode = modemap[by/16+1][bx/16+1];

      /* forward motion compensation for B-frame */
      if (pb_frame)
        reconstruct(bx,by,0,pmvdbx,pmvdby);
      
      /* motion compensation for P-frame */
      if (Mode == MODE_INTER || Mode == MODE_INTER_Q || Mode == MODE_INTER4V)
        reconstruct(bx,by,1,0,0);

      /* copy or add block data into P-picture */
      for (comp=0; comp<blk_cnt; comp++) {
        /* inverse DCT */
        if (Mode == MODE_INTRA || Mode == MODE_INTRA_Q) {
          if (refidct)
            idctref(ld->block[comp]);
          else
            idct(ld->block[comp]);
          addblock(comp,bx,by,0);
        }
        else if ( (pCBP & (1<<(blk_cnt-1-comp))) ) {
          /* No need to to do this for blocks with no coeffs */
          if (refidct)
            idctref(ld->block[comp]);
          else
            idct(ld->block[comp]);
          addblock(comp,bx,by,1);
        }
      }
      
      
      if (pb_frame) {
        /* add block data into B-picture */
        for (comp = 6; comp<blk_cnt+6; comp++) {
          if (!pCOD || adv_pred_mode)
            reconblock_b(comp-6,bx,by,Mode,pmvdbx,pmvdby);
          if ( (pCBPB & (1<<(blk_cnt-1-comp%6))) ) {
            if (refidct)
              idctref(ld->block[comp]);
            else
              idct(ld->block[comp]);
            addblock(comp,bx,by,1);
          }
        }
      }
      
    } /* end if (MBA > 0) */

    if (!COD) {

      Mode = modemap[ypos+1][xpos+1];

      /* decode blocks */
      for (comp=0; comp<blk_cnt; comp++) {

        clearblock(comp);
        if (Mode == MODE_INTRA || Mode == MODE_INTRA_Q) { /* Intra */
          bp = ld->block[comp];
          if(syntax_arith_coding) {
            INTRADC_index = decode_a_symbol(cumf_INTRADC);
            bp[0] = intradctab[INTRADC_index];
            if (trace)
              printf("INTRADC Index: %d INTRADC: %d \n", INTRADC_index, bp[0]);
          }
          else {
            bp[0] = getbits(8);
            if (trace) {
              printf("DC[%d]: (",comp);
              printbits((int)bp[0],8,8);
              printf("): %d\n",(int)bp[0]);
            }
          }

          if (bp[0] == 128)
            if (!quiet)
              fprintf(stderr,"Illegal DC-coeff: 1000000\n");
          if (bp[0] == 255)  /* Spec. in H.26P, not in TMN4 */
            bp[0] = 128;
          bp[0] *= 8; /* Iquant */
          if ( (CBP & (1<<(blk_cnt-1-comp))) ) {
            if (!syntax_arith_coding)
              getblock(comp,0);
            else 
              get_sac_block(comp,0);
          }
        }
        else { /* Inter */
          if ( (CBP & (1<<(blk_cnt-1-comp))) ) {
            if (!syntax_arith_coding)
              getblock(comp,1);
            else
              get_sac_block(comp,1);
          }

        }
        if (fault) goto resync;
      }

      /* Decode B blocks */
      if (pb_frame) {
        for (comp=6; comp<blk_cnt+6; comp++) {
          clearblock(comp);
          if ( (CBPB & (1<<(blk_cnt-1-comp%6))) ) {
            if (!syntax_arith_coding)
              getblock(comp,1);
            else
              get_sac_block(comp,1);
          }
          if (fault) goto resync;
        }
      }
          
    }

    /* advance to next macroblock */
    MBA++;

    pCBP = CBP; pCBPB = CBPB; pCOD = COD;
    pmvdbx = mvdbx; pmvdby = mvdby;
    fflush(stdout);

    if (MBA >= MBAmax && !last_done) {
      COD = 1;
      xpos = 0;
      ypos++;
      last_done = 1;
      goto reconstruct_mb;
    }

  }
}

/* set block to zero */

static void clearblock(comp)
int comp;
{
  int *bp;
  int i;

  bp = (int *)ld->block[comp];

  for (i=0; i<8; i++)
  {
    bp[0] = bp[1] = bp[2] = bp[3] = 0;
    bp += 4;
  }
}


/* move/add 8x8-Block from block[comp] to refframe or bframe */

static void addblock(comp,bx,by,addflag)
int comp,bx,by,addflag;
{
  int cc,i, iincr, P = 1;
  unsigned char *rfp;
  short *bp;

  bp = ld->block[comp];

  if (comp >= 6) {
    /* This is a component for B-frame forward prediction */
    P = 0;
    addflag = 1;
    comp -= 6;
  }

  cc = (comp<4) ? 0 : (comp&1)+1; /* color component index */

  if (cc==0) {
    /* luminance */
    
    /* frame DCT coding */
    if (P)
      rfp = newframe[0]
        + coded_picture_width*(by+((comp&2)<<2)) + bx + ((comp&1)<<3);
    else
      rfp = bframe[0]
        + coded_picture_width*(by+((comp&2)<<2)) + bx + ((comp&1)<<3);
    iincr = coded_picture_width;
  }
  else {
    /* chrominance */

    /* scale coordinates */
    bx >>= 1;
    by >>= 1;
    /* frame DCT coding */
    if (P)
      rfp = newframe[cc] + chrom_width*by + bx;
    else
      rfp = bframe[cc] + chrom_width*by + bx;
    iincr = chrom_width;
  }


  if (addflag) {
    for (i=0; i<8; i++) {
      rfp[0] = clp[bp[0]+rfp[0]];
      rfp[1] = clp[bp[1]+rfp[1]];
      rfp[2] = clp[bp[2]+rfp[2]];
      rfp[3] = clp[bp[3]+rfp[3]];
      rfp[4] = clp[bp[4]+rfp[4]];
      rfp[5] = clp[bp[5]+rfp[5]];
      rfp[6] = clp[bp[6]+rfp[6]];
      rfp[7] = clp[bp[7]+rfp[7]];
      bp += 8;
      rfp+= iincr;
    }
  }
  else  {
    for (i=0; i<8; i++) {
      rfp[0] = clp[bp[0]];
      rfp[1] = clp[bp[1]];
      rfp[2] = clp[bp[2]];
      rfp[3] = clp[bp[3]];
      rfp[4] = clp[bp[4]];
      rfp[5] = clp[bp[5]];
      rfp[6] = clp[bp[6]];
      rfp[7] = clp[bp[7]];
      bp += 8;
      rfp += iincr;
    }
  }
}

/* bidirectionally reconstruct 8x8-Block from block[comp] to bframe */

static void reconblock_b(comp,bx,by,mode,bdx,bdy)
int comp,bx,by;
int mode,bdx,bdy;
{
  int cc,i,j,k, ii;
  unsigned char *bfr, *ffr;
  int BMVx, BMVy;
  int xa,xb,ya,yb,x,y,xvec,yvec,mvx,mvy;
  int xint,xhalf,yint,yhalf,pel;

  x = bx/16+1;y=by/16+1;

  if (mode == MODE_INTER4V) {
    if (comp < 4) {
      /* luma */
      mvx = MV[0][comp+1][y][x];
      mvy = MV[1][comp+1][y][x];
      BMVx = (bdx == 0 ? (trb-trd)* mvx/trd : trb * mvx/trd + bdx - mvx);
      BMVy = (bdy == 0 ? (trb-trd)* mvy/trd : trb * mvy/trd + bdy - mvy);
    }
    else {
      /* chroma */
      xvec = yvec = 0;
      for (k = 1; k <= 4; k++) {
        mvx = MV[0][k][y][x];
        mvy = MV[1][k][y][x];
        xvec += (bdx == 0 ? (trb-trd)* mvx/trd : trb * mvx/trd + bdx - mvx);
        yvec += (bdy == 0 ? (trb-trd)* mvy/trd : trb * mvy/trd + bdy - mvy);
      }
      
      /* chroma rounding (table 16/H.263) */
      BMVx = sign(xvec)*(roundtab[abs(xvec)%16] + (abs(xvec)/16)*2);
      BMVy = sign(yvec)*(roundtab[abs(yvec)%16] + (abs(yvec)/16)*2);
    }
  }
  else {
    if (comp < 4) {
      /* luma */
      mvx = MV[0][0][y][x];
      mvy = MV[1][0][y][x];
      BMVx = (bdx == 0 ? (trb-trd)* mvx/trd : trb * mvx/trd + bdx - mvx);
      BMVy = (bdy == 0 ? (trb-trd)* mvy/trd : trb * mvy/trd + bdy - mvy);
    }
    else {
      /* chroma */
      mvx = MV[0][0][y][x];
      mvy = MV[1][0][y][x];
      xvec = (bdx == 0 ? (trb-trd)* mvx/trd : trb * mvx/trd + bdx - mvx);
      yvec = (bdy == 0 ? (trb-trd)* mvy/trd : trb * mvy/trd + bdy - mvy);
      xvec *= 4;
      yvec *= 4;
      
      /* chroma rounding (table 16/H.263) */
      BMVx = sign(xvec)*(roundtab[abs(xvec)%16] + (abs(xvec)/16)*2);
      BMVy = sign(yvec)*(roundtab[abs(yvec)%16] + (abs(yvec)/16)*2);
    }
  }

  cc = (comp<4) ? 0 : (comp&1)+1; /* color component index */

  if (cc==0) {
    /* luminance */
    find_bidir_limits(BMVx,&xa,&xb,comp&1);
    find_bidir_limits(BMVy,&ya,&yb,(comp&2)>>1);
    bfr = bframe[0] +
      coded_picture_width*(by+((comp&2)<<2)) + bx + ((comp&1)<<3);
    ffr = newframe[0] +
      coded_picture_width*(by+((comp&2)<<2)) + bx + ((comp&1)<<3);
    ii = coded_picture_width;
  }
  else {
    /* chrominance */
    /* scale coordinates and vectors*/
    bx >>= 1;
    by >>= 1;

    find_bidir_chroma_limits(BMVx,&xa,&xb);
    find_bidir_chroma_limits(BMVy,&ya,&yb);

    bfr = bframe[cc]      + chrom_width*(by+((comp&2)<<2)) + bx + (comp&8);
    ffr = newframe[cc]    + chrom_width*(by+((comp&2)<<2)) + bx + (comp&8);
    ii = chrom_width;
  }

  xint = BMVx>>1;
  xhalf = BMVx - 2*xint;
  yint = BMVy>>1;
  yhalf = BMVy - 2*yint;

  ffr += xint + (yint+ya)*ii;
  bfr += ya*ii;
  
  if (!xhalf && !yhalf) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        pel = ffr[i];
        bfr[i] = ((unsigned int)(pel + bfr[i]))>>1;
      }
      bfr += ii;
      ffr += ii;
    }
  }
  else if (xhalf && !yhalf) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        pel = ((unsigned int)(ffr[i]+ffr[i+1]+1))>>1;
        bfr[i] = ((unsigned int)(pel + bfr[i]))>>1;
      }
      bfr += ii;
      ffr += ii;
    }
  }
  else if (!xhalf && yhalf) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        pel = ((unsigned int)(ffr[i]+ffr[ii+i]+1))>>1;
        bfr[i] = ((unsigned int)(pel + bfr[i]))>>1;
      }
      bfr += ii;
      ffr += ii;
    }
  }
  else { /* if (xhalf && yhalf) */
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        pel = ((unsigned int)(ffr[i]+ffr[i+1]+ffr[ii+i]+ffr[ii+i+1]+2))>>2;
        bfr[i] = ((unsigned int)(pel + bfr[i]))>>1;
      }
      bfr += ii;
      ffr += ii;
    }
  }
  return;
}

int motion_decode(vec,pmv)
int vec,pmv;
{
  if (vec > 31) vec -= 64;
  vec += pmv;
  if (!long_vectors) {
    if (vec > 31)
      vec -= 64;
    if (vec < -32)
      vec += 64;
  }
  else {
    if (pmv < -31 && vec < -63)
      vec += 64;
    if (pmv > 32 && vec > 63)
      vec -= 64;
  }
  return vec;
}


int find_pmv(x, y, block,comp)
int x,y,block,comp;
{
  int p1,p2,p3;
  int xin1,xin2,xin3;
  int yin1,yin2,yin3;
  int vec1,vec2,vec3;
  int l8,o8,or8;

  x++;y++;

  l8 = (modemap[y][x-1] == MODE_INTER4V ? 1 : 0);
  o8 =  (modemap[y-1][x] == MODE_INTER4V ? 1 : 0);
  or8 = (modemap[y-1][x+1] == MODE_INTER4V ? 1 : 0);

  switch (block) {
  case 0: 
    vec1 = (l8 ? 2 : 0) ; yin1 = y  ; xin1 = x-1;
    vec2 = (o8 ? 3 : 0) ; yin2 = y-1; xin2 = x;
    vec3 = (or8? 3 : 0) ; yin3 = y-1; xin3 = x+1;
    break;
  case 1:
    vec1 = (l8 ? 2 : 0) ; yin1 = y  ; xin1 = x-1;
    vec2 = (o8 ? 3 : 0) ; yin2 = y-1; xin2 = x;
    vec3 = (or8? 3 : 0) ; yin3 = y-1; xin3 = x+1;
    break;
  case 2:
    vec1 = 1            ; yin1 = y  ; xin1 = x;
    vec2 = (o8 ? 4 : 0) ; yin2 = y-1; xin2 = x;
    vec3 = (or8? 3 : 0) ; yin3 = y-1; xin3 = x+1;
    break;
  case 3:
    vec1 = (l8 ? 4 : 0) ; yin1 = y  ; xin1 = x-1;
    vec2 = 1            ; yin2 = y  ; xin2 = x;
    vec3 = 2            ; yin3 = y  ; xin3 = x;
    break;
  case 4:
    vec1 = 3            ; yin1 = y  ; xin1 = x;
    vec2 = 1            ; yin2 = y  ; xin2 = x;
    vec3 = 2            ; yin3 = y  ; xin3 = x;
    break;
  default:
    fprintf(stderr,"Illegal block number in find_pmv (getpic.c)\n");
    exit(1);
    break;
  }
  p1 = MV[comp][vec1][yin1][xin1];
  p2 = MV[comp][vec2][yin2][xin2];
  p3 = MV[comp][vec3][yin3][xin3];

  if (newgob && (block == 0 || block == 1 || block == 2))
    p2 = NO_VEC;

  if (p2 == NO_VEC) { p2 = p3 = p1; }

  return p1+p2+p3 - mmax(p1,mmax(p2,p3)) - mmin(p1,mmin(p2,p3));
}



void find_bidir_limits(vec, start, stop, nhv)
int vec;
int *start, *stop, nhv;
{
  /* limits taken from C loop in section G5 in H.263 */
  *start = mmax(0,(-vec+1)/2 - nhv*8);
  *stop = mmin(7,15-(vec+1)/2 - nhv*8);

  (*stop)++; /* I use < and not <= in the loop */
}

void find_bidir_chroma_limits(vec, start, stop)
int vec;
int *start, *stop;
{

  /* limits taken from C loop in section G5 in H.263 */
  *start = mmax(0,(-vec+1)/2);
  *stop = mmin(7,7-(vec+1)/2);

  (*stop)++; /* I use < and not <= in the loop */
  return;
}

void make_edge_image(src,dst,width,height,edge)
unsigned char *src, *dst;
int width,height,edge;
{
  int i,j;
  unsigned char *p1,*p2,*p3,*p4;
  unsigned char *o1,*o2,*o3,*o4;

  /* center image */
  p1 = dst;
  o1 = src;
  for (j = 0; j < height;j++) {
    for (i = 0; i < width; i++) {
      *(p1 + i) = *(o1 + i);
    }
    p1 += width + (edge<<1);
    o1 += width;
  }

  /* left and right edges */
  p1 = dst-1;
  o1 = src;
  for (j = 0; j < height;j++) {
    for (i = 0; i < edge; i++) {
      *(p1 - i) = *o1;
      *(p1 + width + i + 1) = *(o1 + width - 1);
    }
    p1 += width + (edge<<1);
    o1 += width;
  }    
    
  /* top and bottom edges */
  p1 = dst;
  p2 = dst + (width + (edge<<1))*(height-1);
  o1 = src;
  o2 = src + width*(height-1);
  for (j = 0; j < edge;j++) {
    p1 = p1 - (width + (edge<<1));
    p2 = p2 + (width + (edge<<1));
    for (i = 0; i < width; i++) {
      *(p1 + i) = *(o1 + i);
      *(p2 + i) = *(o2 + i);
    }
  }    

  /* corners */
  p1 = dst - (width+(edge<<1)) - 1;
  p2 = p1 + width + 1;
  p3 = dst + (width+(edge<<1))*(height)-1;
  p4 = p3 + width + 1;

  o1 = src;
  o2 = o1 + width - 1;
  o3 = src + width*(height-1);
  o4 = o3 + width - 1;
  for (j = 0; j < edge; j++) {
    for (i = 0; i < edge; i++) {
      *(p1 - i) = *o1;
      *(p2 + i) = *o2;
      *(p3 - i) = *o3;
      *(p4 + i) = *o4; 
    }
    p1 = p1 - (width + (edge<<1));
    p2 = p2 - (width + (edge<<1));
    p3 = p3 + width + (edge<<1);
    p4 = p4 + width + (edge<<1);
  }

}

  
void interpolate_image(in, out, width, height)
/* only used for displayed interpolated frames, not reconstructed ones */
unsigned char *in, *out;
int width, height;
{

  int y,w2;


  w2 = 2*width;

  /* Horizontally */
  #pragma omp parallel for schedule(dynamic)
  for (y = 0; y < height-1; y++) {
    unsigned char *pp = out + (w2<<1) * y;
    unsigned char *ii = in + width * y;

    for (int x = 0; x < width-1; x++) {
      int xx = x * 2;
      *(pp + xx) = *(ii + x);
      *(pp + xx+1) = ((unsigned int)(*(ii + x)  + *(ii + x + 1)))>>1;
      *(pp + w2 + xx) = ((unsigned int)(*(ii + x) + *(ii + x + width)))>>1;
      *(pp + w2 + xx+1) = ((unsigned int)(*(ii + x) + *(ii + x + 1) + 
           *(ii + x + width) + *(ii + x + width + 1)))>>2;
      
    }
    *(pp + w2 - 2) = *(ii + width - 1);
    *(pp + w2 - 1) = *(ii + width - 1);
    *(pp + w2 + w2 - 2) = *(ii + width + width - 1);
    *(pp + w2 + w2 - 1) = *(ii + width + width - 1);
  }

  unsigned char *pp = out + (w2<<1) * (height-1);
  unsigned char *ii = in + width * (height-1);

  /* last lines */
  #pragma omp parallel for schedule(dynamic)
  for (int x = 0; x < width-1; x++) {
    int xx = x * 2;
    *(pp+ xx) = *(ii + x);    
    *(pp+ xx+1) = ((unsigned int)(*(ii + x) + *(ii + x + 1) + 1))>>1;
    *(pp+ w2+ xx) = *(ii + x);    
    *(pp+ w2+ xx+1) = ((unsigned int)(*(ii + x) + *(ii + x + 1) + 1))>>1;
  }
  
  /* bottom right corner pels */
  *(pp + (width<<1) - 2) = *(ii + width -1);
  *(pp + (width<<1) - 1) = *(ii + width -1);
  *(pp + (width<<2) - 2) = *(ii + width -1);
  *(pp + (width<<2) - 1) = *(ii + width -1);

  return;
}
    


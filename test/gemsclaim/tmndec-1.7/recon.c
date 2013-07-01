/************************************************************************
 *
 *  recon.c, motion compensation routines for tmndecode (H.263 decoder)
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

/* private prototypes */
static void recon_comp _ANSI_ARGS_((unsigned char *src, unsigned char *dst,
  int lx, int lx2, int w, int h, int x, int y, int dx, int dy, int flag));
static void recon_comp_obmc _ANSI_ARGS_((unsigned char *src, unsigned char *dst,int lx,int lx2,int comp,int w,int h,int x,int y));
static void rec _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void recc _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void reco _ANSI_ARGS_((unsigned char *s, int *d, int lx, int lx2, int addflag,int c, int xa, int xb, int ya, int yb));
static void rech _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void rechc _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void recho _ANSI_ARGS_((unsigned char *s, int *d, int lx, int lx2, int addflag,int c, int xa, int xb, int ya, int yb));
static void recv _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void recvc _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void recvo _ANSI_ARGS_((unsigned char *s, int *d, int lx, int lx2, int addflag,int c, int xa, int xb, int ya, int yb));
static void rec4 _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void rec4c _ANSI_ARGS_((unsigned char *s, unsigned char *d, int lx, int lx2, int h));
static void rec4o _ANSI_ARGS_((unsigned char *s, int *d, int lx, int lx2, int addflag,int c, int xa, int xb, int ya, int yb));

void reconstruct(bx,by,P,bdx,bdy)
int bx, by;
int P,bdx,bdy;
{
  int w,h,lx,lx2,dx,dy,xp,yp,comp,sum;
  int x,y,mode,xvec,yvec;
  unsigned char *src[3];

  x = bx/16+1; y = by/16+1;
  lx = coded_picture_width;

  if (mv_outside_frame) {
    lx2 = coded_picture_width + 64;
    src[0] = edgeframe[0];
    src[1] = edgeframe[1];
    src[2] = edgeframe[2];
  }
  else {
    lx2 = coded_picture_width;
    src[0] = oldrefframe[0];
    src[1] = oldrefframe[1];
    src[2] = oldrefframe[2];
  }

  mode = modemap[y][x];

  if (P) {
    /* P prediction */
    if (adv_pred_mode) {
      w = 8; h = 8;
      /* Y*/
      for (comp = 0; comp < 4; comp++) {
        xp = bx + ((comp&1)<<3);
        yp = by + ((comp&2)<<2);
        recon_comp_obmc(src[0],newframe[0], lx,lx2,comp,w,h,xp,yp);
      }
      if (mode == MODE_INTER4V) {

        sum = MV[0][1][y][x]+MV[0][2][y][x]+ MV[0][3][y][x]+MV[0][4][y][x];
        dx = sign(sum)*(roundtab[abs(sum)%16] + (abs(sum)/16)*2);

        sum = MV[1][1][y][x]+MV[1][2][y][x]+ MV[1][3][y][x]+MV[1][4][y][x];
        dy = sign(sum)*(roundtab[abs(sum)%16] + (abs(sum)/16)*2);

      }
      else {
        dx = MV[0][0][y][x];
        dy = MV[1][0][y][x];
        /* chroma rounding */
        dx = ( dx % 4 == 0 ? dx >> 1 : (dx>>1)|1 );
        dy = ( dy % 4 == 0 ? dy >> 1 : (dy>>1)|1 );
      }
      lx>>=1;bx>>=1; lx2>>=1; 
      by>>=1;
      /* Chroma */
      recon_comp(src[1],newframe[1], lx,lx2,w,h,bx,by,dx,dy,1);
      recon_comp(src[2],newframe[2], lx,lx2,w,h,bx,by,dx,dy,2);
    }
    else { /* normal prediction mode */
      /* P prediction */
      w = 16; h = 16;
      dx = MV[0][0][y][x];
      dy = MV[1][0][y][x];
      
      /* Y */
      recon_comp(src[0],newframe[0], lx,lx2,w,h,bx,by,dx,dy,0);
      
      lx>>=1; w>>=1; bx>>=1; lx2>>=1; 
      h>>=1; by>>=1;  
      /* chroma rounding */
      dx = ( dx % 4 == 0 ? dx >> 1 : (dx>>1)|1 );
      dy = ( dy % 4 == 0 ? dy >> 1 : (dy>>1)|1 );

      /* Chroma */
      recon_comp(src[1],newframe[1], lx,lx2,w,h,bx,by,dx,dy,1);
      recon_comp(src[2],newframe[2], lx,lx2,w,h,bx,by,dx,dy,2);
    }
  }

  else {
    /* B forward prediction */
    if (adv_pred_mode) {
      if (mode == MODE_INTER4V) {
        w = 8; h = 8;
        /* Y*/
        xvec = yvec = 0;
        for (comp = 0; comp < 4; comp++) {
          xvec += dx = (trb)*MV[0][comp+1][y][x]/trd + bdx;
          yvec += dy = (trb)*MV[1][comp+1][y][x]/trd + bdy;
          xp = bx + ((comp&1)<<3);
          yp = by + ((comp&2)<<2);
          recon_comp(src[0],bframe[0], lx,lx2,w,h,xp,yp,dx,dy,0);
        }

        /* chroma rounding (table 16/H.263) */
        dx = sign(xvec)*(roundtab[abs(xvec)%16] + (abs(xvec)/16)*2);
        dy = sign(yvec)*(roundtab[abs(yvec)%16] + (abs(yvec)/16)*2);

        lx>>=1;bx>>=1; lx2>>=1;
        by>>=1;
        /* Chroma */
        recon_comp(src[1],bframe[1], lx,lx2,w,h,bx,by,dx,dy,1);
        recon_comp(src[2],bframe[2], lx,lx2,w,h,bx,by,dx,dy,2);
      }
      else {  /* adv_pred_mode but 16x16 vector */
        w = 16; h = 16;

        dx = (trb)*MV[0][0][y][x]/trd + bdx;
        dy = (trb)*MV[1][0][y][x]/trd + bdy;
        /* Y */
        recon_comp(src[0],bframe[0], lx,lx2,w,h,bx,by,dx,dy,0);
        
        lx>>=1; w>>=1; bx>>=1; lx2>>=1;
        h>>=1; by>>=1;  

        xvec = 4*dx;
        yvec = 4*dy;

        /* chroma rounding (table 16/H.263) */
        dx = sign(xvec)*(roundtab[abs(xvec)%16] + (abs(xvec)/16)*2);
        dy = sign(yvec)*(roundtab[abs(yvec)%16] + (abs(yvec)/16)*2);

        /* Chroma */
        recon_comp(src[1],bframe[1], lx,lx2,w,h,bx,by,dx,dy,1);
        recon_comp(src[2],bframe[2], lx,lx2,w,h,bx,by,dx,dy,2);
      }
    }
    else { /* normal B forward prediction */

      w = 16; h = 16;
      dx = (trb)*MV[0][0][y][x]/trd + bdx;
      dy = (trb)*MV[1][0][y][x]/trd + bdy;
      /* Y */
      recon_comp(src[0],bframe[0], lx,lx2,w,h,bx,by,dx,dy,0);

      lx>>=1; w>>=1; bx>>=1; lx2>>=1;
      h>>=1; by>>=1;  

      xvec = 4*dx;
      yvec = 4*dy;

      /* chroma rounding (table 16/H.263) */ 
      dx = sign(xvec)*(roundtab[abs(xvec)%16] + (abs(xvec)/16)*2);
      dy = sign(yvec)*(roundtab[abs(yvec)%16] + (abs(yvec)/16)*2);

      /* Chroma */
      recon_comp(src[1],bframe[1], lx,lx2,w,h,bx,by,dx,dy,1);
      recon_comp(src[2],bframe[2], lx,lx2,w,h,bx,by,dx,dy,2);
    }
  }
}




static void recon_comp(src,dst,lx,lx2,w,h,x,y,dx,dy,chroma)
unsigned char *src;
unsigned char *dst;
int lx,lx2;
int w,h;
int x,y;
int dx,dy;
int chroma;
{
  int xint, xh, yint, yh;
  unsigned char *s, *d;

  xint = dx>>1;
  xh = dx & 1;
  yint = dy>>1;
  yh = dy & 1;

  /* origins */
  s = src + lx2*(y+yint) + x + xint;
  d = dst + lx*y + x;

  if (!xh && !yh)
    if (w!=8)
      rec(s,d,lx,lx2,h);
    else
      recc(s,d,lx,lx2,h);
  else if (!xh && yh)
    if (w!=8)
      recv(s,d,lx,lx2,h);
    else 
      recvc(s,d,lx,lx2,h);
  else if (xh && !yh)
    if (w!=8)
      rech(s,d,lx,lx2,h);
    else
      rechc(s,d,lx,lx2,h);
  else /* if (xh && yh) */
    if (w!=8)
      rec4(s,d,lx,lx2,h);
    else
      rec4c(s,d,lx,lx2,h);
}

static void rec(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  int j;

  for (j=0; j<h; j++)
  {
    d[0] = s[0];
    d[1] = s[1];
    d[2] = s[2];
    d[3] = s[3];
    d[4] = s[4];
    d[5] = s[5];
    d[6] = s[6];
    d[7] = s[7];
    d[8] = s[8];
    d[9] = s[9];
    d[10] = s[10];
    d[11] = s[11];
    d[12] = s[12];
    d[13] = s[13];
    d[14] = s[14];
    d[15] = s[15];
    s+= lx2;
    d+= lx;
  }
}

static void recc(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  int j;

  for (j=0; j<h; j++)
  {
    d[0] = s[0];
    d[1] = s[1];
    d[2] = s[2];
    d[3] = s[3];
    d[4] = s[4];
    d[5] = s[5];
    d[6] = s[6];
    d[7] = s[7];
    s+= lx2;
    d+= lx;
  }
}

static void reco(s,d,lx,lx2,addflag,c,xa,xb,ya,yb)
unsigned char *s;
int *d;
int lx,lx2,addflag,c,xa,xb,ya,yb;
{
  int i,j;
  int *om;

  om = &OM[c][ya][0];

  if (!addflag) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        d[i] = s[i] * om[i];
      }
      s+= lx2;
      d+= lx;
      om+= 8;
    }
  }
  else {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        d[i] += s[i] * om[i];
      }
      s+= lx2;
      d+= lx;
      om+= 8;
    }
  }
}

static void rech(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp;
  int j;
  unsigned int s1,s2;

  sp = s;
  dp = d;
  for (j=0; j<h; j++)
  {
    s1=sp[0];
    dp[0] = (unsigned int)(s1+(s2=sp[1])+1)>>1;
    dp[1] = (unsigned int)(s2+(s1=sp[2])+1)>>1;
    dp[2] = (unsigned int)(s1+(s2=sp[3])+1)>>1;
    dp[3] = (unsigned int)(s2+(s1=sp[4])+1)>>1;
    dp[4] = (unsigned int)(s1+(s2=sp[5])+1)>>1;
    dp[5] = (unsigned int)(s2+(s1=sp[6])+1)>>1;
    dp[6] = (unsigned int)(s1+(s2=sp[7])+1)>>1;
    dp[7] = (unsigned int)(s2+(s1=sp[8])+1)>>1;
    dp[8] = (unsigned int)(s1+(s2=sp[9])+1)>>1;
    dp[9] = (unsigned int)(s2+(s1=sp[10])+1)>>1;
    dp[10] = (unsigned int)(s1+(s2=sp[11])+1)>>1;
    dp[11] = (unsigned int)(s2+(s1=sp[12])+1)>>1;
    dp[12] = (unsigned int)(s1+(s2=sp[13])+1)>>1;
    dp[13] = (unsigned int)(s2+(s1=sp[14])+1)>>1;
    dp[14] = (unsigned int)(s1+(s2=sp[15])+1)>>1;
    dp[15] = (unsigned int)(s2+sp[16]+1)>>1;
    sp+= lx2;
    dp+= lx;
  }
}

static void rechc(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp;
  int j;
  unsigned int s1,s2;

  sp = s;
  dp = d;
  for (j=0; j<h; j++)
  {
    s1=sp[0];
    dp[0] = (unsigned int)(s1+(s2=sp[1])+1)>>1;
    dp[1] = (unsigned int)(s2+(s1=sp[2])+1)>>1;
    dp[2] = (unsigned int)(s1+(s2=sp[3])+1)>>1;
    dp[3] = (unsigned int)(s2+(s1=sp[4])+1)>>1;
    dp[4] = (unsigned int)(s1+(s2=sp[5])+1)>>1;
    dp[5] = (unsigned int)(s2+(s1=sp[6])+1)>>1;
    dp[6] = (unsigned int)(s1+(s2=sp[7])+1)>>1;
    dp[7] = (unsigned int)(s2+sp[8]+1)>>1;
    sp+= lx2;
    dp+= lx;
  }
}

static void recho(s,d,lx,lx2,addflag,c,xa,xb,ya,yb)
unsigned char *s;
int *d;
int lx,lx2,addflag,c,xa,xb,ya,yb;
{
  int *dp,*om;
  unsigned char *sp;
  int i,j;

  sp = s;
  dp = d;
  om = &OM[c][ya][0];

  if (!addflag) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] = (((unsigned int)(sp[i] + sp[i+1]+1))>>1)*om[i];
      }
      sp+= lx2;
      dp+= lx;
      om+= 8;
    }
  }
  else {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] += (((unsigned int)(sp[i] + sp[i+1]+1))>>1)*OM[c][j][i];
      }
      sp+= lx2;
      dp+= lx;
      om+= 8;
    }
  }
}

static void recv(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp,*sp2;
  int j;

  sp = s;
  sp2 = s+lx2;
  dp = d;
  for (j=0; j<h; j++)
  {
    dp[0] = (unsigned int)(sp[0]+sp2[0]+1)>>1;
    dp[1] = (unsigned int)(sp[1]+sp2[1]+1)>>1;
    dp[2] = (unsigned int)(sp[2]+sp2[2]+1)>>1;
    dp[3] = (unsigned int)(sp[3]+sp2[3]+1)>>1;
    dp[4] = (unsigned int)(sp[4]+sp2[4]+1)>>1;
    dp[5] = (unsigned int)(sp[5]+sp2[5]+1)>>1;
    dp[6] = (unsigned int)(sp[6]+sp2[6]+1)>>1;
    dp[7] = (unsigned int)(sp[7]+sp2[7]+1)>>1;
    dp[8] = (unsigned int)(sp[8]+sp2[8]+1)>>1;
    dp[9] = (unsigned int)(sp[9]+sp2[9]+1)>>1;
    dp[10] = (unsigned int)(sp[10]+sp2[10]+1)>>1;
    dp[11] = (unsigned int)(sp[11]+sp2[11]+1)>>1;
    dp[12] = (unsigned int)(sp[12]+sp2[12]+1)>>1;
    dp[13] = (unsigned int)(sp[13]+sp2[13]+1)>>1;
    dp[14] = (unsigned int)(sp[14]+sp2[14]+1)>>1;
    dp[15] = (unsigned int)(sp[15]+sp2[15]+1)>>1;
    sp+= lx2;
    sp2+= lx2;
    dp+= lx;
  }
}

static void recvc(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp,*sp2;
  int j;

  sp = s;
  sp2 = s+lx2;
  dp = d;

  for (j=0; j<h; j++)
  {
    dp[0] = (unsigned int)(sp[0]+sp2[0]+1)>>1;
    dp[1] = (unsigned int)(sp[1]+sp2[1]+1)>>1;
    dp[2] = (unsigned int)(sp[2]+sp2[2]+1)>>1;
    dp[3] = (unsigned int)(sp[3]+sp2[3]+1)>>1;
    dp[4] = (unsigned int)(sp[4]+sp2[4]+1)>>1;
    dp[5] = (unsigned int)(sp[5]+sp2[5]+1)>>1;
    dp[6] = (unsigned int)(sp[6]+sp2[6]+1)>>1;
    dp[7] = (unsigned int)(sp[7]+sp2[7]+1)>>1;
    sp+= lx2;
    sp2+= lx2;
    dp+= lx;
  }
}

static void recvo(s,d,lx,lx2,addflag,c,xa,xb,ya,yb)
unsigned char *s;
int *d;
int lx,lx2,addflag,c,xa,xb,ya,yb;
{
  int *dp,*om;
  unsigned char *sp,*sp2;
  int i,j;

  sp = s;
  sp2 = s+lx2;
  dp = d;
  om = &OM[c][ya][0];

  if (!addflag) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] = (((unsigned int)(sp[i] + sp2[i]+1))>>1)*om[i];
      }
      sp+= lx2;
      sp2+= lx2;
      dp+= lx;
      om+= 8;
    }
  }
  else {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] += (((unsigned int)(sp[i] + sp2[i]+1))>>1)*om[i];
      }
      sp+= lx2;
      sp2+= lx2;
      dp+= lx;
      om+= 8;
    }
  }
}

static void rec4(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp,*sp2;
  int j;
  unsigned int s1,s2,s3,s4;

  sp = s;
  sp2 = s+lx2;
  dp = d;
  for (j=0; j<h; j++)
  {
    s1=sp[0]; s3=sp2[0];
    dp[0] = (unsigned int)(s1+(s2=sp[1])+s3+(s4=sp2[1])+2)>>2;
    dp[1] = (unsigned int)(s2+(s1=sp[2])+s4+(s3=sp2[2])+2)>>2;
    dp[2] = (unsigned int)(s1+(s2=sp[3])+s3+(s4=sp2[3])+2)>>2;
    dp[3] = (unsigned int)(s2+(s1=sp[4])+s4+(s3=sp2[4])+2)>>2;
    dp[4] = (unsigned int)(s1+(s2=sp[5])+s3+(s4=sp2[5])+2)>>2;
    dp[5] = (unsigned int)(s2+(s1=sp[6])+s4+(s3=sp2[6])+2)>>2;
    dp[6] = (unsigned int)(s1+(s2=sp[7])+s3+(s4=sp2[7])+2)>>2;
    dp[7] = (unsigned int)(s2+(s1=sp[8])+s4+(s3=sp2[8])+2)>>2;
    dp[8] = (unsigned int)(s1+(s2=sp[9])+s3+(s4=sp2[9])+2)>>2;
    dp[9] = (unsigned int)(s2+(s1=sp[10])+s4+(s3=sp2[10])+2)>>2;
    dp[10] = (unsigned int)(s1+(s2=sp[11])+s3+(s4=sp2[11])+2)>>2;
    dp[11] = (unsigned int)(s2+(s1=sp[12])+s4+(s3=sp2[12])+2)>>2;
    dp[12] = (unsigned int)(s1+(s2=sp[13])+s3+(s4=sp2[13])+2)>>2;
    dp[13] = (unsigned int)(s2+(s1=sp[14])+s4+(s3=sp2[14])+2)>>2;
    dp[14] = (unsigned int)(s1+(s2=sp[15])+s3+(s4=sp2[15])+2)>>2;
    dp[15] = (unsigned int)(s2+sp[16]+s4+sp2[16]+2)>>2;
    sp+= lx2;
    sp2+= lx2;
    dp+= lx;
  }
}

static void rec4c(s,d,lx,lx2,h)
unsigned char *s, *d;
int lx,lx2,h;
{
  unsigned char *dp,*sp,*sp2;
  int j;
  unsigned int s1,s2,s3,s4;

  sp = s;
  sp2 = s+lx2;
  dp = d;
  for (j=0; j<h; j++)
  {
    s1=sp[0]; s3=sp2[0];
    dp[0] = (unsigned int)(s1+(s2=sp[1])+s3+(s4=sp2[1])+2)>>2;
    dp[1] = (unsigned int)(s2+(s1=sp[2])+s4+(s3=sp2[2])+2)>>2;
    dp[2] = (unsigned int)(s1+(s2=sp[3])+s3+(s4=sp2[3])+2)>>2;
    dp[3] = (unsigned int)(s2+(s1=sp[4])+s4+(s3=sp2[4])+2)>>2;
    dp[4] = (unsigned int)(s1+(s2=sp[5])+s3+(s4=sp2[5])+2)>>2;
    dp[5] = (unsigned int)(s2+(s1=sp[6])+s4+(s3=sp2[6])+2)>>2;
    dp[6] = (unsigned int)(s1+(s2=sp[7])+s3+(s4=sp2[7])+2)>>2;
    dp[7] = (unsigned int)(s2+sp[8]+s4+sp2[8]+2)>>2;
    sp+= lx2;
    sp2+= lx2;
    dp+= lx;
  }
}

static void rec4o(s,d,lx,lx2,addflag,c,xa,xb,ya,yb)
unsigned char *s;
int *d;
int lx,lx2,addflag,c,xa,xb,ya,yb;
{
  int *dp,*om;
  unsigned char *sp,*sp2;
  int i,j;

  sp = s;
  sp2 = s+lx2;
  dp = d;
  om = &OM[c][ya][0];

  if (!addflag) {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] = (((unsigned int)(sp[i]+sp2[i]+sp[i+1]+sp2[i+1]+2))>>2)*om[i];
      }
      sp+= lx2;
      sp2+= lx2;
      dp+= lx;
      om+= 8;
    }
  }
  else {
    for (j = ya; j < yb; j++) {
      for (i = xa; i < xb; i++) {
        dp[i] += (((unsigned int)(sp[i]+sp2[i]+sp[i+1]+sp2[i+1]+2))>>2)*om[i];
      }
      sp+= lx2;
      sp2+= lx2;
      dp+= lx;
      om += 8;
    }
  }
}

static void recon_comp_obmc(src,dst,lx,lx2,comp,w,h,x,y)
unsigned char *src;
unsigned char *dst;
int lx,lx2,comp;
int w,h;
int x,y;
{
  int j,k;
  int xmb,ymb;
  int c8,t8,l8,r8;
  int ti8,li8,ri8;
  int xit,xib,xir,xil;
  int yit,yib,yir,yil;
  int vect,vecb,vecr,vecl;
  int nx[5],ny[5],xint[5],yint[5],xh[5],yh[5];
  int p[64],*pd;
  unsigned char *d,*s[5];

  xmb = (x>>4)+1;
  ymb = (y>>4)+1;

  c8  = (modemap[ymb][xmb] == MODE_INTER4V ? 1 : 0);

  t8  = (modemap[ymb-1][xmb] == MODE_INTER4V ? 1 : 0);
  ti8 = (modemap[ymb-1][xmb] == MODE_INTRA ? 1 : 0);
  ti8 = (modemap[ymb-1][xmb] == MODE_INTRA_Q ? 1 : ti8);

  l8  = (modemap[ymb][xmb-1] == MODE_INTER4V ? 1 : 0);
  li8 = (modemap[ymb][xmb-1] == MODE_INTRA ? 1 : 0);
  li8 = (modemap[ymb][xmb-1] == MODE_INTRA_Q ? 1 : li8);
  
  r8  = (modemap[ymb][xmb+1] == MODE_INTER4V ? 1 : 0);
  ri8 = (modemap[ymb][xmb+1] == MODE_INTRA ? 1 : 0);
  ri8 = (modemap[ymb][xmb+1] == MODE_INTRA_Q ? 1 : ri8);

  if (pb_frame) {
    ti8 = li8 = ri8 = 0;
  }

  switch (comp+1) {

  case 1:
    vect = (ti8 ? (c8 ? 1 : 0) : (t8 ? 3 : 0)); 
    yit  = (ti8 ? ymb : ymb - 1); 
    xit = xmb;

    vecb = (c8 ? 3 : 0) ; yib = ymb; xib = xmb;

    vecl = (li8 ? (c8 ? 1 : 0) : (l8 ? 2 : 0)); 
    yil = ymb; 
    xil = (li8 ? xmb : xmb-1);

    vecr = (c8 ? 2 : 0) ; yir = ymb; xir = xmb;

    /* edge handling */
    if (ymb == 1) {
      yit = ymb;
      vect = (c8 ? 1 : 0);
    }
    if (xmb == 1) {
      xil = xmb;
      vecl = (c8 ? 1 : 0);
    }
    break;
  
  case 2:
    vect = (ti8 ? (c8 ? 2 : 0) : (t8 ? 4 : 0)); 
    yit = (ti8 ? ymb : ymb-1); 
    xit = xmb;

    vecb = (c8 ? 4 : 0) ; yib = ymb; xib = xmb;
    vecl = (c8 ? 1 : 0) ; yil = ymb; xil = xmb;

    vecr = (ri8 ? (c8 ? 2 : 0) : (r8 ? 1 : 0)); 
    yir = ymb; 
    xir = (ri8 ? xmb : xmb+1);

    /* edge handling */
    if (ymb == 1) {
      yit = ymb;
      vect = (c8 ? 2 : 0);
    }
    if (xmb == mb_width) {
      xir = xmb;
      vecr = (c8 ? 2 : 0);
    }
    break;

  case 3:
    vect = (c8 ? 1 : 0) ; yit = ymb  ; xit = xmb;
    vecb = (c8 ? 3 : 0) ; yib = ymb  ; xib = xmb;
      
    vecl = (li8 ? (c8 ? 3 : 0) : (l8 ? 4 : 0)); 
    yil = ymb;  
    xil = (li8 ? xmb : xmb-1);
    
    vecr = (c8 ? 4 : 0) ; yir = ymb  ; xir = xmb;

    /* edge handling */
    if (xmb == 1) {
      xil = xmb;
      vecl = (c8 ? 3 : 0);
    }
    break;

  case 4:
    vect = (c8 ? 2 : 0) ; yit = ymb  ; xit = xmb;
    vecb = (c8 ? 4 : 0) ; yib = ymb  ; xib = xmb;
    vecl = (c8 ? 3 : 0) ; yil = ymb  ; xil = xmb;

    vecr = (ri8 ? (c8 ? 4 : 0) : (r8 ? 3 : 0)); 
    yir = ymb; 
    xir = (ri8 ? xmb : xmb+1);

    /* edge handling */
    if (xmb == mb_width) {
      xir = xmb;
      vecr = (c8 ? 4 : 0);
    }
    break;

  default:
    fprintf(stderr,"Illegal block number in recon_comp_obmc (recon.c)\n");
    exit(1);
    break;
  }

  nx[0] = MV[0][c8 ? comp + 1 : 0][ymb][xmb];
  ny[0] = MV[1][c8 ? comp + 1 : 0][ymb][xmb];
  
  nx[1] = MV[0][vect][yit][xit]; ny[1] = MV[1][vect][yit][xit];
  nx[2] = MV[0][vecb][yib][xib]; ny[2] = MV[1][vecb][yib][xib];
  nx[3] = MV[0][vecr][yir][xir]; ny[3] = MV[1][vecr][yir][xir];
  nx[4] = MV[0][vecl][yil][xil]; ny[4] = MV[1][vecl][yil][xil];

  for (k = 0; k < 5; k++) {
    xint[k] = nx[k]>>1;
    xh[k] = nx[k] & 1;
    yint[k] = ny[k]>>1;
    yh[k] = ny[k] & 1;
    s[k] = src + lx2*(y+yint[k]) + x + xint[k];
  }
  
  d = dst + lx*y + x;

  if (!xh[0] && !yh[0])
    reco(s[0],&p[0],8,lx2,0,0,0,8,0,8);
  else if (!xh[0] && yh[0])
    recvo(s[0],&p[0],8,lx2,0,0,0,8,0,8);
  else if (xh[0] && !yh[0])
    recho(s[0],&p[0],8,lx2,0,0,0,8,0,8);
  else /* if (xh[] && yh[]) */
    rec4o(s[0],&p[0],8,lx2,0,0,0,8,0,8);

  if (!xh[1] && !yh[1])
    reco(s[1],&p[0],8,lx2,1,1,0,8,0,4);
  else if (!xh[1] && yh[1])
    recvo(s[1],&p[0],8,lx2,1,1,0,8,0,4);
  else if (xh[1] && !yh[1])
    recho(s[1],&p[0],8,lx2,1,1,0,8,0,4);
  else /* if (xh[] && yh[]) */
    rec4o(s[1],&p[0],8,lx2,1,1,0,8,0,4);

  if (!xh[2] && !yh[2])
    reco(s[2]+(lx2<<2),&p[32],8,lx2,1,2,0,8,4,8);
  else if (!xh[2] && yh[2])
    recvo(s[2]+(lx2<<2),&p[32],8,lx2,1,2,0,8,4,8);
  else if (xh[2] && !yh[2])
    recho(s[2]+(lx2<<2),&p[32],8,lx2,1,2,0,8,4,8);
  else /* if (xh[] && yh[]) */
    rec4o(s[2]+(lx2<<2),&p[32],8,lx2,1,2,0,8,4,8);

  if (!xh[3] && !yh[3])
    reco(s[3],&p[0],8,lx2,1,3,4,8,0,8);
  else if (!xh[3] && yh[3])
    recvo(s[3],&p[0],8,lx2,1,3,4,8,0,8);
  else if (xh[3] && !yh[3])
    recho(s[3],&p[0],8,lx2,1,3,4,8,0,8);
  else /* if (xh[] && yh[]) */
    rec4o(s[3],&p[0],8,lx2,1,3,4,8,0,8);

  if (!xh[4] && !yh[4])
    reco(s[4],&p[0],8,lx2,1,4,0,4,0,8);
  else if (!xh[4] && yh[4])
    recvo(s[4],&p[0],8,lx2,1,4,0,4,0,8);
  else if (xh[4] && !yh[4])
    recho(s[4],&p[0],8,lx2,1,4,0,4,0,8);
  else /* if (xh[] && yh[]) */
    rec4o(s[4],&p[0],8,lx2,1,4,0,4,0,8);

  pd = &p[0];
  for (j = 0; j < 8; j++) {
    d[0] = (pd[0] + 4 )>>3;	
    d[1] = (pd[1] + 4 )>>3;	
    d[2] = (pd[2] + 4 )>>3;	
    d[3] = (pd[3] + 4 )>>3;	
    d[4] = (pd[4] + 4 )>>3;	
    d[5] = (pd[5] + 4 )>>3;	
    d[6] = (pd[6] + 4 )>>3;	
    d[7] = (pd[7] + 4 )>>3;	
    d += lx;
    pd += 8;
  }

}


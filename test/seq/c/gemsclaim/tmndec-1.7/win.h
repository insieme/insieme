/************************************************************************
 *
 *  win.h, display routines for Win32 for tmndecode (H.263 decoder)
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
 *  
 ************************************************************************/

/************************************************************************
 *
 * These routines were written by Intel Architecture Labs (IAL)
 *
 ************************************************************************/


/*
Copyright © 1996 Intel Corporation
All Rights Reserved

Permission is granted to use, copy and distribute the software in this
file for any purpose and without fee, provided, that the above
copyright notice and this statement appear in all copies.  Intel makes
no representations about the suitability of this software for any
purpose.  This software is provided "AS IS."

Intel specifically disclaims all warranties, express or implied, and
all liability, including consequential and other indirect damages, for
the use of this software, including liability for infringement of any
proprietary rights, and including the warranties of merchantability
and fitness for a particular purpose.  Intel does not assume any
responsibility for any errors which may appear in this software nor
any responsibility to update it.  
*/

#ifdef WINDOWS

#include <windows.h>
#include <process.h>
#include <vfw.h>
#include <memory.h>


typedef struct
{
  HANDLE hThread;
  HANDLE hEvent;
  HWND hWnd;
  MSG msg;
  WNDCLASS wc;
  HDRAWDIB hDrawDib;
  HDC hDC;
  BITMAPINFOHEADER biHeader;
  char lpszAppName[15];
  DWORD dwThreadID;
  BOOL imageIsReady;
  unsigned char *bufRGB;
  RECT rect;
  unsigned char *src[3];

  int width, height;
  int zoom, oldzoom;
  int windowDismissed;
	
} T_VDWINDOW;


#define VIDEO_BEGIN			    (WM_USER + 0)
#define VIDEO_DRAW_FRAME	  (WM_USER + 1)
#define VIDEO_REDRAW_FRAME	(WM_USER + 2)
#define VIDEO_END			      (WM_USER + 3)


int initDisplay (int pels, int lines);
int displayImage (unsigned char *lum, unsigned char *Cr, unsigned char *Cb);
int closeDisplay ();

void DisplayWinMain (void *);
LONG APIENTRY MainWndProc (HWND, UINT, UINT, LONG);
int DrawDIB ();
void init_dither_tab();
void ConvertYUVtoRGB(
  unsigned char *src0,
  unsigned char *src1,
  unsigned char *src2,
  unsigned char *dst_ori,
  int width,
  int height
);
int InitDisplayWindowThread ();

#endif

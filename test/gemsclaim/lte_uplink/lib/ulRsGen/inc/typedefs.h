/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef ULRSGEN_TYPEDEFS_H
#define ULRSGEN_TYPEDEFS_H

#ifdef __STDC__
#ifndef __GNUC__

/* General C typedefs */
typedef unsigned int  U16;
typedef unsigned long U32;
typedef int           S16;
typedef long          S32;
typedef int           BOOL;
typedef U16           PID;
typedef U16           THREADID;
typedef U16           LBID;
typedef U16           SEMID;
typedef struct { U32 msw, lsw; } U64;

/* Flex C Special typedefs */
typedef unsigned char CHAR;

/* Only for OSE compatibility, used when including ose header files */
/* Has no meaning what so ever in LPP, use U16 instead.             */
typedef U32 SIGSELECT;

/* The return of U8, only a temp. solution */
typedef unsigned char U8;

#else

typedef unsigned short  U16;
typedef unsigned int    U32;
typedef short           S16;
typedef int             S32;
typedef short           BOOL;
typedef U16             PID;
typedef U16             LBID;
typedef U16             THREADID;
typedef U16             SEMID;
typedef struct { U32 msw, lsw; } U64;
typedef unsigned short CHAR;
typedef U32 SIGSELECT;
typedef unsigned short U8;

#endif

#endif

#endif

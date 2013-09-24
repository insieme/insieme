/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#ifndef _X86INTRIN_H_INCLUDED
# error "Never use <ia32intrin.h> directly; include <x86intrin.h> instead."
#endif

//INSIEME HACK
#ifdef __clang__
extern int __builtin_ia32_bsrsi(int);
extern int __builtin_ia32_bsrdi(long long);
extern unsigned long long __builtin_ia32_rdpmc(int);
extern unsigned long long __builtin_ia32_rdtsc(void);
extern unsigned long long __builtin_ia32_rdtscp(unsigned int *);
extern unsigned char __builtin_ia32_rolqi(unsigned char, int);
extern unsigned short __builtin_ia32_rolhi(unsigned short, int);
extern unsigned char __builtin_ia32_rorqi(unsigned char, int);
extern unsigned short __builtin_ia32_rorhi(unsigned short, int);
extern void __builtin_ia32_pause(void);
#endif
//INSIEME HACK

/* 32bit bsf */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bsfd (int __X)
{
  return __builtin_ctz (__X);
}

/* 32bit bsr */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bsrd (int __X)
{
  return __builtin_ia32_bsrsi (__X);
}

/* 32bit bswap */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bswapd (int __X)
{
  return __builtin_bswap32 (__X);
}

#ifdef __SSE4_2__
/* 32bit accumulate CRC32 (polynomial 0x11EDC6F41) value.  */
extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__crc32b (unsigned int __C, unsigned char __V)
{
  return __builtin_ia32_crc32qi (__C, __V);
}

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__crc32w (unsigned int __C, unsigned short __V)
{
  return __builtin_ia32_crc32hi (__C, __V);
}

extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__crc32d (unsigned int __C, unsigned int __V)
{
  return __builtin_ia32_crc32si (__C, __V);
}
#endif /* SSE4.2 */

/* 32bit popcnt */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__popcntd (unsigned int __X)
{
  return __builtin_popcount (__X);
}

/* rdpmc */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rdpmc (int __S)
{
  return __builtin_ia32_rdpmc (__S);
}

/* rdtsc */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rdtsc (void)
{
  return __builtin_ia32_rdtsc ();
}

/* rdtscp */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rdtscp (unsigned int *__A)
{
  return __builtin_ia32_rdtscp (__A);
}

/* 8bit rol */
extern __inline unsigned char
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rolb (unsigned char __X, int __C)
{
  return __builtin_ia32_rolqi (__X, __C);
}

/* 16bit rol */
extern __inline unsigned short
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rolw (unsigned short __X, int __C)
{
  return __builtin_ia32_rolhi (__X, __C);
}

/* 32bit rol */
extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rold (unsigned int __X, int __C)
{
  return (__X << __C) | (__X >> (32 - __C));
}

/* 8bit ror */
extern __inline unsigned char
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rorb (unsigned char __X, int __C)
{
  return __builtin_ia32_rorqi (__X, __C);
}

/* 16bit ror */
extern __inline unsigned short
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rorw (unsigned short __X, int __C)
{
  return __builtin_ia32_rorhi (__X, __C);
}

/* 32bit ror */
extern __inline unsigned int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rord (unsigned int __X, int __C)
{
  return (__X >> __C) | (__X << (32 - __C));
}

/* Pause */
extern __inline void
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__pause (void)
{
  __builtin_ia32_pause ();
}

#ifdef __x86_64__
/* 64bit bsf */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bsfq (long long __X)
{
  return __builtin_ctzll (__X);
}

/* 64bit bsr */
extern __inline int
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bsrq (long long __X)
{
  return __builtin_ia32_bsrdi (__X);
}

/* 64bit bswap */
extern __inline long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__bswapq (long long __X)
{
  return __builtin_bswap64 (__X);
}

#ifdef __SSE4_2__
/* 64bit accumulate CRC32 (polynomial 0x11EDC6F41) value.  */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__crc32q (unsigned long long __C, unsigned long long __V)
{
  return __builtin_ia32_crc32di (__C, __V);
}
#endif

/* 64bit popcnt */
extern __inline long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__popcntq (unsigned long long __X)
{
  return __builtin_popcountll (__X);
}

/* 64bit rol */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rolq (unsigned long long __X, int __C)
{
  return (__X << __C) | (__X >> (64 - __C));
}

/* 64bit ror */
extern __inline unsigned long long
__attribute__((__gnu_inline__, __always_inline__, __artificial__))
__rorq (unsigned long long __X, int __C)
{
  return (__X >> __C) | (__X << (64 - __C));
}

#define _bswap64(a)		__bswapq(a)
#define _popcnt64(a)		__popcntq(a)
#define _lrotl(a,b)		__rolq((a), (b))
#define _lrotr(a,b)		__rorq((a), (b))
#else
#define _lrotl(a,b)		__rold((a), (b))
#define _lrotr(a,b)		__rord((a), (b))
#endif

#define _bit_scan_forward(a)	__bsfd(a)
#define _bit_scan_reverse(a)	__bsrd(a)
#define _bswap(a)		__bswapd(a)
#define _popcnt32(a)		__popcntd(a)
#define _rdpmc(a)		__rdpmc(a)
#define _rdtsc()		__rdtsc()
#define _rdtscp(a)		__rdtscp(a)
#define _rotwl(a,b)		__rolw((a), (b))
#define _rotwr(a,b)		__rorw((a), (b))
#define _rotl(a,b)		__rold((a), (b))
#define _rotr(a,b)		__rord((a), (b))

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

typedef double __v2df __attribute__ ((__vector_size__ (16)));
typedef int __v4si __attribute__ ((__vector_size__ (16)));
typedef float __v4sf __attribute__ ((__vector_size__ (16)));
typedef float __v2sf __attribute__ ((__vector_size__ (8)));
typedef long long __v2di __attribute__ ((__vector_size__ (16)));
typedef char __v16qi __attribute__ ((__vector_size__ (16)));
typedef short __v8hi __attribute__ ((__vector_size__ (16)));
typedef long long __v1di __attribute__ ((__vector_size__ (8)));

//emmintrin.h
//INSIEME HACK
#ifdef __clang__
extern __v2df __builtin_ia32_movsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_loadupd(const double *); 
extern __v2df __builtin_ia32_shufpd(__v2df, __v2df, const int); 
extern double __builtin_ia32_vec_ext_v2df(__v2df, const int); 
extern int __builtin_ia32_vec_ext_v4si(__v4si, const int); 
extern long long __builtin_ia32_vec_ext_v2di(__v2di, const int);
extern __v2df __builtin_ia32_addpd(__v2df, __v2df);
extern __v2df __builtin_ia32_addsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_subpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_subsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_mulpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_mulsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_divpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_divsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_andpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_andnpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_orpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_xorpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpeqpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpltpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmplepd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpgtpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpgepd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpordpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpneqpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpnltpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpnlepd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpngtpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpngepd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpunordpd(__v2df, __v2df); 

extern __v2df __builtin_ia32_cmpeqsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpltsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmplesd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpordsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpneqsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpnltsd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpnlesd(__v2df, __v2df); 
extern __v2df __builtin_ia32_cmpunordsd(__v2df, __v2df); 

extern __v16qi __builtin_ia32_loaddqu(const char *);
extern __v2di __builtin_ia32_movq128(__v2di);
extern int __builtin_ia32_cvttsd2si(__v2df); 
extern long long __builtin_ia32_cvttsd2si64(__v2df); 
extern __v4sf __builtin_ia32_cvtsd2ss(__v4sf, __v2df); 
extern __v2df __builtin_ia32_cvtsi2sd(__v2df, int); 
extern __v2df __builtin_ia32_cvtsi642sd(__v2df, int);
extern __v2df __builtin_ia32_cvtss2sd(__v2df, __v4sf);
extern __v2df __builtin_ia32_unpckhpd(__v2df, __v2df);  
extern __v2df __builtin_ia32_unpcklpd(__v2df, __v2df); 
extern __v2df __builtin_ia32_loadhpd(__v2df, double const *);
extern __v2df __builtin_ia32_loadlpd(__v2df, double const *);
extern __v16qi __builtin_ia32_punpckhbw128(__v16qi, __v16qi); 
extern __v8hi __builtin_ia32_punpckhwd128(__v8hi, __v8hi); 
extern __v4si __builtin_ia32_punpckhdq128(__v4si, __v4si);
extern __v2di __builtin_ia32_punpckhqdq128(__v2di, __v2di);
extern __v16qi __builtin_ia32_punpcklbw128(__v16qi, __v16qi); 
extern __v8hi __builtin_ia32_punpcklwd128(__v8hi, __v8hi); 
extern __v4si __builtin_ia32_punpckldq128(__v4si, __v4si);
extern __v2di __builtin_ia32_punpcklqdq128(__v2di, __v2di); 
extern __v16qi __builtin_ia32_paddb128(__v16qi, __v16qi); 
extern __v8hi __builtin_ia32_paddw128(__v8hi, __v8hi);
extern __v4si __builtin_ia32_paddd128(__v4si, __v4si); 
extern __v2di __builtin_ia32_paddq128(__v2di, __v2di); 
extern __v16qi __builtin_ia32_psubb128(__v16qi, __v16qi); 
extern __v8hi __builtin_ia32_psubw128(__v8hi, __v8hi); 
extern __v4si __builtin_ia32_psubd128(__v4si, __v4si);
extern __v2di __builtin_ia32_psubq128(__v2di, __v2di);
extern __v8hi __builtin_ia32_pmullw128(__v8hi, __v8hi); 
extern __v2di __builtin_ia32_pand128(__v2di, __v2di); 
extern __v2di __builtin_ia32_pandn128(__v2di, __v2di); 
extern __v2di __builtin_ia32_por128(__v2di, __v2di); 
extern __v2di __builtin_ia32_pxor128(__v2di, __v2di); 

extern __v16qi __builtin_ia32_pcmpeqb128(__v16qi, __v16qi); 
extern __v16qi __builtin_ia32_pcmpgtb128(__v16qi, __v16qi); 

extern __v8hi __builtin_ia32_pcmpeqw128(__v8hi, __v8hi); 
extern __v8hi __builtin_ia32_pcmpgtw128(__v8hi, __v8hi); 

extern __v4si __builtin_ia32_pcmpeqd128(__v4si, __v4si); 
extern __v4si __builtin_ia32_pcmpgtd128(__v4si, __v4si); 
extern void __builtin_ia32_movnti64(long long int *, long long int); 
//INSIEME HACK
#endif

//ia32intrin.h
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

//xmmintrin.h
//INSIEME HACK
#ifdef __clang__
typedef int __v4si __attribute__ ((__vector_size__ (16)));
extern __v4sf __builtin_ia32_addss(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_subss(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_mulss(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_divss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpeqss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpltss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpless(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpneqss(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_movss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnltss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnless(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpordss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnordss(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpunordss(__v4sf, __v4sf);

extern __v4sf __builtin_ia32_addps(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_subps(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_mulps(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_divps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_andps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_andnps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_orps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_xorps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpeqps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpltps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpleps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpgtps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpgeps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpneqps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnltps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnleps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpngtps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpngeps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpordps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpnordps(__v4sf, __v4sf);
extern __v4si __builtin_ia32_cmpunordps(__v4sf, __v4sf);

extern int __builtin_ia32_cvttss2si(__v4sf);
extern int __builtin_ia32_cvtss2si(__v4sf);

extern long long __builtin_ia32_cvttss2si64(__v4sf);
extern long long __builtin_ia32_cvtss2si64(__v4sf);
extern __v4sf __builtin_ia32_cvtsi2ss(__v4sf, int);
extern __v4sf __builtin_ia32_cvtsi642ss(__v4sf, long long );

extern __v4sf __builtin_ia32_movlhps(__v4sf, __v4sf); 
extern __v4sf __builtin_ia32_movhlps(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_unpckhps(__v4sf, __v4sf);
extern __v4sf __builtin_ia32_unpcklps(__v4sf ,__v4sf);
extern __v4sf __builtin_ia32_loadhps(__v4sf, const __v2sf *);
extern __v4sf __builtin_ia32_loadlps(__v4sf, const __v2sf *);

//FIXME different signature in clang than in gcc
//CLANG extern void __builtin_ia32_storehps(__v2si *, __v4sf); //CLANG
extern void __builtin_ia32_storehps(__v2sf *, __v4sf);	//GCC
//CLANG extern void __builtin_ia32_storelps(__v2si *, __v4sf); //CLANG
extern void __builtin_ia32_storelps(__v2sf *, __v4sf);	//GCC
//GCC extern void __builtin_ia32_movntq(unsigned long long *, unsigned long long); //GCC
extern void __builtin_ia32_movntq(__v1di*,__v1di); //CLANG
//FIXMEEND 

extern __v4sf __builtin_ia32_loadups(const float *);
extern void __builtin_ia32_storeups(float *, __v4sf);
extern __v4sf  __builtin_ia32_shufps(__v4sf, __v4sf, int const);
extern float __builtin_ia32_vec_ext_v4sf(__v4sf, const int); 
extern void __builtin_ia32_movntq(__v1di *, __v1di);
#endif
//INSIEME HACK

/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

int main() {	
	
	// BASE TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "var ref<char,f,f> v0;"
	char c;
	#pragma test expected "var ref<uint<1>,f,f> v0;"
	unsigned char uc;
	#pragma test expected "var ref<int<1>,f,f> v0;"
	signed char sc;
	
	#pragma test expected "var ref<int<2>,f,f> v0;"
	short ss;
	#pragma test expected "var ref<uint<2>,f,f> v0;"
	unsigned short us;
	
	#pragma test expected "var ref<int<4>,f,f> v0;"
	int m;
	#pragma test expected "var ref<int<4>,t,f> v0;"
	const int y;
	#pragma test expected "var ref<int<4>,f,t> v0;"
	volatile int z;
	#pragma test expected "var ref<int<4>,t,t> v0;"
	const volatile int q;
	
	#pragma test expected "var ref<real<4>,f,f> v0;"
	float f1;
	#pragma test expected "var ref<real<8>,f,f> v0;"
	double f2;
	
	// BASE TYPE TYPEDEFS //////////////////////////////////////////////////////////////
	
	typedef float fluffy;
	typedef volatile float fluffier;
	
	#pragma test expected "var ref<real<4>,f,f> v0;"
	fluffy fluff;
	
	#pragma test expected "var ref<real<4>,f,t> v0;"
	fluffier fluffer;
	
	// POINTER TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "var ref<ptr<int<4>,f,f>,f,f> v0;"
	int* pi;
	#pragma test expected "var ref<ptr<int<4>,t,f>,f,f> v0;"
	const int* pci;
	#pragma test expected "var ref<ptr<int<4>,f,t>,f,f> v0;"
	volatile int* pvi;
	#pragma test expected "var ref<ptr<int<4>,t,t>,f,f> v0;"
	const volatile int* pcvi;
	
	#pragma test expected "var ref<ptr<int<4>,f,f>,t,f> v0;"
	int *const cpi;
	#pragma test expected "var ref<ptr<int<4>,t,f>,t,f> v0;"
	const int *const cpci;
	
	#pragma test expected "var ref<ptr<ptr<int<4>,f,f>,f,f>,f,f> v0;"
	int** ppi;
	#pragma test expected "var ref<ptr<ptr<ptr<int<4>,f,f>,f,f>,t,f>,f,f> v0;"
	int * *const * ppcpi;
	
	// FIXED SIZE ARRAY TYPES //////////////////////////////////////////////////////////////
	
	#pragma test expected "var ref<array<real<4>,2>,f,f> v0;"
	float arrf[2];
	#pragma test expected "var ref<array<real<4>,2>,t,f> v0;"
	const float arrcf[2];
	#pragma test expected "var ref<array<real<4>,2>,f,t> v0;"
	volatile float arrvf[2];
	
	#pragma test expected "var ref<array<array<real<4>,5>,2>,f,f> v0;"
	float arrarrf[2][5];
	#pragma test expected "var ref<array<array<array<real<4>,3>,5>,2>,f,f> v0;"
	float arrarrarrf[2][5][3];
	
	#pragma test expected "var ref<array<ptr<real<4>,f,f>,2>,f,f> v0;"
	float* arrpf[2];
	#pragma test expected "var ref<array<ptr<real<4>,t,f>,2>,f,f> v0;"
	const float* arrpcf[2];
	#pragma test expected "var ref<array<ptr<real<4>,t,f>,2>,f,t> v0;"
	const float *volatile arrvpcf[2];
	
	// ENUM TYPES //////////////////////////////////////////////////////////////
		
	typedef enum { Bla, Alb } enum_t;
	#pragma test expected "var ref<__insieme_enum<enum_t,Bla,Alb>,f,f> v0;"
	enum_t enu;
	
	// STRUCT TYPES //////////////////////////////////////////////////////////////
	
	//#pragma test expected "REGEX;"
	//struct { int i; } swi_anon;
	
	//typedef struct { int i; } swi_t;
	//#pragma test expected "var ref<array<ptr<real<4>,t,f>,2>,f,t> v0;"
	//swi_t swi; 
	
	//typedef union { int i; } union_t;
	//union_t uni;
	
	//int length = 2;
	//#pragma test expected "var ref<array<real<4>,2>,f,t> v0;"
	//volatile float vlarrvf[length];
}

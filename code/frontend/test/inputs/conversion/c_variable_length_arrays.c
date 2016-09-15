/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

	// VARIABLE LENGTH ARRAY TYPES //////////////////////////////////////////////////////////////
	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0,type_lit(uint<inf>));
			var ref<array<real<4>,#v1>,f,f> v2;
		})")
	{
		int i = 3;
		float arrf[i];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0, type_lit(uint<inf>));
			var ref<real<4>,f,f> v2;
			var ref<real<4>,f,f> v3;
			var ref<array<real<4>,#v1>,f,f> v4;
		})")
	{
		int i = 3;
		float x,y,arrxy[i];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0+3, type_lit(uint<inf>));
			var ref<array<real<4>,#v1>,f,f> v2;
		})")
	{
		int i = 3;
		float arrfm[i+3];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0, type_lit(uint<inf>));
			var ref<array<real<4>,#v1>,t,f> v2;
		})")
	{
		int i = 3;
		const float arrcf[i];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0, type_lit(uint<inf>));
			var ref<array<real<4>,#v1>,t,f> v2; ptr_from_array(v2);
		})")
	{
		int i = 3;
		const float arrcf[i];
		arrcf;
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var uint<inf> v1 = num_cast(*v0, type_lit(uint<inf>));
			var ref<array<real<4>,#v1>,f,t> v2;
		})")
	{
		int i = 3;
		volatile float arrvf[i];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var ref<int<4>,f,f> v1 = 6;
			var uint<inf> v2 = num_cast(*v0, type_lit(uint<inf>));
			var uint<inf> v3 = num_cast(*v1, type_lit(uint<inf>));
			var ref<array<array<real<4>,#v3>,#v2>,f,f> v4;
		})")
	{
		int i = 3;
		int j = 6;
		float arrarrf[i][j];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var ref<int<4>,f,f> v1 = 6;
			var uint<inf> v2 = num_cast(*v0+2, type_lit(uint<inf>));
			var uint<inf> v3 = num_cast(*v1+5, type_lit(uint<inf>));
			var ref<array<array<real<4>,#v3>,#v2>,f,f> v4;
		})")
	{
		int i = 3;
		int j = 6;
		float arrarrfmm[i+2][j+5];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var ref<int<4>,f,f> v1 = 6;
			var ref<int<4>,f,f> v2 = 10;
			var uint<inf> v3 = num_cast(*v0, type_lit(uint<inf>));
			var uint<inf> v4 = num_cast(*v1, type_lit(uint<inf>));
			var uint<inf> v5 = num_cast(*v2, type_lit(uint<inf>));
			var ref<array<array<array<real<4>,#v5>,#v4>,#v3>,f,f> v6;
		})")
	{
		int i = 3;
		int j = 6;
		int k = 10;
		float arrarrarrf[i][j][k];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 3;
			var ref<int<4>,f,f> v1 = 6;
			var ref<int<4>,f,f> v2 = 10;
			var uint<inf> v3 = num_cast(*v0+2, type_lit(uint<inf>));
			var uint<inf> v4 = num_cast(*v1, type_lit(uint<inf>));
			var uint<inf> v5 = num_cast(*v2+1, type_lit(uint<inf>));
			var ref<array<array<array<real<4>,#v5>,#v4>,#v3>,f,f> v6;
		})")
	{
		int i = 3;
		int j = 6;
		int k = 10;
		float arrarrarrfm[i+2][j][k+1];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 61;
			var uint<inf> v1 = num_cast(10, type_lit(uint<inf>));
			var uint<inf> v2 = num_cast(*v0+2, type_lit(uint<inf>));
			var ref<array<array<array<real<4>,10>,#v2>,#v1>,t,t> v3;
		})")
	{
		int j = 61;
		const volatile float arrarrarrffm[10][j+2][10];
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 4;
			var uint<inf> v1 = num_cast(*v0,type_lit(uint<inf>));
			var ref<array<ptr<real<4>,f,f>,#v1>,f,f> v2;
		})")
	{
		int i = 4;
		float* arrpf[i];
	}
	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 4;
			var uint<inf> v1 = num_cast(*v0,type_lit(uint<inf>));
			var ref<array<ptr<real<4>,t,f>,#v1>,f,f> v2;
		})")
	{
		int i = 4;
		const float* arrpcf[i];
	}
	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0 = 4;
			var uint<inf> v1 = num_cast(*v0,type_lit(uint<inf>));
			var ref<array<ptr<real<4>,t,f>,#v1>,f,t> v2;
		})")
	{
		int i = 4;
		const float *volatile arrvpcf[i];
	}

	// VARIABLE LENGTH ARRAY SIZEOF //////////////////////////////////////////////////////////////

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0;
			sizeof(type_lit(int<4>))*num_cast(*v0, type_lit(uint<8>));
		})")
	{
		int i;
		sizeof(int[i]);
	}

	#pragma test expect_ir(R"({
			var ref<int<4>,f,f> v0;
			var ref<int<4>,f,f> v1;
			sizeof(type_lit(array<int<4>,2>))*num_cast(*v0, type_lit(uint<8>))*num_cast(5, type_lit(uint<8>))*num_cast(*v1, type_lit(uint<8>));
		})")
	{
		int i, j;
		sizeof(int[i][5][j][2]);
	}

	#pragma test expect_ir(R"({
			var ref<int<4>> v0 = 10;
			var uint<inf> v1 = num_cast(*v0, type_lit(uint<inf>));
			var ref<array<int<4>,#v1>> v2;
			sizeof(type_lit(array<int<4>,#v1>));
		})")
	{
		int i=10;
		int k[i];
		sizeof(k);
	}

	#pragma test expect_ir(R"({
			var ref<int<4>> v0 = 10;
			var ref<int<4>> v1 = 20;
			var uint<inf> v2 = num_cast(*v0, type_lit(uint<inf>));
			var uint<inf> v3 = num_cast(*v1, type_lit(uint<inf>));
			var ref<array<array<int<4>,#v3>,#v2>> v4; sizeof(type_lit(array<array<int<4>,#v3>,#v2>));
		})")
	{
		int i=10;
		int j=20;
		int l[i][j];
		sizeof(l);
	}

	#pragma test expect_ir(R"({
			var ref<int<4>> v0 = 10;
			var ref<int<4>> v1 = 20;
			var uint<inf> v2 = num_cast(*v0, type_lit(uint<inf>));
			var uint<inf> v3 = num_cast(*v1, type_lit(uint<inf>));
			var ref<array<array<int<4>,#v3>,#v2>> v4;
			var uint<inf> v5 = num_cast(*v0, type_lit(uint<inf>));
			var uint<inf> v6 = num_cast(*v1, type_lit(uint<inf>));
			var ref<array<array<int<4>,#v6>,#v5>> v7;
			sizeof(type_lit(array<array<int<4>,#v3>,#v2>));
			sizeof(type_lit(array<array<int<4>,#v6>,#v5>));
		})")
	{
		int i=10;
		int j=20;
		int l[i][j];
		int x[i][j];
		sizeof(l);
		sizeof(x);
	}
}

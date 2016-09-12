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

	#pragma test expect_ir("{ 1; var ref<int<4>,f,f> v0; v0; }")
	{
		#pragma test expect_num_vars(0)
		1; // dummy statement to attach to
		int x;
		#pragma test expect_num_vars(1)
		x;
	}

	#pragma test expect_num_vars(0)
	1;

	#pragma test expect_ir("{ var ref<int<4>,f,f> v0; { var ref<int<4>,f,f> v1; v0; v1; } }")
	{
		int x;
		{
			int y;
			#pragma test expect_num_vars(2)
			x;
			y;
		}
	}

	#pragma test expect_ir("{ { var ref<int<4>,f,f> v1; } 1; }")
	{
		{
			int x;
		}
		#pragma test expect_num_vars(0)
		1;
	}

}

int x;

void bla() {
	#pragma test expect_num_vars(1)
	1;

	int c;
	#pragma test expect_num_vars(2)
}

void bla2(int x) {
	#pragma test expect_num_vars(2)
	x;
}

void bla3(char* filename);
void bla4(char* filename) { filename; }

typedef struct { int i; } Image;
typedef unsigned long long ull;

void write_image(Image target, Image dist, char* filename, ull minSteps, ull maxSteps) {

	#pragma test expect_num_vars(6)
	bla3(filename);
	bla4(filename);
}

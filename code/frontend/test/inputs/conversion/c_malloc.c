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

#include <stdlib.h>

int main() {

	#pragma test expect_ir(R"({
		var ref<ptr<unit>,f,f,plain> v0 = malloc_wrapper(num_cast(5, type_lit(uint<8>)));
	})")
	{
		void* a = malloc(5);
	}

	#pragma test expect_ir(R"({
		var ref<ptr<unit>,f,f,plain> v0 = malloc_wrapper(sizeof(type_lit(int<4>)));
	})")
	{
		void* a = malloc(sizeof(int));
	}

	// implicit casting
	#pragma test expect_ir(R"({
		var ref<ptr<int<4>>,f,f,plain> v0 = ptr_reinterpret(malloc_wrapper(sizeof(type_lit(int<4>))*num_cast(10, type_lit(uint<8>))), type_lit(int<4>));
	})")
	{
		int* a = malloc(sizeof(int)*10);
	}

	// explicit casting
	#pragma test expect_ir(R"({
		var ref<ptr<char>,f,f,plain> v0 = ptr_reinterpret(malloc_wrapper(sizeof(type_lit(char))*num_cast(30, type_lit(uint<8>))), type_lit(char));
	})")
	{
		char* a = (char*)malloc(sizeof(char) * 30);
	}

	return 0;
}

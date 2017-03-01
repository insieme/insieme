/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

	//===------------------------------------------------------------------------------------------------------------------------------------------- FREE ---===

	#pragma test expect_ir(R"({
		var ref<ptr<unit>,f,f,plain> v0 = malloc_wrapper(num_cast(5, type_lit(uint<8>)));
		free_wrapper(*v0);
	})")
	{
		void* a = malloc(5);
		free(a);
	}

	#pragma test expect_ir(R"({
		var ref<ptr<char>,f,f,plain> v0 = ptr_reinterpret(malloc_wrapper(sizeof(type_lit(char))*num_cast(30, type_lit(uint<8>))), type_lit(char));
		free_wrapper(ptr_reinterpret(*v0, type_lit(unit)));
	})")
	{
		char* a = (char*)malloc(sizeof(char) * 30);
		free(a);
	}

	return 0;
}

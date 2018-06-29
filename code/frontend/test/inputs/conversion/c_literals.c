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
 */

int main() {

	#pragma test expect_ir("1")
	1;

	#pragma test expect_ir("-1")
	-1;

	#pragma test expect_ir("2u")
	2u;

	#pragma test expect_ir("3u")
	3U;

	#pragma test expect_ir("4l")
	4l;

	#pragma test expect_ir("5ul")
	5ul;

	#pragma test expect_ir("6ul")
	6UL;

	#pragma test expect_ir("2567483615ul")
	0x9908b0dfUL;

	#pragma test expect_ir("18444473444759240704ul")
	0xfff7eee000000000UL;

	#pragma test expect_ir("7ull")
	7ull;

	// test bit size available
	#pragma test expect_ir("9223372036854775808ull")
	9223372036854775808ull;

	#pragma test expect_ir("lit(\"8.0E+0\":real<4>)")
	8.0f;

	#pragma test expect_ir("lit(\"9.0E+0\":real<4>)")
	9.0F;

	#pragma test expect_ir("lit(\"1.0E+0\":real<8>)")
	1.0;

	#pragma test expect_ir("lit(\"1.0E+0\":real<16>)")
	1.0L;

	#pragma test expect_ir("lit(\"3.0E+1\":real<8>)")
	3.0e+1;

	#pragma test expect_ir("lit(\"4.0E+1\":real<8>)")
	4.0E+1;

	#pragma test expect_ir("lit(\"'z'\":int<4>)")
	'z';

	// Note: we actually have to create a string literal which isn't const here, as Clang says that the single string literal below the pragma also isn't const.
	// In order to do this, we need to create the literal ourselves specifying the complete type. Also to create these string literals we need to
	// enclose them within double quotes, in order for the inner ones to end up in the string literal, as the outer quotes are consumed by the parser.
	#pragma test expect_ir("ptr_from_array(lit(\"\"abc\"\":ref<array<char,4u>,f,f>))")
	"abc";

	// Note: we also need to encode escaped characters once again (\n -> \\n) and manually specify to correct length of the
	// _unescaped_ string _including_ the terminating \0 character (here 5)
	#pragma test expect_ir("ptr_from_array(lit(\"\"ab\\nc\"\":ref<array<char,5u>,f,f>))")
	"ab\nc";

}

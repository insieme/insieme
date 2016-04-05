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

	#pragma test expect_ir("7ull")
	7ull;

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
	#pragma test expect_ir("ptr_from_array(lit(\"\"abc\"\":ref<array<char,4>,f,f>))")
	"abc";

	// Note: we also need to encode escaped characters once again (\n -> \\n) and manually specify to correct length of the
	// _unescaped_ string _including_ the terminating \0 character (here 5)
	#pragma test expect_ir("ptr_from_array(lit(\"\"ab\\nc\"\":ref<array<char,5>,f,f>))")
	"ab\nc";

}

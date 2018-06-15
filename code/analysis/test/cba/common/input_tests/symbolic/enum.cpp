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

#include "../cba.h"

enum {
	A, B
};


int main() {

	// just taking the local value should be fine
	cba_expect_symbolic_value("[(type_lit(enum_def<IMP___anon_tagtype__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1_IMLOC__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1,int<4>,enum_entry<IMP__colon__colon_A,0>,enum_entry<IMP__colon__colon_B,1>>), 0)]", A );
	cba_expect_symbolic_value("[(type_lit(enum_def<IMP___anon_tagtype__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1_IMLOC__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1,int<4>,enum_entry<IMP__colon__colon_A,0>,enum_entry<IMP__colon__colon_B,1>>), 1)]", B );
	cba_expect_symbolic_value("[enum_to_int((type_lit(enum_def<IMP___anon_tagtype__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1_IMLOC__slash_home_slash_herbert_slash_coding_slash_c_plus__plus__slash_allscale_compiler_slash_insieme_slash_code_slash_analysis_slash_test_slash_cba_slash_common_slash_input_tests_slash_symbolic_slash_enum_dot_cpp_40_1,int<4>,enum_entry<IMP__colon__colon_A,0>,enum_entry<IMP__colon__colon_B,1>>), 0))]",int(A));

//	cba_debug();

	return 0;
}

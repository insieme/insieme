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

#include "template_interception_names.h"

int main() {

	int magic;

	#pragma test expect_ir(R"({
		type_instantiation(type_lit((int<4>, IMP_testNs_colon__colon_X<ref<int<4>,f,f,qualified>>) -> unit),
				lit("IMP_testNs_colon__colon_fun" : ('T_0_0, IMP_testNs_colon__colon_X<'T_0_0>) -> unit))(1,
				ref_cast(lit("IMP_testNs_colon__colon_X::ctor" : IMP_testNs_colon__colon_X<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>::())
			(ref_temp(type_lit(IMP_testNs_colon__colon_X<ref<int<4>,f,f,qualified>>))), type_lit(f), type_lit(f), type_lit(cpp_rref)));
	})")
	{
		testNs::fun(1);
	}

	return 0;
}

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

#include <gtest/gtest.h>

#include <fstream>

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/config.h"

#include "insieme/backend/preprocessor.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace backend {

	TEST(Preprocessor, GlobalElimination) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);
						
		core::ProgramPtr program = builder.parseProgram(R"(
			alias gstruct = struct __insieme_anonymous_record_2_20 { a: vector<int<4>,20>; f : real<8>; };
			
			int<4> main() {
				var ref<gstruct> v1 = <ref<gstruct>> (v1) { zero(type_lit(vector<int<4>,20>)), 0.0 };
				v1.a;
				composite_member_access(*v1, lit("a"), type_lit(vector<int<4>,20>));
				(v2: ref<gstruct>) -> unit {
					v2.a;
					composite_member_access(*v2, lit("a"), type_lit(vector<int<4>,20>));
				} (v1);
				{
					v1.a = lit("X":vector<int<4>,20>);
				}
				return 0;
			}
            )");

		EXPECT_TRUE(program);

		//	std::cout << "Input: " << core::printer::PrettyPrinter(program) << "\n";


		// check for semantic errors
		auto errors = core::checks::check(program);
		core::printer::dumpErrors(errors, std::cout);
		EXPECT_EQ(core::checks::MessageList(), errors);
	}


} // namespace backend
} // namespace insieme

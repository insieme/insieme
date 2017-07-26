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

#include "insieme/core/transform/sequentialize.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/core/printer/pretty_printer.h"



#include "insieme/core/lang/parallel.h"

namespace insieme {
namespace core {
namespace transform {

	TEST(Manipulation, SequentializeBug) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto op = mgr.getLangExtension<lang::ParallelExtension>().getAtomicFetchAndAdd();
		EXPECT_TRUE(lang::isBuiltIn(op));

		auto seq = transform::sequentialize(mgr, op);

		EXPECT_NE(op, seq);
		EXPECT_FALSE(lang::isBuiltIn(seq));

	}

	TEST(Manipulation, SequentializeAtomic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		StatementPtr code = analysis::normalize(builder.parseStmt("alias int = int<4>;"
		                                                          "{"
		                                                          "	var ref<int> a = 2;"
		                                                          "	atomic_add_and_fetch(a, 10);"
		                                                          "}")
		                                            .as<StatementPtr>());

		ASSERT_TRUE(code);

		EXPECT_EQ("{var ref<int<4>,f,f,plain> v0 = 2;atomic_add_and_fetch(v0, 10);}",
		          toString(printer::PrettyPrinter(code, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
		EXPECT_TRUE(check(code, checks::getFullCheck()).empty()) << check(code, checks::getFullCheck());


		auto res = analysis::normalize(transform::trySequentialize(mgr, code));
		EXPECT_EQ("{\n    var ref<int<4>,f,f,plain> v0 = 2;\n    *comp_assign_add(v0, 10);\n}", toString(printer::PrettyPrinter(res))) << printer::PrettyPrinter(res);

		EXPECT_TRUE(check(res, checks::getFullCheck()).empty()) << check(res, checks::getFullCheck());

	}

} // end namespace transform
} // end namespace core
} // end namespace insieme

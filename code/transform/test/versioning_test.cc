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
#include <gtest/gtest.h>

#include "insieme/transform/versioning.h"
#include "insieme/transform/primitives.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"



namespace insieme {
namespace transform {


	TEST(Versioning, SimpleVersioning) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::StatementPtr one = builder.intLit(1);
		core::StatementPtr two = builder.intLit(2);


		core::StatementPtr in = builder.compoundStmt(one, two);
		core::StatementPtr out;

		out = versioning(makeNoOp(), makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(out)), "pick([0u,1u])");

		// try special case - on transformation only
		out = versioning(makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_EQ(*in, *out);

		// and a large number of versions
		out = versioning(makeNoOp(), makeNoOp(), makeNoOp(), makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(out)), "pick([0u,1u,2u,3u])");
	}


} // end namespace transform
} // end namespace insieme

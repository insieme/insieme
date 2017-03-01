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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"

namespace insieme {
namespace core {


	TEST(Node, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

		IntValuePtr node = builder.intValue(14);

		EXPECT_TRUE(node->isValue());


		TypeList list;

		TupleTypePtr tuple1 = builder.tupleType(list);

		list.push_back(tuple1);
		TupleTypePtr tuple2 = builder.tupleType(list);

		EXPECT_EQ(0u, tuple1.getChildList().size());
		EXPECT_EQ(1u, tuple2.getChildList().size());

		EXPECT_EQ(NT_TupleType, tuple1.getNodeType());


		LiteralPtr lit = builder.literal(manager.getLangBasic().getBool(), "true");
		EXPECT_EQ(NT_Literal, lit->getNodeType());

		IfStmtPtr stmt = builder.ifStmt(lit, lit, lit);
		EXPECT_EQ(NT_IfStmt, stmt->getNodeType());
		EXPECT_EQ(3u, stmt->getChildList().size());

		EXPECT_EQ(lit, stmt->getChildList()[0]);
		EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildList()[1]);
		EXPECT_EQ(builder.compoundStmt(lit), stmt->getChildList()[2]);

	}

	TEST(Node, MemberAccess) {
		NodeManager manager;
		IRBuilder builder(manager);

		LiteralPtr literal = builder.intLit(1);
		ExpressionPtr lit(literal);

		IfStmtPtr stmt = builder.ifStmt(lit, lit, lit);

		IfStmtPtr ptr(stmt);
		IfStmtAddress adr(stmt);

		ExpressionPtr stmtPtr = ptr->getCondition();
		ExpressionAddress stmtAdr = adr->getCondition();

		ptr->getThenBody();
		ptr->getElseBody();

		adr->getThenBody();
		adr->getElseBody();
	}

} // end namespace core
} // end namespace insieme

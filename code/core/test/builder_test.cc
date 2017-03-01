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
#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(IRBuilder, Basic) {
	// With Builder
	NodeManager manager;
	IRBuilder build(manager);
	VariablePtr var1 = build.variable(build.getLangBasic().getBool(), 1);
	std::vector<StatementPtr> statements;
	statements.push_back(build.breakStmt());
	statements.push_back(build.declarationStmt(var1, build.getLangBasic().getTrue()));
	auto compound = build.compoundStmt(statements);

	// Without Builder
	NodeManager manager2;
	VariablePtr var2 = Variable::get(manager2, build.getLangBasic().getBool(), 1);
	std::vector<StatementPtr> statements2;
	statements2.push_back(BreakStmt::get(manager2));
	statements2.push_back(DeclarationStmt::get(manager2, var2, build.getLangBasic().getTrue()));
	auto compound2 = CompoundStmt::get(manager2, statements2);

	EXPECT_EQ(*compound2, *compound);
}

TEST(IRBuilder, TypeMatch) {
	NodeManager manager;
	IRBuilder builder(manager);

	auto type = builder.parseType("ref<vector<int<4>,6>>");
	EXPECT_TRUE(builder.matchType("ref<vector<'a,'n>>", type));
	EXPECT_FALSE(builder.matchType("ref<vector<'a,5>>", type));
	EXPECT_TRUE(builder.matchType("ref<vector<int<'b>,6>>", type));
}

int check(NodePtr nodeToCheck) {
	auto semantic = checks::check(nodeToCheck);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) { LOG(INFO) << cur << std::endl; });

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) << " failed on: " << nodeToCheck;
	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
		/*        core::NodeAddress address = cur.getAddress();
		 core::NodePtr context = address.getParentNode(address.getDepth()-1);
		 std::cout << "\t Context: " <<
		 insieme::core::printer::PrettyPrinter(context, insieme::core::printer::PrettyPrinter::OPTIONS_SINGLE_LINE, 3) << std::endl;
		 */
	});

	return errors.size();
}

TEST(IRBuilder, Assign) {
	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	VariablePtr lhs = builder.variable(builder.refType(basic.getInt4()));
	VariablePtr rhs = builder.variable(basic.getInt4());

	ExpressionPtr simpleAssign = builder.assign(lhs, rhs);

	check(simpleAssign);

	// NOTE: this makes no sense anymore since the builder should not look for the union field you want to access
	//	vector<std::pair<StringValuePtr,TypePtr>> unionEntries;
	//	unionEntries.push_back(std::make_pair(builder.stringValue("a"), basic.getReal4()));
	//	unionEntries.push_back(std::make_pair(builder.stringValue("b"), basic.getUInt2()));
	//	unionEntries.push_back(std::make_pair(builder.stringValue("c"), builder.vectorType(basic.getChar(), builder.concreteIntTypeParam(4))));
	//
	//	VariablePtr unionRhs = builder.variable(builder.unionType(unionEntries));
	//
	//	ExpressionPtr unionAssign = builder.assign(lhs, unionRhs);
	//
	//	check(unionAssign);
}

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

#include <iostream>

#include "insieme/utils/container_utils.h"
#include "insieme/utils/set_utils.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
// #include "insieme/core/lang_basic.h"

#include "ir_node_test.inc"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::lang;
using namespace insieme::utils::set;


TEST(Program, HelloWorld) {
	NodeManager manager;
	IRBuilder build(manager);

	TypePtr stringType = build.genericType("string");
	TypePtr varArgType = build.genericType("var_list");
	TypePtr unitType = build.getLangBasic().getUnit();
	TypePtr printfType = build.functionType(toVector(stringType, varArgType), unitType);

	auto printfDefinition = build.literal(printfType, "printf");

	FunctionTypePtr voidNullaryFunctionType = build.functionType(TypeList(), unitType);

	ExpressionPtr intLiteral = build.literal(build.getLangBasic().getIntGen(), "4");
	auto invocation = build.callExpr(unitType, build.literal(printfType, "printf"), toVector(intLiteral));
	auto mainBody = build.lambdaExpr(voidNullaryFunctionType, VariableList(), invocation);
	auto mainDefinition = build.lambdaExpr(voidNullaryFunctionType, VariableList(), mainBody);

	ExpressionPtr intLiteral2 = build.literal(build.getLangBasic().getIntGen(), "8");
	auto invocation2 = build.callExpr(unitType, build.literal(printfType, "printf"), toVector(intLiteral2));
	auto mainBody2 = build.lambdaExpr(voidNullaryFunctionType, VariableList(), invocation2);
	auto mainDefinition2 = build.lambdaExpr(voidNullaryFunctionType, VariableList(), mainBody2);

	ProgramPtr pro = build.program(toVector<ExpressionPtr>(mainDefinition));
	ProgramPtr pro2 = build.program(toVector<ExpressionPtr>(mainDefinition2));

	EXPECT_NE(pro, pro2);
	EXPECT_NE(*pro, *pro2);
	EXPECT_NE((*pro).hash(), (*pro2).hash());

	cout << pro;

	basicNodeTests(pro, toVector<NodePtr>(mainDefinition));
	basicNodeTests(pro2, toVector<NodePtr>(mainDefinition2));
}

TEST(Program, ProgramData) {
	// create local manager
	NodeManager manager;

	// start with empty program
	NodeManager programManager;
	ProgramPtr program = Program::get(programManager);

	// check some basic properties
	EXPECT_EQ(0u, manager.size());
	EXPECT_EQ(1u, programManager.size());

	EXPECT_TRUE(program->getEntryPoints().empty());

	TypePtr typeInt = GenericType::get(manager, "int");
	TypePtr typeDouble = GenericType::get(manager, "double");

	// ------------- Entry Points ------------
	ExpressionPtr entryA = Variable::get(manager, typeInt, 1);
	ExpressionPtr entryB = Variable::get(manager, typeInt, 2);
	ExpressionPtr entryC = Variable::get(manager, typeDouble, 3);

	program = Program::addEntryPoint(programManager, program, entryA);
	EXPECT_NE(entryA, *program->getEntryPoints().begin());
	EXPECT_EQ(toVector<ExpressionPtr>(programManager.get(entryA)), program->getEntryPoints());

	ExpressionList entrySet;
	entrySet.push_back(entryA);
	entrySet.push_back(entryB);
	entrySet.push_back(entryC);

	program = Program::addEntryPoints(programManager, program, entrySet);
	EXPECT_EQ((std::size_t)3, program->getEntryPoints().size());

	const ExpressionList& points = program->getEntryPoints();
	std::for_each(points.cbegin(), points.cend(), [&manager, &programManager](const ExpressionPtr& cur) {
		EXPECT_FALSE(manager.addressesLocal(cur));
		EXPECT_TRUE(programManager.addressesLocal(cur));
	});

	// print resulting program
	cout << *program << endl;
}

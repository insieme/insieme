/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
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

#include <gtest/gtest.h>

#include "insieme/analysis/cba/prototype.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace core;

	TEST(CBA, Development) {

		CBAContext context;



	}


	TEST(CBA, GetDefinitionPoint) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto w = builder.variable(builder.parseType("A"), 123);
		map<string, NodePtr> symbols;
		symbols["w"] = w;

		auto pos = builder.parseAddresses("(int x, int y)->unit { "
				"	$x$;"
				"	$y$;"
				"	$w$;"
				"	int<4> z = 123;"
				"	$z$;"
				"	{"
				"		$z$;"
				"		auto z = 1234;"
				"		$z$;"
				"	};"
				"	$z$;"
				"	int<4> w = 123;"
				"	$w$;"
				"	int<4> w = 123;"
				"	$w$;"
				"}",
				symbols
		);

		EXPECT_EQ(9u, pos.size());

		vector<VariableAddress> vars;
		for (auto cur : pos) { vars.push_back(cur.as<VariableAddress>()); }

		auto root = LambdaExprAddress(pos[0].getRootNode().as<LambdaExprPtr>());

		auto paramX = root->getParameterList()[0];
		auto paramY = root->getParameterList()[1];
		auto freeW = VariableAddress(w);
		auto localZ1 = pos[0].getParentAddress().getAddressOfChild(3,0).as<VariableAddress>();
		auto localZ2 = pos[0].getParentAddress().getAddressOfChild(5,1,0).as<VariableAddress>();
		auto localW1 = pos[0].getParentAddress().getAddressOfChild(7,0).as<VariableAddress>();
		auto localW2 = pos[0].getParentAddress().getAddressOfChild(9,0).as<VariableAddress>();

		// simple cases
		EXPECT_EQ(paramX,  getDefinitionPoint(paramX));
		EXPECT_EQ(paramY,  getDefinitionPoint(paramY));
		EXPECT_EQ(freeW,   getDefinitionPoint(freeW));
		EXPECT_EQ(localZ1, getDefinitionPoint(localZ1));
		EXPECT_EQ(localZ2, getDefinitionPoint(localZ2));
		EXPECT_EQ(localW1, getDefinitionPoint(localW1));
		EXPECT_EQ(localW2, getDefinitionPoint(localW2));

		EXPECT_EQ(paramX,  getDefinitionPoint(vars[0]));
		EXPECT_EQ(paramY,  getDefinitionPoint(vars[1]));
		EXPECT_EQ(freeW,   getDefinitionPoint(vars[2]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[3]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[4]));
		EXPECT_EQ(localZ2, getDefinitionPoint(vars[5]));
		EXPECT_EQ(localZ1, getDefinitionPoint(vars[6]));
		EXPECT_EQ(localW1, getDefinitionPoint(vars[7]));
		EXPECT_EQ(localW2, getDefinitionPoint(vars[8]));


		// check within CBA context
		CBAContext context;
		auto varX = context.getVariable(paramX);
		EXPECT_EQ(varX, context.getVariable(paramX));
		EXPECT_EQ(varX, context.getVariable(vars[0]));

		auto varY = context.getVariable(vars[1]);
		EXPECT_EQ(varY, context.getVariable(paramY));
		EXPECT_NE(varX, varY);

		auto varW = context.getVariable(freeW);
		EXPECT_EQ(varW, context.getVariable(vars[2]));

		auto varW1 = context.getVariable(localW1);
		EXPECT_EQ(varW1, context.getVariable(vars[7]));

		auto varW2 = context.getVariable(vars[8]);
		EXPECT_EQ(varW2, context.getVariable(localW2));

		auto varZ1 = context.getVariable(localZ1);
		auto varZ2 = context.getVariable(localZ2);

		EXPECT_NE(varZ1, varZ2);
		EXPECT_EQ(varZ1, context.getVariable(vars[3]));
		EXPECT_EQ(varZ1, context.getVariable(vars[4]));
		EXPECT_EQ(varZ2, context.getVariable(vars[5]));
		EXPECT_EQ(varZ1, context.getVariable(vars[6]));

		// check that all variables are distinct
		std::set<Variable> allVars = { varX, varY, varZ1, varZ2, varW, varW1, varW2 };
		EXPECT_EQ(7u, allVars.size()) << allVars;

	}

	TEST(CBA, ConstraintGeneration) {

			NodeManager mgr;
			IRBuilder builder(mgr);

			auto code = builder.parseStmt(
					"{"
					"	int<4> x = 12;"
					"	auto y = (int z)->unit {};"
					"	y(14);"
					"	y(16);"
					"}"
			).as<CompoundStmtPtr>();

			EXPECT_TRUE(code);

			CBAContext context;
			auto constraints = generateConstraints(context, code);
			std::cout << "Constraint: " << constraints << "\n";

			auto solution = cba::solve(constraints);
			std::cout << "Solutions:  " << solution << "\n";

			auto declX = CompoundStmtAddress(code)[0].as<DeclarationStmtAddress>();
			VariableAddress varX = declX->getVariable();
			ExpressionAddress initX = declX->getInitialization();

			std::cout << *varX << " = " << cba::getValuesOf(context, solution, varX) << "\n";
			std::cout << *initX << " = " << cba::getValuesOf(context, solution, initX) << "\n";


			auto declY = CompoundStmtAddress(code)[1].as<DeclarationStmtAddress>();
			VariableAddress varY = declY->getVariable();
			ExpressionAddress initY = declY->getInitialization();

			std::cout << *varY << " = " << cba::getValuesOf(context, solution, varY) << "\n";
			std::cout << *initY << " = " << cba::getValuesOf(context, solution, initY) << "\n";


			auto varZ = initY.as<LambdaExprAddress>()->getParameterList()[0];
			std::cout << *varZ << " = " << cba::getValuesOf(context, solution, varZ) << "\n";

	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme


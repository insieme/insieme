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

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

	struct BuilderModule {
		NodeManager& mgr;
		BuilderModule(NodeManager& mgr) : mgr(mgr) {}
	};

	template<typename ... Modules>
	struct Builder : public virtual Modules... {
		Builder(NodeManager& mgr) : Modules(mgr)...  {}

		int f() { return 0; };
	};

	struct DefaultBuilder : public Builder<> {
		DefaultBuilder(NodeManager& mgr) : Builder<>(mgr) {}
	};

	struct ModuleA : public BuilderModule {
		ModuleA(NodeManager& mgr) : BuilderModule(mgr) {}
		int a() { return 0; };
	};

	struct ModuleB : public BuilderModule {
		ModuleB(NodeManager& mgr) : BuilderModule(mgr) {}
		int b() { return 0; };
	};


	TEST(BuilderTest, ModularStructure) {

		NodeManager mgr;

		DefaultBuilder builder(mgr);
		builder.f();

		Builder<ModuleA> builderA(mgr);
		builderA.a();

		Builder<ModuleA, ModuleB> builderAB(mgr);
		builderAB.a();
		builderAB.b();


	}


	TEST(BuilderTest, Struct) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = builder.structType(FieldList());
		EXPECT_TRUE(checks::check(type).empty())
			<< "Struct: " << *type << "\n"
			<< checks::check(type);
	}

	TEST(BuilderTest, MinMax) {

		NodeManager nm;
		IRBuilder builder(nm);

		auto expr1 = builder.parseExpr("1");
		auto expr2 = builder.parseExpr("2");

		auto result_min = builder.min(expr1, expr2);
		auto result_max = builder.max(expr1, expr2);

		const auto& basic = nm.getLangBasic();
		const core::LiteralPtr& select = basic.getSelect();

		auto op_Lt = nm.getLangBasic().getOperator(expr1->getType(), lang::BasicGenerator::Lt);
		auto op_Gt = nm.getLangBasic().getOperator(expr1->getType(), lang::BasicGenerator::Gt);

		auto exp_min = builder.callExpr(builder.infereExprType(select, expr1, expr2, op_Lt), select, expr1, expr2, op_Lt);
		auto exp_max = builder.callExpr(builder.infereExprType(select, expr1, expr2, op_Gt), select, expr1, expr2, op_Gt);

		EXPECT_FALSE(result_min == result_max);

		EXPECT_EQ(exp_min, result_min);
		EXPECT_EQ(exp_max, result_max);
	}

}
}

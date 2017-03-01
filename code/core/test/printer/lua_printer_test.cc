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

#include "insieme/core/printer/lua_printer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/normalize.h"

#include "insieme/utils/lua/lua.h"
#include "insieme/utils/string_utils.h"

namespace insieme {
namespace core {
namespace printer {

	TEST(LuaPrinter, SimpleExpr) {
		// sum up two values
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr intType = manager.getLangBasic().getInt4();
		VariablePtr a = builder.variable(intType, 1);
		VariablePtr b = builder.variable(intType, 2);
		ExpressionPtr t = builder.add(a, b);

		EXPECT_EQ("int_add(v1, v2)", toString(*t));
		EXPECT_EQ("v1 + v2", toLuaScript(t));

		t = builder.mul(t, builder.sub(t, a));
		EXPECT_EQ("int_mul(int_add(v1, v2), int_sub(int_add(v1, v2), v1))", toString(*t));
		EXPECT_EQ("(v1 + v2) * ((v1 + v2) - v1)", toLuaScript(t));

		// test whether script is syntactically correct
		utils::lua::Lua lua;
		lua.run("v1 = 1 ; v2 = 2");
		EXPECT_EQ(6, lua.eval<int>(toLuaScript(t)));
	}

	TEST(LuaPrinter, ControlFlow) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		std::map<std::string, core::NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,10>>"), 1);
		symbols["x"] = builder.variable(builder.parseType("ref<int<4>>"), 2);

		auto forStmt = analysis::normalize(builder.parseStmt(R"(
			for(int<4> k = 0 .. 10) {
				for(int<4> i = 0 .. 20) {
					var ref<int<4>> m = 10;
					for(int<4> j = 0 .. 30) {
						v[i] = *m;
						x = x + 1;
					}
				}
			})", symbols).as<ForStmtPtr>());

		string script = toLuaScript(forStmt);
		EXPECT_PRED2(containsSubString, script, "(10 - 1)");
		EXPECT_PRED2(containsSubString, script, "v1[v3] = (v4)");
		EXPECT_PRED2(containsSubString, script, "v2 = (v2) + 1");

		// test whether script is syntactically correct
		utils::lua::Lua lua;
		lua.run("v1 = {}; v2 = 0");
		lua.run(toLuaScript(forStmt));
		EXPECT_EQ(10 * 20 * 30, lua.eval<int>("v2"));
	}


} // end namespace printer
} // end namespace core
} // end namespace insieme

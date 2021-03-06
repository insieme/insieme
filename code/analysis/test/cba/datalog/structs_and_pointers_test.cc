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

#include "insieme/analysis/cba/datalog/structs_and_pointers_analysis.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace cba {

	using namespace insieme::core;
	using namespace datalog;

	TEST(SAP, PointersInteger) {
		NodeManager nm;
		IRBuilder builder(nm);
		Context ctxt;

		const string controlValue = "2222";

		auto in("{"
		        "var ref<int<4>> x = " + controlValue + ";"
		        "var ref<ptr<int<4>>> y = ptr_from_ref(x);"
		        "var ref<ptr<ptr<int<4>>>,f,f,plain> z = ptr_from_ref(y);"
		        "$x$;"
		        "$y$;"
		        "$z$;"
		        "}");

		auto ptr = builder.parseAddressesStatement(in);

		ExpressionAddress x = ptr[0].as<VariableAddress>();
		ExpressionAddress y = ptr[1].as<VariableAddress>();
		ExpressionAddress z = ptr[2].as<VariableAddress>();

		PointerResults res = runPointerAnalysis(ctxt, {x,y,z});

		EXPECT_TRUE(res[x].first);
		EXPECT_TRUE(res[y].first);
		EXPECT_TRUE(res[z].first);

		EXPECT_EQ(controlValue, res[x].second);
		EXPECT_EQ(controlValue, res[y].second);
		EXPECT_EQ(controlValue, res[z].second);

		// Also test non-bulk version
		for (const auto &var : {x,y,z}) {
			PointerResult res = runPointerAnalysis(ctxt, var);
			EXPECT_TRUE(res.first);
			EXPECT_EQ(controlValue, res.second);
		}
	}

	TEST(SAP, PointersCharacter) {
		NodeManager nm;
		IRBuilder builder(nm);
		Context ctxt;

		const string controlValue = "'x'";

		auto in("{"
		        "var ref<char> x = num_cast(" + controlValue + ", type_lit(char));"
		        "var ref<ptr<char>> y = ptr_from_ref(x);"
		        "var ref<ptr<ptr<char>>> z = ptr_from_ref(y);"
		        "$x$;"
		        "$y$;"
		        "$z$;"
		        "}");

		auto ptr = builder.parseAddressesStatement(in);

		ExpressionAddress x = ptr[0].as<VariableAddress>();
		ExpressionAddress y = ptr[1].as<VariableAddress>();
		ExpressionAddress z = ptr[2].as<VariableAddress>();

		PointerResults res = runPointerAnalysis(ctxt, {x,y,z});

		EXPECT_TRUE(res[x].first);
		EXPECT_TRUE(res[y].first);
		EXPECT_TRUE(res[z].first);

		EXPECT_EQ(controlValue, res[x].second);
		EXPECT_EQ(controlValue, res[y].second);
		EXPECT_EQ(controlValue, res[z].second);
	}

	TEST(SAP, DISABLED_StructsInteger) {
		NodeManager nm;
		IRBuilder builder(nm);
		Context ctxt;

		const string controlValueOne   = "111";
		const string controlValueTwo   = "222";
		const string controlValueThree = controlValueOne + controlValueTwo;

		auto in("decl struct FunkyStructure;"
		        ""
		        "decl FunkyStructure::eins : char;"
		        "decl FunkyStructure::zwei : char;"
		        "decl FunkyStructure::drei : int<4>;"
		        ""
		        "def struct FunkyStructure {"
		        "    eins : char;"
		        "    zwei : char;"
		        "    drei : int<4>;"
		        "};"
		        ""
		        "{"
		        "var ref<FunkyStructure> fooStruct = ref_decl(type_lit(ref<FunkyStructure>));"
		        ""
		        "fooStruct.eins = num_cast(" + controlValueOne + ", type_lit(char));"
		        "fooStruct.zwei = num_cast(" + controlValueTwo + ", type_lit(char));"
		        "fooStruct.drei = num_cast(*fooStruct.eins, type_lit(int<4>))+num_cast(*fooStruct.zwei, type_lit(int<4>));"
		        ""
		        "var ref<int<4>> deux = num_cast(*fooStruct.zwei, type_lit(int<4>));"
		        ""
		        "$fooStruct.eins$;"
		        "$deux$;"
		        "$fooStruct.drei$;"
		        "}"
		        );

		auto ptr = builder.parseAddressesStatement(in);

		ExpressionAddress eins = ptr[0].as<CallExprAddress>();
		ExpressionAddress deux = ptr[1].as<VariableAddress>();
		ExpressionAddress drei = ptr[2].as<CallExprAddress>();

		PointerResults res = runPointerAnalysis(ctxt, {eins, deux, drei});

		EXPECT_TRUE(res[eins].first);
		EXPECT_TRUE(res[deux].first);
		EXPECT_TRUE(res[drei].first);

		EXPECT_EQ(controlValueOne,   res[eins].second);
		EXPECT_EQ(controlValueTwo,   res[deux].second);
		EXPECT_EQ(controlValueThree, res[drei].second);
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

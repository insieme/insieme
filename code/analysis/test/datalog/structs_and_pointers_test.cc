#include <gtest/gtest.h>

#include "insieme/analysis/datalog/structs_and_pointers_analysis.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {

	using namespace insieme::core;

	TEST(SAP, PointersInteger) {
		NodeManager nm;
		IRBuilder builder(nm);

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

		datalog::PointerResult res = datalog::runPointerAnalysis({x,y,z});

		EXPECT_TRUE(res[x].first);
		EXPECT_TRUE(res[y].first);
		EXPECT_TRUE(res[z].first);

		EXPECT_EQ(controlValue, res[x].second);
		EXPECT_EQ(controlValue, res[y].second);
		EXPECT_EQ(controlValue, res[z].second);
	}

	TEST(SAP, PointersCharacter) {
		NodeManager nm;
		IRBuilder builder(nm);

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

		datalog::PointerResult res = datalog::runPointerAnalysis({x,y,z});

		EXPECT_TRUE(res[x].first);
		EXPECT_TRUE(res[y].first);
		EXPECT_TRUE(res[z].first);

		EXPECT_EQ(controlValue, res[x].second);
		EXPECT_EQ(controlValue, res[y].second);
		EXPECT_EQ(controlValue, res[z].second);
	}

} // end namespace analysis
} // end namespace insieme

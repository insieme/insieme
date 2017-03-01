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

#include "insieme/analysis/cba/haskell/context.h"
#include "insieme/analysis/cba/haskell/code_properties.h"

#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/dump/binary_dump.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

using namespace std;
using namespace insieme::core;
using namespace insieme::core::dump;

extern "C" {

	using insieme::analysis::cba::haskell::StablePtr;

	// Arithmetic
	arithmetic::Formula* hat_test_formulaZero();
	arithmetic::Formula* hat_test_formulaOne();
	arithmetic::Formula* hat_test_formulaExample1(const StablePtr addr);
	arithmetic::Formula* hat_test_formulaExample2(const StablePtr addr);

}

namespace insieme {
namespace analysis {
namespace cba {
namespace haskell {

	struct SimpleDeclaration {
		NodeManager manager;
		IRBuilder builder;
		NodePtr root;

		SimpleDeclaration() : builder(manager) {
			root = builder.parseStmt("{ "
				                     "   var int<4> x = 12; "
				                     "   2 + 3; "
				                     "   x; "
				                     "} ");
		}
	};

	class HaskellContext :
		public ::testing::Test,
		public SimpleDeclaration {};

	TEST_F(HaskellContext, Construction) {
		Context ctx(root);
	}

	TEST_F(HaskellContext, GetDefinitionPoint) {
		Context ctx(root);

		// get the targeted variable
		CompoundStmtAddress addrRoot(root.as<CompoundStmtPtr>());
		StatementAddress addrVar = addrRoot[2];
		EXPECT_TRUE(addrVar.isa<VariableAddress>());

		VariableAddress def = getDefinitionPoint(ctx,addrVar.as<VariableAddress>());
		EXPECT_TRUE(def);
		EXPECT_EQ(addrRoot[0].as<DeclarationStmtAddress>().getVariable(), def);
	}

	TEST_F(HaskellContext, FormulaZero) {
		arithmetic::Formula* formula = hat_test_formulaZero();
		EXPECT_EQ(arithmetic::Formula(), *formula);
		delete formula;
	}

	TEST_F(HaskellContext, FormulaOne) {
		arithmetic::Formula* formula = hat_test_formulaOne();
		EXPECT_EQ(arithmetic::Formula(1), *formula);
		delete formula;
	}

} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

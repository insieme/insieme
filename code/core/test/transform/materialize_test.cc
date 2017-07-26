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

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/transform/materialize.h"

namespace insieme {
namespace core {
namespace transform {

	TEST(Materialize, Basic) {
		NodeManager nm;
		IRBuilder builder(nm);

		const auto& refDeref = nm.getLangExtension<lang::ReferenceExtension>().getRefDeref();

		VariableList params;
		params.push_back(builder.variable(nm.getLangBasic().getInt4()));
		params.push_back(builder.variable(builder.refType(nm.getLangBasic().getInt4())));
		params.push_back(builder.variable(builder.refType(nm.getLangBasic().getInt4(), false, false, lang::ReferenceType::Kind::CppReference)));
		params.push_back(builder.variable(builder.refType(nm.getLangBasic().getInt4(), false, false, lang::ReferenceType::Kind::CppRValueReference)));
		auto body = builder.compoundStmt(params[0], params[1], params[2], params[3]);

		LambdaIngredients result = materialize({params, body});

		//ensure the parameters get wrapped in an additional ref as intended
		auto newParams = result.params;
		EXPECT_EQ(newParams[0]->getType(), builder.refType(nm.getLangBasic().getInt4())); //plain types get wrapped
		EXPECT_EQ(newParams[1]->getType(), builder.refType(builder.refType(nm.getLangBasic().getInt4()))); //plain refs get wrapped
		EXPECT_EQ(newParams[2]->getType(), builder.refType(nm.getLangBasic().getInt4(), false, false, lang::ReferenceType::Kind::CppReference)); //cpp refs do not get wrapped
		EXPECT_EQ(newParams[3]->getType(), builder.refType(nm.getLangBasic().getInt4(), false, false, lang::ReferenceType::Kind::CppRValueReference)); //cpp rrefs do not get wrapped

		//ensure the use of the parameters get wrapped in a deref as intended
		auto newBody = result.body.as<CompoundStmtPtr>();
		EXPECT_TRUE(analysis::isCallOf(newBody->getStatement(0), refDeref));
		EXPECT_TRUE(analysis::isCallOf(newBody->getStatement(1), refDeref));
		EXPECT_FALSE(analysis::isCallOf(newBody->getStatement(2), refDeref));
		EXPECT_FALSE(analysis::isCallOf(newBody->getStatement(3), refDeref));
	}

} //end namespace transform
} //end namespace core
} //end namespace insieme

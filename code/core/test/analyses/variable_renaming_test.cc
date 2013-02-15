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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/type_variable_renamer.h"

namespace insieme {
namespace core {
namespace analysis {

TEST(VariableRenamer, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);
	auto& basic = manager.getLangBasic();

	VariablePtr p1 = builder.variable(basic.getInt4());
	VariablePtr q1 = builder.variable(basic.getInt4());
	LambdaExprPtr lambda = builder.lambdaExpr(basic.getInt4(), builder.returnStmt(builder.intLit(0)), builder.parameters(p1,q1));
	VariablePtr p2 = builder.variable(basic.getInt4());
	VariablePtr q2 = builder.variable(basic.getUInt4());
	CallExprPtr call = builder.callExpr(lambda, p2, builder.castExpr(basic.getInt4(), q2));
	DeclarationStmtPtr decl2 = builder.declarationStmt(q2, builder.intLit(0));
	CompoundStmtPtr cmp1 = builder.compoundStmt(decl2, builder.returnStmt(call));

	VariablePtr p3 = builder.variable(builder.refType(basic.getInt4()));
	LambdaExprPtr lambda2 = builder.lambdaExpr(basic.getInt4(), cmp1, builder.parameters(p2));
	CallExprPtr call2 = builder.callExpr(lambda2, builder.deref(p3));
	DeclarationStmtPtr decl = builder.declarationStmt(p3, builder.intLit(0));


	CompoundStmtPtr cmp = builder.compoundStmt(toVector<StatementPtr>(decl, call2));
	std::cout << cmp << std::endl;

	const VariableAddress& p1Addr = core::Address<const core::Variable>::find(p1, cmp);
	const VariableAddress& q1Addr = core::Address<const core::Variable>::find(q1, cmp);

	utils::map::PointerMap<VariableAddress, VariableAddress> vm = getRenamedVariableMap(toVector<VariableAddress>(p1Addr, q1Addr));

	if(vm[q1Addr] && vm[p1Addr]) {
		EXPECT_EQ(*q2, *vm[q1Addr]);
		EXPECT_EQ(*p3, *vm[p1Addr]);
	} else
		EXPECT_TRUE(vm[q1Addr] && vm[p1Addr]);
}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

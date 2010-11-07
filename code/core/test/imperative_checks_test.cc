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

#include "insieme/core/ast_builder.h"
#include "insieme/core/checks/imperativechecks.h"

namespace insieme {
namespace core {
namespace checks {

bool containsMSG(const MessageList& list, const Message& msg) {
	return contains(list, msg);
}

TEST(UndeclaredVariableCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a function literal
	TypePtr type = builder.genericType("int");
	VariablePtr varA = builder.variable(type);
	VariablePtr varB = builder.variable(type);

	ExpressionPtr init = builder.literal(type, "4");

	FunctionTypePtr funType = builder.functionType(builder.tupleType(), type);

	NodePtr ok = builder.lambdaExpr(funType, toVector<DeclarationStmtPtr>(), toVector<VariablePtr>(), builder.declarationStmt(varA, init));
	NodePtr err = builder.lambdaExpr(funType, toVector<DeclarationStmtPtr>(), toVector<VariablePtr>(), builder.declarationStmt(varA, varB));


	CheckPtr typeCheck = make_check<UndeclaredVariableCheck>();
	EXPECT_TRUE(check(ok, typeCheck).empty());
	ASSERT_FALSE(check(err, typeCheck).empty());

	EXPECT_PRED2(containsMSG, check(err,typeCheck), Message(NodeAddress(err), EC_IMPERATIVE_UNDECLARED_VARIABLE_USAGE, "", Message::ERROR));
}


} // end namespace checks
} // end namespace core
} // end namespace insieme


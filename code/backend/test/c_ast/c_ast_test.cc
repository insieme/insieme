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

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_ast_printer.h"

namespace insieme {
namespace backend {
namespace c_ast {


TEST(C_AST, Basic) {

	// create a simple expression
	CNodeManager manager;
	CBasics basics(manager);

	TypePtr intType = basics.getIntType();
	EXPECT_TRUE(intType);
	EXPECT_EQ("int", toString(intType));

	VariablePtr x = var(intType, "x");
	VariablePtr y = var(intType, "y");
	ExpressionPtr sum = add(x,y);

	EXPECT_EQ("x", x->name->name);

	EXPECT_EQ("x", toString(x));
	EXPECT_EQ("y", toString(y));
	EXPECT_EQ("x+y", toString(sum));

	sum = add(sum, x);
	EXPECT_EQ("x+y+x", toString(sum));

}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme


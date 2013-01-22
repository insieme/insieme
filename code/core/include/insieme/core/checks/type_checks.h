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

#pragma once

#include "insieme/core/checks/ir_checks.h"

namespace insieme {
namespace core {
namespace checks {

// defines macros for generating CHECK declarations
#include "insieme/core/checks/check_macros.inc"

SIMPLE_CHECK(Keyword, GenericType, true);
SIMPLE_CHECK(FunctionKind, FunctionType, true);

SIMPLE_CHECK(Parent, Parent, true);
SIMPLE_CHECK(ClassInfo, Type, true);

SIMPLE_CHECK(CallExprType, CallExpr, false);
SIMPLE_CHECK(FunctionType, LambdaExpr, false);
SIMPLE_CHECK(BindExprType, BindExpr, false);
SIMPLE_CHECK(ExternalFunctionType, Literal, false);
SIMPLE_CHECK(ReturnType, Lambda, false);
SIMPLE_CHECK(LambdaType, LambdaExpr, false);
SIMPLE_CHECK(ArrayType, Node, true);

SIMPLE_CHECK(DeclarationStmtType, DeclarationStmt, false);
SIMPLE_CHECK(IfConditionType, IfStmt, false);
SIMPLE_CHECK(ForStmtType, ForStmt, false);
SIMPLE_CHECK(WhileConditionType, WhileStmt, false);
SIMPLE_CHECK(SwitchExpressionType, SwitchStmt, false);

SIMPLE_CHECK(StructExprType, StructExpr, false);
SIMPLE_CHECK(MemberAccessElementType, CallExpr, false);
SIMPLE_CHECK(ComponentAccessType, CallExpr, false);

SIMPLE_CHECK(BuiltInLiteral, Literal, false);

SIMPLE_CHECK(RefCast, CastExpr, false);

SIMPLE_CHECK(Cast, CastExpr, false);

SIMPLE_CHECK(Narrow, CallExpr, false);
SIMPLE_CHECK(Expand, CallExpr, false);

// TODO:
//	- check that only concrete types are used for variables

#undef SIMPLE_CHECK

} // end namespace check
} // end namespace core
} // end namespace insieme


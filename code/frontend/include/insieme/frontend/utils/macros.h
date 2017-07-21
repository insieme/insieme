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
#pragma once

/*****************************************************************************************************
 *      LOG MACROS
*****************************************************************************************************/

#define LOG_EXPR_CONVERSION(parentExpr, expr)                                                                                                                  \
	FinalActions attachLog([&]() {                                                                                                                             \
		VLOG(1) << "******      EXPR  [class:'" << parentExpr->getStmtClassName() << "'] ******";                                                              \
		if(VLOG_IS_ON(2)) {                                                                                                                                    \
			VLOG(2) << "Dump of clang expression: ";                                                                                                           \
			parentExpr->dump();                                                                                                                                \
		}                                                                                                                                                      \
		VLOG(1) << "-> at location: (" << utils::location(parentExpr->getLocStart(), converter.getSourceManager()) << "); ";                                   \
		VLOG(1) << "Converted into IR expression: ";                                                                                                           \
		if(expr) {                                                                                                                                             \
			VLOG(1) << "\n" << dumpOneLine(expr) << "\n of type:( " << *expr->getType() << " )";                                                               \
		} else {                                                                                                                                               \
			VLOG(1) << "\tno expression";                                                                                                                      \
		}                                                                                                                                                      \
		VLOG(1) << "****** DONE EXPR [class:'" << parentExpr->getStmtClassName() << "'] ******";                                                               \
	})


#define LOG_STMT_CONVERSION(parentStmt, stmt)                                                                                                                  \
	FinalActions attachLog([&]() {                                                                                                                             \
		VLOG(1) << "******      STMT [class:'" << parentStmt->getStmtClassName() << "'] ******";                                                               \
		if(VLOG_IS_ON(2)) {                                                                                                                                    \
			VLOG(2) << "Dump of clang statement:";                                                                                                             \
			parentStmt->dump(converter.getSourceManager());                                                                                                    \
		}                                                                                                                                                      \
		VLOG(1) << "-> at location: (" << utils::location(parentStmt->getLocStart(), converter.getSourceManager()) << "); ";                                   \
		VLOG(1) << "Converted 'statement' into IR stmt: ";                                                                                                     \
		VLOG(1) << stmt;                                                                                                                                       \
		VLOG(1) << "****** DONE STMT [class:'" << parentStmt->getStmtClassName() << "'] ******";                                                               \
	})


#define LOG_BUILTIN_TYPE_CONVERSION(parentType)                                                                                                                \
	VLOG(1) << "**********************TYPE*[class:'" << parentType->getTypeClassName() << "']**********************";                                          \
	if(VLOG_IS_ON(2)) {                                                                                                                                        \
		VLOG(2) << "Dump of clang type:";                                                                                                                      \
		parentType->dump();                                                                                                                                    \
	}                                                                                                                                                          \
	VLOG(1) << "****************************************************************************************";


#define LOG_TYPE_CONVERSION(parentType, retType)                                                                                                               \
	FinalActions attachLog([&]() {                                                                                                                             \
		VLOG(1) << "**********************TYPE*[class:'" << parentType->getTypeClassName() << "']**********************";                                      \
		if(VLOG_IS_ON(2)) {                                                                                                                                    \
			VLOG(2) << "Dump of clang type:";                                                                                                                  \
			parentType->dump();                                                                                                                                \
		}                                                                                                                                                      \
		if(retType) {                                                                                                                                          \
			VLOG(1) << "Converted 'type' into IR type: ";                                                                                                      \
			VLOG(1) << "\t" << *retType;                                                                                                                       \
		}                                                                                                                                                      \
		VLOG(1) << "****************************************************************************************";                                                 \
	})

/*****************************************************************************************************
 *      ASSERT macros
 *      asserts specific for the frontend, use some features that can only be found in the frontend
*****************************************************************************************************/

#include "insieme/utils/assert.h"

#define frontend_assert(_COND) assert_true(_COND)

#define frontend_assert_expr(_PRED, _EXPR)                                                                                                                     \
	assert_true(_PRED(_EXPR)) << "Expr: " << dumpColor(_EXPR) << "\nType: " << dumpColor(_EXPR->getType())

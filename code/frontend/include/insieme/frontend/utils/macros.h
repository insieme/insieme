/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

/**
  * this macro is meant to be used in the visitors (stmt, expr and type) it requires the object converter to be present
  * in the scope to able to print the current translating location
  */
#define frontend_assert(_COND) assert_true(_COND) << " ==> last Trackable location: " << converter.getLastTrackableLocation() << "\n"

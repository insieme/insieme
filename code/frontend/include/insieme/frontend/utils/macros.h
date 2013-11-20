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

/*****************************************************************************************************
 *      IR QUERING METHODS
 *      shortcuts to access or retrieve specific IR features
*****************************************************************************************************/

#define GET_TYPE_PTR(type) (type)->getType().getTypePtr()

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define IS_ARRAY_TYPE(type)\
	(type.isa<core::RefTypePtr>() && type.as<core::RefTypePtr>()->getElementType().isa<core::ArrayTypePtr>())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())

#define IS_CPP_REF(type) \
	(core::analysis::isCppRef(type) || core::analysis::isConstCppRef(type))

#define IS_IR_REF(type) \
	(type->getNodeType() == core::NT_RefType)

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())


/*****************************************************************************************************
 *      LOG MACROS
*****************************************************************************************************/

#define LOG_EXPR_CONVERSION(parentExpr, expr) \
	FinalActions attachLog( [&] () { \
        VLOG(1) << "*************     EXPR  [class:'"<< parentExpr->getStmtClassName() <<"']         ***************************"; \
        if( VLOG_IS_ON(2) ) { \
            VLOG(2) << "Dump of clang expression: "; \
            parentExpr->dump(); \
        } \
        VLOG(1) << "-> at location: (" <<	\
                    utils::location(parentExpr->getLocStart(), convFact.getSourceManager()) << "); "; \
        VLOG(1) << "Converted into IR expression: "; \
        if(expr) { \
            VLOG(1) << "\t" << *expr << " type:( " << *expr->getType() << " )"; \
        } \
        VLOG(1) << "****************************************************************************************"; \
    } )


#define LOG_STMT_CONVERSION(parentStmt, stmt) \
	FinalActions attachLog( [&] () { \
        VLOG(1) << "**********************STMT*[class:'"<< parentStmt->getStmtClassName() <<"']**********************"; \
        if( VLOG_IS_ON(2) ) { \
            VLOG(2) << "Dump of clang statement:"; \
            parentStmt->dump(convFact.getSourceManager()); \
        } \
        VLOG(1) << "-> at location: (" \
                << utils::location(parentStmt->getLocStart(), convFact.getSourceManager()) << "); "; \
        VLOG(1) << "Converted 'statement' into IR stmt: "; \
        VLOG(1) << "\t" << stmt << ""; \
        VLOG(1) << "****************************************************************************************"; \
    } )


#define LOG_BUILTIN_TYPE_CONVERSION(parentType) \
    VLOG(1) << "**********************TYPE*[class:'"<< parentType->getTypeClassName() <<"']**********************"; \
    if( VLOG_IS_ON(2) ) { \
        VLOG(2) << "Dump of clang type:"; \
        parentType->dump(); \
    } \
    VLOG(1) << "****************************************************************************************";


#define LOG_TYPE_CONVERSION(parentType, retType) \
	FinalActions attachLog( [&] () { \
        VLOG(1) << "**********************TYPE*[class:'"<< parentType->getTypeClassName() <<"']**********************"; \
        if( VLOG_IS_ON(2) ) { \
            VLOG(2) << "Dump of clang type:";\
            parentType->dump(); \
        } \
        if(retType) { \
            VLOG(1) << "Converted 'type' into IR type: "; \
            VLOG(1) << "\t" << *retType; \
        } \
        VLOG(1) << "****************************************************************************************"; \
    } )

/*****************************************************************************************************
 *      ASSERT macros
 *      asserts specific for the frontend, they make use of some featues that can only be found in 
 *      frontend
*****************************************************************************************************/

#include "insieme/utils/assert.h"

#ifdef NDEBUG

	#define frontend_assert(_COND) _assert_ignore

#else

	/**
	 * this macro is ment to be used in the visitors ( stmt, expr and type) it requires the object convFact to be pressent
	 * in the scope to able to print the current translating location
	 */
	#define frontend_assert(_COND) \
		if (__unused auto x = insieme::utils::detail::LazyAssertion((bool)(_COND))) \
			std::cerr \
			<< "\nAssertion " #_COND " of " __FILE__ ":" __xstr(__LINE__) " failed!\n"\
			<< " ==> last Trackable location: " << convFact.getLastTrackableLocation() << "\n"\
			<< "Message: "

#endif

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

#ifndef NDEBUG

#include "insieme/core/checks/full_check.h"
#include "insieme/core/types/subtyping.h"

#define DEBUG_CHECK(expr)\
	core::checks::MessageList&& errors = core::checks::check(expr); \
	if(!errors.empty()){\
		std::cout << " ======================= " << std::endl; \
		dumpPretty(expr); \
		std::cout << errors << std::endl;\
		std::cout << " ======================= " << std::endl; \
		abort();\
	}

#define ASSERT_EQ_TYPES(typeA, typeB)\
	if( *typeA != *typeB ){\
		std::cout << " === TYPES MISSMATCH [" << __FILE__ << ":" << __LINE__ << "] ===" << std::endl; \
		dumpPretty(typeA); \
		std::cout << " vs " << std::endl; \
		dumpPretty(typeB); \
		std::cout << " ======================= " << std::endl; \
		exit(-1);\
	}

#define ASSERT_IS_SUBTYPE(typeA, typeB)\
	if( !(insieme::core::types::isSubTypeOf(typeA, typeB)) ){\
		std::cout << " === SUB-TYPE MISSMATCH [" << __FILE__ << ":" << __LINE__ << "] ===" << std::endl; \
		dumpPretty(typeA); \
		std::cout << " vs " << std::endl; \
		dumpPretty(typeB); \
		std::cout << " ======================= " << std::endl; \
		exit(-1);\
	}

#define PRINTLOCATION(expr)\
	std::cout << utils::location(expr->getLocStart(), expr->getASTContext().getSourceManager()) << std::endl;


////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////
//
#else

#define DEBUG_CHECK(expr)\
	{ }

#define ASSERT_EQ_TYPES(typeA, typeB)\
	{ }

#define ASSERT_IS_SUBTYPE(typeA, typeB)\
	{ }

#define PRINTLOCATION(expr)\
	{ }



#endif

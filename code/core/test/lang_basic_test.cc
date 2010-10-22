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

#include "lang_basic.h"


using namespace insieme::core;
using namespace insieme::core::lang;

TEST(LangBasic, Types) {

	// test some signed integers ...
	{
		GenericType types[] = {TYPE_INT_1_VAL, TYPE_INT_2_VAL, TYPE_INT_4_VAL, TYPE_INT_8_VAL};
		for (int i=0; i<4; i++) {
			EXPECT_TRUE ( isIntegerType(types[i]) );
			EXPECT_TRUE ( isIntType(types[i]) );
			EXPECT_FALSE ( isUIntType(types[i]) );
			EXPECT_EQ ( 1<<i, getNumBytes(types[i]) );
		}
		EXPECT_TRUE ( isIntType(TYPE_INT_GEN_VAL) );
		EXPECT_TRUE ( isIntType(TYPE_INT_INF_VAL) );
	}


	// test some unsigned integers ...
	{
		GenericType types[] = {TYPE_UINT_1_VAL, TYPE_UINT_2_VAL, TYPE_UINT_4_VAL, TYPE_UINT_8_VAL};
		for (int i=0; i<4; i++) {
			EXPECT_TRUE ( isIntegerType(types[i]) );
			EXPECT_FALSE ( isIntType(types[i]) );
			EXPECT_TRUE ( isUIntType(types[i]) );
			EXPECT_EQ ( 1<<i, getNumBytes(types[i]) );
		}
		EXPECT_TRUE ( isUIntType(TYPE_UINT_GEN_VAL) );
		EXPECT_TRUE ( isUIntType(TYPE_UINT_INF_VAL) );
	}
}


TEST(LangBasic, BuildInTest) {

	NodeManager manager;

	// verify the build-in functionality (not for all types)
	EXPECT_TRUE( lang::isBuildIn(lang::CONST_BOOL_FALSE_PTR->getValue()) );
	EXPECT_TRUE( lang::isBuildIn(lang::CONST_BOOL_FALSE_PTR) );
	EXPECT_EQ( lang::CONST_BOOL_FALSE_PTR, lang::getBuildInForValue(lang::CONST_BOOL_FALSE_PTR->getValue()) );

	// same type within other manager
	LiteralPtr type = manager.get(lang::CONST_BOOL_FALSE_PTR);
	EXPECT_TRUE( lang::isBuildIn(type) );
	EXPECT_TRUE( lang::isBuildIn(type->getValue()) );

}

TEST(LangBasic, Subscript) {
	NodeManager manager;

//	EXPECT_EQ ("", toString(*lang::OP_VAR_LIST_PACK->getType()));
//	EXPECT_EQ ("", toString(*lang::OP_VAR_LIST_PACK));
//
//	EXPECT_EQ ("", toString(*lang::TYPE_CHAR));
//
//	EXPECT_EQ ("", toString(*lang::OP_SUBSCRIPT->getType()));
//	EXPECT_EQ ("", toString(*lang::OP_SUBSCRIPT));
//	EXPECT_EQ ("", toString(*lang::OP_LENGTH_VAL.getType()));
}

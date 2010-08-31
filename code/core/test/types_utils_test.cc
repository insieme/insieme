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
#include <algorithm>
#include <stdexcept>
#include <vector>

#include "types_utils.h"
#include "container_utils.h"

using std::vector;

using namespace insieme::core;

TEST(TypeTest, IntType) {

	// create type manager and element types
	NodeManager manager;
	IntTypePtr intType = IntType::get(manager, 4);
	EXPECT_EQ ( 4 , intType->getNumBytes());
	EXPECT_EQ ( "int<4>", intType->getName() );
	EXPECT_TRUE ( intType->getTypeParameter().empty() );
	EXPECT_TRUE ( toVector(IntTypeParam::getConcreteIntParam(4)) == intType->getIntTypeParameter());
	EXPECT_EQ ( TypePtr(NULL), intType->getBaseType() );

	IntTypePtr intType2 = IntType::get(manager, 2);
	EXPECT_EQ ( 2 , intType2->getNumBytes());
	EXPECT_TRUE ( intType2->getTypeParameter().empty() );
	EXPECT_EQ ( "int<2>", intType2->getName() );
	EXPECT_TRUE ( intType2->getTypeParameter().empty() );
	EXPECT_TRUE ( toVector(IntTypeParam::getConcreteIntParam(2)) == intType2->getIntTypeParameter());
	EXPECT_EQ ( TypePtr(NULL), intType2->getBaseType() );

	IntTypePtr intType3 = IntType::get(manager, 4);
	EXPECT_NE (intType, intType2);
	EXPECT_EQ (intType, intType3);
}



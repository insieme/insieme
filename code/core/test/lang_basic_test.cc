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

#include "insieme/core/lang/basic.h"

#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"

using namespace insieme::core;
using namespace insieme::core::lang;

TEST(LangBasic, Generation) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isInt4(nm.basic.getInt4()));
}

TEST(LangBasic, BoolChecks) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isBuiltIn(nm.basic.getInt4()));
	EXPECT_TRUE(nm.basic.isBuiltIn(nm.basic.getIntLShift()));
	EXPECT_FALSE(nm.basic.isBuiltIn(GenericType::get(nm, "surelyNotBuiltInISincerelyHope__")));
}

TEST(LangBasic, StringGet) {
	NodeManager nm;

	EXPECT_EQ(nm.basic.getBarrier(), nm.basic.getLiteral("barrier"));
	EXPECT_EQ(LiteralPtr(), nm.basic.getLiteral("surelyNotBuiltInISincerelyHope__"));
}

TEST(LangBasic, Grouping) {
	NodeManager nm;

	EXPECT_TRUE(nm.basic.isUnsignedInt(nm.basic.getUInt4()));
	EXPECT_TRUE(nm.basic.isInt(nm.basic.getUInt4()));
	EXPECT_FALSE(nm.basic.isReal(nm.basic.getUInt4()));
}

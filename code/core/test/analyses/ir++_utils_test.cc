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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace analysis {

	TEST(IRppUtils, PureVirtual) {
		NodeManager manager;
		IRBuilder builder(manager);

		// BUG: free variables within binds have not been recognized correctly
		// reason: recursive call for bound parameters was wrong =>

		auto funType = builder.parseType("A::()->unit").as<FunctionTypePtr>();

		ASSERT_TRUE(funType);
		EXPECT_TRUE(funType->isMemberFunction());

		// create a pure virtual version
		auto pureVirtual = builder.getPureVirtual(funType);

		// should be correct
		EXPECT_TRUE(checks::check(pureVirtual).empty()) << checks::check(pureVirtual);

		EXPECT_TRUE(isPureVirtual(pureVirtual));
	}

	TEST(IRppUtils, References) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto type = builder.genericType("A");

		// test references
		EXPECT_FALSE(isCppRef(type));
		EXPECT_PRED1(isCppRef, getCppRef(type));
		EXPECT_EQ(type,getCppRefElementType(getCppRef(type)));


		// test const references
		EXPECT_FALSE(isConstCppRef(type));
		EXPECT_PRED1(isConstCppRef, getConstCppRef(type));
		EXPECT_EQ(type,getCppRefElementType(getConstCppRef(type)));


		// test mixture
		EXPECT_FALSE(isCppRef(getConstCppRef(type)));
		EXPECT_FALSE(isConstCppRef(getCppRef(type)));
	}

	TEST(IRppUtils, DefaultCtorTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create a struct type
		StructTypePtr type = builder.parseType("struct { int x; int y; }").as<StructTypePtr>();
		ASSERT_TRUE(type);

		// create a default constructor for this type
		auto ctor = createDefaultConstructor(type);
		EXPECT_TRUE(checks::check(ctor).empty()) << ctor << checks::check(ctor);

		EXPECT_PRED1(isDefaultConstructor, ctor);
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

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

#include <gtest/gtest.h>

#include "insieme/core/lang/reference.h"
#include "insieme/core/test/test_utils.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Reference, SemanticChecks) {
		NodeManager nm;
		auto& ext = nm.getLangExtension<ReferenceExtension>();
		semanticCheckSecond(ext.getSymbols());
	}


	TEST(Reference, IsReference) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto A = builder.parseType("A");
		EXPECT_TRUE(isReference(ReferenceType::create(A)));
		EXPECT_TRUE(isReference(ReferenceType::create(A, false, true)));

		EXPECT_FALSE(isReference(builder.parseType("A")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_TRUE(isReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isReference(builder.parseType("ref<A,f,t,bla>")));
		EXPECT_FALSE(isReference(builder.parseType("ref<A,c,t,cpp_rref>")));
		EXPECT_FALSE(isReference(builder.parseType("ref<A,f,c,cpp_rref>")));


		EXPECT_TRUE(isPlainReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_FALSE(isPlainReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isPlainReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isCppReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_TRUE(isCppReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_FALSE(isCppReference(builder.parseType("ref<A,f,t,cpp_rref>")));

		EXPECT_FALSE(isCppRValueReference(builder.parseType("ref<A,f,t,plain>")));
		EXPECT_FALSE(isCppRValueReference(builder.parseType("ref<A,f,t,cpp_ref>")));
		EXPECT_TRUE(isCppRValueReference(builder.parseType("ref<A,f,t,cpp_rref>")));

	}

} // end namespace lang
} // end namespace core
} // end namespace insieme

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

#include <sstream>

#include "insieme/core/lang/lang.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/dump/binary_dump.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Lang, DerivedTagBasic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto A = builder.genericType("A");
		auto B = builder.genericType("B", toVector<TypePtr>(A));

		// should not be considered a derived node by default
		EXPECT_FALSE(isDerived(A));
		EXPECT_FALSE(isDerived(B));

		// mark as derived
		markAsDerived(B, "B");
		EXPECT_FALSE(isDerived(A));
		EXPECT_TRUE(isDerived(B));

		EXPECT_EQ("B", getConstructName(B));
	}

	TEST(Lang, DerivedTagCloning) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto A = builder.genericType("A");
		auto B = builder.genericType("B", toVector<TypePtr>(A));

		// mark B as being derived
		markAsDerived(B, "B");
		EXPECT_FALSE(isDerived(A));
		EXPECT_TRUE(isDerived(B));

		// clone nodes to other node manager
		NodeManager mgrB;
		EXPECT_FALSE(isDerived(mgrB.get(A)));
		EXPECT_TRUE(isDerived(mgrB.get(B)));
		EXPECT_EQ("B", getConstructName(mgrB.get(B)));
	}

	TEST(Lang, DerivedTagManipulation) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto A = builder.genericType("A");
		auto B = builder.genericType("B", toVector<TypePtr>(A));

		// mark B as being derived
		markAsDerived(B, "B");
		EXPECT_FALSE(isDerived(A));
		EXPECT_TRUE(isDerived(B));

		// alter B - derived flag should be gone
		auto C = builder.genericType("C");

		auto B2 = transform::replaceNode(mgr, GenericTypeAddress(B)->getTypeParameter(0), C);
		EXPECT_EQ("B<C>", toString(*B2));
		EXPECT_FALSE(isDerived(B2));
	}

	TEST(Lang, DerivedTagDumping) {

		// the tag should survive to be dumped and restored again ...

		NodeManager mgr;
		IRBuilder builder(mgr);
		auto A = builder.genericType("A");
		auto B = builder.genericType("B", toVector<TypePtr>(A));

		// mark as derived
		markAsDerived(B, "B");
		EXPECT_FALSE(isDerived(A));
		EXPECT_TRUE(isDerived(B));


		// create a in-memory stream
		std::stringstream buffer(std::ios_base::out | std::ios_base::in | std::ios_base::binary);

		// dump IR using a binary format
		dump::binary::dumpIR(buffer, B);

		// reload IR using a different node manager
		NodeManager managerB;
		NodePtr B2 = dump::binary::loadIR(buffer, managerB);

		EXPECT_NE(B, B2);
		EXPECT_EQ(*B, *B2);

		// annotation should still be available
		EXPECT_TRUE(isDerived(B2));
		EXPECT_EQ("B", getConstructName(B2));

	}

} // end namespace lang
} // end namespace core
} // end namespace insieme

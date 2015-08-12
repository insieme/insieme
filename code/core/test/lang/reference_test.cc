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

#include "insieme/core/ir_node.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace lang {

	TEST(Reference, Basic) {
		NodeManager nm;

		IRBuilder builder(nm);

		auto a = builder.genericType("A");
		auto raff = ReferenceType::create(a);
		auto raft = ReferenceType::create(a,false,true);
		auto ratf = ReferenceType::create(a,true,false);
		auto ratt = ReferenceType::create(a,true,true);

		EXPECT_EQ("ref<A,f,f>", toString(*raff));
		EXPECT_EQ("ref<A,f,t>", toString(*raft));
		EXPECT_EQ("ref<A,t,f>", toString(*ratf));
		EXPECT_EQ("ref<A,t,t>", toString(*ratt));

		EXPECT_TRUE(ReferenceType::isReferenceType(raff));
		EXPECT_TRUE(ReferenceType::isReferenceType(raft));
		EXPECT_TRUE(ReferenceType::isReferenceType(ratf));
		EXPECT_TRUE(ReferenceType::isReferenceType(ratt));


		EXPECT_FALSE(ReferenceType(raff).isConst());
		EXPECT_FALSE(ReferenceType(raff).isVolatile());

		EXPECT_FALSE(ReferenceType(raft).isConst());
		EXPECT_TRUE(ReferenceType(raft).isVolatile());

		EXPECT_TRUE(ReferenceType(ratf).isConst());
		EXPECT_FALSE(ReferenceType(ratf).isVolatile());

		EXPECT_TRUE(ReferenceType(ratt).isConst());
		EXPECT_TRUE(ReferenceType(ratt).isVolatile());

		EXPECT_EQ(raff, (GenericTypePtr)(ReferenceType(raff)));
		EXPECT_EQ(ratf, (GenericTypePtr)(ReferenceType(ratf)));
		EXPECT_EQ(raft, (GenericTypePtr)(ReferenceType(raft)));
		EXPECT_EQ(ratt, (GenericTypePtr)(ReferenceType(ratt)));

	}

	TEST(Reference, IsReferenceType) {
		NodeManager nm;
		IRBuilder builder(nm);

		auto& ext = nm.getLangExtension<ReferenceExtension>();

		EXPECT_TRUE(ReferenceType::isReferenceType(ext.getGenRef()));

		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,f,f>")));
		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,t,f>")));
		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,f,t>")));
		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,t,t>")));

		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<ref<A,t,f>,t,t>")));

		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,'a,t>")));
		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,t,'b>")));
		EXPECT_TRUE(ReferenceType::isReferenceType(builder.parseType("ref<A,'a,'b>")));

		EXPECT_FALSE(ReferenceType::isReferenceType(builder.parseType("A")));
		EXPECT_FALSE(ReferenceType::isReferenceType(builder.parseType("ref<A,t>")));
		EXPECT_FALSE(ReferenceType::isReferenceType(builder.parseType("ref<A,t,t,t>")));

	}

} // end namespace lang
} // end namespace core
} // end namespace insieme



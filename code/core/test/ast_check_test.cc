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

#include "ast_builder.h"
#include "ast_check.h"
#include "container_utils.h"

namespace insieme {
namespace core {


class AllFine : public ASTCheck {};

class IDontLikeAnythingCheck : public ASTCheck {
	MessageList visitNode(const NodeAddress& node) {
		return toVector(Message(node, 1, "I hate it!"));
	}
};

class IAmScaredCheck : public ASTCheck {
	MessageList visitNode(const NodeAddress& node) {
		return toVector(Message(node, 2, "Don't know - I'm scared!", Message::WARNING));
	}
};

TEST(ASTCheck, Basic) {
	ASTBuilder builder;

	// OK ... create a simple node
	TypePtr type = builder.genericType("A");
	NodeAddress addr(type);

	Message msgA(addr, 1, "I hate it!");
	Message msgB(addr, 2, "Don't know - I'm scared!", Message::WARNING);

	EXPECT_EQ("0", toString(addr));
	NodeAddress& adr2 = addr;
	EXPECT_EQ("0", toString(adr2));
	EXPECT_EQ("ERROR:   [00001]@ (0) - MSG: I hate it!", toString(msgA));
	EXPECT_EQ("WARNING: [00002]@ (0) - MSG: Don't know - I'm scared!", toString(msgB));

	MessageList res = check(type, make_check<AllFine>());
	EXPECT_TRUE(equals(toVector<Message>(), res));

	res = check(type, make_check<IDontLikeAnythingCheck>());
	EXPECT_TRUE(equals(toVector(msgA), res));

	res = check(type, make_check<IAmScaredCheck>());
	EXPECT_TRUE(equals(toVector(msgB), res));

	res = check(type, combine(toVector<CheckPtr>(std::make_shared<IAmScaredCheck>(), std::make_shared<IDontLikeAnythingCheck>())));
	EXPECT_EQ(toVector(msgB, msgA), res);

}

TEST(ASTCheck, Decorators) {
	ASTBuilder builder;

	// build diamond - again ...
	TypePtr typeD = builder.genericType("D");
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

	// check recursive
	CheckPtr recCheck = makeRecursive(make_check<IDontLikeAnythingCheck>());

	EXPECT_EQ((std::size_t)5, check(typeA, recCheck).size());
	EXPECT_EQ((std::size_t)2, check(typeB, recCheck).size());
	EXPECT_EQ((std::size_t)2, check(typeC, recCheck).size());
	EXPECT_EQ((std::size_t)1, check(typeD, recCheck).size());

	CheckPtr onceCheck = makeVisitOnce(make_check<IDontLikeAnythingCheck>());
	EXPECT_EQ((std::size_t)4, check(typeA, onceCheck).size());
	EXPECT_EQ((std::size_t)2, check(typeB, onceCheck).size());
	EXPECT_EQ((std::size_t)2, check(typeC, onceCheck).size());
	EXPECT_EQ((std::size_t)1, check(typeD, onceCheck).size());


	// derive list of all addresses
	NodeAddress adr1(typeA);
	NodeAddress adr2 = adr1.getAddressOfChild(0);
	NodeAddress adr3 = adr2.getAddressOfChild(0);
	NodeAddress adr4 = adr1.getAddressOfChild(1);
	NodeAddress adr5 = adr4.getAddressOfChild(0);

	MessageList res = check(typeA, recCheck);
	std::sort(res.begin(), res.end());
	ASSERT_EQ((std::size_t)5, res.size());
	EXPECT_EQ(res[0], Message(adr1, 1, "", Message::ERROR));
	EXPECT_EQ(res[1], Message(adr2, 1, "", Message::ERROR));
	EXPECT_EQ(res[2], Message(adr3, 1, "", Message::ERROR));
	EXPECT_EQ(res[3], Message(adr4, 1, "", Message::ERROR));
	EXPECT_EQ(res[4], Message(adr5, 1, "", Message::ERROR));

	res = check(typeA, onceCheck);
	std::sort(res.begin(), res.end());
	ASSERT_EQ((std::size_t)4, res.size());
	EXPECT_EQ(res[0], Message(adr1, 1, "", Message::ERROR));
	EXPECT_EQ(res[1], Message(adr2, 1, "", Message::ERROR));
	EXPECT_EQ(res[2], Message(adr3, 1, "", Message::ERROR));
	EXPECT_EQ(res[3], Message(adr4, 1, "", Message::ERROR));

}


} // end namespace core
} // end namespace insieme


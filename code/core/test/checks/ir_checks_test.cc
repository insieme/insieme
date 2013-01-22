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

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {
namespace checks {


class AllFine : public IRCheck {
public:
	AllFine() : IRCheck(true) {}
};

class IDontLikeAnythingCheck : public IRCheck {
public:
	IDontLikeAnythingCheck() : IRCheck(true) {}
	OptionalMessageList visitNode(const NodeAddress& node) {
		if (node->getNodeType() != NT_GenericType) return 0;
		return MessageList(Message(node, (ErrorCode)1, "I hate it!"));
	}
};

class IAmScaredCheck : public IRCheck {
public:
	IAmScaredCheck() : IRCheck(true) {}
	OptionalMessageList visitNode(const NodeAddress& node) {
		return MessageList(Message(node, (ErrorCode)2, "Don't know - I'm scared!", Message::WARNING));
	}
};

TEST(IRCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// OK ... create a simple node
	TypePtr type = builder.genericType("A");
	NodeAddress addr(type);

	Message msgA(addr, (ErrorCode)1, "I hate it!");
	Message msgB(addr, (ErrorCode)2, "Don't know - I'm scared!", Message::WARNING);

	EXPECT_EQ("0", toString(addr));
	NodeAddress& adr2 = addr;
	EXPECT_EQ("0", toString(adr2));
	EXPECT_EQ("ERROR:   [00001] - TYPE / INVALID_NUMBER_OF_ARGUMENTS @ (0) - MSG: I hate it!", toString(msgA));
	EXPECT_EQ("WARNING: [00002] - TYPE / INVALID_ARGUMENT_TYPE @ (0) - MSG: Don't know - I'm scared!", toString(msgB));

	MessageList res = check(type, make_check<AllFine>());
	EXPECT_EQ(MessageList(),res);

	res = check(type, make_check<IDontLikeAnythingCheck>());
	EXPECT_EQ(MessageList(msgA), res);

	res = check(type, make_check<IAmScaredCheck>());
	EXPECT_EQ(MessageList(msgB), res);

	res = check(type, combine(toVector<CheckPtr>(std::make_shared<IAmScaredCheck>(), std::make_shared<IDontLikeAnythingCheck>())));
	EXPECT_EQ(MessageList(msgB, msgA), res);

}

TEST(IRCheck, Decorators) {
	NodeManager manager;
	IRBuilder builder(manager);

	// build diamond - again ...
	GenericTypePtr typeD = builder.genericType("D");
	GenericTypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	GenericTypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	GenericTypePtr typeA = builder.genericType("A", toVector<TypePtr>(typeB, typeC));

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
	GenericTypeAddress adr1(typeA);
	GenericTypeAddress adr2 = static_address_cast<GenericTypeAddress>(adr1->getTypeParameter(0));
	GenericTypeAddress adr3 = static_address_cast<GenericTypeAddress>(adr2->getTypeParameter(0));
	GenericTypeAddress adr4 = static_address_cast<GenericTypeAddress>(adr1->getTypeParameter(1));
	GenericTypeAddress adr5 = static_address_cast<GenericTypeAddress>(adr4->getTypeParameter(0));

	vector<Message> res = check(typeA, recCheck).getAll();
	std::sort(res.begin(), res.end());
	ASSERT_EQ((std::size_t)5, res.size());
	EXPECT_EQ(res[0], Message(adr1, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[1], Message(adr2, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[2], Message(adr3, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[3], Message(adr4, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[4], Message(adr5, (ErrorCode)1, "", Message::ERROR));

	res = check(typeA, onceCheck).getAll();
	std::sort(res.begin(), res.end());
	ASSERT_EQ((std::size_t)4, res.size());
	EXPECT_EQ(res[0], Message(adr1, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[1], Message(adr2, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[2], Message(adr3, (ErrorCode)1, "", Message::ERROR));
	EXPECT_EQ(res[3], Message(adr4, (ErrorCode)1, "", Message::ERROR));

}


struct InspectableAnnotation : public value_annotation::has_child_list {

	NodeList nodes;

	InspectableAnnotation(const NodeList& list) : nodes(list) {}

	template<typename ... T>
	InspectableAnnotation(const T& ... values) : nodes(toVector<NodePtr>(values...)) {}

	const NodeList& getChildNodes() const {
		return nodes;
	}

	bool operator==(const InspectableAnnotation& other) const {
		return nodes == other.nodes;
	}
};


TEST(IRCheck, Annotations) {

	NodeManager manager;
	IRBuilder builder(manager);

	// make recursive check
	CheckPtr recCheck = makeRecursive(make_check<IDontLikeAnythingCheck>());

	// create a node having a inspectable annotation
	NodePtr nodeA = builder.genericType("A");
	NodePtr nodeB = builder.genericType("B");

	// simply A => there should be 1 error
	EXPECT_EQ(1u, check(nodeA, recCheck).size());

	// adding the annotation => there should be 2 errors now
	nodeA->attachValue(InspectableAnnotation(nodeB));
	EXPECT_EQ(2u, check(nodeA, recCheck).size());

	// check that cycle is not followed indefinitely
	nodeB->attachValue(InspectableAnnotation(nodeA));
	EXPECT_EQ(2u, check(nodeA, recCheck).size());

}


} // end namespace checks
} // end namespace core
} // end namespace insieme


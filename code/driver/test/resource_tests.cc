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

#include "insieme/utils/timer.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_statistic.h"

#include "insieme/frontend/frontend.h"

#include "insieme/driver/integration/tests.h"

namespace insieme {

using namespace driver::integration;
using namespace core;

namespace {

	// ------------------------------------------------------------------------------------------------------------------
	//                                     Hash code evaluation
	// ------------------------------------------------------------------------------------------------------------------


	bool countHashCollisions(const NodePtr& code) {

		std::cout << "==== Check Hash Collitions =====================================" << std::endl;

		// create a set of all nodes
		insieme::utils::set::PointerSet<NodePtr> allNodes;
		insieme::core::visitDepthFirstOnce(code, insieme::core::makeLambdaVisitor([&allNodes](const NodePtr& cur) {
			allNodes.insert(cur);
		}, true));

		// evaluate hash codes
		std::cout << "Number of nodes: " << allNodes.size() << std::endl;
		std::map<std::size_t, NodePtr> hashIndex;
		int collisionCount = 0;
		for_each(allNodes, [&](const NodePtr& cur) {
			// try inserting node
			std::size_t hash = (*cur).hash();
			//std::size_t hash = boost::hash_value(cur->toString());
			//std::size_t hash = ::computeHash(cur);

			auto res = hashIndex.insert(std::make_pair(hash, cur));
			if (!res.second) {
				std::cout << "Hash Collision detected: \n"
							  << "   Hash code:     " << hash << "\n"
							  << "   First Element: " << *res.first->second << "\n"
							  << "   New Element:   " << *cur << "\n"
							  << "   Equal:         " << ((*cur==*res.first->second)?"true":"false") << "\n";
				collisionCount++;
			}
		});
		std::cout << "Number of Collisions: " << collisionCount << std::endl;

		return collisionCount;
	}
}; 


TEST(SpeedTest, GetStatus) {
	Logger::setLevel(ERROR);
	core::NodeManager manager;

	// load test case
	auto root = loadIntegrationTest(manager, "nas/bt/w");
	ASSERT_TRUE(root);

	std::cout << IRStatistic::evaluate(root) << "\n";
	std::cout << NodeStatistic::evaluate(manager) << "\n";

	std::cout << "Node Size:                      " << sizeof(core::Node) << "\n";
	std::cout << "Node Type:                      " << sizeof(core::NodeType) << "\n";
	std::cout << "Node Category:                  " << sizeof(core::NodeCategory) << "\n";
	std::cout << "Node Value Size:                " << sizeof(core::NodeValue) << "\n";
	std::cout << "Node List Size:                 " << sizeof(core::NodeList) << "\n";
	std::cout << "NodeManager*:                   " << sizeof(core::NodeManager*) << "\n";
	std::cout << "EqualityID:                     " << sizeof(uint64_t) << "\n";
	std::cout << "Hash Code:                      " << sizeof(std::size_t) << "\n";
	std::cout << "Node Annotation Container Size: " << sizeof(core::Node::annotation_container) << "\n";

}

// define the test case pattern
TEST(SpeedTest, IRCopy) {
	Logger::setLevel(ERROR);
	core::NodeManager manager;

	// load test case
	auto root = loadIntegrationTest(manager, "nas/bt/w");
	ASSERT_TRUE(root);

	core::NodePtr root2;
	core::NodeManager manager2;

//	std::cout << NodeStatistic::evaluate(manager2) << "\n";

	double copyTime = TIME(root2 = manager2.get(root));

//	std::cout << NodeStatistic::evaluate(manager2) << "\n";


	std::cout << "Time to copy: " << copyTime << "\n";
	EXPECT_EQ(*root, *root2);
}

// define the test case pattern
TEST(SpeedTest, VisitAllPtr) {
	Logger::setLevel(ERROR);
	core::NodeManager manager;
	
	// load test case
	auto root = loadIntegrationTest(manager, "nas/bt/w");
	ASSERT_TRUE(root);

	// visit all pointer
	int counterPtr = 0;
	double timePtr = TIME(visitDepthFirst(root, [&](const NodePtr& cur) { counterPtr++; }, true, true));

	// visit all addresses
	int counterAdr = 0;
	double timeAdr = TIME(visitDepthFirst(NodeAddress(root), [&](const NodeAddress& cur) { counterAdr++; }, true, true));

	std::cout << "Count-Pointer: " << counterPtr << " in " << timePtr << " - avg: " << (timePtr / counterPtr)*1000000000 << "ns\n";
	std::cout << "Count-Address: " << counterAdr << " in " << timeAdr << " - avg: " << (timeAdr / counterAdr)*1000000000 << "ns\n";

	EXPECT_EQ(counterPtr, counterAdr);
}

// define the test case pattern
TEST(SpeedTest, VisitOncePtr) {
	Logger::setLevel(ERROR);
	core::NodeManager manager;

	// load test case
	auto root = loadIntegrationTest(manager, "nas/bt/w");
	ASSERT_TRUE(root);

	// visit all pointer
	int counterPtr = 0;
	double timePtr = TIME(visitDepthFirstOnce(root, [&](const NodePtr& cur) { counterPtr++; }, true, true));

	// visit all addresses
	int counterAdr = 0;
	double timeAdr = TIME(visitDepthFirstOnce(NodeAddress(root), [&](const NodeAddress& cur) { counterAdr++; }, true, true));

	std::cout << "Count-Pointer: " << counterPtr << " in " << timePtr << " - avg: " << (timePtr / counterPtr)*1000000000 << "ns\n";
	std::cout << "Count-Address: " << counterAdr << " in " << timeAdr << " - avg: " << (timeAdr / counterAdr)*1000000000 << "ns\n";

	EXPECT_EQ(counterPtr, counterAdr);
}

// define the test case pattern
TEST(IRQualityTest, HashCollisions) {
	Logger::setLevel(ERROR);
	core::NodeManager manager;

	// load test case
	auto root = loadIntegrationTest(manager, "nas/bt/w");
	ASSERT_TRUE(root);

	// check for hash collisions
	int count = countHashCollisions(root);
	EXPECT_LT(count, 5);
}


}

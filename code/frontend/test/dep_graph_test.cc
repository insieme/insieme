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

#include "insieme/frontend/utils/dep_graph.h"
#include <vector>
#include <set>

using namespace insieme::frontend;

struct Node {
	std::string identifier;
	std::vector<Node*> children;
};

namespace std {

std::ostream& operator<<(std::ostream& out, Node* node) {
	return out << node->identifier;
}

} // end std namespace

namespace insieme {
namespace frontend {
namespace utils {

template <>
void DependencyGraph<Node*>::Handle(Node* node, const DependencyGraph<Node*>::VertexTy& v) {

	std::for_each(node->children.begin(), node->children.end(),
			[ this, v ](Node* currChild) { this->addNode(currChild, &v); }
	);
}

} // end insieme namespace
} // end frontend namespace
} // end utils namespace

TEST(DepGraphTest, SimpleDAG) {

	Node a = { "A", std::vector<Node*>() };
	Node b = { "B", std::vector<Node*>() };
	Node c = { "C", std::vector<Node*>({&a, &b}) };

	utils::DependencyGraph<Node*> dep;

	dep.addNode(&a);
	dep.addNode(&b);
	dep.addNode(&c);
	dep.print(std::cerr);

	std::set<Node*>&& ret = dep.getStronglyConnectedComponents(&a);

	// this is a DAG so we expect no strongly connected components
	EXPECT_EQ(static_cast<unsigned>(0), ret.size());
}

TEST(DepGraphTest, SimpleCyclicGraph) {

	Node d;
	d.identifier = "D";
	Node a = { "A", std::vector<Node*>( { &d } ) };
	Node b = { "B", std::vector<Node*>() };
	Node c = { "C", std::vector<Node*>({&a, &b}) };
	d.children.push_back(&c);

	utils::DependencyGraph<Node*> dep;

	dep.addNode(&a);
	dep.addNode(&b);
	dep.addNode(&c);
	dep.addNode(&d);
	dep.print(std::cerr);

	std::set<Node*>&& ret = dep.getStronglyConnectedComponents(&a);

	// this is a DAG so we expect 3 connected components, the method should then return 2
	// because the root node is eliminated
	EXPECT_EQ(static_cast<unsigned>(2), ret.size());
	// C is in the strong component set
	{
		auto fit = ret.find(&c);
		EXPECT_TRUE(fit != ret.end());
	}

	// D is in the strong component set
	{
		auto fit = ret.find(&d);
		EXPECT_TRUE(fit != ret.end());
	}
}

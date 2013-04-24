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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_statistic.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace core {

namespace {

	/**
	 * Obtains the height of the subtree rooted by the given node.
	 *
	 * @param node the node representing the root node of the tree to be evaluated
	 */
	unsigned evalHeight(const NodePtr& node) {
		unsigned res = 1;
		for_each(node->getChildList(), [&res](const NodePtr& cur) {
			unsigned height = evalHeight(cur)+1;
			res = (res<height)?height:res;
		});
		return res;
	}

}

IRStatistic::IRStatistic() : numSharedNodes(0), numAddressableNodes(0), height(0) {
	memset(nodeTypeInfo, 0, sizeof(NodeTypeInfo)*NUM_CONCRETE_NODE_TYPES);
};

IRStatistic IRStatistic::evaluate(const NodePtr& node) {

	IRStatistic res;

	// count number of shared nodes ...
	visitDepthFirstOnce(node, makeLambdaVisitor([&res](const NodePtr& ptr) {
		res.numSharedNodes++;
		res.nodeTypeInfo[ptr->getNodeType()].numShared++;
	}, true));

	// ... and addressable nodes
	visitDepthFirst(node, makeLambdaVisitor([&res](const NodePtr& ptr) {
		res.numAddressableNodes++;
		res.nodeTypeInfo[ptr->getNodeType()].numAddressable++;
	}, true));

	// ... and height (lightweight)
	res.height = evalHeight(node);

	// build result
	return res;
}

NodeStatistic::NodeStatistic() : numNodes(0), totalMemory(0) {
	memset(nodeTypeInfo, 0, sizeof(NodeTypeInfo)*NUM_CONCRETE_NODE_TYPES);
};

NodeStatistic NodeStatistic::evaluate(const NodeManager& manager) {

	NodeStatistic res;

	// collect memory requirement
	unsigned perNodeMemory[NUM_CONCRETE_NODE_TYPES];

	#define CONCRETE(name) \
		perNodeMemory[NT_##name] = sizeof(name); \
		// std::cout << "Nodes of type " #name " require " << sizeof(name) << " bytes.\n";

	// the necessary information is obtained from the node-definition file
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE

	for_each(manager, [&](const NodePtr& ptr) {

		unsigned curMem = perNodeMemory[ptr->getNodeType()] + sizeof(NodePtr) * ptr->getChildList().size();

		res.numNodes++;
		res.totalMemory += curMem;
		res.nodeTypeInfo[ptr->getNodeType()].num++;
		res.nodeTypeInfo[ptr->getNodeType()].memory += curMem;
	});

	return res;
}


} // end namespace core
} // end namespace insieme



namespace std {

namespace {

struct NodeInfo {

	const char* name;
	unsigned num;
	unsigned used;
	float ratio;

	NodeInfo(const char* name, unsigned num, unsigned used)
		: name(name), num(num), used(used), ratio((num==0)?0.0:used/(float)num) { }

	bool operator<(const NodeInfo& other) const {
		return num < other.num;
	}
};

}


std::ostream& operator<<(std::ostream& out, const insieme::core::IRStatistic& statistics) {

	// extract node info records
	vector<NodeInfo> infos;

	int numShared;
	int numAddressable;

	#define CONCRETE(name) \
		numShared = statistics.getNodeTypeInfo(insieme::core::NT_ ## name).numShared; \
		numAddressable = statistics.getNodeTypeInfo(insieme::core::NT_ ## name).numAddressable; \
		if (numShared > 0) infos.push_back(NodeInfo(" " #name, numShared, numAddressable));

	// the necessary information is obtained from the node-definition file
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE

	// sort records
	sort(infos.begin(), infos.end());


	// print data
	out << "                           --- General Information ---" << std::endl;
	out << "                                Height of tree: " << statistics.getHeight() << std::endl;
	out << std::endl;
	out << "                           --- Node Sharing Infos ---" << std::endl;

	// print data
	out << format("%30s%10s%10s%12s", "NodeType", "Nodes", "Shared", "Ratio") << std::endl;
	out << "        ---------------------------------------------------------------" << std::endl;
	std::for_each(infos.rbegin(), infos.rend(), [&out](const NodeInfo& cur) {
		out << format("%30s%10d%10d%12.1f", cur.name , cur.num, cur.used, cur.ratio) << std::endl;
	});
	out << "        ---------------------------------------------------------------" << std::endl;
	out << format("%30s%10d%10d%12.1f", "Total", statistics.getNumSharedNodes(), statistics.getNumAddressableNodes(), statistics.getShareRatio()) << std::endl;
	return out;
}


namespace {
	struct SimpleNodeInfo {

		const char* name;
		unsigned num;
		unsigned memory;

		SimpleNodeInfo(const char* name, unsigned num, unsigned memory)
			: name(name), num(num), memory(memory) { }

		bool operator<(const SimpleNodeInfo& other) const {
			return num < other.num;
		}
	};
}

std::ostream& operator<<(std::ostream& out, const insieme::core::NodeStatistic& statistics) {

	// extract node info records
	vector<SimpleNodeInfo> infos;

	unsigned num;
	unsigned memory;

	#define CONCRETE(name) \
		num = statistics.getNodeTypeInfo(insieme::core::NT_ ## name).num; \
		memory = statistics.getNodeTypeInfo(insieme::core::NT_ ## name).memory; \
		if (num > 0) infos.push_back(SimpleNodeInfo(" " #name, num, memory));

	// the necessary information is obtained from the node-definition file
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE

	// sort records
	sort(infos.begin(), infos.end());


	// print data
	out << "                           --- Node Information ---" << std::endl;

	// print data
	out << format("%30s%10s%15s%15s", "NodeType", "Num", "Memory","Memory/Node") << std::endl;
	out << "        --------------------------------------------------------------------------" << std::endl;
	std::for_each(infos.rbegin(), infos.rend(), [&out](const SimpleNodeInfo& cur) {
		out << format("%30s%10d%15d%14.1f", cur.name , cur.num, cur.memory, cur.memory /(double)cur.num) << std::endl;
	});
	out << "        --------------------------------------------------------------------------" << std::endl;
	out << format("%30s%10d%15d%14.1f", "Total", statistics.getNumNodes(), statistics.getTotalMemory(),
			statistics.getTotalMemory() / (double) statistics.getNumNodes()) << std::endl;
	return out;
}

}

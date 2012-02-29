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

#pragma once

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {


class IRStatistic {

public:

	/**
	 * A type definition for the information stored per node type.
	 */
	typedef struct {
		unsigned numShared;
		unsigned numAddressable;
	} NodeTypeInfo;

private:

	/**
	 * The number of nodes within an IR.
	 */
	unsigned numSharedNodes;

	/**
	 * The number of addressable nodes within an IR.
	 */
	unsigned numAddressableNodes;

	/**
	 * The height of the IR, hence, the maximum length of a path
	 * from the root node to one of the leafs.
	 */
	unsigned height;

	/**
	 * The statistical information stored per node type.
	 */
	NodeTypeInfo nodeTypeInfo[NUM_CONCRETE_NODE_TYPES];

	/**
	 * Creates a new instance of this class, initializing all values to 0.
	 */
	IRStatistic();

public:

	/**
	 * Creates an IR statistic summary for the given IR tree.
	 *
	 * @param node the root of the tree to be evaluated
	 * @return the collected statistic information
	 */
	static IRStatistic evaluate(const NodePtr& node);

	/**
	 * Obtains the number of shared nodes.
	 *
	 * @return the total number of shared nodes within the IR
	 */
	unsigned getNumSharedNodes() const {
		return numSharedNodes;
	}

	/**
	 * Obtains the number of addressable nodes within the IR.
	 *
	 * @return the total number of addressable nodes within the IR
	 */
	unsigned getNumAddressableNodes() const {
		return numAddressableNodes;
	}

	/**
	 * Obtains the height of the IR.
	 *
	 * @return the height of the IR.
	 */
	unsigned getHeight() const {
		return height;
	}

	/**
	 * Returns to average number nodes are shared within an IR, hence
	 * the ratio between the number of addressable nodes and the number
	 * of shared nodes.
	 *
	 * @return the average node sharing ratio
	 */
	float getShareRatio() const {
		return numAddressableNodes/(float)numSharedNodes;
	}

	/**
	 * Returns an array filled with the statistical data describing the
	 * distribution of the various node types within the covered IR.
	 *
	 * @return the statistical data collected regarding the node types.
	 */
	const NodeTypeInfo& getNodeTypeInfo(NodeType nodeType) const {
		return nodeTypeInfo[nodeType];
	}

};


class NodeStatistic {

public:

	/**
	 * A type definition for the information stored per node type.
	 */
	typedef struct {
		unsigned num;
		unsigned memory;
	} NodeTypeInfo;

private:

	/**
	 * The number of nodes encountered.
	 */
	unsigned numNodes;

	/**
	 * The total amount of memory consumed by those nodes (not including
	 * any annotations).
	 */
	unsigned totalMemory;

	/**
	 * The statistical information stored per node type.
	 */
	NodeTypeInfo nodeTypeInfo[NUM_CONCRETE_NODE_TYPES];

	/**
	 * Creates a new instance of this class, initializing all values to 0.
	 */
	NodeStatistic();

public:

	/**
	 * Creates an Node statistic summary for all the nodes managed by the given manager.
	 *
	 * @param manager the manager for which's content a statistic should be created.
	 * @return the collected statistic information
	 */
	static NodeStatistic evaluate(const NodeManager& manager);

	/**
	 * Obtains the total number of nodes encountered when producing this statistic.
	 *
	 * @return total number of nodes
	 */
	unsigned getNumNodes() const {
		return numNodes;
	}

	/**
	 * Obtains the total amount of memory consumed by the encountered nodes. The
	 * computation is not including any memory spend on annotations.
	 *
	 * @return the total amount of memory consumed by the encountered nodes.
	 */
	unsigned getTotalMemory() const {
		return totalMemory;
	}

	/**
	 * Returns an array filled with the statistical data describing the
	 * distribution of the various node types within the covered IR.
	 *
	 * @return the statistical data collected regarding the node types.
	 */
	const NodeTypeInfo& getNodeTypeInfo(NodeType nodeType) const {
		return nodeTypeInfo[nodeType];
	}

};



} // end namespace core
} // end namespace insieme


namespace std {

	/**
	 * Allows IR statistics to be directly printed into output streams.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::IRStatistic& statistics);

	/**
	 * Allows a node statistics to be directly printed into output streams.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::NodeStatistic& statistics);

}

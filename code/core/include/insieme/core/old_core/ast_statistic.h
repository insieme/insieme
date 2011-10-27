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

#include "insieme/core/ast_node.h"

namespace insieme {
namespace core {


class ASTStatistic {

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
	 * The number of nodes within an AST.
	 */
	unsigned numSharedNodes;

	/**
	 * The number of addressable nodes within an AST.
	 */
	unsigned numAddressableNodes;

	/**
	 * The height of the AST, hence, the maximum length of a path
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
	ASTStatistic();

public:

	/**
	 * Creates an AST statistic summary for the given AST tree.
	 *
	 * @param node the root of the tree to be evaluated
	 * @return the collected statistic information
	 */
	static ASTStatistic evaluate(const NodePtr& node);

	/**
	 * Obtains the number of shared nodes.
	 *
	 * @return the total number of shared nodes within the AST
	 */
	unsigned getNumSharedNodes() const {
		return numSharedNodes;
	}

	/**
	 * Obtains the number of addressable nodes within the AST.
	 *
	 * @return the total number of addressable nodes within the AST
	 */
	unsigned getNumAddressableNodes() const {
		return numAddressableNodes;
	}

	/**
	 * Obtains the height of the AST.
	 *
	 * @return the height of the AST.
	 */
	unsigned getHeight() const {
		return height;
	}

	/**
	 * Returns to average number nodes are shared within an AST, hence
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
	 * distribution of the various node types within the covered AST.
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
	 * Allows AST statistics to be directly printed into output streams.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::ASTStatistic& statistics);

}

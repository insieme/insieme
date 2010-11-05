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

#include "core/ast_node.h"

namespace insieme {
namespace core {

class ASTStatistic {

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
	 * Creates a new instance of this class based on the given arguments.
	 * The constructor is private and may only be used by a static factory method.
	 *
	 * @param numSharedNodes the number of nodes used within an AST
	 * @param numAddressableNodes the total number of nodes when unfolding all shared nodes
	 * @param height the height of the AST
	 */
	ASTStatistic(unsigned numSharedNodes, unsigned numAddressableNodes, unsigned height);

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

};


} // end namespace core
} // end namespace insieme

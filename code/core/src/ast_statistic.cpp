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

#include "ast_statistic.h"
#include "ast_visitor.h"

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

ASTStatistic::ASTStatistic(unsigned numSharedNodes, unsigned numAddressableNodes, unsigned height)
	: numSharedNodes(numSharedNodes), numAddressableNodes(numAddressableNodes), height(height) {};

ASTStatistic ASTStatistic::evaluate(const NodePtr& node) {

	// count number of shared nodes ...
	unsigned numNodes = 0;
	visitAllOnce(node, makeLambdaVisitor([&numNodes](const NodePtr&) {
		numNodes++;
	}));

	// ... and addressable nodes
	unsigned numAddressableNodes = 0;
	visitAll(node, makeLambdaVisitor([&numAddressableNodes](const NodePtr&) {
		numAddressableNodes++;
	}));

	// ... and height (lightweight)
	unsigned height = evalHeight(node);

	// build result
	return ASTStatistic(numNodes, numAddressableNodes, height);
}



} // end namespace core
} // end namespace insieme

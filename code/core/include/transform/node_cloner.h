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

#include "ast_builder.h"
#include "ast_visitor.h"

namespace insieme {
namespace core {

class ASTBuilder;

namespace transform {

/**
 * Clones the nodes of the IR.
 * Alone this Visitor is pointless, but combined with other visitors can be used to replace/remove/insert nodes to an existing tree.
 */
class NodeCloner: public core::ASTVisitor<NodePtr>, public NodeMapping {
	NodeManager& nodeManager;

public:
	NodeCloner(NodeManager& manager) : nodeManager(nodeManager) { };

	/**
	 * This method bridges the gap between a visitor and a NodeMapper by
	 * forwarding requests from the mapper to the visitor method.
	 *
	 * @param ptr the pointer to be mapped to another pointer
	 * @return the resulting pointer
	 */
	virtual NodePtr mapElement(const NodePtr& ptr) {
		return visit(ptr);
	}

protected:

	/**
	 * Performs the recursive clone operation on all nodes passed on to this visitor.
	 */
	virtual NodePtr visitNode(const NodePtr& ptr) {
		return ptr->substitute(nodeManager, *this);
	}

	/**
	 * Obtains a reference to the associated node manager.
	 *
	 * @return a reference to the associated node manager.
	 */
	NodeManager& getNodeManager() {
		return nodeManager;
	}

};

} // End transform namespace
} // End core namespace
} // End insieme namespace

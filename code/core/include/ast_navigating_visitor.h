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

#include <list>

#include "ast_address.h"
#include "ast_visitor.h"

namespace insieme {
namespace core {


class NavigatingASTVisitor : public ASTVisitor<void> {

	/**
	 * This list maintains the path to the current node and is used
	 * to obtain the current node's address.
	 */
	std::list<NodeAddress::PathEntry>* path;

public:

	/**
	 * A default constructor for this class.
	 */
	NavigatingASTVisitor() : path(NULL) {}

	/**
	 * Visits the given node. The path will be tracked.
	 */
	virtual void visit(const NodePtr& node);

protected:

	/**
	 * Obtains the address of the current node relative to the first node the visit has been
	 * invoked on.
	 *
	 * @return the address of the currently visited node.
	 *
	 * NOTE: this method may only be invoked within a visiting routine.
	 */
	NodeAddress getCurrentAddress();

};


} // end namespace core
} // end namespace insieme

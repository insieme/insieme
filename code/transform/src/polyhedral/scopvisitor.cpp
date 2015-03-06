/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <cstdlib>
#include <boost/optional.hpp>
#include <iostream>
#include <memory>
#include <vector>

#include "insieme/transform/polyhedral/scopvisitor.h"
#include "insieme/utils/logging.h"

using namespace insieme::core;
using namespace insieme::transform::polyhedral::novel;

/// The constructor initializes class variables and triggers the visit of all nodes in the program.
SCoPVisitor::SCoPVisitor(const ProgramAddress &node): lvl(0), scoplist(node) {
	visit(node);
}

/// visitNode is the entry point for visiting all statements within a program to search for a SCoP. It will visit
/// all child nodes and call their respective visitor methods.
void SCoPVisitor::visitNode(const NodeAddress &node) {
	// process this very node
	std::cout << lvl << "\tFound node " << node << std::endl;
	if (node->getNodeType() == NT_CallExpr)
		ExpressionPtr func=static_address_cast<const CallExpr>(node)->getFunctionExpr();
	visitChildren(node);
}

/// Visit all the child nodes of the given node. The given node itself is not taken into account.
void SCoPVisitor::visitChildren(const NodeAddress &node) {
	for (auto child: node->getChildList()) visit(child);
}

/// visitForStmt describes what should happen when a for stmt is encountered within the program.
/// Is it the outermost for, or is it already nested?
void SCoPVisitor::visitForStmt(const ForStmtAddress &forStmt) {
	lvl++;
	std::cout << "for() stmt @" << forStmt << std::endl;
	visitChildren(forStmt);
	lvl--;
}

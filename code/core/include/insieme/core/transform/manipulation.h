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
#include "insieme/core/ast_address.h"

namespace insieme {
namespace core {
namespace transform {

// TODO: merge this file and the node replacer.h

/**
 * A utility function to insert a statement within a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement within which the element should be inserted
 * @param statement the statement to be inserted
 * @param index the index at which the element should be inserted (0 ... before current node 0)
 * @param preservePtrAnnotationsWhenModified if enabled, new nodes created due to the manipulation will
 * 				get a copy of the annotations of the original node by default, this feature is disabled
 * 				and it should be used with care. In case on of the resulting nodes is already present
 * 				within the manager, the present node and its version of the annotations will be preserved
 * 				and returned.
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr insert(NodeManager& manager, const CompoundStmtAddress& target, const StatementPtr& statement, unsigned index, bool preservePtrAnnotationsWhenModified = false);

/**
 * A utility function to remove a statement from a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement from which a statement should be removed
 * @param index the index of the statement to be removed (counting starts with 0)
 * @param preservePtrAnnotationsWhenModified if enabled, new nodes created due to the manipulation will
 * 				get a copy of the annotations of the original node by default, this feature is disabled
 * 				and it should be used with care. In case on of the resulting nodes is already present
 * 				within the manager, the present node and its version of the annotations will be preserved
 * 				and returned.
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr remove(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, bool preservePtrAnnotationsWhenModified = false);

/**
 * A utility function to move a statement within a compound statement block.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be manipulated
 * @param index the index of the statement to be moved (counting starts with 0)
 * @param displacement the amount of displacement (-1 .. one up, 2 .. two down)
 * @param preservePtrAnnotationsWhenModified if enabled, new nodes created due to the manipulation will
 * 				get a copy of the annotations of the original node by default, this feature is disabled
 * 				and it should be used with care. In case on of the resulting nodes is already present
 * 				within the manager, the present node and its version of the annotations will be preserved
 * 				and returned.
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr move(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, int displacement, bool preservePtrAnnotationsWhenModified = false);

/**
 * A utility function replacing a statement within a compound statement block with another statement.
 *
 * @param manager the manager used to create new nodes
 * @param target the compound statement which should be altered
 * @param index the index of the statement to be replaced (counting starts with 0)
 * @param replacement the statement to be inserted as a replacement
 * @param preservePtrAnnotationsWhenModified if enabled, new nodes created due to the manipulation will
 * 				get a copy of the annotations of the original node by default, this feature is disabled
 * 				and it should be used with care. In case on of the resulting nodes is already present
 * 				within the manager, the present node and its version of the annotations will be preserved
 * 				and returned.
 * @return the root node of the modified AST tree (according to the root of the address)
 */
NodePtr replace(NodeManager& manager, const CompoundStmtAddress& target, unsigned index, const StatementPtr& replacement, bool preservePtrAnnotationsWhenModified = false);

ExpressionPtr tryInline(NodeManager& manager, const CallExprPtr& call);

} // end namespace transform
} // end namespace core
} // end namespace insieme

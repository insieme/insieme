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

#include <iostream>

#include "insieme/core/ast_node.h"

namespace insieme {
namespace core {
namespace printer {

/**
 * A struct representing a pretty print of a AST subtree. Instances may be streamed
 * into an output stream to obtain a readable version of an AST.
 */
struct PrettyPrint {

	/**
	 * The root node of the sub-try to be printed
	 */
	const NodePtr& root;

	/**
	 * A flag to indicate whether ref.deref operations should be visible or not.
	 */
	const bool hideDeref;

	/**
	 * A flag to indicate whether cast operations should be visible or not.
	 */
	const bool hideCasts;

	/**
	 * A flag to indicate whether brackets fixing operater precidence should be ommited or
	 * not.
	 */
	const bool hideBrackets;

	/**
	 * Creates a new pretty print instance.
	 *
	 * @param root the root node of the AST to be printed
	 * @param hideDeref if set to true, no deref operations will be printed
	 * @param hideCasts if set to true, no casts will be printed
	 * @param hideBrackets if set to true, no brackets to fix the order of operations within a expression will be printed
	 */
	PrettyPrint(const NodePtr& root, bool hideDeref = true, bool hideCasts = true, bool hideBrackets = true)
		: root(root), hideDeref(hideDeref), hideCasts(hideCasts), hideBrackets(hideBrackets) {}
};

} // end of namespace printer
} // end of namespace core
} // end of namespace insieme

namespace std {

	/**
	 * Allows pretty prints to be directly printed into output streams.
	 */
	std::ostream& operator<<(std::ostream& out, const insieme::core::printer::PrettyPrint& print);

}

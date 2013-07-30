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

#include <string>
#include "insieme/core/forward_decls.h"

/**
 * A header file for naming annotations to be considered by IR utilities.
 * Name annotations will be utilized by the pretty printer, the parser,
 * the backends and other utilities for attaching / resolving names of
 * objects. Names are also preserved by the binary dump.
 */

namespace insieme {
namespace core {
namespace annotations {

	using std::string;

	/**
	 * Checks whether a name is attached to the given node.
	 *
	 * @param node the node to be tested
	 * @return true if a name is attached, false otherwise
	 */
	bool hasNameAttached(const NodePtr& node);

	/**
	 * Obtains a reference to the name attached to the given node. If
	 * no name has been attached the result is undefined (an assertion
	 * in debug mode).
	 *
	 * @param node the node to obtain the attached name from
	 * @return the name attached to the given node
	 */
	const string& getAttachedName(const NodePtr& node);

	/**
	 * Updates the name attached to the given node.
	 *
	 * @param node the node to attach a name to
	 * @param name the name to be attached to the node
	 */
	void attachName(const NodePtr& node, const string& name);


	/**
	 * Updates the name attached to the given node
	 * and returns the given node.
	 *
	 * @param node the node to attach a name to
	 * @param name the name to be attached to the node
	 * @return the given node
	 */
	 template<typename T>
     const T& attachNameWithReturn(const T& node, const string& name) {
	    const core::NodePtr& cur = node;
        attachName(cur, name);
        return node;
	 }

} // end namespace annotations
} // end namespace core
} // end namespace insieme

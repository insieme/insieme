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

/**
 * A header for general definitions of language extensions.
 */

#include <string>
#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {
namespace lang {

	using std::string;

	/**
	 * Checks whether the given construct is a derived construct within
	 * any language extension. Derived constructs are annotated by a
	 * corresponding annotation.
	 *
	 * @param node the node to be tested
	 * @return true if the given node is marked as a derived construct, false otherwise
	 */
	bool isDerived(const NodePtr& node);

	/**
	 * Extracts the name of the derived construct. If the given node
	 * is not a derived node, the result is undefined (an assertion
	 * in debug mode).
	 *
	 * @param node the node being a derived definition which needs to be named
	 * @return the name attached to the given construct
	 */
	const string& getConstructName(const NodePtr& node);

	/**
	 * Marks the given construct as being a derived construct being named
	 * accordingly.
	 *
	 * @param node the node to be marked as being derived
	 * @param name the name of the the derived construct
	 * @return the handed in node
	 */
	NodePtr markAsDerived(const NodePtr& node, const string& name);

	/**
	 * A generic version of the function above.
	 */
	template<typename T>
	Pointer<T> markAsDerived(const Pointer<T>& node, const string& name) {
		return markAsDerived(NodePtr(node), name).as<Pointer<T>>();
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme

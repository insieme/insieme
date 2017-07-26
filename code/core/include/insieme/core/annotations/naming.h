/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
	bool hasAttachedName(const NodePtr& node);

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
	template <typename T>
	const T& attachNameWithReturn(const T& node, const string& name) {
		const core::NodePtr& cur = node;
		attachName(cur, name);
		return node;
	}

} // end namespace annotations
} // end namespace core
} // end namespace insieme

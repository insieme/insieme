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
	template <typename T>
	Pointer<T> markAsDerived(const Pointer<T>& node, const string& name) {
		return markAsDerived(NodePtr(node), name).as<Pointer<T>>();
	}

	/**
	 * Checks whether the given construct is marked as a built-in construct within
	 * any language extension. Built-in constructs are annotated by a
	 * corresponding annotation.
	 *
	 * @param node the node to be tested
	 * @return true if the given node is marked as a built-in construct, false otherwise
	 */
	bool isBuiltIn(const core::NodePtr& node);

	/**
	 * Marks the given construct as being a built-in construct.
	 *
	 * @param node the node to be marked as being a built-in
	 * @return the handed in node
	 */
	void markAsBuiltIn(const NodePtr& node);

} // end namespace lang
} // end namespace core
} // end namespace insieme

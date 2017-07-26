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

#include "insieme/core/lang/extension.h"

namespace insieme {
namespace core {
namespace lang {


	// --------------------- Extension ----------------------------


	/**
	 * An extension covering data paths. A data path describes the
	 * access path when navigating within an object allocated by a single
	 * memory allocation call (ref_alloc).
	 */
	class DatapathExtension : public core::lang::Extension {
		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		DatapathExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		/**
		 * The root path is modeling the empty path -- the identity when utilized for narrow / expand operations.
		 */
		LANG_EXT_LITERAL(DataPathRoot, "dp_root", "(type<'a>) -> datapath<'a,'a>")

		/**
		 * The member path extends a given path by accessing a member field  of type 'c of a struct / union 'a.
		 */
		LANG_EXT_LITERAL(DataPathMember, "dp_member", "(datapath<'a,'b>, identifier, type<'c>) -> datapath<'a,'c>")

		/**
		 * An element access operation extends a given path by an access to an element of an array.
		 */
		LANG_EXT_LITERAL(DataPathElement, "dp_element", "(datapath<'a,array<'b,'s>>, int<8>) -> datapath<'a,'b>")

		/**
		 * A component access is extend a data path by accessing an element of a tuple type.
		 */
		LANG_EXT_LITERAL(DataPathComponent, "dp_component", "(datapath<'a,'b>, uint<8>, type<'c>) -> datapath<'a,'c>")

		/**
		 * A parent access path is moving a reference to a base class.
		 */
		LANG_EXT_LITERAL(DataPathParent, "dp_parent", "(datapath<'a,'b>, type<'c>) -> datapath<'a,'c>")
	};

} // end namespace lang
} // end namespace core
} // end namespace insieme

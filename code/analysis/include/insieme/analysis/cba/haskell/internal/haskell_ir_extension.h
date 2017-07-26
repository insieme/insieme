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
namespace analysis {
namespace cba {
namespace haskell {
namespace internal {


	// --------------------- Extension ----------------------------

	/**
	 * An extension covering a list of literals simplifying the generation
	 * of IR code on the haskell side of analysis.
	 */
	class HaskellExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		HaskellExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		/**
		 * A token representing the stack.
		 */
		LANG_EXT_LITERAL(HaskellRefMemberAccess, "hs_ref_member_access", "(ref<'a,'c,'v,'k>, identifier) -> ref<'b,'c,'v, plain>")

	};

	/**
	 * Cleans the given IR code fragment by removing all references
	 * to IR extensions utilized for simplifying code generation on
	 * the haskell side of the analysis.
	 */
	core::NodePtr clean(const core::NodePtr&);

} // end namespace internal
} // end namespace haskell
} // end namespace cba
} // end namespace analysis
} // end namespace insieme

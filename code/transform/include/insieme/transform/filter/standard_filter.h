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
 *
 */
#pragma once

#include "insieme/transform/filter/filter.h"

namespace insieme {
namespace transform {
namespace filter {

	/**
	 * Obtains a target filter selecting all outermost for-loops within
	 * a program fragment.
	 */
	TargetFilter outermostLoops();

	/**
	 * Obtains a target filter selecting all innermost for-loops of a
	 * given nesting level within a program fragment.
	 *
	 * @param level the nesting-level of the selected loops
	 */
	TargetFilter innermostLoops(unsigned level = 1);

	/**
	 * A filter picking the loop with the given index. Indices are defined
	 * in a hierarchical way according to the code structure.
	 */
	TargetFilter pickLoop(const vector<unsigned>& index);

	/**
	 * A convenience wrapper for the function above.
	 */
	template <typename... T>
	TargetFilter pickLoop(unsigned first, T... rest) {
		return pickLoop(toVector<unsigned>(first, rest...));
	}

	/**
	 * A filter picking the location addressed by the given relative address.
	 * The filter will only select a single node in any case. Further, a node will
	 * only be selected in case the node the filter is applied to is the root of the given
	 * address.
	 *
	 * @param relativeAddress the address of the node to be selected
	 * @return a filter selecting the specified node
	 */
	TargetFilter pickRelative(const core::NodeAddress& relativeAddress);


} // end namespace filter
} // end namespace transform
} // end namespace insieme

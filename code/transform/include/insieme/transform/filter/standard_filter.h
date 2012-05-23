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
	 * A filter selecting all outermost SCoPs of a program fragment.
	 */
	TargetFilter outermostSCoPs();


	/**
	 * A filter picking the loop with the given index. Indices are defined
	 * in a hierarchical way according to the code structure.
	 */
	TargetFilter pickLoop(const vector<unsigned>& index);

	/**
	 * A convenience wrapper for the function above.
	 */
	template<typename ... T>
	TargetFilter pickLoop(unsigned first, T ... rest) {
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

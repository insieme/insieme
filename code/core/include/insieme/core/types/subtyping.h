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

#include "insieme/core/ir_types.h"

namespace insieme {
namespace core {
namespace types {

	// -------------------------------------------------------------------------------------------------------------------------
	//                                                    SubTyping
	// -------------------------------------------------------------------------------------------------------------------------

	/**
	 * Tests whether the given sub-type is in deed a sub-type o the given super type.
	 *
	 * @param subType the sub-type to be tested
	 * @param superType the super type to be compared with
	 * @return true if subType is in deed a sub-type of the super type
	 */
	bool isSubTypeOf(const TypePtr& subType, const TypePtr& superType);

	/**
	 * Tries to obtain the smallest common super-type of the given types (Join Type in the sub-type relation).
	 *
	 * @param typeA the first type to be considered
	 * @param typeB the second type to be considered
	 * @return the smallest common super type or null, if no such type exists
	 */
	TypePtr getSmallestCommonSuperType(const TypePtr& typeA, const TypePtr& typeB);

	/**
	 * Computes the smallest common super type of the types within the given container.
	 *
	 * @param types the typed to be considered
	 * @return the smallest common super type or null, if no such type exists
	 */
	template <typename Container>
	TypePtr getSmallestCommonSuperType(const Container& types) {
		if(types.empty()) { return 0; }
		auto it = types.begin();
		TypePtr res = *it;
		for(++it; res && it != types.end(); ++it) {
			res = getSmallestCommonSuperType(res, *it);
		}
		return res;
	}

	/**
	 * Tries to obtain the biggest common sub-type of the given types (Meet Type in the sub-type relation).
	 *
	 * @param typeA the first type to be considered
	 * @param typeB the second type to be considered
	 * @return the biggest common sub type or null, if no such type exists
	 */
	TypePtr getBiggestCommonSubType(const TypePtr& typeA, const TypePtr& typeB);

	/**
	 * Tries to obtain the biggest common sub-type of the given types (Meet Type in the sub-type relation).
	 *
	 * @param types the typed to be considered
	 * @return the biggest common sub type or null, if no such type exists
	 */
	template <typename Container>
	TypePtr getBiggestCommonSubType(const Container& types) {
		if(types.empty()) { return 0; }
		auto it = types.begin();
		TypePtr res = *it;
		for(++it; res && it != types.end(); ++it) {
			res = getBiggestCommonSubType(res, *it);
		}
		return res;
	}


} // end namespace types
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/types/substitution.h"

namespace insieme {
namespace core {
namespace types {

	// -------------------------------------------------------------------------------------------------------------------------
	//                                                    matching
	// -------------------------------------------------------------------------------------------------------------------------

	/**
	 * Tries to match a type to another given type by mapping the type variables contained in it
	 *
	 * @param manager the node manager to be used for creating temporal results and the mappings within the resulting substitution
	 * @param type     the type to be matched against the given pattern
	 * @param pattern  the type describing the pattern
	 * @return an optional substitution mapping type variables in the pattern to types such that the given type and the pattern are equivalent
	 */
	SubstitutionOpt match(NodeManager& manager, const TypePtr& type, const TypePtr& pattern);

	/**
	 * Tests whether a given type can be matched to a pattern.
	 *
	 * @param type the type to be matched against the pattern
	 * @param pattern the pattern to be tested
	 * @return true if matchable, false otherwise
	 */
	bool isMatchable(const TypePtr& type, const TypePtr& pattern);

	bool typeMatchesWithOptionalMaterialization(NodeManager& nm, const TypePtr& targetT, const TypePtr& valueT);

} // end namespace types
} // end namespace core
} // end namespace insieme

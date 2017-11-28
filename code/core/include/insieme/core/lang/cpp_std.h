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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {
namespace lang {

	//
	// --------------------- Utilities ----------------------------
	//

	/**
	 * Determines whether the given type or expression is a std::string.
	 */
	bool isStdString(const NodePtr& node);


	// --------------------- std::pair ----------------------------

	/**
	 * Determines whether the given type or expression is a std::pair.
	 */
	bool isStdPair(const NodePtr& node);

	/**
	 * Determines the first element type of a std::pair or null, if
	 * the given node is not a std::pair.
	 */
	TypePtr getStdPairFirstType(const NodePtr& node);

	/**
	 * Determines the second element type of a std::pair or null, if
	 * the given node is not a std::pair.
	 */
	TypePtr getStdPairSecondType(const NodePtr& node);


	// --------------------- std::array ----------------------------

	/**
	 * Determines whether the given type or expression is a std::array.
	 */
	bool isStdArray(const NodePtr& node);

	/**
	 * Determines the element type of a std::array or a null pointer if
	 * the given node is not of a std::array.
	 */
	TypePtr getStdArrayElementType(const NodePtr& node);


	// --------------------- std::vector ---------------------------

	/**
	 * Determines whether the given type or expression is a std::vector.
	 */
	bool isStdVector(const NodePtr& node);

	/**
	 * Determines the element type of a std::vector or a null pointer if
	 * the given node is not of a std::vector.
	 */
	TypePtr getStdVectorElementType(const NodePtr& node);


} // end namespace lang
} // end namespace core
} // end namespace insieme

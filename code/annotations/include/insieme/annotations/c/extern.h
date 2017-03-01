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

#include <string>
#include "insieme/core/forward_decls.h"

/**
 * A header file for literals to be marked as being declared extern.
 * Extern literals are literals within an external storage - and therefore
 * only need to declared but not defined within resulting code.
 */

namespace insieme {
namespace annotations {
namespace c {

	/**
	 * Checks whether the given literal is marked to be extern.
	 *
	 * @param literal the literal to be tested
	 * @return true if extern, false otherwise
	 */
	bool isExtern(const insieme::core::LiteralPtr& literal);

	/**
	 * Updates the extern flag of the given literal to fit the given value.
	 *
	 * @param literal the literal to be marked extern
	 * @param value a flag determining whether to mark it extern or not
	 */
	void markExtern(const insieme::core::LiteralPtr& literal, bool value = true);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

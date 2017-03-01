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

namespace insieme {
namespace annotations {
namespace c {

	using std::string;

	/**
	 * Checks whether a tag (e.g. "enum", "struct" or "union") is attached to the given node.
	 */
	bool hasAttachedCTag(const core::NodePtr& node);

	/**
	 * Obtains a reference to the C tag (e.g. "enum", "struct" or "union") attached to the given node. If
	 * no tag has been attached the result is undefined (an assertion in debug mode).
	 */
	const string& getAttachedCTag(const core::NodePtr& node);

	/**
	 * Attaches the given C tag (e.g. "enum", "struct" or "union") to the given node.
	 */
	const core::NodePtr& attachCTag(const core::NodePtr& node, const string& tag);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

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

#include "insieme/utils/assert.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/printer/error_printer.h"

namespace insieme {
namespace core {
namespace checks {


	/**
	 * Obtains a combined check case containing all the checks defined within this header file.
	 */
	CheckPtr getFullCheck();

	/**
	 * Allies all known semantic checks on the given node and returns the obtained message list.
	 */
	MessageList check(const NodePtr& node);

} // end namespace checks
} // end namespace core
} // end namespace insieme


/**
 * An assertion for verifying correct IR.
 */
#define assert_correct_ir(_code) \
		assert_true(_code && insieme::core::checks::check(_code).empty()) \
			<< insieme::core::printer::dumpErrors(insieme::core::checks::check(_code)) \
			<< "\n----------------------------------------------------------\n" \
			<< insieme::core::checks::check(_code) \
			<< "\n----------------------------------------------------------\n"

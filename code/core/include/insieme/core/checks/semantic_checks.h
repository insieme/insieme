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

#include "insieme/core/checks/ir_checks.h"

namespace insieme {
namespace core {
namespace checks {

// defines macros for generating CHECK declarations
#include "insieme/core/checks/check_macros.inc"

	/**
	 * This check verifies that undefined(...) is only called within ref.new or ref.var.
	 */
	SIMPLE_CHECK(Undefined, CallExpr, false);

	/**
	 * This check verifies that no defaulted and deleted marker bodies remain in the final IR.
	 */
	SIMPLE_CHECK(DefaultedDeletedPreTUMarker, Node, false);

	/**
	 * This check verifies that there are no free break or return statements inside for loops,
	 * and that the step expression is statically evaluatable.
	 */
	SIMPLE_CHECK(ForLoopSemantics, ForStmt, false);

	/**
	 * This check verifies that functions with non-unit return type return something on every code path.
	 */
	SIMPLE_CHECK(MissingReturnStmt, LambdaExpr, false);

	/**
	 * This check verifies that init expressions are executed on synthesizable locations.
	 */
	SIMPLE_CHECK(ValidInitExprMemLocation, InitExpr, false);

	/**
	 * This check verifies that materializing declarations are backed up through a constructor or an implicit conversion.
	 */
	SIMPLE_CHECK(ValidMaterializingDeclaration, Declaration, false);

	#undef SIMPLE_CHECK

} // end namespace check
} // end namespace core
} // end namespace insieme

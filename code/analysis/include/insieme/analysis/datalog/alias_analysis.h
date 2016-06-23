/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_address.h"

namespace insieme {
namespace analysis {
namespace datalog {

	/**
	 * Determines whether the two given expressions may reference the same object.
	 * Both expression addresses need to be rooted by the same node.
	 *
	 * @param a the first expression
	 * @param b the second expression
	 * @return true if they may reference the same object, false otherwise
	 */
	bool mayAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b);

	/**
	 * Determines whether the two given expressions are for sure referencing the same object.
	 * Both expression addresses need to be rooted by the same node.
	 *
	 * @param a the first expression
	 * @param b the second expression
	 * @return true if they are referencing the same object, otherwise false
	 */
	bool areAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b);

	/**
	 * Determines whether the two given expressions do certainly NOT reference the same object.
	 * Both expression addresses need to be rooted by the same node.
	 * Hint: This is the converse of 'mayAlias'
	 *
	 * @param a the first expression
	 * @param b the second expression
	 * @return true if they certainly do not reference the same object, false otherwise
	 */
	bool notAlias(const core::ExpressionAddress& a, const core::ExpressionAddress& b) {
		return !mayAlias(a, b);
	}


} // end namespace datalog
} // end namespace analysis
} // end namespace insieme

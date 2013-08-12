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

#include <string>
#include "insieme/core/forward_decls.h"

/**
 * A header file for (global) literals to be marked as external C literals.
 * Literals marked using this flag will be created within an extern "C" { .. }
 * scope and excluded for the name mangling.
 */

namespace insieme {
namespace annotations {
namespace c {

	/**
	 * Checks whether the given literal is marked to be an external C object
	 * where no name mangling should be applied to.
	 *
	 * @param literal the literal to be tested
	 * @return true if marked to be extern C, false otherwise
	 */
	bool isExternC(const insieme::core::LiteralPtr& literal);

	/**
	 * Updates the extern-C flag of the given literal to fit the given value.
	 *
	 * @param literal the literal to be marked extern
	 * @param value a flag determining whether to mark it extern or not
	 */
	void markAsExternC(const insieme::core::LiteralPtr& literal, bool value = true);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

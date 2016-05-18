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

#include <string>
#include "insieme/core/forward_decls.h"

/**
 * A header file for generic types to be marked as being forward declared.
 * Generic types marked as a declaration were generated in the frontend and
 * need to be synthesized in the backend even if they have no visible
 * definition (before linking).
 *
 */

namespace insieme {
namespace annotations {
namespace c {

	/**
	 * Checks whether the given generic type is marked as a forward declaration.
	 *
	 * @param genTy the generic type to be tested
	 * @return true if forward declared, false otherwise
	 */
	bool isDeclaration(const insieme::core::GenericTypePtr& genTy);

	/**
	 * Updates the forward declaration flag of the given generic type to fit the given value.
	 *
	 * @param genTy the generic type to be marked as a forward declaration
	 * @param value a flag determining whether to mark it forward declared or not
	 */
	void markDeclaration(const insieme::core::GenericTypePtr& genTy, bool value = true);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

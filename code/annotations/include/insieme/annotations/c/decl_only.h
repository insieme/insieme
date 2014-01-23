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
 * Marks the given type as declaration only, which means that the type is only declared but never
 * defined but regardless of that used in the given code. for example in a template code.
 */

namespace insieme {
namespace annotations {
namespace c {

	/**
	 * Checks whether the given type is marked to be declaration only.
	 *
	 * @param type the type to be tested
	 * @return true if declaration only, false otherwise
	 */
	bool isDeclOnly(const insieme::core::GenericTypePtr& type);

	/**
	 * Updates the declaration only flag of the given type to fit the given value.
	 *
	 * @param type the GenericType to be marked declaration only 
	 * @param value a flag determining whether to mark it declaration only or not
	 */
	void markDeclOnly(const insieme::core::GenericTypePtr& type, bool value = true);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

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

#include "insieme/core/forward_decls.h"

namespace insieme {
namespace core {
namespace types {

// -------------------------------------------------------------------------------------------------------------------------
//                                                    Variable Sized Structs
// -------------------------------------------------------------------------------------------------------------------------

/**
 * Determines whether the given type is a variable sized data structure. A variable sized
 * data structure is either an array or a struct / tuple / union containing a variable sized
 * data structure as an element type (for structs / tuples it needs to be the laste element).
 *
 * @param cur the type to be checked
 * @return true if it is a variable sized type, false otherwise
 */
bool isVariableSized(const TypePtr& type);

/**
 * Every variable sized type needs to contain an array of elements covering an variable amount
 * of elements - this function obtains the type of elements stored within this array.
 *
 * @param type the type to be analysis - must be a variable sized type
 * @return the type of element forming the variable sized array within the given type
 */
TypePtr getRepeatedType(const TypePtr& type);

} // end namespace types
} // end namespace core
} // end namespace insieme

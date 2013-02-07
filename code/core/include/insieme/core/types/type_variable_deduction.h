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

#include <boost/unordered_set.hpp>

#include "insieme/core/forward_decls.h"
#include "insieme/core/types/substitution.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace core {
namespace types {

/**
 * Tries to compute a valid type variable substitution for a call to a function accepting the given parameter type using
 * the given argument type.
 *
 * @param manager the node manager to be used for temporary IR nodes
 * @param parameter the type of the parameter
 * @param argument the type of the argument
 * @return a type-variable substitution mapping for the type variables within the parameter or an uninitialized option
 * 		   if no such substitution exists.
 */
SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypePtr& parameter, const TypePtr& argument);

/**
 * Tries to compute a valid type variable substitution for a call to a function accepting the given parameter types using
 * the given argument types.
 *
 * @param manager the node manager to be used for temporary IR nodes
 * @param parameter the list of parameter types accepted by the function
 * @param arguments the list of argument types passed to the function
 * @return a type-variable substitution mapping all the variables to argument specific types
 */
SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const TypeList& parameter, const TypeList& arguments);

/**
 * Tries to compute a valid type variable substitution for a call to a function of the given type using the given argument
 * types.
 *
 * @param manager the node manager to be used for temporary IR nodes
 * @param function the function to be invoked
 * @param arguments the list of argument types passed to the function
 * @return a type-variable substitution mapping all the variables to argument specific types
 */
SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const FunctionTypePtr& function, const TypeList& arguments);

/**
 * Tries to obtain the type variable instantiation implied by the given call.
 *
 * @param manager the node manager to be used for temporary IR nodes
 * @param call the call to be analyzed
 * @return a type-variable substitution mapping the type variables to their substitutions or null if no
 * 			valid substitution could be derived
 */
SubstitutionOpt getTypeVariableInstantiation(NodeManager& manager, const CallExprPtr& call);


} // end namespace types
} // end namespace core
} // end namespace insieme

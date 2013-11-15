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

#include "insieme/frontend/extensions/frontend_plugin.h"

 namespace insieme {
 namespace frontend {

/**
 * 		Long long cleanup
 * 		=================
 *
 * 		during the translation, long long behaves like a built in type, but in the backend it is mapped into the same bitwith as a long
 * 		the problem comes when we pass it as paramenter to functions, because it produces overload, even when in the backend both functions
 * 		are going to have the same parameter type
 *
 * 		after all translation units have being merged, we can safely remove this type, all the function calls are already mapped and staticaly 
 * 		resolved, so we wont find any problem related with typing.
 *
 * 		The only unresolved issue is 3rd party compatibility, now we wont have long long variables in the generated code, so we can not exploit 
 * 		overloads in 3rd party libraries (they do not make much sense anyway, do they? )
 *
 */
class LongLongExtension : public insieme::frontend::extensions::FrontendPlugin {
		insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& prog);
};

} // frontend
} // insieme

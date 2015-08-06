/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <vector>
#include <functional>

#include "insieme/core/ir_node.h"

#include "insieme/frontend/extensions/frontend_extension.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

namespace insieme {
namespace frontend {
namespace extensions {

/**
 *
 * This extension handles "insieme" pragmas
 */
class InsiemePragmaExtension: public FrontendExtension {
private:
	vector<insieme::core::NodePtr> entryPoints;
	
public:
	/**
	 * Registers all "insieme" pragmas and their handlers
	 */
	InsiemePragmaExtension();
	
	/**
	 * Used for processing previously established TransformationAnnotation objects
	 */
	virtual insieme::core::ProgramPtr IRVisit(insieme::core::ProgramPtr& program);
	
	/**
	 * Used for adding previously identified entry points
	 */
	virtual insieme::frontend::tu::IRTranslationUnit IRVisit(insieme::frontend::tu::IRTranslationUnit& tu);
	
	/**
	 * Visits the entire program to look for and process transformation annotations
	 * @param program the program which to check for TransformationAnnotation objects
	 * @return the transformed program
	 */
	core::ProgramPtr applyTransformations(const core::ProgramPtr& program);
};

} // extensions
} // frontend
} // insieme

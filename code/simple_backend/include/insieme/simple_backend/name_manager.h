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

#include <unordered_map>

#include "insieme/core/ast_node.h"

#include "insieme/utils/map_utils.h"

namespace insieme {
namespace simple_backend {


/**
 * Generates unique names for anonymous IR nodes when required.
 * Uses a simple counting system. Not thread safe, and won't necessarily generate the same name
 * for the same node in different circumstances. Names will, however, stay the same for unchanged
 * programs over multiple runs of the compiler.
 */
class NameManager {

	/**
	 * A counter used to generate individual names.
	 */
	unsigned long num;

	/**
	 * The internal cache used to maintain mapped names.
	 */
	utils::map::PointerMap<core::NodePtr, string> nameMap;

public:

	/**
	 * The default constructor for a new name manager.
	 */
	NameManager() : num(0) { }

	virtual string getName(const core::NodePtr& ptr, const string& fragment = "");

	virtual void setName(const core::NodePtr& ptr, const string& name);

	virtual string getVarName(const core::VariablePtr& var);
};

} // end: namespace simple_backend
} // end: namespace insieme

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

namespace insieme {
namespace annotations {
namespace c {

	using std::string;

	/**
	 * Checks whether a tag (e.g. "enum", "struct" or "union") is attached to the given node.
	 */
	bool hasAttachedCTag(const core::NodePtr& node);

	/**
	 * Obtains a reference to the C tag (e.g. "enum", "struct" or "union") attached to the given node. If
	 * no tag has been attached the result is undefined (an assertion in debug mode).
	 */
	const string& getAttachedCTag(const core::NodePtr& node);

	/**
	 * Attaches the given C tag (e.g. "enum", "struct" or "union") to the given node.
	 */
	const core::NodePtr& attachCTag(const core::NodePtr& node, const string& tag);

} // end namespace c
} // end namespace annotations
} // end namespace insieme

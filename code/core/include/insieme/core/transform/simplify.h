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

#include "insieme/core/ir_pointer.h"

namespace insieme {
namespace core {
namespace transform {



	/**
	 * Iterates through the given code fragment and replaces statically computable constructs with their
	 * simplified equivalents. The following simplifications are applied:
	 *
	 * 	- Direct calls of bind expressions like "bind(..){ ... } ( .. )" are contracted to a single call.
	 * 	- Calls to functions consisting of a single line (e.g. fun('a v1) { return v1; } are inlined.
	 * 	- if with a conditionals evaluating to true / false are substituted by the corresponding body
	 * 	- NoOps are eliminated
	 *
	 * @param manager the manager to be used for constructing the resulting code
	 * @param code the code to be simplified
	 * @return the simplified program code
	 */
	NodePtr simplify(NodeManager& manager, const NodePtr& code);

	/**
	 * A generic alternative of the simplify function.
	 *
	 * @param manager the manager to be used for constructing the resulting code
	 * @param code the code to be simplified
	 * @return the simplified program code
	 */
	template<typename T>
	Pointer<const T> simplify(NodeManager& manager, const Pointer<const T>& code) {
		return simplify(manager, NodePtr(code)).as<Pointer<const T>>();
	}


} // end namespace transform
} // end namespace core
} // end namespace insieme

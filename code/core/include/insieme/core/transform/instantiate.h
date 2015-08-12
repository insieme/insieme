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

#include "insieme/core/forward_decls.h"
#include "insieme/utils/pointer.h"

namespace insieme {
namespace core {
namespace transform {

namespace detail {

	static auto skipNone = [&](const NodePtr& node) {
		return false;
	};

	NodePtr instantiate(const NodePtr& root, std::function<bool(const core::NodePtr& node)> skip);

} // end namespace detail

/**
 * A transformation which instantiates all type variables within the given code fragment
 * where concrete types can be deduced from the call parameters.
 *
 * @param root the node to start the instantiation from
 * @return IR with the same root node, with all deducible types instantiated
 */
template<typename T>
core::Pointer<T> instantiateTypes(const core::Pointer<T>& root,
                                  std::function<bool(const NodePtr& node)> skip = detail::skipNone) {
	return static_pointer_cast<T>(detail::instantiate(root, skip));
}

} // end namespace transform
} // end namespace core
} // end namespace insieme

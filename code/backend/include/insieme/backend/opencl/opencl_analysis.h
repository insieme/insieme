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
#include "insieme/annotations/opencl/opencl_annotations.h"

namespace insieme {
namespace backend {
namespace opencl {
namespace analysis {
	/**
	 * Shortcut for eg. core::lang::PointerType(node).getElementType() & friends
	 */
	core::TypePtr getElementType(const core::TypePtr& type);
	/**
     * Returns the underlying type of a given node:
	 * eg: ref<ptr<array<int<4>>> yields int<4>
	 */
	core::TypePtr getUnderlyingType(const core::TypePtr& type);
	/**
	 * Removes any nested refs and ptrs until a non-indirect type is encountered.
	 */
	core::TypePtr getReferencedType(const core::NodePtr& node);
	core::TypePtr getReferencedType(const core::TypePtr& type);
	/**
	 * Tests whether the given node is outline-able or not.
	 */
	bool isOffloadAble(core::NodeManager& manager, const core::NodePtr& node);
	/**
	 * Obtain all variable requirements for callExpr which must have been built
	 * using opencl::transform::outline.
	 */
	annotations::opencl::VariableRequirementList getVariableRequirements(core::NodeManager& manager, const core::CallExprPtr& callExpr);
	/**
	 * Determine if a given for statement is independent and thus suitable for parallelization
	 */
	bool isIndependentStmt(const core::StatementPtr& stmt);
	/**
     * Determine all offloadable statements within a given node
     */
	core::NodeList getOffloadAbleStmts(const core::NodePtr& node);
}
}
}
}

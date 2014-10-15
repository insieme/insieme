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

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {
namespace transform {
namespace datalayout {

core::ExpressionAddress extractVariable(core::ExpressionAddress expr);

core::ExpressionPtr getBaseExpression(core::ExpressionPtr expr);

/*
 * Returns either the expression itself or the expression inside a nest of ref.deref calls
 */
core::ExpressionAddress tryRemoveDeref(const core::ExpressionAddress& expr);

core::NodeAddress getRootVariable(const core::NodeAddress scope, core::NodeAddress var);

core::ExpressionPtr removeRefVar(core::ExpressionPtr refVar);

core::TypePtr removeRef(core::TypePtr refTy);

core::TypePtr removeRefArray(core::TypePtr refTy);

core::TypePtr getBaseType(core::TypePtr type, core::StringValuePtr field);

core::ExpressionPtr valueAccess(core::ExpressionPtr thing, core::ExpressionPtr index, core::StringValuePtr field,
		core::ExpressionPtr vecIndex = core::ExpressionPtr());

core::ExpressionPtr refAccess(core::ExpressionPtr thing, core::ExpressionPtr index, core::StringValuePtr field,
		core::ExpressionPtr vecIndex = core::ExpressionPtr());

core::pattern::TreePattern declOrAssignment(core::pattern::TreePattern lhs, core::pattern::TreePattern rhs);

core::StatementAddress getStatementReplacableParent(core::NodeAddress toBeReplacedByAStatement);

bool validVar(core::ExpressionPtr toTest);

bool isInsideJob(core::NodeAddress toTest);

core::StatementPtr allocTypeUpdate(const core::StatementPtr& stmt, core::pattern::TreePattern& oldStructTypePattern,
		core::pattern::TreePattern& newStructTypePattern);

} // datalayout
} // transform
} // insieme

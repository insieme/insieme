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

#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/annotations/backend_instantiate.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"

namespace insieme {
namespace frontend {
namespace utils {

	using namespace core;

	FrontendInspireModule::FrontendInspireModule(core::NodeManager& manager) : core::lang::Extension(manager) {
		annotations::markBackendInstantiate(getCStyleAssignment());
		annotations::markBackendInstantiate(getCxxStyleAssignment());
		annotations::markBackendInstantiate(getCommaOperator());
		annotations::markBackendInstantiate(getBoolToInt());
	}

	ExpressionPtr buildCStyleAssignment(const ExpressionPtr& lhs, const ExpressionPtr& rhs) {
		NodeManager& mgr = lhs->getNodeManager();
		IRBuilder builder(mgr);
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(core::analysis::getReferencedType(lhs->getType()), inspMod.getCStyleAssignment(), lhs, rhs);
	}

    ExpressionPtr buildCxxStyleAssignment(const ExpressionPtr& lhs, const ExpressionPtr& rhs) {
		NodeManager& mgr = lhs->getNodeManager();
		IRBuilder builder(mgr);
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(lhs->getType(), inspMod.getCxxStyleAssignment(), lhs, rhs);
	}

	ExpressionPtr buildCommaOperator(const core::ExpressionPtr& lhs, const core::ExpressionPtr& rhs) {
		NodeManager& mgr = lhs->getNodeManager();
		IRBuilder builder(mgr);
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(inspMod.getCommaOperator(), lhs, rhs);
	}

	core::ExpressionPtr buildBoolToInt(const core::ExpressionPtr& b) {
		NodeManager& mgr = b->getNodeManager();
		IRBuilder builder(mgr);
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(mgr.getLangBasic().getInt4(), inspMod.getBoolToInt(), b);
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

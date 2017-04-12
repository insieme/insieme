/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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

	core::ExpressionPtr buildFERefTemp(const core::TypePtr& t) {
		NodeManager& mgr = t->getNodeManager();
		IRBuilder builder(mgr);
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(inspMod.getFERefTemp(), builder.getTypeLiteral(t));
	}

	core::ExpressionPtr buildCxxPlacementNew(const core::ExpressionPtr& loc, const core::ExpressionPtr& init) {
		NodeManager& mgr = loc->getNodeManager();
		IRBuilder builder(mgr);
		assert_true(core::lang::isPointer(loc)) << "Loc has to be a pointer";
		auto& inspMod = mgr.getLangExtension<FrontendInspireModule>();
		return builder.callExpr(inspMod.getCxxPlacementNew(), loc, init);
	}

} // end namespace utils
} // end namespace frontend
} // end namespace insieme

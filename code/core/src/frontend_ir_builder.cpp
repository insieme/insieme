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
 */

#include "insieme/core/frontend_ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/static_vars.h"

#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {


	ExpressionPtr FrontendIRBuilder::initStaticVariable(const LiteralPtr& staticVariable, const ExpressionPtr& initValue, bool constant) const {
		const lang::StaticVariableExtension& ext = getNodeManager().getLangExtension<lang::StaticVariableExtension>();

		assert_true(analysis::isRefType(staticVariable.getType()));
		assert_true(ext.isStaticType(analysis::getReferencedType(staticVariable->getType())));

		if(constant) {
			return callExpr(refType(initValue->getType()), ext.getStaticInitConst(), staticVariable, initValue);
		} else {
			return callExpr(refType(initValue->getType()), ext.getStaticInitLazy(), staticVariable, wrapLazy(initValue));
		}
	}

	StatementPtr FrontendIRBuilder::createStaticVariable(const LiteralPtr& staticVariable) const {
		const lang::StaticVariableExtension& ext = getNodeManager().getLangExtension<lang::StaticVariableExtension>();

		return callExpr(ext.getStaticCreate(), staticVariable);
	}


} // namespace core
} // namespace insieme

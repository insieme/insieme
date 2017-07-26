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

#include "insieme/core/ir_statements.h"

#include "insieme/core/ir_expressions.h"

namespace insieme {
namespace core {

	std::ostream& Declaration::printTo(std::ostream& out) const {
		return out << "decl " <<  *getType() << " : " << *getInitialization();
	}

	std::ostream& DeclarationStmt::printTo(std::ostream& out) const {
		return out << *getVariable()->getType() << " " << *getVariable() << " = " << *getInitialization();
	}

	std::ostream& ForStmt::printTo(std::ostream& out) const {
		return out << "for(" << *getIterator()->getType() << " " << *getIterator() << " = " << *getStart() << " .. " << *getEnd() << " : " << *getStep() << ") "
		           << *getBody();
	}

	std::ostream& SwitchCase::printTo(std::ostream& out) const {
		return out << "case " << *getGuard() << ": " << *getBody();
	}

	std::ostream& SwitchStmt::printTo(std::ostream& out) const {
		return out << "switch(" << *getSwitchExpr() << ") [ " << *getCases() << ((getCases()->empty()) ? " " : " | ") << "default: " << *getDefaultCase()
		           << " ]";
	}

	std::ostream& CatchClause::printTo(std::ostream& out) const {
		return out << "catch (" << *getVariable()->getType() << " " << *getVariable() << ") " << *getBody();
	}

	std::ostream& TryCatchStmt::printTo(std::ostream& out) const {
		return out << "try " << *getBody() << " " << join(" ", getClauses(), print<deref<CatchClausePtr>>());
	}

	DeclarationStmtPtr DeclarationStmt::get(NodeManager & manager, const VariablePtr& variable, const ExpressionPtr& initExpression) {
		return manager.get(DeclarationStmt(Declaration::get(manager, variable->getType(), initExpression), variable));
	}

	DeclarationStmtPtr DeclarationStmt::get(NodeManager& manager, const DeclarationPtr& declaration, const VariablePtr& variable) {
		return manager.get(DeclarationStmt(declaration, variable));
	}

	ForStmtPtr ForStmt::get(NodeManager& manager, const VariablePtr& iterator, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step,
	                        const CompoundStmtPtr& body) {
		return get(manager, DeclarationStmt::get(manager, iterator, start), end, step, body);
	}

} // end namespace core
} // end namespace insieme

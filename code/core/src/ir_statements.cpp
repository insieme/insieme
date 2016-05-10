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

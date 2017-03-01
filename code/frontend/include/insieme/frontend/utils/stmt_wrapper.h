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
#pragma once

#include "insieme/core/ir_statements.h"
#include "insieme/core/lang/basic.h"

namespace insieme {
namespace frontend {
namespace stmtutils {

	using namespace insieme::core;

	namespace {
		typedef vector<StatementPtr> StatementList;
	}

	//-------------------------------------------- StmtWrapper ------------------------------------------------------------
	/*
	 * Utility class used as a return type for the StmtVisitor. It can store a list of statement
	 * as conversion of a single C stmt can result in multiple IR statements.
	 */
	struct StmtWrapper : public StatementList {
		StmtWrapper(const StatementList& list) : StatementList(list) {}

		StmtWrapper() : StatementList() {}
		StmtWrapper(const StatementPtr& stmt) : StatementList({stmt}) {}
		StmtWrapper(const CompoundStmtPtr& stmt) : StatementList({stmt}) {}

		StatementPtr getSingleStmt() const {
			assert_eq(size(), 1) << "More than 1 statement present";
			return front();
		}

		bool isSingleStmt() const {
			return size() == 1;
		}

		std::ostream& operator<<(std::ostream& out) {
			for(auto s : *this) {
				out << "-" << dumpOneLine(s) << "\n";
			}
			return out;
		}
	};

	StatementPtr aggregateStmt(const IRBuilder& builder, const StatementPtr& stmt);
	StatementPtr aggregateStmts(const IRBuilder& builder, const StatementList& stmtVect);

} // end namespace stmtutils
} // end namespace frontend
} // end namespace insieme

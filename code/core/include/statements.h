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

#include <algorithm>
#include <iterator>
#include <map>
#include <memory>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

#include <boost/algorithm/string/join.hpp>

#include "expressions.h"

using std::vector;

class Statement {
public:
	virtual string toString() const { return ""; }
};
typedef std::shared_ptr<Statement> StmtPtr;

class BreakStmt : public Statement {
public:
	virtual string toString() const { return "break"; }
};
class ContinueStmt : public Statement {
public:
	virtual string toString() const { return "continue"; }
};


class ExpressionStmt : public Statement {
	ExprPtr expression;

public:
	ExpressionStmt(ExprPtr expression) : Statement(), expression(expression) {
	}
	virtual string toString() const { return expression->toString(); }
};

class DeclarationStmt : public ExpressionStmt {
	TypePtr type;
	
public:
	DeclarationStmt(TypePtr type, ExprPtr expression) : ExpressionStmt(expression), type(type) {
	}
	virtual string toString() const { return type->toString() + expression->toString(); }
};

class ReturnStmt : public ExpressionStmt {
public:
	ReturnStmt(ExprPtr expression) : ExpressionStmt(expression) {
	}
	virtual string toString() const { return string("return ") + expression->toString(); }
};


class CompoundStmt : public Statement {
	vector<StmtPtr> statements;
public:
	CompoundStmt() : Statement() {
	}
	CompoundStmt(StmtPtr stmt) : Statement() {
		statements.push_back(stmt);
	}
	CompoundStmt(vector<StmtPtr> stmts) : Statement(), statements(stmts) {
	}
	virtual string toString() const { 
		vector<string> list;
		std::transform(statements.cbegin(), statements.cend(), back_inserter(list), [](const StmtPtr cur) { return cur->toString(); });
		return boost::join(list, ";\n");
	}
};

class WhileStmt : public CompoundStmt {
};

class ForStmt : public CompoundStmt {
};

class IfStmt : public CompoundStmt {
};

class SwitchStmt : public CompoundStmt {
};
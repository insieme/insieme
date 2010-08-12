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

#include "annotated_ptr.h"
#include "container_utils.h"
#include "expressions.h"
#include "instance_manager.h"
#include "types.h"
#include "visitor.h"

using std::string;
using std::vector;

enum {
	STMT_HASH_NOOP, STMT_HASH_BREAK, STMT_HASH_CONTINUE
};

// Forward Declarations { -----------------------------------------------------

class Statement;
typedef AnnotatedPtr<const Statement> StmtPtr;

class NoOpStmt;
typedef AnnotatedPtr<const NoOpStmt> NoOpStmtPtr;

class BreakStmt;
typedef AnnotatedPtr<const BreakStmt> BreakStmtPtr;

class ContinueStmt;
typedef AnnotatedPtr<const ContinueStmt> ContinueStmtPtr;

class ExprStmt;
typedef AnnotatedPtr<const ExprStmt> ExprStmtPtr;

class DeclarationStmt;
typedef AnnotatedPtr<const DeclarationStmt> DeclarationStmtPtr;

class ReturnStmt;
typedef AnnotatedPtr<const ReturnStmt> ReturnStmtPtr;

class CompoundStmt;
typedef AnnotatedPtr<const CompoundStmt> CompoundStmtPtr;

class WhileStmt;
typedef AnnotatedPtr<const WhileStmt> WhileStmtPtr;

class ForStmt;
typedef AnnotatedPtr<const ForStmt> ForStmtPtr;

class IfStmt;
typedef AnnotatedPtr<const IfStmt> IfStmtPtr;

class SwitchStmt;
typedef AnnotatedPtr<const SwitchStmt> SwitchStmtPtr;

class StatementManager;

// Forward Declarations } -----------------------------------------------------

class StatementManager: public InstanceManager<const Statement, StmtPtr> {
	const TypeManager& typeManager;

friend class Statement;
friend class BreakStmt;


protected:
	StmtPtr getStmtPtr(const Statement& stmt) {
		// get master copy
		std::pair<StmtPtr, bool > res = add(stmt);

		// if new element has been added ...
		if (res.second) {
			// ... check whether sub-statements are present
			ChildVisitor<StmtPtr> visitor([&](StmtPtr cur) { this->getStmtPtr(*cur);});
		}
		return res.first;
	}

public:
	StatementManager(const TypeManager& typeManager) : typeManager(typeManager) { }
};


class Statement : public Visitable<StmtPtr> {

protected:
	virtual bool equals(const Statement& stmt) const = 0;


public:
	virtual string toString() const = 0;

	virtual Statement* clone() const = 0;

	virtual std::size_t hash() const = 0;


	bool operator==(const Statement& stmt) const {
		return (typeid(*this) == typeid(stmt)) && equals(stmt);
	}

	virtual ChildList getChildren() const {
		return newChildList();
	}

};

std::size_t hash_value(const Statement& stmt) {
	return stmt.hash();
}

class NoOpStmt: public Statement {
public:
	virtual string toString() const {
		return "{ /* NoOp */ }";
	}
	std::size_t hash_value() const {
		return 0;
	}

	virtual bool equals(const Statement& stmt) const {
		return true;
	}

	virtual std::size_t hash() const {
		return STMT_HASH_NOOP;
	}

	virtual NoOpStmt* clone() const {
		return new NoOpStmt();
	}

};

class BreakStmt: public Statement {
private:
	BreakStmt() {};

public:

	virtual string toString() const {
		return "break";
	}

	virtual bool equals(const Statement& stmt) const {
		return true;
	}

	virtual std::size_t hash() const {
		return STMT_HASH_BREAK;
	}

	virtual BreakStmt* clone() const {
		return new BreakStmt();
	}

	BreakStmtPtr get(StatementManager& manager) {
		return dynamic_pointer_cast<const BreakStmt>(manager.getStmtPtr(BreakStmt()));
	}
};

class ContinueStmt: public Statement {
public:
	virtual string toString() const {
		return "continue";
	}

	virtual bool equals(const Statement& stmt) const {
		return true;
	}

	virtual std::size_t hash() const {
		return STMT_HASH_CONTINUE;
	}

	virtual ContinueStmt* clone() const {
		return new ContinueStmt();
	}
};

//class ExprStmt: public Statement {
//	const ExprPtr expression;
//
//public:
//	ExprStmt(const ExprPtr& expression) :
//		expression(expression) {
//	}
//	virtual string toString() const {
//		return expression->toString();
//	}
//};
//
//class DeclarationStmt: public Statement {
//	const ExprPtr initExpression;
//	const TypePtr type;
//
//public:
//	DeclarationStmt(const TypePtr& type, const ExprPtr& initExpression) :
//		initExpression(initExpression), type(type) {
//	}
//	virtual string toString() const {
//		return type->toString() + " " + initExpression->toString();
//	}
//};
//
//class ReturnStmt: public Statement {
//	const ExprPtr returnExpression;
//
//public:
//	ReturnStmt(const ExprPtr& returnExpression) :
//		returnExpression(returnExpression) {
//	}
//	virtual string toString() const {
//		return string("return ") + returnExpression->toString();
//	}
//};
//
//class CompoundStmt: public Statement {
//	const vector<StmtPtr> statements;
//public:
//	CompoundStmt() {
//	}
//	CompoundStmt(const StmtPtr& stmt) :
//		statements(toVector<StmtPtr> (stmt)) {
//	}
//	CompoundStmt(const vector<StmtPtr>& stmts) :
//		statements(stmts) {
//	}
//	virtual string toString() const {
//		vector<string> list;
//		std::transform(statements.cbegin(), statements.cend(), back_inserter(list), [](const StmtPtr& cur) {return cur->toString();});
//		return boost::join(list, ";\n");
//	}
//};
//
//class WhileStmt: public Statement {
//	ExprPtr condition;
//	StmtPtr body;
//public:
//	WhileStmt(StmtPtr body, ExprPtr condition) :
//		condition(condition), body(body) {
//	}
//	virtual string toString() const {
//		return string("while(") + condition->toString() + ")\n" + body->toString();
//	}
//};
//
//class ForStmt: public Statement {
//	VarExprPtr variable;
//	ExprPtr start, end, step;
//	StmtPtr body;
//public:
//	ForStmt(StmtPtr body, VarExprPtr var, ExprPtr start, ExprPtr end, ExprPtr step) :
//		variable(var), start(start), end(end), step(step), body(body) {
//	}
//	virtual string toString() const {
//		return string("for(") + variable->toString() + "=" + start->toString() + ".." + end->toString() + ":"
//				+ step->toString() + ")\n" + body->toString();
//	}
//};
//
//class IfStmt: public Statement {
//	ExprPtr condition;
//	StmtPtr body;
//	StmtPtr elseBody;
//};
//
//class SwitchStmt: public Statement {
//};



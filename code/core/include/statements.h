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
#include "instance_manager.h"
#include "types.h"
#include "visitor.h"
#include "expressions.h"

using std::string;
using std::vector;

// Forward Declarations { -----------------------------------------------------

class Statement;
typedef AnnotatedPtr<const Statement> StmtPtr;

class NoOpStmt;
typedef AnnotatedPtr<const NoOpStmt> NoOpStmtPtr;

class BreakStmt;
typedef AnnotatedPtr<const BreakStmt> BreakStmtPtr;

class ContinueStmt;
typedef AnnotatedPtr<const ContinueStmt> ContinueStmtPtr;

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

// ------------------------------------- Statements ---------------------------------

class Statement : public Visitable<StmtPtr> {

	/**
	 * Statements are not assignable
	 */
	Statement& operator=(const Statement&) { }

protected:
	// Hash values for terminal statements
	enum {
		HASHVAL_NOOP, HASHVAL_BREAK, HASHVAL_CONTINUE, HASHVAL_DECLARATION, HASHVAL_RETURN,
		HASHVAL_COMPOUND, HASHVAL_WHILE, HASHVAL_FOR, HASHVAL_IF, HASHVAL_SWITCH
	};

	Statement() {}

	virtual bool equals(const Statement& stmt) const = 0;

public:
	virtual string toString() const = 0;
	virtual Statement* clone() const = 0;
	virtual std::size_t hash() const = 0;
	bool operator==(const Statement& stmt) const;
	virtual ChildList getChildren() const;
};
std::size_t hash_value(const Statement& stmt);


class NoOpStmt : public Statement {
	NoOpStmt() {}
public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual NoOpStmt* clone() const;

	static NoOpStmtPtr get(StatementManager& manager);
};


class BreakStmt : public Statement {
	BreakStmt() {}
public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual BreakStmt* clone() const;

	static BreakStmtPtr get(StatementManager& manager);
};


class ContinueStmt : public Statement {
	ContinueStmt() {}
public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ContinueStmt* clone() const;
	
	static ContinueStmtPtr get(StatementManager& manager);
};


class DeclarationStmt : public Statement {
	const TypePtr type;
	const ExprPtr initExpression;

	DeclarationStmt(const TypePtr& type, const ExprPtr& initExpression) :
		type(type), initExpression(initExpression) { }

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual DeclarationStmt* clone() const;
	
	static DeclarationStmtPtr get(StatementManager& manager, const TypePtr& type, const ExprPtr& initExpression);
};


class ReturnStmt: public Statement {
	const ExprPtr returnExpression;

	ReturnStmt(const ExprPtr& returnExpression) :
		returnExpression(returnExpression) { }

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ReturnStmt* clone() const;

	static ReturnStmtPtr get(StatementManager& manager, const ExprPtr& returnExpression);
};


class CompoundStmt: public Statement {
	const vector<StmtPtr> statements;

	CompoundStmt() { }
	CompoundStmt(const StmtPtr& stmt) :	statements(toVector<StmtPtr> (stmt)) { }
	CompoundStmt(const vector<StmtPtr>& stmts) : statements(stmts) { }

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual CompoundStmt* clone() const;

	static CompoundStmtPtr get(StatementManager& manager);
	static CompoundStmtPtr get(StatementManager& manager, const StmtPtr& stmt);
	static CompoundStmtPtr get(StatementManager& manager, const vector<StmtPtr>& stmts);
};


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

// ------------------------------------- Statement Manager ---------------------------------

class StatementManager : public InstanceManager<const Statement, StmtPtr> {
	const TypeManager& typeManager;
	
	friend NoOpStmtPtr NoOpStmt::get(StatementManager&);
	friend BreakStmtPtr BreakStmt::get(StatementManager&);
	friend ContinueStmtPtr ContinueStmt::get(StatementManager&);
	friend DeclarationStmtPtr DeclarationStmt::get(StatementManager& manager, const TypePtr& type, const ExprPtr& initExpression);
	friend ReturnStmtPtr ReturnStmt::get(StatementManager& manager, const ExprPtr& returnExpression);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const StmtPtr& stmt);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const vector<StmtPtr>& stmts);
	
	StmtPtr getStmtPtrImpl(const Statement& stmt);

protected:
	template<typename T>
	AnnotatedPtr<const T> getStmtPtr(const T& stmt) {
		return dynamic_pointer_cast<const T>(getStmtPtrImpl(stmt));
	}

public:
	StatementManager(const TypeManager& typeManager) : typeManager(typeManager) { }
};

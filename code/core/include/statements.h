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
#include <utility>

#include <boost/algorithm/string/join.hpp>

#include "annotated_ptr.h"
#include "container_utils.h"
#include "instance_manager.h"
#include "tree_manager.h"
#include "types.h"
#include "visitor.h"
#include "identifiers.h"

using std::string;
using std::vector;

class Expression;
typedef AnnotatedPtr<const Expression> ExprPtr;

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
	
	friend class InstanceManager<const Statement, StmtPtr>;

	virtual Statement* clone() const = 0;


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
	virtual std::size_t hash() const = 0;
	virtual ChildList getChildren() const;
	bool operator==(const Statement& stmt) const;
};
std::size_t hash_value(const Statement& stmt);


class NoOpStmt : public Statement {
	NoOpStmt() {}
	virtual NoOpStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;

	static NoOpStmtPtr get(StatementManager& manager);
};


class BreakStmt : public Statement {
	BreakStmt() {}
	virtual BreakStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;

	static BreakStmtPtr get(StatementManager& manager);
};


class ContinueStmt : public Statement {
	ContinueStmt() {}
	virtual ContinueStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	
	static ContinueStmtPtr get(StatementManager& manager);
};


class DeclarationStmt : public Statement {
	const TypePtr type;
	const Identifier id;
	const ExprPtr initExpression;

	DeclarationStmt(const TypePtr& type, const Identifier& id, const ExprPtr& initExpression);
	virtual DeclarationStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ChildList getChildren() const;
	
	static DeclarationStmtPtr get(StatementManager& manager, const TypePtr& type, const Identifier& id, const ExprPtr& initExpression);
};


class ReturnStmt: public Statement {
	const ExprPtr returnExpression;

	ReturnStmt(const ExprPtr& returnExpression);
	virtual ReturnStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ChildList getChildren() const;

	static ReturnStmtPtr get(StatementManager& manager, const ExprPtr& returnExpression);
};


class CompoundStmt: public Statement {
	const vector<StmtPtr> statements;

	CompoundStmt() { }
	CompoundStmt(const StmtPtr& stmt) :	statements(toVector<StmtPtr> (stmt)) { }
	CompoundStmt(const vector<StmtPtr>& stmts) : statements(stmts) { }
	virtual CompoundStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ChildList getChildren() const;

	const StmtPtr& operator[](unsigned index) const;

	static CompoundStmtPtr get(StatementManager& manager);
	static CompoundStmtPtr get(StatementManager& manager, const StmtPtr& stmt);
	static CompoundStmtPtr get(StatementManager& manager, const vector<StmtPtr>& stmts);
};


class WhileStmt: public Statement {
	ExprPtr condition;
	StmtPtr body;

	WhileStmt(ExprPtr condition, StmtPtr body);
	virtual WhileStmt* clone() const;

public:
	virtual string toString() const;
	virtual bool equals(const Statement& stmt) const;
	virtual std::size_t hash() const;
	virtual ChildList getChildren() const;

	static WhileStmtPtr get(StatementManager& manager, ExprPtr condition, StmtPtr body);
};

//class ForStmt: public Statement {
//	DeclarationStmtPtr declaration;
//	ExprPtr end, step;
//	StmtPtr body;
//
//	ForStmt(DeclarationStmtPtr declaration, StmtPtr body, ExprPtr end, ExprPtr step);
//	virtual ForStmt* clone() const;
//
//public:
//	virtual string toString() const;
//	virtual bool equals(const Statement& stmt) const;
//	virtual std::size_t hash() const;
//	virtual ChildList getChildren() const;
//	
//};

//class IfStmt: public Statement {
//	ExprPtr condition;
//	StmtPtr body;
//	StmtPtr elseBody;
//};
//
//class SwitchStmt: public Statement {
//};

// ------------------------------------- Statement Manager ---------------------------------

class StatementManager : public TreeManager<const Statement, StmtPtr> {
	TypeManager& typeManager;
	
	friend NoOpStmtPtr NoOpStmt::get(StatementManager&);
	friend BreakStmtPtr BreakStmt::get(StatementManager&);
	friend ContinueStmtPtr ContinueStmt::get(StatementManager&);
	friend DeclarationStmtPtr DeclarationStmt::get(StatementManager& manager, const TypePtr& type, const Identifier& id, const ExprPtr& initExpression);
	friend ReturnStmtPtr ReturnStmt::get(StatementManager& manager, const ExprPtr& returnExpression);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const StmtPtr& stmt);
	friend CompoundStmtPtr CompoundStmt::get(StatementManager& manager, const vector<StmtPtr>& stmts);
	friend WhileStmtPtr WhileStmt::get(StatementManager& manager, ExprPtr condition, StmtPtr body);
	

protected:
	template<typename T>
	AnnotatedPtr<const T> getStmtPtr(const T& stmt) {
		return dynamic_pointer_cast<const T>(getPointer(stmt));
	}

public:
	StatementManager(TypeManager& typeManager) : typeManager(typeManager) { }

	TypeManager& getTypeManager() {
		return typeManager;
	}
};

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
#include <memory>
#include <sstream>
#include <string>
#include <vector>
#include <utility>

#include "ast_node.h"
#include "annotated_ptr.h"
#include "container_utils.h"
#include "instance_manager.h"
#include "types.h"
#include "identifier.h"

using std::string;
using std::vector;

namespace insieme {
namespace core {

// some expression types ...
DECLARE_NODE_TYPE(Expression);
DECLARE_NODE_TYPE(VarExpr);

// Forward Declarations { -----------------------------------------------------

DECLARE_NODE_TYPE(Statement)

DECLARE_NODE_TYPE(BreakStmt)
DECLARE_NODE_TYPE(ContinueStmt)
DECLARE_NODE_TYPE(ReturnStmt)

DECLARE_NODE_TYPE(DeclarationStmt)
DECLARE_NODE_TYPE(CompoundStmt)
DECLARE_NODE_TYPE(ForStmt)
DECLARE_NODE_TYPE(WhileStmt)
DECLARE_NODE_TYPE(IfStmt)
DECLARE_NODE_TYPE(SwitchStmt)


// Forward Declarations } -----------------------------------------------------

// ------------------------------------- Statements ---------------------------------

/**
 * The abstract statement class provides the foundation for all AST nodes representing statements
 * and expressions.
 */
class Statement : public Node {
	// needs InstanceManager not NodeManager since base type calls clone
	friend class InstanceManager<Statement, AnnotatedPtr>;
	virtual Statement* clone(NodeManager& manager) const = 0;

protected:

	Statement(std::size_t hashCode, bool isExpr = false) : Node((isExpr)?EXPRESSION:STATEMENT, hashCode) {}

	virtual bool equals(const Node& node) const;
	virtual bool equalsStmt(const Statement& stmt) const = 0;

public:

	typedef NodeManager Manager;

	virtual ~Statement() {}
};

class BreakStmt : public Statement {
	BreakStmt();
	virtual BreakStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	static BreakStmtPtr get(NodeManager& manager);
};


class ContinueStmt : public Statement {
	ContinueStmt();
	virtual ContinueStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;
	
	static ContinueStmtPtr get(NodeManager& manager);
};


class ReturnStmt: public Statement {
	const ExpressionPtr returnExpression;

	ReturnStmt(const ExpressionPtr& returnExpression);
	virtual ReturnStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	static ReturnStmtPtr get(NodeManager& manager, const ExpressionPtr& returnExpression);
};


class DeclarationStmt : public Statement {
	const VarExprPtr varExpression;
	const ExpressionPtr initExpression;

	DeclarationStmt(const VarExprPtr& varExpression, const ExpressionPtr& initExpression);
	virtual DeclarationStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const VarExprPtr& getVarExpression() const { return varExpression; }
	const ExpressionPtr& getInitialization() const { return initExpression; }

	static DeclarationStmtPtr get(NodeManager& manager, const TypePtr& type, const Identifier& id, const ExpressionPtr& initExpression);
};


class CompoundStmt: public Statement {
	const vector<StatementPtr> statements;

	CompoundStmt(const vector<StatementPtr>& stmts);
	virtual CompoundStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const StatementPtr& operator[](unsigned index) const;

	static CompoundStmtPtr get(NodeManager& manager);
	static CompoundStmtPtr get(NodeManager& manager, const StatementPtr& stmt);
	static CompoundStmtPtr get(NodeManager& manager, const vector<StatementPtr>& stmts);
};


class WhileStmt: public Statement {
	ExpressionPtr condition;
	StatementPtr body;

	WhileStmt(const ExpressionPtr& condition, const StatementPtr& body);
	virtual WhileStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getCondition() const { return condition; }
	const StatementPtr& getBody() const { return body; }

	static WhileStmtPtr get(NodeManager& manager, const ExpressionPtr& condition, const StatementPtr& body);
};

class ForStmt: public Statement {
	DeclarationStmtPtr declaration;
	StatementPtr body;
	ExpressionPtr end, step;

	ForStmt(const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step);
	virtual ForStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const DeclarationStmtPtr& getDeclaration() const { return declaration; }
	const StatementPtr& getBody() const { return body; }
	const ExpressionPtr& getEnd() const { return end; }
	const ExpressionPtr& getStep() const { return step; }
	
	static ForStmtPtr get(NodeManager& manager, const DeclarationStmtPtr& declaration, const StatementPtr& body, const ExpressionPtr& end, const ExpressionPtr& step = NULL);
};

class IfStmt: public Statement {
	ExpressionPtr condition;
	StatementPtr thenBody;
	StatementPtr elseBody;
	
	IfStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody);
	virtual IfStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;
	
	const ExpressionPtr& getCondition() const { return condition; }
	const StatementPtr& getThenBody() const { return thenBody; }
	const StatementPtr& getElseBody() const { return elseBody; }

	static IfStmtPtr get(NodeManager& manager, const ExpressionPtr& condition, const StatementPtr& body, const StatementPtr& elseBody = NULL);
};

class SwitchStmt: public Statement {
public:
	typedef std::pair<ExpressionPtr, StatementPtr> Case;

private:
	const ExpressionPtr switchExpr;
	const vector<Case> cases;

	SwitchStmt(const ExpressionPtr& switchExpr, const vector<Case>& cases);
	virtual SwitchStmt* clone(NodeManager& manager) const;
	
protected:
	virtual bool equalsStmt(const Statement& stmt) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getSwitchExpr() const { return switchExpr; }
	const vector<Case>& getCases() const { return cases; }
	
	SwitchStmtPtr get(NodeManager& manager, const ExpressionPtr& switchExpr, const vector<Case>& cases);
};

} // end namespace core
} // end namespace insieme

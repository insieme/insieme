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

#include <cassert>
#include <memory>

#include "insieme/core/annotated_ptr.h"
#include "insieme/core/ast_node.h"
#include "insieme/core/identifier.h"
#include "insieme/core/statements.h"
#include "insieme/core/types.h"

#include "insieme/utils/numeric_cast.h"

//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
// 						HACK
// used to reset the value of the counter from tests
class VariableResetHack;
//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

namespace insieme {
namespace core {


class Expression : public Statement {

	/**
	 * Allow the test case to access private methods.
	 */
	template<typename PT>
	friend void basicExprTests(PT, const TypePtr&, const Node::ChildList& children = Node::ChildList());

private:

	virtual Expression* createCopyUsing(NodeMapping& mapper) const = 0;

protected:	

	/** The type of the expression. */
	const TypePtr type;
	
	Expression(NodeType nodeType, const TypePtr& type, const std::size_t& hashCode);

	virtual bool equals(const Node& stmt) const;
	virtual bool equalsExpr(const Expression& expr) const = 0;

	virtual bool equalsStmt(const Statement&) const {
		assert( false && "Should not be compared with a statement!");
		return false;
	}

	/**
	 * The Expression class provides this default implementation of the getChildNodes() method
	 * returning a list containing only the type of the expression.
	 */
	virtual OptionChildList getChildNodes() const {
		return OptionChildList(new ChildList(1, type));
	}

public:

	virtual ~Expression() {}

	virtual std::ostream& printTo(std::ostream& out) const = 0;

	/** Retrieves the type of this expression. */
	TypePtr getType() const { return type; }
};


class Literal : public Expression {

	const string value;

public:

	Literal(const TypePtr& type, const string& value);

	virtual ~Literal() {}

protected:

	bool equalsExpr(const Expression& expr) const;

	virtual Literal* createCopyUsing(NodeMapping& mapper) const;

public:

	virtual std::ostream& printTo(std::ostream& out) const {
		return (out << value);
	}

	template <class T>
    const T getValueAs() const { return utils::numeric_cast<T>(value); }

	const string& getValue() const { return value; }

	static LiteralPtr get(NodeManager& manager, const string& value, const TypePtr& type);
	static LiteralPtr get(NodeManager& manager, const TypePtr& type, const string& value);
};


class Variable : public Expression {
private:

	static unsigned int counter;

	const unsigned int id;

	//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
	// 						HACK
	friend class ::VariableResetHack;
	//@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

private:
    Variable(const TypePtr& type, unsigned int id);

protected:
	virtual Variable* createCopyUsing(NodeMapping& mapper) const;
	virtual bool equalsExpr(const Expression& expr) const;

public:

	unsigned int getId() const { return id; }

	virtual std::ostream& printTo(std::ostream& out) const;

	static VariablePtr get(NodeManager& manager, const TypePtr& type);
	static VariablePtr get(NodeManager& manager, const TypePtr& type, unsigned int id);
};


class LambdaExpr : public Expression {
public:
	typedef std::vector<DeclarationStmtPtr> CaptureList;
	typedef vector<VariablePtr> ParamList;

private:
	const CaptureList captureList;
	const ParamList params;
	const StatementPtr body;

	LambdaExpr(const TypePtr& type, const CaptureList& captureList, const ParamList& params, const StatementPtr& body);
	virtual LambdaExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	virtual bool equalsExpr(const Expression& expr) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const CaptureList& getCaptureList() const { return captureList; }
	const ParamList& getParams() const { return params; }
	const StatementPtr& getBody() const { return body; }

	static LambdaExprPtr get(NodeManager& manager, const TypePtr& type, const ParamList& params, const StatementPtr& body);
	static LambdaExprPtr get(NodeManager& manager, const TypePtr& type, const CaptureList& captureList, const ParamList& params, const StatementPtr& body);
};


class RecLambdaDefinition : public Node {

public:
	/**
	 * The type used to model the body of of this definition.
	 *
	 * TODO: think about replacing the function body by a value, not a pointer?
	 */
	typedef std::unordered_map<VariablePtr, LambdaExprPtr, hash_target<VariablePtr>, equal_target<VariablePtr>> RecFunDefs;

private:

	/**
	 * The definitions forming the body of this recursive lambda definition.
	 */
	const RecFunDefs definitions;

	/**
	 * Creates a new instance of this type based on a copy of the handed in definition.
	 */
	RecLambdaDefinition(const RecFunDefs& definitions);

	/**
	 * Creates a clone / deep copy of this instance referencing instances maintained
	 * by the given node manager.
	 */
	RecLambdaDefinition* createCopyUsing(NodeMapping& mapper) const;

protected:

	/**
	 * Compares this instance with the given node. In case it is representing the
	 * same recursive function definitions, the return value will be true. In any
	 * other case the test-result will be negative.
	 *
	 * @return true if equivalent (though possibly not identical), false otherwise
	 */
	virtual bool equals(const Node& other) const;

	/**
	 * Retrieves a list of all directly referenced nodes.
	 */
	virtual OptionChildList getChildNodes() const;

public:

	/**
	 * A static factory method to obtain a fresh pointer to a potentially shared instance
	 * of a recursive lambda definition instance representing the given definitions.
	 *
	 * @param manager the manager to be used for obtaining a proper instance
	 * @param definitions the definitions to be represented by the requested object
	 * @return a pointer to the requested recursive definition
	 */
	static RecLambdaDefinitionPtr get(NodeManager& manager, const RecFunDefs& definitions);

	/**
	 * Retrieves the definitions of the recursive functions represented by this instance.
	 */
	const RecFunDefs& getDefinitions() const { return definitions; }

	/**
	 * Obtains a pointer to the function body defining the recursive function represented
	 * by the given variable within this recursive definition.
	 *
	 * @param variable the variable used to define the requested recursive function within
	 * 				   this recursive function definition.
	 * @return a copy of the internally maintained pointer to the actual function definition.
	 */
	LambdaExprPtr getDefinitionOf(const VariablePtr& variable) const;

	/**
	 * Unrolls this definition once for the given variable.
	 *
	 * @param manager the manager to be used for maintaining the resulting reference
	 * @param variable the variable defining the definition to be unrolled once
	 * @return the resulting, unrolled lambda expression
	 */
	LambdaExprPtr unrollOnce(NodeManager& manager, const VariablePtr& variable) const;

	/**
	 * Prints a readable representation of this instance to the given output stream.
	 *
	 * @param out the stream to be printed to
	 * @return a reference to the stream
	 */
	virtual std::ostream& printTo(std::ostream& out) const;

};


class RecLambdaExpr : public Expression {

	/**
	 * The variable used within the recursive definition to describe the
	 * recursive function to be described by this expression.
	 */
	const VariablePtr variable;

	/**
	 * The definition body of this recursive type. Identical definitions may be
	 * shared among recursive type definitions.
	 */
	const RecLambdaDefinitionPtr definition;

	/**
	 * A constructor for creating a new recursive lambda.
	 *
	 * @param variable the variable identifying the recursive function within the definition block
	 * 				   to be represented by this expression.
	 * @param definition the recursive definitions to be based on.
	 */
	RecLambdaExpr(const VariablePtr& variable, const RecLambdaDefinitionPtr& definition);

	/**
	 * Creates a clone of this node.
	 */
	virtual RecLambdaExpr* createCopyUsing(NodeMapping& mapper) const;

	/**
	 * Obtains a list of all sub-nodes referenced by this AST node.
	 */
	virtual OptionChildList getChildNodes() const;

	/**
	 * Compares this recursive lambda expression with the given expression. If they
	 * are equivalent (though potentially not identical), true will be returned. Otherwise
	 * the result will be negative.
	 *
	 * @param expr the expression to be compared to.
	 * @return true if equivalent, false otherwise
	 */
	virtual bool equalsExpr(const Expression& expr) const;

public:

	/**
	 * A factory method for obtaining a new recursive lambda expression instance.
	 *
	 * @param manager the manager which should be maintaining the new instance
	 * @param variable the name of the variable used within the recursive lambda definition for representing the
	 * 					   recursive function to be defined by the resulting expression.
	 * @param definition the definition of the recursive lambda.
	 */
	static RecLambdaExprPtr get(NodeManager& manager, const VariablePtr& variable, const RecLambdaDefinitionPtr& definition);

	/**
	 * Prints a readable representation of this instance to the given output stream.
	 *
	 * @param out the stream to be printed to
	 * @return a reference to the stream
	 */
	virtual std::ostream& printTo(std::ostream& out) const;
	
	const VariablePtr& getVariable() const { return variable; }
	
	const RecLambdaDefinitionPtr& getDefinition() const { return definition; }
};


class TupleExpr : public Expression {
	const vector<ExpressionPtr> expressions;

	TupleExpr(const TupleTypePtr& type, const vector<ExpressionPtr>& expressions);
	virtual TupleExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const vector<ExpressionPtr>& getExpressions() const { return expressions; }

	static TupleExprPtr get(NodeManager& manager, const vector<ExpressionPtr>& expressions);
	static TupleExprPtr get(NodeManager& manager, const TupleTypePtr& type, const vector<ExpressionPtr>& expressions);
};


class VectorExpr : public Expression {
	const vector<ExpressionPtr> expressions;

	VectorExpr(const VectorTypePtr& type, const vector<ExpressionPtr>& expressions);
	virtual VectorExpr* createCopyUsing(NodeMapping& mapper) const;

protected:

	bool equalsExpr(const Expression& expr) const;
	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const vector<ExpressionPtr>& getExpressions() const { return expressions; }

	static VectorExprPtr get(NodeManager& manager, const vector<ExpressionPtr>& expressions);
	static VectorExprPtr get(NodeManager& manager, const VectorTypePtr& type, const vector<ExpressionPtr>& expressions);
};

class StructExpr : public Expression {
public:
	typedef std::pair<Identifier, ExpressionPtr> Member;
	typedef std::vector<Member> Members;

private:
	const Members members;

	StructExpr(const StructTypePtr& type, const Members& members);
	virtual StructExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	bool equalsExpr(const Expression& expr) const;
	virtual OptionChildList getChildNodes() const;

public:
	const Members& getMembers() const{ return members; }
	virtual std::ostream& printTo(std::ostream& out) const;
	static StructExprPtr get(NodeManager& manager, const Members& members);
	static StructExprPtr get(NodeManager& manager, const StructTypePtr& type, const Members& members);
};

class UnionExpr : public Expression {

	const Identifier memberName;
	const ExpressionPtr member;

	UnionExpr(const UnionTypePtr& type, const Identifier& memberName, const ExpressionPtr& member);
	virtual UnionExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	bool equalsExpr(const Expression& expr) const;
	virtual OptionChildList getChildNodes() const;

public:
	const Identifier& getMemberName() const { return memberName; }
	const ExpressionPtr& getMember() const { return member; }
	virtual std::ostream& printTo(std::ostream& out) const;
	static UnionExprPtr get(NodeManager& manager, const UnionTypePtr& type, const Identifier& memberName, const ExpressionPtr& member);
};


class JobExpr : public Expression {
public:
	typedef std::vector<DeclarationStmtPtr> LocalDecls;
	typedef std::pair<LambdaExprPtr, LambdaExprPtr> GuardedStmt;
	typedef std::vector<GuardedStmt> GuardedStmts;

private:
	const LocalDecls localDecls;
	const GuardedStmts guardedStmts;
	const LambdaExprPtr defaultStmt;

	JobExpr(const TypePtr& type, const LambdaExprPtr& defaultStmt,
		const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
	virtual JobExpr* createCopyUsing(NodeMapping& mapper) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;
	
public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const LocalDecls& getLocalDecls() const { return localDecls; }
	const GuardedStmts& getGuardedStmts() const { return guardedStmts; }
	const LambdaExprPtr& getDefaultStmt() const { return defaultStmt; }

	static JobExprPtr get(NodeManager& manager, const LambdaExprPtr& defaultStmt,
		const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
};


class CallExpr : public Expression {
	const ExpressionPtr functionExpr;
	const vector<ExpressionPtr> arguments;

	CallExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
	CallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
private:
	virtual CallExpr* createCopyUsing(NodeMapping& mapper) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;
	
public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getFunctionExpr() const { return functionExpr; }
	const vector<ExpressionPtr>& getArguments() const { return arguments; }

	// TODO: re-add with proper type inferencing of return type
	// static CallExprPtr get(NodeManager& manager, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
	static CallExprPtr get(NodeManager& manager, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
};

class CastExpr : public Expression {
	const ExpressionPtr subExpression;

	CastExpr(const TypePtr& type, const ExpressionPtr& subExpression);
	virtual CastExpr* createCopyUsing(NodeMapping& mapper) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getSubExpression() const { return subExpression; }

	static CastExprPtr get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& subExpression);
};


class MemberAccessExpr : public Expression {
	const ExpressionPtr subExpression;
	const Identifier member;

	MemberAccessExpr(const ExpressionPtr& subExpression, const Identifier& member);
	virtual MemberAccessExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getSubExpression() const { return subExpression; }
	
	const Identifier& getMemberName() const { return member; }

	static MemberAccessExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression, const Identifier& member);
};


class TupleProjectionExpr : public Expression {
	const ExpressionPtr subExpression;
	const unsigned index;

	TupleProjectionExpr( const ExpressionPtr& subExpression, const unsigned index);
	virtual TupleProjectionExpr* createCopyUsing(NodeMapping& mapper) const;

protected:
	bool equalsExpr(const Expression& expr) const;

	virtual OptionChildList getChildNodes() const;

public:
	virtual std::ostream& printTo(std::ostream& out) const;

	const ExpressionPtr& getSubExpression() const { return subExpression; }
	
	const unsigned getIndex() const { return index; }

	static TupleProjectionExprPtr get(NodeManager& manager, const ExpressionPtr& subExpression, const unsigned index);
};

} // end namespace core
} // end namespace insieme


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

#include <memory>

#include "annotated_ptr.h"
#include "ast_node.h"
#include "identifiers.h"
#include "types.h"

namespace insieme {
namespace core {
	
DECLARE_NODE_TYPE(Statement);
DECLARE_NODE_TYPE(DeclarationStmt);

// Forward Declarations { -----------------------------------------------------

DECLARE_NODE_TYPE(Expression);

DECLARE_NODE_TYPE(BoolLiteral);
DECLARE_NODE_TYPE(IntLiteral);
DECLARE_NODE_TYPE(FloatLiteral);

DECLARE_NODE_TYPE(VarExpr);
DECLARE_NODE_TYPE(ParamExpr);
DECLARE_NODE_TYPE(CallExpr);
DECLARE_NODE_TYPE(CastExpr);

DECLARE_NODE_TYPE(TupleExpr);
DECLARE_NODE_TYPE(NamedCompositeExpr);
DECLARE_NODE_TYPE(StructExpr);
DECLARE_NODE_TYPE(UnionExpr);
DECLARE_NODE_TYPE(JobExpr);
DECLARE_NODE_TYPE(LambdaExpr);

// Forward Declarations } -----------------------------------------------------

class Expression : public Node {
protected:	

	/** The type of the expression. */
	const TypePtr type;
	
	Expression(const TypePtr& type, const std::size_t& hashCode) : Node(EXPRESSION, hashCode), type(type) { };

	virtual bool equals(const Node& stmt) const;
	virtual bool equalsExpr(const Expression& expr) const = 0;

public:

	virtual ~Expression() {}

	virtual void printTo(std::ostream& out) const = 0;
	/** Retrieves the type of this expression. */
	TypePtr getType() const { return type; }
};


template<typename T>
class Literal : public Expression {

	static std::size_t hashLiteral(std::size_t seed, const TypePtr& type, const T& value) {
		boost::hash_combine(seed, type->hash());
		boost::hash_combine(seed, boost::hash_value(value));
		return seed;
	}

protected:
    const T value;
	
	Literal(const TypePtr& type, const T& value, const std::size_t& hashSeed) :
		Expression(type,hashLiteral(hashSeed, type, value)), value(value) { }
	virtual ~Literal() {}

	bool equalsExpr(const Expression& expr) const {
		const Literal<T>& rhs = static_cast<const Literal<T>&>(expr);
		return (value == rhs.value);
	}

public:
	virtual void printTo(std::ostream& out) const {
		out << value;
	}

    const T getValue() const { return value; }
};

class IntLiteral : public Literal<int> {
	IntLiteral(const TypePtr& type, int val);
	virtual IntLiteral* clone(NodeManager& manager) const;
	
public:
	
	static IntLiteralPtr get(NodeManager& manager, int value, unsigned short bytes = 4);
	static IntLiteralPtr one(NodeManager& manager) { return get(manager, 1); }
	static IntLiteralPtr zero(NodeManager& manager) { return get(manager, 0); }
};

class FloatLiteral : public Literal<double> {
	const string originalString;

	FloatLiteral(const TypePtr& type, double val, const string& originalString);
	virtual FloatLiteral* clone(NodeManager& manager) const;

public:
	virtual void printTo(std::ostream& out) const;

	static FloatLiteralPtr get(NodeManager& manager, double value, unsigned short bytes = 8);
	static FloatLiteralPtr get(NodeManager& manager, const string& from, unsigned short bytes = 8);
};

class BoolLiteral : public Literal<bool> {
	BoolLiteral(const TypePtr& type, bool val);
	virtual BoolLiteral* clone(NodeManager& manager) const;
	
public:

	static BoolLiteralPtr get(NodeManager& manager, bool value);
};

//class StringLiteral : public Literal<string> {
//public:
//	// TODO: fix null type
//	StringLiteral(const string& val) : Literal(NULL, val) { }
//};

class VarExpr : public Expression {
protected:
	const Identifier id;
	
    VarExpr(const TypePtr& type, const Identifier& id);
    VarExpr(const TypePtr& type, const Identifier& id, const std::size_t& hashCode);

	virtual VarExpr* clone(NodeManager& manager) const;
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;

	static VarExprPtr get(NodeManager& manager, const TypePtr& type, const Identifier &id);
};


class ParamExpr : public VarExpr {
	ParamExpr(const TypePtr& type, const Identifier& id);
	virtual ParamExpr* clone(NodeManager& manager) const;

public:
	virtual void printTo(std::ostream& out) const;

	static ParamExprPtr get(NodeManager& manager, const TypePtr& type, const Identifier &id);
};


class LambdaExpr : public Expression {
public:
	typedef vector<ParamExprPtr> ParamList;

private:
	const StatementPtr body;
	const ParamList params;

	LambdaExpr(const TypePtr& type, const ParamList& params, const StatementPtr& body);
	virtual LambdaExpr* clone(NodeManager& manager) const;

protected:
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;

	static LambdaExprPtr get(NodeManager& manager, const TypePtr& type, const ParamList& params, const StatementPtr& body);
};


class TupleExpr : public Expression {
	const vector<ExpressionPtr> expressions;

	TupleExpr(const TypePtr& type, const vector<ExpressionPtr>& expressions);
	virtual TupleExpr* clone(NodeManager& manager) const;

protected:
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;

	static TupleExprPtr get(NodeManager& manager, const vector<ExpressionPtr>& expressions);
};


class NamedCompositeExpr : public Expression {
public:
	typedef std::pair<Identifier, ExpressionPtr> Member;
	typedef std::vector<Member> Members;

protected:
	NamedCompositeExpr(const TypePtr& type, size_t hashval, const Members& members);

	const Members members;
	bool equalsExpr(const Expression& expr) const;

	Members getManagedMembers(NodeManager& manager) const;
	static NamedCompositeType::Entries getTypeEntries(const Members& mem);
};

class StructExpr : public NamedCompositeExpr {
	StructExpr(const TypePtr& type, const Members& members);
	virtual StructExpr* clone(NodeManager& manager) const;

public:
	virtual void printTo(std::ostream& out) const;
	static StructExprPtr get(NodeManager& manager, const Members& members);
};

class UnionExpr : public NamedCompositeExpr {
	UnionExpr(const TypePtr& type, const Members& members);
	virtual UnionExpr* clone(NodeManager& manager) const;

public:
	virtual void printTo(std::ostream& out) const;
	static UnionExprPtr get(NodeManager& manager, const Members& members);
};


class JobExpr : public Expression {
public:
	typedef std::vector<DeclarationStmtPtr> LocalDecls;
	typedef std::pair<ExpressionPtr, StatementPtr> GuardedStmt;
	typedef std::vector<GuardedStmt> GuardedStmts;

private:
	LocalDecls localDecls;
	GuardedStmts guardedStmts;
	StatementPtr defaultStmt;

	JobExpr(const TypePtr& type, const StatementPtr& defaultStmt, 
		const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
	virtual JobExpr* clone(NodeManager& manager) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;
	
public:
	virtual void printTo(std::ostream& out) const;

	static JobExprPtr get(NodeManager& manager, const StatementPtr& defaultStmt, 
		const GuardedStmts& guardedStmts = GuardedStmts(), const LocalDecls& localDecs = LocalDecls());
};


class CallExpr : public Expression {
	const ExpressionPtr functionExpr;
	const vector<ExpressionPtr> arguments;

	CallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
	virtual CallExpr* clone(NodeManager& manager) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;
	
public:
	virtual void printTo(std::ostream& out) const;

	static CallExprPtr get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments);
};

class CastExpr : public Expression {
	const ExpressionPtr subExpression;

	CastExpr(const TypePtr& type, const ExpressionPtr& subExpression);
	virtual CastExpr* clone(NodeManager& manager) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;

	static CastExprPtr get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& subExpression);
};

} // end namespace core
} // end namespace insieme

/// Allows expressions to be printed to a stream (especially useful during debugging and
/// within test cases where equals expects values to be printable).
std::ostream& operator<<(std::ostream& out, const insieme::core::Expression& expression);



//class ParenExpr : public Expression {
//	const ExprPtr subExpression;
//
//	ParenExpr(const ExprPtr& subExp)
//		: Expression(subExp->getType()), subExpression(subExp) { }
//	
//	virtual ParenExpr* clone(NodeManager& manager) const;
//
//protected:
//	bool equalsExpr(const Expression& expr) const;
//	
//public:
//	virtual void printTo(std::ostream& out) const;
//	virtual std::size_t hash() const;
//
//	static ParenExprPtr ParenExpr::get(NodeManager& manager, const ExprPtr& subExpr);
//};


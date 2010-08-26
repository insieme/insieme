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
#include "types.h"
#include "statements.h"
#include "identifiers.h"

// Forward Declarations { -----------------------------------------------------

class Expression;
typedef AnnotatedPtr<const Expression> ExprPtr;

class IntLiteral;
typedef AnnotatedPtr<const IntLiteral> IntLiteralPtr;

class FloatLiteral;
typedef AnnotatedPtr<const FloatLiteral> FloatLiteralPtr;

class BoolLiteral;
typedef AnnotatedPtr<const BoolLiteral> BoolLiteralPtr;

class VariableExpr;
typedef AnnotatedPtr<const VariableExpr> VariableExprPtr;

class ParameterExpr;
typedef AnnotatedPtr<const ParameterExpr> ParameterExprPtr;

class LambdaExpr;
typedef AnnotatedPtr<const LambdaExpr> LambdaExprPtr;

class CallExpr;
typedef AnnotatedPtr<const CallExpr> CallExprPtr;

class CastExpr;
typedef AnnotatedPtr<const CastExpr> CastExprPtr;

// Forward Declarations } -----------------------------------------------------

class Expression : public Statement {
protected:	
	enum {
		HASHVAL_INTLITERAL = 100 /* offset from statements */, HASHVAL_FLOATLITERAL, HASHVAL_BOOLLITERAL,
		HASHVAL_VAREXPR, HASHVAL_CALLEXPR, HASHVAL_CASTEXPR, HASHVAL_PARAMEXPR, HASHVAL_LAMBDAEXPR
	};

	/** The type of the represented expression. */
	const TypePtr type;
	
	Expression(const TypePtr& type) : type(type) { };
	virtual ~Expression() {}

	virtual bool equals(const Statement& stmt) const;
	virtual bool equalsExpr(const Expression& expr) const = 0;

public:
	/** Retrieves the type of this expression. */
	TypePtr getType() const { return type; }
};


template<typename T>
class Literal : public Expression {
protected:
    const T value;
	
	Literal(const TypePtr& type, const T& val) : Expression(type), value(val) { };
	virtual ~Literal() {}

	bool equalsExpr(const Expression& expr) const {
		// conversion is guaranteed by base operator==
		const Literal<T>& rhs = dynamic_cast<const Literal<T>&>(expr); 
		return (value == rhs.value);
	}

public:
	virtual void printTo(std::ostream& out) const {
		out << value;
	}

    const T getValue() const { return value; }
};

class IntLiteral : public Literal<int> {
	IntLiteral(const TypePtr& type, int val) : Literal<int>(type, val) { }
	virtual IntLiteral* clone(StatementManager& manager) const;
	
public:
	virtual std::size_t hash() const;

	static IntLiteralPtr get(StatementManager& manager, int value, unsigned short bytes = 4);
	static IntLiteralPtr one(StatementManager& manager) { return get(manager, 1); }
	static IntLiteralPtr zero(StatementManager& manager) { return get(manager, 0); }
};

class FloatLiteral : public Literal<double> {
	const string originalString;

	FloatLiteral(const TypePtr& type, double val, const string& originalString)
		: Literal<double>(type, val), originalString(originalString) { }
	virtual FloatLiteral* clone(StatementManager& manager) const;

public:
	virtual std::size_t hash() const;

	static FloatLiteralPtr get(StatementManager& manager, double value, unsigned short bytes = 8);
	static FloatLiteralPtr get(StatementManager& manager, const string& from, unsigned short bytes = 8);
};

class BoolLiteral : public Literal<bool> {
	BoolLiteral(const TypePtr& type, bool val) : Literal<bool>(type, val) { }
	virtual BoolLiteral* clone(StatementManager& manager) const;
	
public:
	virtual std::size_t hash() const;

	static BoolLiteralPtr get(StatementManager& manager, bool value);
};

//class StringLiteral : public Literal<string> {
//public:
//	// TODO: fix null type
//	StringLiteral(const string& val) : Literal(NULL, val) { }
//};

class VariableExpr : public Expression {
protected:
	const Identifier id;
	
    VariableExpr(const TypePtr& type, const Identifier& id) : Expression(type), id(id) { };
	virtual VariableExpr* clone(StatementManager& manager) const;
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;
	virtual std::size_t hash() const;

	static VariableExprPtr get(StatementManager& manager, const TypePtr& type, const Identifier &id);
};


class ParameterExpr : public VariableExpr {
	ParameterExpr(const TypePtr& type, const Identifier& id) : VariableExpr(type, id) { };
	virtual ParameterExpr* clone(StatementManager& manager) const;

public:
	virtual void printTo(std::ostream& out) const;
	virtual std::size_t hash() const;

	static ParameterExprPtr get(StatementManager& manager, const TypePtr& type, const Identifier &id);
};


class LambdaExpr : public Expression {
public:
	typedef vector<ParameterExprPtr> ParamList;

private:
	const StmtPtr body;
	const ParamList params;

	LambdaExpr(const TypePtr& type, const ParamList& params, const StmtPtr& body) 
		: Expression(type), body(body), params(params) { };
	virtual LambdaExpr* clone(StatementManager& manager) const;

protected:
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;
	virtual std::size_t hash() const;

	static LambdaExprPtr get(StatementManager& manager, const TypePtr& type, const ParamList& params, const StmtPtr& body);
};


class CallExpr : public Expression {
	const ExprPtr functionExpr;
	const vector<ExprPtr> arguments;

	CallExpr(const TypePtr& type, const ExprPtr& functionExpr, const vector<ExprPtr>& arguments) 
		: Expression(type), functionExpr(functionExpr), arguments(arguments) { }
	virtual CallExpr* clone(StatementManager& manager) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;
	
public:
	virtual void printTo(std::ostream& out) const;
	virtual std::size_t hash() const;

	static CallExprPtr get(StatementManager& manager, const TypePtr& type, const ExprPtr& functionExpr, const vector<ExprPtr>& arguments);
};

class CastExpr : public Expression {
	const ExprPtr subExpression;

	CastExpr(const TypePtr& type, const ExprPtr& subExpression)
		: Expression(type), subExpression(subExpression) { }
	virtual CastExpr* clone(StatementManager& manager) const;
	
protected:
	bool equalsExpr(const Expression& expr) const;

public:
	virtual void printTo(std::ostream& out) const;
	virtual std::size_t hash() const;

	static CastExprPtr get(StatementManager& manager, const TypePtr& type, const ExprPtr& subExpression);
};


/**
 * Allows expressions to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const Expression& expression);

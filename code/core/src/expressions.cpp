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

#include "expressions.h"

#include "types_utils.h"
#include "statements.h"
#include "string_utils.h"
#include "functional_utils.h"

using namespace insieme::core;

enum {
	HASHVAL_INTLITERAL = 100 /* offset from statements */, HASHVAL_FLOATLITERAL, HASHVAL_BOOLLITERAL,
	HASHVAL_VAREXPR, HASHVAL_CALLEXPR, HASHVAL_CASTEXPR, HASHVAL_PARAMEXPR, HASHVAL_LAMBDAEXPR, HASHVAL_PARENEXPR
};

// ------------------------------------- Expression ---------------------------------

bool Expression::equals(const Node& stmt) const {
	// conversion is guaranteed by base Node::operator==
	const Expression& rhs = static_cast<const Expression&>(stmt);
	return (*type == *rhs.type) && equalsExpr(rhs);
}

std::ostream& operator<<(std::ostream& out, const Expression& expression) {
	expression.printTo(out);
	return out;
}


// ------------------------------------- IntLiteral ---------------------------------

IntLiteral::IntLiteral(const TypePtr& type, int val) : Literal<int>(type, val, HASHVAL_INTLITERAL) { }

IntLiteral* IntLiteral::clone(NodeManager& manager) const {
	return new IntLiteral(manager.get(type), value);
}


IntLiteralPtr IntLiteral::get(NodeManager& manager, int value, unsigned short bytes) {
	return manager.get(IntLiteral(IntType::get(manager, bytes), value));
}

// ------------------------------------- FloatLiteral ---------------------------------

FloatLiteral::FloatLiteral(const TypePtr& type, double val, const string& originalString)
		: Literal<double>(type, val, HASHVAL_FLOATLITERAL), originalString(originalString) { }

FloatLiteral* FloatLiteral::clone(NodeManager& manager) const {
	return new FloatLiteral(manager.get(type), value, originalString);
}

void FloatLiteral::printTo(std::ostream& out) const {
	out << originalString;
}

FloatLiteralPtr FloatLiteral::get(NodeManager& manager, double value, unsigned short bytes) {
	return manager.get(FloatLiteral(FloatType::get(manager, bytes), value, toString(value)));
}

FloatLiteralPtr FloatLiteral::get(NodeManager& manager, const string& from, unsigned short bytes) {
	// TODO atof good idea? What about hex/octal/etc
	return manager.get(FloatLiteral(FloatType::get(manager, bytes), atof(from.c_str()), from));
}

// ------------------------------------- BoolLiteral ---------------------------------

BoolLiteral::BoolLiteral(const TypePtr& type, bool val) : Literal<bool>(type, val, HASHVAL_BOOLLITERAL) { }

BoolLiteral* BoolLiteral::clone(NodeManager& manager) const {
	return new BoolLiteral(manager.get(type), value);
}

BoolLiteralPtr BoolLiteral::get(NodeManager& manager, bool value) {
	return manager.get(BoolLiteral(BoolType::get(manager), value));
}

// ------------------------------------- VarExpr ---------------------------------

std::size_t hashVarExpr(const TypePtr& type, const Identifier& id) {
	size_t seed = HASHVAL_VAREXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

VarExpr::VarExpr(const TypePtr& type, const Identifier& id) : Expression(type, ::hashVarExpr(type, id)), id(id) { };
VarExpr::VarExpr(const TypePtr& type, const Identifier& id, const std::size_t& hashCode) : Expression(type, hashCode), id(id) { };

VarExpr* VarExpr::clone(NodeManager& manager) const {
	return new VarExpr(manager.get(type), id);
}

bool VarExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const VarExpr& rhs = dynamic_cast<const VarExpr&>(expr);
	return rhs.id == id;
}

void VarExpr::printTo(std::ostream& out) const {
	out << id;
}

VarExprPtr VarExpr::get(NodeManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(VarExpr(type, id));
}

// ------------------------------------- ParamExpr ---------------------------------


std::size_t hashParamExpr(const TypePtr& type, const Identifier& id) {
	size_t seed = HASHVAL_PARAMEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

ParamExpr::ParamExpr(const TypePtr& type, const Identifier& id) : VarExpr(type, id, ::hashParamExpr(type,id)) { };

ParamExpr* ParamExpr::clone(NodeManager& manager) const {
	return new ParamExpr(manager.get(type), id);
}

void ParamExpr::printTo(std::ostream& out) const {
	out << *type << id;
}

ParamExprPtr ParamExpr::get(NodeManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(ParamExpr(type, id));
}

// ------------------------------------- LambdaExpr ---------------------------------

std::size_t hashLambdaExpr(const TypePtr& type, const LambdaExpr::ParamList& params, const StatementPtr& body) {
	size_t seed = HASHVAL_LAMBDAEXPR;
	boost::hash_combine(seed, type->hash());
	hashPtrRange(seed, params);
	boost::hash_combine(seed, body->hash());
	return seed;
}

LambdaExpr::LambdaExpr(const TypePtr& type, const ParamList& params, const StatementPtr& body)
		: Expression(type, ::hashLambdaExpr(type, params, body)), body(body), params(params) { };

LambdaExpr* LambdaExpr::clone(NodeManager& manager) const {
	return new LambdaExpr(manager.get(type), manager.getAll(params), manager.get(body));
}

bool LambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const LambdaExpr& rhs = dynamic_cast<const LambdaExpr&>(expr);
	return (*body == *rhs.body) && std::equal(params.cbegin(), params.cend(), rhs.params.cbegin(), equal_target<ExpressionPtr>());
}

void LambdaExpr::printTo(std::ostream& out) const {
	out << "lambda(" << join(", ", params, deref<ExpressionPtr>()) << ") { " << body << " }";
}

LambdaExprPtr LambdaExpr::get(NodeManager& manager, const TypePtr& type, const ParamList& params, const StatementPtr& body) {
	return manager.get(LambdaExpr(type, params, body));
}

// ------------------------------------- CallExpr ---------------------------------

std::size_t hashCallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	size_t seed = HASHVAL_CALLEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, functionExpr->hash());
	hashPtrRange(seed, arguments);
	return seed;
}

CallExpr::CallExpr(const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments)
		: Expression(type, ::hashCallExpr(type, functionExpr, arguments)), functionExpr(functionExpr), arguments(arguments) { }

CallExpr* CallExpr::clone(NodeManager& manager) const {
	return new CallExpr(manager.get(type), manager.get(*functionExpr), manager.getAll(arguments));
}
	
bool CallExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CallExpr& rhs = dynamic_cast<const CallExpr&>(expr);
	return (*rhs.functionExpr == *functionExpr) && 
		equal(arguments.cbegin(), arguments.cend(), rhs.arguments.cbegin(), equal_target<ExpressionPtr>());

}
	
void CallExpr::printTo(std::ostream& out) const {
	out << functionExpr << "(" << join(", ", arguments) << ")";
}

CallExprPtr CallExpr::get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
	return manager.get(CallExpr(type, functionExpr, arguments));
}

// ------------------------------------- CastExpr ---------------------------------

std::size_t hashCastExpr(const TypePtr& type, const ExpressionPtr& subExpression) {
	size_t seed = HASHVAL_CASTEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, subExpression->hash());
	return seed;
}

CastExpr::CastExpr(const TypePtr& type, const ExpressionPtr& subExpression)
		: Expression(type, hashCastExpr(type, subExpression)), subExpression(subExpression) { }

CastExpr* CastExpr::clone(NodeManager& manager) const {
	return new CastExpr(manager.get(type), manager.get(*subExpression));
}
	
bool CastExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CastExpr& rhs = dynamic_cast<const CastExpr&>(expr);
	return (*rhs.subExpression == *subExpression);
}
	
void CastExpr::printTo(std::ostream& out) const {
	out << "(" << type << ")" << "(" << subExpression <<")";
}

CastExprPtr CastExpr::get(NodeManager& manager, const TypePtr& type, const ExpressionPtr& subExpr) {
	return manager.get(CastExpr(type, subExpr));
}

// ------------------------------------- ParenExpr ---------------------------------

//ParenExpr* ParenExpr::clone(NodeManager& manager) const {
//	return new ParenExpr(manager.get(*subExpression));
//}
//	
//bool ParenExpr::equalsExpr(const Expression& expr) const {
//	// conversion is guaranteed by base operator==
//	const ParenExpr& rhs = dynamic_cast<const ParenExpr&>(expr);
//	return (*rhs.subExpression == *subExpression);
//}
//	
//void ParenExpr::printTo(std::ostream& out) const {
//	out << "(" << subExpression <<")";
//}
//
//std::size_t ParenExpr::hash() const {
//	size_t seed = HASHVAL_PARENEXPR;
//	boost::hash_combine(seed, type->hash());
//	boost::hash_combine(seed, subExpression->hash());
//	return seed;
//}
//
//ParenExprPtr ParenExpr::get(NodeManager& manager, const ExprPtr& subExpr) {
//	return manager.get(ParenExpr(subExpr));
//}

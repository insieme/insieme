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

using namespace insieme::core;

// ------------------------------------- Expression ---------------------------------

bool Expression::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const Expression& rhs = dynamic_cast<const Expression&>(stmt); 
	return (*type == *rhs.type) && equalsExpr(rhs);
}

std::ostream& operator<<(std::ostream& out, const Expression& expression) {
	expression.printTo(out);
	return out;
}

// ------------------------------------- IntLiteral ---------------------------------

IntLiteral* IntLiteral::clone(StatementManager& manager) const {
	return new IntLiteral(manager.getTypeManager().get(type), value);
}

std::size_t IntLiteral::hash() const {
	size_t seed = HASHVAL_INTLITERAL;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, boost::hash_value(value));
	return seed;
}

IntLiteralPtr IntLiteral::get(StatementManager& manager, int value, unsigned short bytes) {
	return manager.get(IntLiteral(IntType::get(manager.getTypeManager(), bytes), value));
}

// ------------------------------------- FloatLiteral ---------------------------------

FloatLiteral* FloatLiteral::clone(StatementManager& manager) const {
	return new FloatLiteral(manager.getTypeManager().get(type), value, originalString);
}

void FloatLiteral::printTo(std::ostream& out) const {
	out << originalString;
}

std::size_t FloatLiteral::hash() const {
	size_t seed = HASHVAL_FLOATLITERAL;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, boost::hash_value(value));
	boost::hash_combine(seed, boost::hash_value(originalString));
	return seed;
}

FloatLiteralPtr FloatLiteral::get(StatementManager& manager, double value, unsigned short bytes) {
	return manager.get(FloatLiteral(FloatType::get(manager.getTypeManager(), bytes), value, toString(value)));
}

FloatLiteralPtr FloatLiteral::get(StatementManager& manager, const string& from, unsigned short bytes) {
	// TODO atof good idea? What about hex/octal/etc
	return manager.get(FloatLiteral(FloatType::get(manager.getTypeManager(), bytes), atof(from.c_str()), from));
}

// ------------------------------------- BoolLiteral ---------------------------------

BoolLiteral* BoolLiteral::clone(StatementManager& manager) const {
	return new BoolLiteral(manager.getTypeManager().get(type), value);
}
	
std::size_t BoolLiteral::hash() const {
	size_t seed = HASHVAL_BOOLLITERAL;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, boost::hash_value(value));
	return seed;
}

BoolLiteralPtr BoolLiteral::get(StatementManager& manager, bool value) {
	return manager.get(BoolLiteral(BoolType::get(manager.getTypeManager()), value));
}

// ------------------------------------- VarExpr ---------------------------------

VarExpr* VarExpr::clone(StatementManager& manager) const {
	return new VarExpr(manager.getTypeManager().get(type), id);
}

bool VarExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const VarExpr& rhs = dynamic_cast<const VarExpr&>(expr);
	return rhs.id == id;
}

void VarExpr::printTo(std::ostream& out) const {
	out << id;
}
	
std::size_t VarExpr::hash() const {
	size_t seed = HASHVAL_VAREXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

VarExprPtr VarExpr::get(StatementManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(VarExpr(type, id));
}

// ------------------------------------- ParamExpr ---------------------------------

ParamExpr* ParamExpr::clone(StatementManager& manager) const {
	return new ParamExpr(manager.getTypeManager().get(type), id);
}

void ParamExpr::printTo(std::ostream& out) const {
	out << *type << id;
}
	
std::size_t ParamExpr::hash() const {
	size_t seed = HASHVAL_PARAMEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

ParamExprPtr ParamExpr::get(StatementManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(ParamExpr(type, id));
}

// ------------------------------------- LambdaExpr ---------------------------------

LambdaExpr* LambdaExpr::clone(StatementManager& manager) const {
	return new LambdaExpr(manager.getTypeManager().get(type), manager.getAll(params), manager.get(body));
}

bool LambdaExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const LambdaExpr& rhs = dynamic_cast<const LambdaExpr&>(expr);
	return (*body == *rhs.body) && std::equal(params.cbegin(), params.cend(), rhs.params.cbegin(), equal_target<ExprPtr>());
}

void LambdaExpr::printTo(std::ostream& out) const {
	out << "lambda(";
	joinTo(out, ", ", params);
	out << ") { " << body << " }";
}

std::size_t LambdaExpr::hash() const {
	size_t seed = HASHVAL_LAMBDAEXPR;
	hashPtrRange(seed, params);
	return seed;
}

LambdaExprPtr LambdaExpr::get(StatementManager& manager, const TypePtr& type, const ParamList& params, const StmtPtr& body) {
	return manager.get(LambdaExpr(type, params, body));
}

// ------------------------------------- TupleExpr ---------------------------------

TupleExpr* TupleExpr::clone(StatementManager& manager) const {
	return new TupleExpr(manager.getTypeManager().get(type), manager.getAll(expressions));
}

bool TupleExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const TupleExpr& rhs = dynamic_cast<const TupleExpr&>(expr);
	return ::equals(expressions, rhs.expressions, equal_target<ExprPtr>());
}

void TupleExpr::printTo(std::ostream& out) const {
	out << "tuple(";
	joinTo(out, ", ", expressions);
	out << ")";
}

std::size_t TupleExpr::hash() const {
	size_t seed = HASHVAL_TUPLEEXPR;
	hashPtrRange(seed, expressions);
	return seed;
}

TupleExprPtr TupleExpr::get(StatementManager& manager, const vector<ExprPtr>& expressions) {
	TupleType::ElementTypeList elemTypes;
	std::transform(expressions.cbegin(), expressions.cend(), back_inserter(elemTypes), [](ExprPtr e) { return e->getType(); });
	return manager.get(TupleExpr(TupleType::get(manager.getTypeManager(), elemTypes), expressions));
}

// ------------------------------------- CallExpr ---------------------------------

CallExpr* CallExpr::clone(StatementManager& manager) const {
	return new CallExpr(manager.getTypeManager().get(type), manager.get(*functionExpr), manager.getAll(arguments));
}
	
bool CallExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CallExpr& rhs = dynamic_cast<const CallExpr&>(expr);
	return (*rhs.functionExpr == *functionExpr) && 
		::equals(arguments, rhs.arguments, equal_target<ExprPtr>());

}
	
void CallExpr::printTo(std::ostream& out) const {
	out << functionExpr << "(";
	joinTo(out, ", ", arguments);
	out << ")";
}

std::size_t CallExpr::hash() const {
	size_t seed = HASHVAL_CALLEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, functionExpr->hash());
	std::for_each(arguments.begin(), arguments.end(), [&seed](StmtPtr cur) { 
		boost::hash_combine(seed, cur->hash());
	} );
	return seed;
}

CallExprPtr CallExpr::get(StatementManager& manager, const TypePtr& type, const ExprPtr& functionExpr, const vector<ExprPtr>& arguments) {
	return manager.get(CallExpr(type, functionExpr, arguments));
}

// ------------------------------------- CastExpr ---------------------------------

CastExpr* CastExpr::clone(StatementManager& manager) const {
	return new CastExpr(manager.getTypeManager().get(type), manager.get(*subExpression));
}
	
bool CastExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CastExpr& rhs = dynamic_cast<const CastExpr&>(expr);
	return (*rhs.subExpression == *subExpression);
}
	
void CastExpr::printTo(std::ostream& out) const {
	out << "(" << type << ")" << "(" << subExpression <<")";
}

std::size_t CastExpr::hash() const {
	size_t seed = HASHVAL_CASTEXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, subExpression->hash());
	return seed;
}

CastExprPtr CastExpr::get(StatementManager& manager, const TypePtr& type, const ExprPtr& subExpr) {
	return manager.get(CastExpr(type, subExpr));
}

// ------------------------------------- ParenExpr ---------------------------------

//ParenExpr* ParenExpr::clone(StatementManager& manager) const {
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
//ParenExprPtr ParenExpr::get(StatementManager& manager, const ExprPtr& subExpr) {
//	return manager.get(ParenExpr(subExpr));
//}

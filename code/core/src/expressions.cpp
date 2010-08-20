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

// ------------------------------------- Expression ---------------------------------

bool Expression::equals(const Statement& stmt) const {
	// conversion is guaranteed by base operator==
	const Expression& rhs = dynamic_cast<const Expression&>(stmt); 
	return (type == rhs.type) && equalsExpr(rhs);
}

// ------------------------------------- IntLiteral ---------------------------------

IntLiteral* IntLiteral::clone(StatementManager& manager) const {
	return new IntLiteral(manager.getTypeManager().get(type), value, bytes);
}

std::size_t IntLiteral::hash() const {
	size_t seed = HASHVAL_INTLITERAL;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, boost::hash_value(value));
	return seed;
}

IntLiteralPtr IntLiteral::get(StatementManager& manager, int value, unsigned short bytes) {
	return manager.get(IntLiteral(IntType::get(manager.getTypeManager(), bytes), value, bytes));
}

// ------------------------------------- VariableExpr ---------------------------------

VariableExpr* VariableExpr::clone(StatementManager& manager) const {
	return new VariableExpr(manager.getTypeManager().get(type), id);
}

bool VariableExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const VariableExpr& rhs = dynamic_cast<const VariableExpr&>(expr);
	return rhs.id == id;
}

void VariableExpr::printTo(std::ostream& out) const {
	out << id;
}
	
std::size_t VariableExpr::hash() const {
	size_t seed = HASHVAL_VAREXPR;
	boost::hash_combine(seed, type->hash());
	boost::hash_combine(seed, id.hash());
	return seed;
}

VarExprPtr VariableExpr::get(StatementManager& manager, const TypePtr& type, const Identifier &id) {
	return manager.get(VariableExpr(type, id));
}

// ------------------------------------- CallExpr ---------------------------------

CallExpr* CallExpr::clone(StatementManager& manager) const {
	return new CallExpr(manager.getTypeManager().get(type), manager.get(*functionExpr), manager.getAll(arguments));
}
	
bool CallExpr::equalsExpr(const Expression& expr) const {
	// conversion is guaranteed by base operator==
	const CallExpr& rhs = dynamic_cast<const CallExpr&>(expr);
	return (rhs.functionExpr == functionExpr) && (rhs.arguments == arguments);
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


std::ostream& operator<<(std::ostream& out, const Expression& expression) {
	expression.printTo(out);
	return out;
}

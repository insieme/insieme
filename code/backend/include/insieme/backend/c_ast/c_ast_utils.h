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

#include <string>
#include <utility>
#include <vector>
#include <cassert>

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace backend {
namespace c_ast {

	class CBasics {

		CNodeManager& manager;

	public:

		CBasics(CNodeManager& manager) : manager(manager) {}

		IdentifierPtr getIdentifier(const string& name) {
			return manager.create(name);
		}

		TypePtr getIntType() {
			return manager.create<PrimitiveType>(PrimitiveType::INT);
		}

	};

	// --- types ------------------------------------------------

	inline PointerTypePtr ptr(const TypePtr& type) {
		return type->getManager()->create<c_ast::PointerType>(type);
	}

	// --- create literals and variables ------------------------

	inline VariablePtr var(const TypePtr& type, const IdentifierPtr& name) {
		assert(type && type->getManager() && name->getManager() && type->getManager() == name->getManager() && "Expected consistent managers!");
		CNodeManager* manager = type->getManager();
		return manager->create<Variable>(type, name);
	}

	inline VariablePtr var(const TypePtr& type, const string& name) {
		assert(type && type->getManager() && "Expected type to be present!");
		CNodeManager* manager = type->getManager();
		return var(type, manager->create(name));
	}

	inline LiteralPtr lit(const TypePtr& type, const string& value) {
		assert(type && type->getManager() && "Expected type to be present!");
		CNodeManager* manager = type->getManager();
		return manager->create<Literal>(value);
	}

	// --- create a pair of parentheses -------------------------

	inline ExpressionPtr parenthese(ExpressionPtr expr) {
		assert(expr && expr->getManager() && "There should be a manager!");
		switch(expr->getType()) {
		case NT_Parentheses:
		case NT_Variable:
		case NT_Identifier:
		case NT_Call:
			// those do not need (extra) parentheses
			return expr;
		default: {}
		}
		return expr->getManager()->create<Parentheses>(expr);
	}

	inline NodePtr parenthese(NodePtr node) {
		// only expressions need to be encapsulated using parentheses
		if (ExpressionPtr expr = dynamic_pointer_cast<const Expression>(node)) {
			return parenthese(expr);
		}
		// no parentheses required for the rest
		return node;
	}

	// --- all kind of overloaded operators ---

	template<typename ... E>
	inline CallPtr call(NodePtr fun, E ... args) {
		return fun->getManager()->create<c_ast::Call>(fun, args ...);
	}

	// -- Unary Operations --------------------------------------

	inline ExpressionPtr unaryOp(UnaryOperation::UnaryOp op, NodePtr a) {
		assert(a && a->getManager() && "There should be a manager!");
		return a->getManager()->create<UnaryOperation>(op, parenthese(a));
	}

	inline ExpressionPtr deref(IdentifierPtr expr) {
		return unaryOp(UnaryOperation::Indirection, expr);
	}

	inline ExpressionPtr deref(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::Indirection, expr);
	}

	inline ExpressionPtr ref(IdentifierPtr expr) {
		return unaryOp(UnaryOperation::Reference, expr);
	}

	inline ExpressionPtr ref(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::Reference, expr);
	}

	inline ExpressionPtr logicNot(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::LogicNot, expr);
	}

	inline ExpressionPtr bitwiseNot(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::BitwiseNot, expr);
	}

	inline ExpressionPtr preInc(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::PrefixInc, expr);
	}

	inline ExpressionPtr preDec(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::PrefixDec, expr);
	}

	inline ExpressionPtr postInc(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::PostfixInc, expr);
	}

	inline ExpressionPtr postDec(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::PostfixDec, expr);
	}

	inline ExpressionPtr sizeOf(NodePtr element) {
		return unaryOp(UnaryOperation::SizeOf, element);
	}

	// -- Binary Operations -------------------------------------

	inline ExpressionPtr binaryOp(BinaryOperation::BinaryOp op, NodePtr a, NodePtr b) {
		assert(a && b && a->getManager() && a->getManager() == b->getManager() && "Manager should match!");
		return a->getManager()->create<BinaryOperation>(op, parenthese(a), parenthese(b));
	}

	inline ExpressionPtr assign(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Assignment, a, b);
	}

	inline ExpressionPtr add(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Additon, a, b);
	}

	inline ExpressionPtr sub(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Subtraction, a, b);
	}

	inline ExpressionPtr mul(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Multiplication, a, b);
	}

	inline ExpressionPtr div(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Division, a, b);
	}

	inline ExpressionPtr mod(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Modulo, a, b);
	}

	inline ExpressionPtr logicAnd(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::LogicAnd, a, b);
	}

	inline ExpressionPtr logicOr(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::LogicOr, a, b);
	}

	inline ExpressionPtr bitwiseAnd(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::BitwiseAnd, a, b);
	}

	inline ExpressionPtr bitwiseOr(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::BitwiseOr, a, b);
	}

	inline ExpressionPtr bitwiseXor(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::BitwiseXOr, a, b);
	}

	inline ExpressionPtr lShift(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::BitwiseLeftShift, a, b);
	}

	inline ExpressionPtr rShift(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::BitwiseRightShift, a, b);
	}

	inline ExpressionPtr eq(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::Equal, a, b);
	}

	inline ExpressionPtr ne(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::NotEqual, a, b);
	}

	inline ExpressionPtr ge(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::GreaterOrEqual, a, b);
	}

	inline ExpressionPtr gt(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::GreaterThan, a, b);
	}

	inline ExpressionPtr le(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::LessOrEqual, a, b);
	}

	inline ExpressionPtr lt(ExpressionPtr a, ExpressionPtr b) {
		return binaryOp(BinaryOperation::LessThan, a, b);
	}

	inline ExpressionPtr cast(TypePtr type, NodePtr expr) {
		return binaryOp(BinaryOperation::Cast, type, expr);
	}

	inline ExpressionPtr access(ExpressionPtr expr, const string& element) {
		return binaryOp(BinaryOperation::MemberAccess, expr, expr->getManager()->create(element));
	}

	inline ExpressionPtr access(ExpressionPtr expr, NodePtr element) {
		return binaryOp(BinaryOperation::MemberAccess, expr, element);
	}

	inline ExpressionPtr indirectAccess(NodePtr expr, const string& element) {
		return binaryOp(BinaryOperation::IndirectMemberAccess, expr, expr->getManager()->create(element));
	}

	inline ExpressionPtr indirectAccess(NodePtr expr, IdentifierPtr element) {
		return binaryOp(BinaryOperation::IndirectMemberAccess, expr, element);
	}

	inline ExpressionPtr indirectAccess(NodePtr expr, VariablePtr element) {
		return binaryOp(BinaryOperation::IndirectMemberAccess, expr, element);
	}

	inline ExpressionPtr subscript(ExpressionPtr expr, NodePtr subscript) {
		return binaryOp(BinaryOperation::Subscript, expr, subscript);
	}

	template<typename ... E>
	inline InitializerPtr init(TypePtr type, E ... elements) {
		return type->getManager()->create<c_ast::Initializer>(type, toVector<c_ast::NodePtr>(elements...));
	}

	// -- Ternary Operations -------------------------------------

	inline ExpressionPtr ternaryOp(TernaryOperation::TernaryOp op, ExpressionPtr a, ExpressionPtr b, ExpressionPtr c) {
		assert(a && b && c && a->getManager() && a->getManager() == b->getManager() && a->getManager() == c->getManager() && "Manager should match!");
		return a->getManager()->create<TernaryOperation>(op, parenthese(a), parenthese(b), parenthese(c));
	}

	inline ExpressionPtr ite(ExpressionPtr condition, ExpressionPtr thenValue, ExpressionPtr elseValue) {
		return ternaryOp(TernaryOperation::TernaryCondition, condition, thenValue, elseValue);
	}

	// -- Statements -------------------------------------

	inline StatementPtr ret(StatementPtr stmt) {
		assert(dynamic_pointer_cast<Expression>(stmt) && "Handed in statement is not an expression!");
		return stmt->getManager()->create<Return>(static_pointer_cast<Expression>(stmt));
	}

	template<typename ... E>
	inline CompoundPtr compound(NodePtr first, E ... rest) {
		return first->getManager()->create<Compound>(first, rest...);
	}

	// -- Some tests ----------------------------------------

	inline bool isVoid(TypePtr type) {
		return type->getType() == NT_PrimitiveType && static_pointer_cast<const PrimitiveType>(type)->type == PrimitiveType::VOID;
	}

} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

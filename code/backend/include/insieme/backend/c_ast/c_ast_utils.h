/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/core/lang/reference.h"
#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace backend {
namespace c_ast {

// a switch to enable / disable improvements
#define MAKE_PRETTY(X)                                                                                                                                         \
	{ X }

	// -- Operator Precedence Handling ----------------------

	static inline unsigned getPriority(UnaryOperation::UnaryOp op) {
		switch(op) {
		case UnaryOperation::UnaryPlus: return 14;
		case UnaryOperation::UnaryMinus: return 14;
		case UnaryOperation::PrefixInc: return 14;
		case UnaryOperation::PrefixDec: return 14;
		case UnaryOperation::PostfixInc: return 15;
		case UnaryOperation::PostfixDec: return 15;
		case UnaryOperation::LogicNot: return 14;
		case UnaryOperation::BitwiseNot: return 14;
		case UnaryOperation::Indirection: return 14;
		case UnaryOperation::Reference: return 14;
		case UnaryOperation::SizeOf: return 14;
		case UnaryOperation::Typeid: return 14;
		case UnaryOperation::ComplexReal: return 14;
		case UnaryOperation::ComplexImag: return 14;
		case UnaryOperation::New: return 1;
		case UnaryOperation::Delete: return 1;
		case UnaryOperation::DeleteArray: return 1;
		}
		assert_fail() << "Uncovered operator encountered!";
		return 0;
	}

	static inline unsigned getPriority(BinaryOperation::BinaryOp op) {
		switch(op) {
		case BinaryOperation::Assignment: return 2;
		case BinaryOperation::Additon: return 12;
		case BinaryOperation::Subtraction: return 12;
		case BinaryOperation::Multiplication: return 13;
		case BinaryOperation::Division: return 13;
		case BinaryOperation::Modulo: return 13;
		case BinaryOperation::Equal: return 9;
		case BinaryOperation::NotEqual: return 9;
		case BinaryOperation::GreaterThan: return 10;
		case BinaryOperation::LessThan: return 10;
		case BinaryOperation::GreaterOrEqual: return 10;
		case BinaryOperation::LessOrEqual: return 10;
		case BinaryOperation::LogicAnd: return 5;
		case BinaryOperation::LogicOr: return 4;
		case BinaryOperation::BitwiseAnd: return 8;
		case BinaryOperation::BitwiseOr: return 6;
		case BinaryOperation::BitwiseXOr: return 7;
		case BinaryOperation::BitwiseLeftShift: return 11;
		case BinaryOperation::BitwiseRightShift: return 11;
		case BinaryOperation::AdditionAssign: return 2;
		case BinaryOperation::SubtractionAssign: return 2;
		case BinaryOperation::MultiplicationAssign: return 2;
		case BinaryOperation::DivisionAssign: return 2;
		case BinaryOperation::ModuloAssign: return 2;
		case BinaryOperation::BitwiseAndAssign: return 2;
		case BinaryOperation::BitwiseOrAssign: return 2;
		case BinaryOperation::BitwiseXOrAssign: return 2;
		case BinaryOperation::BitwiseLeftShiftAssign: return 2;
		case BinaryOperation::BitwiseRightShiftAssign: return 2;
		case BinaryOperation::MemberAccess: return 16;
		case BinaryOperation::IndirectMemberAccess: return 16;
		case BinaryOperation::Subscript: return 16;
		case BinaryOperation::Cast: return 15;
		case BinaryOperation::Comma: return 1;
		case BinaryOperation::StaticCast: return 15;
		case BinaryOperation::DynamicCast: return 15;
		case BinaryOperation::ScopeResolution: return 17;
		case BinaryOperation::PointerToMember: return 14;
		}
		assert_fail() << "Uncovered operator encountered!";
		return 0;
	}

	static inline unsigned getPriority(TernaryOperation::TernaryOp op) {
		switch(op) {
		case TernaryOperation::NewArray: return 1;
		case TernaryOperation::TernaryCondition: return 3;
		}
		assert_fail() << "Uncovered operator encountered!";
		return 0;
	}

	static inline unsigned getPriority(const NodePtr& node) {
		assert_true(node) << "Cannot obtain type of null-node!";

		// select priority based on operator
		NodeType type = node->getType();

		if(type == NT_UnaryOperation) { return getPriority(static_pointer_cast<const UnaryOperation>(node)->operation); }

		if(type == NT_BinaryOperation) { return getPriority(static_pointer_cast<const BinaryOperation>(node)->operation); }

		if(type == NT_TernaryOperation) { return getPriority(static_pointer_cast<const TernaryOperation>(node)->operation); }

		// for all the rest => priority is even higher
		return 16;
	}


	// --- types ------------------------------------------------

	inline PointerTypePtr ptr(const TypePtr& type, bool isConst = false, bool isVolatile = false) {
		return type->getManager()->create<c_ast::PointerType>(type, isConst, isVolatile);
	}

	inline ReferenceTypePtr ref(const TypePtr& type, bool isConst = false, bool isVolatile = false) {
		return type->getManager()->create<c_ast::ReferenceType>(type, isConst, isVolatile);
	}

	inline RValueReferenceTypePtr rvalue_ref(const TypePtr& type, bool isConst = false, bool isVolatile = false) {
		return type->getManager()->create<c_ast::RValueReferenceType>(type, isConst, isVolatile);
	}

	inline VectorTypePtr vec(const TypePtr& element) {
		return element->getManager()->create<c_ast::VectorType>(element);
	}

	inline VectorTypePtr vec(const TypePtr& element, const ExpressionPtr& size) {
		return element->getManager()->create<c_ast::VectorType>(element, size);
	}

	inline VectorTypePtr vec(const TypePtr& element, unsigned size) {
		return vec(element, element->getManager()->create<Literal>(toString(size)).as<ExpressionPtr>());
	}

	inline AttributedTypePtr attribute(const string& attribute, const TypePtr& type) {
		return type->getManager()->create<c_ast::AttributedType>(attribute, type);
	}

	inline TypePtr qualify(const TypePtr& type, bool isConst, bool isVolatile) {
		if (auto prim = type.isa<c_ast::PrimitiveTypePtr>()) {
			return type->getManager()->create<c_ast::PrimitiveType>(prim->type, isConst, isVolatile);
		}
		if (auto ptrt = type.isa<c_ast::PointerTypePtr>()) {
			return ptr(ptrt->elementType, isConst, isVolatile);
		}
		if (auto reft = type.isa<c_ast::ReferenceTypePtr>()) {
			return ref(reft->elementType, isConst, isVolatile);
		}
		if (auto reft = type.isa<c_ast::RValueReferenceTypePtr>()) {
			return rvalue_ref(reft->elementType, isConst, isVolatile);
		}
		if (auto namedType = type.isa<c_ast::NamedTypePtr>()) {
			auto res = type->getManager()->create<c_ast::NamedType>(namedType->name, isConst, isVolatile);
			res->isFunctionType = namedType->isFunctionType;
			res->parameters = namedType->parameters;
			return res;
		}
		if (auto compositeType = type.isa<c_ast::NamedCompositeTypePtr>()) {
			if (!isConst && !isVolatile) return compositeType;
			return type->getManager()->create<c_ast::NamedType>(compositeType->name, isConst, isVolatile);
		}
		if (auto vectorType = type.isa<c_ast::VectorTypePtr>()) {
			return vec(qualify(vectorType->elementType, isConst, isVolatile), vectorType->size);
		}
		if (auto attributedType = type.isa<c_ast::AttributedTypePtr>()) {
			return attribute(attributedType->attribute, qualify(attributedType->type, isConst, isVolatile));
		}
		assert_fail() << "Unsupported input type: " << type->getType() << "\n";
		return type;
	}

	inline FunctionTypePtr fun(const TypePtr& returnType, const vector<c_ast::TypePtr>& params) {
		return returnType->getManager()->create<c_ast::FunctionType>(returnType, TypePtr(), params);
	}

	inline FunctionTypePtr fun(const TypePtr& returnType, const TypePtr& classTy, const vector<c_ast::TypePtr>& params) {
		return returnType->getManager()->create<c_ast::FunctionType>(returnType, classTy, params);
	}

	template <typename... P>
	inline FunctionTypePtr fun(const TypePtr& returnType, P... params) {
		return fun(returnType, toVector<c_ast::TypePtr>(params...));
	}

	inline ComplexTypePtr complexType(const TypePtr& type) {
		return type->getManager()->create<c_ast::ComplexType>(type);
	}


	// --- create literals and variables ------------------------

	inline VariablePtr var(const TypePtr& type, const IdentifierPtr& name) {
		assert_true(type && type->getManager() && name->getManager() && type->getManager() == name->getManager()) << "Expected consistent managers!";
		CNodeManager* manager = type->getManager();
		return manager->create<Variable>(type, name);
	}

	inline VariablePtr var(const TypePtr& type, const string& name) {
		assert_true(type && type->getManager()) << "Expected type to be present!";
		CNodeManager* manager = type->getManager();
		return var(type, manager->create(name));
	}

	inline LiteralPtr lit(const TypePtr& type, const string& value) {
		assert_true(type && type->getManager()) << "Expected type to be present!";
		CNodeManager* manager = type->getManager();
		return manager->create<Literal>(value);
	}

	// --- create a pair of parentheses -------------------------

	inline ExpressionPtr parentheses(ExpressionPtr expr) {
		assert_true(expr && expr->getManager()) << "There should be a manager!";
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

	inline NodePtr parentheses(NodePtr node) {
		// only expressions need to be encapsulated using parentheses
		if(ExpressionPtr expr = dynamic_pointer_cast<Expression>(node)) { return parentheses(expr); }
		// no parentheses required for the rest
		return node;
	}

	inline NodePtr removeParentheses(NodePtr node) {
		if(node->getType() == NT_Parentheses) { return removeParentheses(static_pointer_cast<Parentheses>(node)->expression); }
		return node;
	}

	// --- all kind of overloaded operators ---

	template <typename... E>
	inline CallPtr call(NodePtr fun, E... args) {
		return fun->getManager()->create<c_ast::Call>(fun, args...);
	}

	inline CallPtr call(NodePtr fun, const vector<NodePtr>& args) {
		return fun->getManager()->create<c_ast::Call>(fun, args);
	}

	inline MemberCallPtr memberCall(NodePtr obj, NodePtr fun, const vector<NodePtr>& args = vector<NodePtr>(), const vector<TypePtr>& instantiationTypes = vector<TypePtr>()) {
		if(getPriority(obj) < 15) { obj = parentheses(obj); }
		return fun->getManager()->create<c_ast::MemberCall>(fun, obj, args, instantiationTypes);
	}

	inline ConstructorCallPtr ctorCall(TypePtr classType, const vector<NodePtr>& args = vector<NodePtr>(), ExpressionPtr location = ExpressionPtr()) {
		return classType->getManager()->create<c_ast::ConstructorCall>(classType, args, location);
	}

	inline DestructorCallPtr dtorCall(TypePtr classType, ExpressionPtr obj, bool isVirtual = true) {
		if(getPriority(obj) < 15) { obj = parentheses(obj); }
		return classType->getManager()->create<c_ast::DestructorCall>(classType, obj, isVirtual);
	}

	// -- Unary Operations --------------------------------------

	inline ExpressionPtr unaryOp(UnaryOperation::UnaryOp op, NodePtr a) {
		assert_true(a && a->getManager()) << "There should be a manager!";
		a = removeParentheses(a);
		a = (getPriority(a) < getPriority(op)) ? parentheses(a) : a;
		return a->getManager()->create<UnaryOperation>(op, a);
	}

	inline bool isUnaryOp(NodePtr candidate, UnaryOperation::UnaryOp op) {
		NodePtr cur = removeParentheses(candidate);
		if(cur->getType() == NT_UnaryOperation) { return static_pointer_cast<UnaryOperation>(cur)->operation == op; }
		return false;
	}

	inline ExpressionPtr deref(IdentifierPtr expr) {
		return unaryOp(UnaryOperation::Indirection, expr);
	}

	inline ExpressionPtr deref(ExpressionPtr expr) {
		MAKE_PRETTY(NodePtr sub = removeParentheses(expr); if(isUnaryOp(sub, UnaryOperation::Reference)) {
			return static_pointer_cast<Expression>(static_pointer_cast<UnaryOperation>(sub)->operand);
		});
		return unaryOp(UnaryOperation::Indirection, expr);
	}

	inline ExpressionPtr derefIfNotImplicit(const ExpressionPtr& cExpr, const core::ExpressionPtr& irExpr) {
		// deref of a cpp ref is implicit
		if(core::lang::isCppReference(irExpr) || core::lang::isCppRValueReference(irExpr)) {
			return cExpr;
		}
		return deref(cExpr);
	}

	inline ExpressionPtr ref(IdentifierPtr expr) {
		return unaryOp(UnaryOperation::Reference, expr);
	}

	inline ExpressionPtr ref(ExpressionPtr expr) {
		MAKE_PRETTY(NodePtr sub = removeParentheses(expr); if(isUnaryOp(sub, UnaryOperation::Indirection)) {
			return static_pointer_cast<Expression>(static_pointer_cast<UnaryOperation>(sub)->operand);
		});
		return unaryOp(UnaryOperation::Reference, expr);
	}

	inline ExpressionPtr minus(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::UnaryMinus, expr);
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

	inline ExpressionPtr newCall(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::New, expr);
	}

	inline ExpressionPtr deleteCall(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::Delete, expr);
	}

	inline ExpressionPtr deleteArrayCall(ExpressionPtr expr) {
		return unaryOp(UnaryOperation::DeleteArray, expr);
	}

	inline ExpressionPtr typeId(NodePtr element) {
		return unaryOp(UnaryOperation::Typeid, element);
	}

	inline ExpressionPtr complexReal(NodePtr element) {
		return unaryOp(UnaryOperation::ComplexReal, element);
	}

	inline ExpressionPtr complexImag(NodePtr element) {
		return unaryOp(UnaryOperation::ComplexImag, element);
	}

	// -- Binary Operations -------------------------------------

	inline ExpressionPtr binaryOp(BinaryOperation::BinaryOp op, NodePtr a, NodePtr b) {
		assert_true(a && b && a->getManager() && a->getManager() == b->getManager()) << "Manager should match!";
		unsigned priority = getPriority(op);
		// comparison can be less than, since all binary operators are left-associative
		a = removeParentheses(a);
		b = removeParentheses(b);
		a = (getPriority(a) < priority) ? parentheses(a) : a;
		b = (getPriority(b) <= priority && op != BinaryOperation::Subscript) ? parentheses(b) : b;
		return a->getManager()->create<BinaryOperation>(op, a, b);
	}

	inline bool isBinaryOp(NodePtr candidate, BinaryOperation::BinaryOp op) {
		NodePtr cur = removeParentheses(candidate);
		if(cur->getType() == NT_BinaryOperation) { return static_pointer_cast<BinaryOperation>(cur)->operation == op; }
		return false;
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

	inline ExpressionPtr staticCast(TypePtr type, NodePtr expr) {
		return binaryOp(BinaryOperation::StaticCast, type, expr);
	}

	inline ExpressionPtr dynamicCast(TypePtr type, NodePtr expr) {
		return binaryOp(BinaryOperation::DynamicCast, type, expr);
	}

	inline ExpressionPtr scope(NodePtr a, NodePtr b) {
		return binaryOp(BinaryOperation::ScopeResolution, a, b);
	}

	inline ExpressionPtr pointerToMember(NodePtr a, NodePtr b) {
		return binaryOp(BinaryOperation::PointerToMember, a, b);
	}

	inline ExpressionPtr access(ExpressionPtr expr, const string& element) {
		return binaryOp(BinaryOperation::MemberAccess, expr, expr->getManager()->create(element));
	}

	inline ExpressionPtr access(ExpressionPtr expr, NodePtr element) {
		// special handling for anonymous inner structs/unions
		if(auto lit = element.isa<c_ast::IdentifierPtr>()) {
			if(insieme::utils::demangle(lit->name).empty()) return expr;
		}
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
		MAKE_PRETTY(subscript = removeParentheses(subscript);
		            if(isBinaryOp(subscript, BinaryOperation::Cast)) { subscript = static_pointer_cast<BinaryOperation>(subscript)->operandB; });
		return binaryOp(BinaryOperation::Subscript, expr, subscript);
	}

	inline ExpressionPtr comma(StatementPtr a, StatementPtr b) {
		return binaryOp(BinaryOperation::Comma, a, b);
	}

	template <typename... T>
	inline ExpressionPtr comma(StatementPtr a, StatementPtr b, T... rest) {
		return comma(a, comma(b, rest...));
	}

	template <typename... E>
	inline InitializerPtr init(TypePtr type, E... elements) {
		return type->getManager()->create<c_ast::Initializer>(type, toVector<c_ast::NodePtr>(elements...));
	}

	inline InitializerPtr init(TypePtr type, const vector<c_ast::NodePtr>& elements) {
		return type->getManager()->create<c_ast::Initializer>(type, elements);
	}

	inline InitializerPtr init(const vector<c_ast::NodePtr>& elements) {
		assert_false(elements.empty());
		return elements[0]->getManager()->create<c_ast::Initializer>(elements);
	}

	inline DesignatedInitializerPtr init(TypePtr type, IdentifierPtr member, ExpressionPtr value) {
		return type->getManager()->create<c_ast::DesignatedInitializer>(type, member, value);
	}

	inline ArrayInitPtr initArray(TypePtr type, ExpressionPtr size) {
		return type->getManager()->create<c_ast::ArrayInit>(type, toVector(size));
	}

	inline ArrayInitPtr initArray(TypePtr type, ExpressionPtr sizeA, ExpressionPtr sizeB) {
		return type->getManager()->create<c_ast::ArrayInit>(type, toVector(sizeA, sizeB));
	}

	// -- Ternary Operations -------------------------------------

	inline ExpressionPtr ternaryOp(TernaryOperation::TernaryOp op, NodePtr a, NodePtr b, NodePtr c) {
		if(op == TernaryOperation::NewArray) return a->getManager()->create<TernaryOperation>(op, a, b, c);
		assert_true(a && b && c && a->getManager() && a->getManager() == b->getManager() && a->getManager() == c->getManager()) << "Manager should match!";
		a = (getPriority(a) <= getPriority(op)) ? parentheses(a) : a;
		b = (getPriority(b) <= getPriority(op)) ? parentheses(b) : b;
		c = (getPriority(c) <= getPriority(op)) ? parentheses(c) : c;
		return a->getManager()->create<TernaryOperation>(op, a, b, c);
	}

	inline ExpressionPtr ite(ExpressionPtr condition, ExpressionPtr thenValue, ExpressionPtr elseValue) {
		return ternaryOp(TernaryOperation::TernaryCondition, condition, thenValue, elseValue);
	}

	inline ExpressionPtr newArrayCall(NodePtr elem_type, NodePtr num_elems, NodePtr init) {
		return ternaryOp(TernaryOperation::NewArray, elem_type, num_elems, init);
	}

	// -- Statements -------------------------------------

	inline StatementPtr ret(StatementPtr stmt) {
		assert(dynamic_pointer_cast<Expression>(stmt) && "Handed in statement is not an expression!");
		return stmt->getManager()->create<Return>(static_pointer_cast<Expression>(stmt));
	}

	template <typename... E>
	inline CompoundPtr compound(NodePtr first, E... rest) {
		return first->getManager()->create<Compound>(first, rest...);
	}

	// -- Some tests ----------------------------------------

	inline bool isVoid(TypePtr type) {
		return type->getType() == NT_PrimitiveType && static_pointer_cast<const PrimitiveType>(type)->type == PrimitiveType::Void;
	}


} // end namespace c_ast
} // end namespace backend
} // end namespace insieme

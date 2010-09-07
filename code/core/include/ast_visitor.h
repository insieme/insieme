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
#include <cassert>

#include "annotated_ptr.h"
#include "definition.h"
#include "expressions.h"
#include "program.h"
#include "statements.h"
#include "types.h"
#include "types_utils.h"

namespace insieme {
namespace core {

template<typename Derived, typename ReturnType = void>
class ASTVisitor {

#define DISPATCH(CLASS, PTR) \
		static_cast<Derived*>(this)->dispatch ## CLASS(PTR)

#define VISIT(CLASS, PTR) \
		static_cast<Derived*>(this)->visit ## CLASS(PTR)

public:

	typedef ReturnType return_type;

	ReturnType visit(const NodePtr& node) {
		assert ( node && "Cannot visit NULL node!");
		return DISPATCH(Node, node);
	}

protected:

#define TRY_DISPATCH(PTR, CLASS) \
		if (dynamic_cast<const CLASS*>(&*PTR)) \
			return DISPATCH(CLASS, dynamic_pointer_cast<const CLASS>(PTR))

	// ---------------- Dispatcher ---------------------------------

	ReturnType dispatchNode(const NodePtr& node) {
		assert (node && "Cannot dispatch NULL node!");

		// dispatch node based on its type
		switch (node->getNodeType()) {
		case NodeType::SUPPORT:
			return dispatchSupport(node);
		case NodeType::TYPE:
			return dispatchType(dynamic_pointer_cast<const Type>(node));
		case NodeType::EXPRESSION:
			return dispatchExpression(dynamic_pointer_cast<const Expression>(node));
		case NodeType::STATEMENT:
			return dispatchStatement(dynamic_pointer_cast<const Statement>(node));
		case NodeType::DEFINITION:
			return dispatchDefinition(dynamic_pointer_cast<const Definition>(node));
		case NodeType::PROGRAM:
			return dispatchProgram(dynamic_pointer_cast<const Program>(node));
		}

		// fail => invalid node type!
		assert ( false && "Cannot dispatch unknown node type!" );
		return ReturnType();
	}

	ReturnType dispatchSupport(const NodePtr& node) {
		assert ( node && "Cannot dispatch NULL node!");

		TRY_DISPATCH(node, RecursiveTypeDefinition);

		assert( false && "Cannot dispatch unknown supportive AST node!");
		return ReturnType();
	}

	ReturnType dispatchStatement(const StatementPtr& statement) {
		assert ( statement && "Cannot dispatch NULL statement!");

		TRY_DISPATCH(statement, BreakStmt);
		TRY_DISPATCH(statement, ContinueStmt);
		TRY_DISPATCH(statement, ReturnStmt);

		TRY_DISPATCH(statement, DeclarationStmt);
		TRY_DISPATCH(statement, CompoundStmt);

		TRY_DISPATCH(statement, WhileStmt);
		TRY_DISPATCH(statement, ForStmt);
		TRY_DISPATCH(statement, IfStmt);
		TRY_DISPATCH(statement, SwitchStmt);

		assert ( false && "Cannot dispatch unknown statement pointer type." );
		return ReturnType();
	}

	ReturnType dispatchExpression(const ExpressionPtr& expression) {
		assert ( expression && "Cannot dispatch NULL expression!");

		TRY_DISPATCH(expression, IntLiteral);
		TRY_DISPATCH(expression, FloatLiteral);
		TRY_DISPATCH(expression, BoolLiteral);
		TRY_DISPATCH(expression, VarExpr);
		TRY_DISPATCH(expression, ParamExpr);
		TRY_DISPATCH(expression, LambdaExpr);
		TRY_DISPATCH(expression, CallExpr);
		TRY_DISPATCH(expression, CastExpr);

		assert ( false && "Cannot dispatch unknown expression pointer type." );
		return ReturnType();
	}



	ReturnType dispatchType(const TypePtr& type) {
		assert ( type && "Cannot dispatch NULL type!");

		TRY_DISPATCH(type, TypeVariable);
		TRY_DISPATCH(type, FunctionType);
		TRY_DISPATCH(type, TupleType);
		TRY_DISPATCH(type, GenericType);
		TRY_DISPATCH(type, RecursiveType);

		assert ( false && "Cannot dispatch unknown type pointer." );
		return ReturnType();
	}

	ReturnType dispatchGenericType(const GenericTypePtr& type) {
		assert ( type && "Cannot dispatch NULL pointer to type!");

		TRY_DISPATCH(type, ArrayType);
		TRY_DISPATCH(type, VectorType);
		TRY_DISPATCH(type, RefType);
		TRY_DISPATCH(type, ChannelType);

		TRY_DISPATCH(type, IntType);
		TRY_DISPATCH(type, FloatType);
		TRY_DISPATCH(type, BoolType);
		TRY_DISPATCH(type, UnitType);

		// just forward visit generic type
		return VISIT(GenericType, type);
	}

	ReturnType dispatchVarExpr(const VarExprPtr& expression) {
			assert ( expression && "Cannot dispatch NULL pointer!");

			// try only sub-type
			TRY_DISPATCH(expression, ParamExpr);

			// just forward visit generic type
			return VISIT(VarExpr, expression);
		}


#undef TRY_DISPATCH

	/**
	 * The following set of terminal dispatcher form the bridge between the dispatching
	 * and the visiting methods. Each of those terminals is just forwarding the dispatch
	 * request to the corresponding visitor method. Subclasses may overwrite their
	 * behavior to introduce further sub-types (e.g. a filtered set of generic types).
	 */

#define DISPATCH_TERMINAL(CLASS) \
	inline ReturnType dispatch ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot dispatch NULL pointer!"); \
		return VISIT(CLASS, ptr); \
	}

	DISPATCH_TERMINAL(Program);
	DISPATCH_TERMINAL(Definition);

	DISPATCH_TERMINAL(TypeVariable);
	DISPATCH_TERMINAL(FunctionType);
	DISPATCH_TERMINAL(TupleType);
	DISPATCH_TERMINAL(RecursiveType);

	DISPATCH_TERMINAL(ArrayType);
	DISPATCH_TERMINAL(VectorType);
	DISPATCH_TERMINAL(RefType);
	DISPATCH_TERMINAL(ChannelType);

	DISPATCH_TERMINAL(IntType);
	DISPATCH_TERMINAL(FloatType);
	DISPATCH_TERMINAL(BoolType);
	DISPATCH_TERMINAL(UnitType);

	DISPATCH_TERMINAL(StructType);
	DISPATCH_TERMINAL(UnionType);


	DISPATCH_TERMINAL(BreakStmt);
	DISPATCH_TERMINAL(ContinueStmt);
	DISPATCH_TERMINAL(ReturnStmt);
	DISPATCH_TERMINAL(DeclarationStmt);
	DISPATCH_TERMINAL(CompoundStmt);
	DISPATCH_TERMINAL(WhileStmt);
	DISPATCH_TERMINAL(ForStmt);
	DISPATCH_TERMINAL(IfStmt);
	DISPATCH_TERMINAL(SwitchStmt);

	DISPATCH_TERMINAL(IntLiteral);
	DISPATCH_TERMINAL(FloatLiteral);
	DISPATCH_TERMINAL(BoolLiteral);
	DISPATCH_TERMINAL(ParamExpr);
	DISPATCH_TERMINAL(LambdaExpr);
	DISPATCH_TERMINAL(CallExpr);
	DISPATCH_TERMINAL(CastExpr);

	DISPATCH_TERMINAL(RecursiveTypeDefinition);

#undef DISPATCH_TERMINAL
#undef VISIT
#undef DISPATCH

	// ------------------ protected visitor methods -----------------------

#define VISIT_NODE(CLASS, PARENT) \
	inline ReturnType visit ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot visit NULL pointer!"); \
		return static_cast<Derived*>(this)->visit ## PARENT(ptr); \
	}

	VISIT_NODE(Type, Node);

	VISIT_NODE(TypeVariable, Type);
	VISIT_NODE(FunctionType, Type);
	VISIT_NODE(TupleType, Type);
	VISIT_NODE(RecursiveType, Type);

	VISIT_NODE(GenericType, Type);
	VISIT_NODE(ArrayType, GenericType);
	VISIT_NODE(VectorType, GenericType);
	VISIT_NODE(RefType, GenericType);
	VISIT_NODE(ChannelType, GenericType);

	VISIT_NODE(IntType, GenericType);
	VISIT_NODE(FloatType, GenericType);
	VISIT_NODE(BoolType, GenericType);
	VISIT_NODE(UnitType, GenericType);

	VISIT_NODE(NamedCompositeType, Type);
	VISIT_NODE(StructType, NamedCompositeType);
	VISIT_NODE(UnionType, NamedCompositeType);

	VISIT_NODE(RecursiveTypeDefinition, Node);

	VISIT_NODE(Statement, Node);

	VISIT_NODE(BreakStmt, Statement);
	VISIT_NODE(ContinueStmt, Statement);
	VISIT_NODE(ReturnStmt, Statement);
	VISIT_NODE(DeclarationStmt, Statement);
	VISIT_NODE(CompoundStmt, Statement);
	VISIT_NODE(WhileStmt, Statement);
	VISIT_NODE(ForStmt, Statement);
	VISIT_NODE(IfStmt, Statement);
	VISIT_NODE(SwitchStmt, Statement);


	VISIT_NODE(Expression, Statement);

	VISIT_NODE(IntLiteral, Expression);
	VISIT_NODE(FloatLiteral, Expression);
	VISIT_NODE(BoolLiteral, Expression);
	VISIT_NODE(VarExpr, Expression);
	VISIT_NODE(ParamExpr, VarExpr);
	VISIT_NODE(LambdaExpr, Expression);
	VISIT_NODE(CallExpr, Expression);
	VISIT_NODE(CastExpr, Expression);


	VISIT_NODE(Program, Node);

	VISIT_NODE(Definition, Node);

	ReturnType visitNode(const NodePtr& node) {
		// by default, do nothing
		return ReturnType();
	}

#undef VISIT_NODE
};


template<typename T>
struct CurrentNodeOnly {
	inline T operator()(const NodePtr& currentNode, T& currentNodeReturn, std::vector<T>& subNodeResult){
		return currentNodeReturn;
	}
};


/**
 * The RecursiveProgramVisitor provides a wrapper around an ordinary visitor which
 * will recursively iterated depth first, pre-order through every visited node. Thereby,
 * within every node, the sub-visitor's visit method will be invoked. Further, the results
 * of the visited nodes may be combined using a generic result combinator.
 */
template<typename ReturnType, typename SubVisitor, typename ResultCombinator = CurrentNodeOnly<ReturnType>>
class RecursiveProgramVisitor : public ASTVisitor<RecursiveProgramVisitor<ReturnType, SubVisitor>, ReturnType> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	SubVisitor& subVisitor;

	/**
	 * The combinator used to combine the result value within complex nodes.
	 */
	ResultCombinator combinator;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	RecursiveProgramVisitor(SubVisitor& subVisitor) : subVisitor(subVisitor) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	ReturnType visitNode(const NodePtr& node) {

		// visit current
		ReturnType cur = subVisitor.visit(node);

		// recursively visit all sub-nodes
		const Node::ChildList& children = node->getChildList();
		std::vector<ReturnType> results;
		std::for_each(children.begin(), children.end(), [&](const NodePtr& cur) {
			results.push_back(this->visit(cur));
		});

		// finally, combine results
		return combinator(node, cur, results);
	}
};

} // end namespace core
} // end namespace insieme




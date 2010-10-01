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
#include <unordered_set>
#include <queue>

#include "annotated_ptr.h"
#include "expressions.h"
#include "program.h"
#include "statements.h"
#include "types.h"

namespace insieme {
namespace core {

template<typename ReturnType = void>
class ASTVisitor {

#define DISPATCH(CLASS, PTR) \
		dispatch ## CLASS(PTR)

#define VISIT(CLASS, PTR) \
		visit ## CLASS(PTR)

public:

	typedef ReturnType return_type;

	virtual ReturnType visit(const NodePtr& node) {
		assert ( node && "Cannot visit NULL node!");
		return DISPATCH(Node, node);
	}

protected:

#define TRY_DISPATCH(PTR, CLASS) \
		if (dynamic_cast<const CLASS*>(&*PTR)) \
			return DISPATCH(CLASS, dynamic_pointer_cast<const CLASS>(PTR))

	// ---------------- Dispatcher ---------------------------------

	virtual ReturnType dispatchNode(const NodePtr& node) {
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
		case NodeType::PROGRAM:
			return dispatchProgram(dynamic_pointer_cast<const Program>(node));
		}

		// fail => invalid node type!
		assert ( false && "Cannot dispatch unknown node type!" );
		return ReturnType();
	}

	virtual ReturnType dispatchSupport(const NodePtr& node) {
		assert ( node && "Cannot dispatch NULL node!");

		TRY_DISPATCH(node, RecTypeDefinition);
		TRY_DISPATCH(node, RecLambdaDefinition);

		assert( false && "Cannot dispatch unknown supportive AST node!");
		return ReturnType();
	}

	virtual ReturnType dispatchStatement(const StatementPtr& statement) {
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

	virtual ReturnType dispatchExpression(const ExpressionPtr& expression) {
		assert ( expression && "Cannot dispatch NULL expression!");

		TRY_DISPATCH(expression, Literal);
		TRY_DISPATCH(expression, VarExpr);
		TRY_DISPATCH(expression, ParamExpr);
		TRY_DISPATCH(expression, LambdaExpr);
		TRY_DISPATCH(expression, CallExpr);
		TRY_DISPATCH(expression, CastExpr);
		TRY_DISPATCH(expression, UnionExpr);
		TRY_DISPATCH(expression, StructExpr);
		TRY_DISPATCH(expression, JobExpr);

		assert ( false && "Cannot dispatch unknown expression pointer type." );
		return ReturnType();
	}



	virtual ReturnType dispatchType(const TypePtr& type) {
		assert ( type && "Cannot dispatch NULL type!");

		TRY_DISPATCH(type, TypeVariable);
		TRY_DISPATCH(type, FunctionType);
		TRY_DISPATCH(type, TupleType);
		TRY_DISPATCH(type, GenericType);
		TRY_DISPATCH(type, RecType);
		
		TRY_DISPATCH(type, StructType);
		TRY_DISPATCH(type, UnionType);

		assert ( false && "Cannot dispatch unknown type pointer." );
		return ReturnType();
	}

	virtual ReturnType dispatchGenericType(const GenericTypePtr& type) {
		assert ( type && "Cannot dispatch NULL pointer to type!");

		TRY_DISPATCH(type, ArrayType);
		TRY_DISPATCH(type, VectorType);
		TRY_DISPATCH(type, RefType);
		TRY_DISPATCH(type, ChannelType);

		// just forward visit generic type
		return VISIT(GenericType, type);
	}

	virtual ReturnType dispatchVarExpr(const VarExprPtr& expression) {
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
	inline virtual ReturnType dispatch ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot dispatch NULL pointer!"); \
		return VISIT(CLASS, ptr); \
	}

	DISPATCH_TERMINAL(Program);

	DISPATCH_TERMINAL(TypeVariable);
	DISPATCH_TERMINAL(FunctionType);
	DISPATCH_TERMINAL(TupleType);
	DISPATCH_TERMINAL(RecType);

	DISPATCH_TERMINAL(ArrayType);
	DISPATCH_TERMINAL(VectorType);
	DISPATCH_TERMINAL(RefType);
	DISPATCH_TERMINAL(ChannelType);

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

	DISPATCH_TERMINAL(Literal);
	DISPATCH_TERMINAL(ParamExpr);
	DISPATCH_TERMINAL(LambdaExpr);
	DISPATCH_TERMINAL(CallExpr);
	DISPATCH_TERMINAL(CastExpr);
	DISPATCH_TERMINAL(UnionExpr);
	DISPATCH_TERMINAL(StructExpr);
	DISPATCH_TERMINAL(JobExpr);

	DISPATCH_TERMINAL(RecTypeDefinition);
	DISPATCH_TERMINAL(RecLambdaDefinition);

#undef DISPATCH_TERMINAL
#undef VISIT
#undef DISPATCH

	// ------------------ protected visitor methods -----------------------

#define VISIT_NODE(CLASS, PARENT) \
	inline virtual ReturnType visit ## CLASS(const CLASS ## Ptr& ptr) { \
		assert ( !!ptr && "Cannot visit NULL pointer!"); \
		return visit ## PARENT(ptr); \
	}

	VISIT_NODE(Type, Node);

	VISIT_NODE(TypeVariable, Type);
	VISIT_NODE(FunctionType, Type);
	VISIT_NODE(TupleType, Type);
	VISIT_NODE(RecType, Type);

	VISIT_NODE(GenericType, Type);
	VISIT_NODE(ArrayType, GenericType);
	VISIT_NODE(VectorType, GenericType);
	VISIT_NODE(RefType, GenericType);
	VISIT_NODE(ChannelType, GenericType);

	VISIT_NODE(NamedCompositeType, Type);
	VISIT_NODE(StructType, NamedCompositeType);
	VISIT_NODE(UnionType, NamedCompositeType);

	VISIT_NODE(RecTypeDefinition, Node);
	VISIT_NODE(RecLambdaDefinition, Node);

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

	VISIT_NODE(Literal, Expression);
	VISIT_NODE(VarExpr, Expression);
	VISIT_NODE(ParamExpr, VarExpr);
	VISIT_NODE(LambdaExpr, Expression);
	VISIT_NODE(CallExpr, Expression);
	VISIT_NODE(CastExpr, Expression);
	VISIT_NODE(UnionExpr, Expression);
	VISIT_NODE(StructExpr, Expression);
	VISIT_NODE(JobExpr, Expression);

	VISIT_NODE(Program, Node);

	/**
	 * Implements a the base not visit.
	 */
	virtual ReturnType visitNode(const NodePtr& node) {
		// by default, do nothing
		return ReturnType();
	}

#undef VISIT_NODE
};

/**
 * TODO: comment
 */
template<typename Lambda, typename ResultType = void>
class LambdaASTVisitor : public ASTVisitor<ResultType> {

	/**
	 * The lambda to be applied to all nodes ...
	 */
	Lambda lambda;

public:

	/**
	 * Create a new visitor based on the given lambda.
	 */
	LambdaASTVisitor(Lambda& lambda) : lambda(lambda) {};

	/**
	 * Visits the given node and applies it to the maintained lambda.
	 */
	ResultType visitNode(const NodePtr& node) {
		// simply apply lambda ...
		return lambda(node);
	}
};


/**
 * The RecursiveProgramVisitor provides a wrapper around an ordinary visitor which
 * will recursively iterated depth first, pre-order through every visited node. Thereby,
 * within every node, the sub-visitor's visit method will be invoked. Further, the results
 * of the visited nodes may be combined using a generic result combinator.
 */
template<typename SubVisitor>
class RecursiveASTVisitor : public ASTVisitor<void> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	SubVisitor& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	RecursiveASTVisitor(SubVisitor& subVisitor, bool preorder = true) : subVisitor(subVisitor), preorder(preorder) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	void visitNode(const NodePtr& node) {

		// visit current (in case of a pre-order)
		if (preorder) {
			subVisitor.visit(node);
		}

		// recursively visit all sub-nodes
		const Node::ChildList& children = node->getChildList();
		std::for_each(children.begin(), children.end(), [&](const NodePtr& cur) {
			this->visit(cur);
		});

		// visit current (in case of a post-order)
		if (!preorder) {
			subVisitor.visit(node);
		}
	}
};

/**
 * The RecursiveProgramVisitor provides a wrapper around an ordinary visitor which
 * will recursively iterated depth first, pre-order through every visited node. Thereby,
 * within every node, the sub-visitor's visit method will be invoked. Further, the results
 * of the visited nodes may be combined using a generic result combinator.
 */
template<typename SubVisitor>
class BreadthFirstASTVisitor : public ASTVisitor<void> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	SubVisitor& subVisitor;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	BreadthFirstASTVisitor(SubVisitor& subVisitor) : subVisitor(subVisitor) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	void visitNode(const NodePtr& node) {

		std::queue<NodePtr> queue;

		ASTVisitor<void>* visitor;
		auto lambdaVisitor = makeLambdaASTVisitor([&queue, &visitor, this](const NodePtr& node) {

			// visit the current node
			this->subVisitor.visit(node);

			// add children of current node to the queue
			auto children = node->getChildList();
			std::for_each(children.begin(), children.end(), [&queue](const NodePtr& cur) {
				queue.push(cur);
			});

			// proceed with next node in the queue
			if (!queue.empty()) {
				NodePtr next = queue.front();
				queue.pop();
				visitor->visit(next);
			}
		});

		// update pointer ..
		visitor = &lambdaVisitor;

		// trigger the visit (only once)
		visitor->visit(node);
	}
};


/**
 * This visitor is visiting all nodes within the AST in a recursive manner. Thereby,
 * the
 */
template<typename SubVisitor = void>
class VisitOnceASTVisitor : public ASTVisitor<void> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	SubVisitor& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	VisitOnceASTVisitor(SubVisitor& subVisitor, bool preorder = true) : subVisitor(subVisitor), preorder(preorder) {};


	/**
	 * Visits
	 */
	virtual void visit(const NodePtr& node) {

		std::unordered_set<NodePtr, hash_target<NodePtr>, equal_target<NodePtr>> all;
		ASTVisitor<void>* visitor;
		auto lambdaVisitor = makeLambdaASTVisitor([&all, &visitor, this](const NodePtr& node) {
			// add current node to set ..
			bool isNew = all.insert(node).second;
			if (!isNew) {
				return;
			}

			if (this->preorder) {
				// visit current node
				this->subVisitor.visit(node);
			}

			// visit all child nodes recursively
			const Node::ChildList& children = node->getChildList();
			std::for_each(children.begin(), children.end(), [&](const NodePtr& cur) {
				visitor->visit(cur);
			});

			if (!this->preorder) {
				// visit current node
				this->subVisitor.visit(node);
			}
		});

		// update pointer ..
		visitor = &lambdaVisitor;

		// trigger the visit (only once)
		visitor->visit(node);
	}
};

/**
 * Creates a visitor where each node is passed as an argument to the given
 * lambda function.
 *
 * @param lambda the lambda function to which all visited nodes shell be passed.
 * @return the resulting visitor.
 */
template<typename Lambda>
inline LambdaASTVisitor<Lambda> makeLambdaASTVisitor(Lambda lambda) {
	return LambdaASTVisitor<Lambda>(lambda);
};

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 */
template<typename NodePtr, typename Visitor>
inline void visitAll(NodePtr& root, Visitor& visitor, bool preorder = true) {
	RecursiveASTVisitor<decltype(visitor)> recVisitor(visitor, preorder);
	recVisitor.visit(root);
}

/**
 * The given lambda is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param lambda the lambda to be applied to all the nodes
 */
template<typename NodePtr, typename Lambda>
inline void visitAllNodes(NodePtr& root, Lambda lambda, bool preorder = true) {
	auto visitor = makeLambdaASTVisitor(lambda);
	visitAll(root, visitor, preorder);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * only once.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 * @param preorder a flag indicating whether nodes should be visited in pre or post order
 */
template<typename NodePtr, typename Visitor>
inline void visitAllOnce(NodePtr& root, Visitor& visitor, bool preorder = true) {
	VisitOnceASTVisitor<decltype(visitor)> recVisitor(visitor, preorder);
	recVisitor.visit(root);
}

/**
 * The given lambda is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * only once.
 *
 * @param root the root not to start the visiting from
 * @param lambda the lambda to be applied to all the nodes
 * @param preorder a flag indicating whether nodes should be visited in pre or post order
 */
template<typename NodePtr, typename Lambda>
inline void visitAllNodesOnce(NodePtr& root, Lambda lambda, bool preorder = true) {
	auto visitor = makeLambdaASTVisitor(lambda);
	visitAllOnce(root, visitor, preorder);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 */
template<typename NodePtr, typename Visitor>
inline void visitAllBreadthFirst(NodePtr& root, Visitor& visitor) {
	BreadthFirstASTVisitor<decltype(visitor)> recVisitor(visitor);
	recVisitor.visit(root);
}

/**
 * The given lambda is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param lambda the lambda to be applied to all the nodes
 */
template<typename NodePtr, typename Lambda>
inline void visitAllNodesBreadthFirst(NodePtr& root, Lambda lambda) {
	auto visitor = makeLambdaASTVisitor(lambda);
	visitAllBreadthFirst(root, visitor);
}

} // end namespace core
} // end namespace insieme




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

#include "ast_address.h"

namespace insieme {
namespace core {

template<
	typename ReturnType = void,
	template<class Target> class Ptr = AnnotatedPtr,
	template<class T> class StaticCast = StaticAnnotatedPtrCast
>
class ASTVisitor {

public:

	typedef ReturnType return_type;

	/**
	 * Instructs this visitor to visit / process the given element.
	 *
	 * @param element the element to be visited / processed
	 * @return the result of the visiting process
	 */
	virtual ReturnType visit(const Ptr<const Node>& element) {
		assert ( element && "Cannot visit NULL element!");

		// dispatch to correct visit method
		switch(element->getNodeType()) {

			// Generate all cases using node definition file
			#define CONCRETE(name) \
				case NT_ ## name : \
					assert(dynamic_cast<const name*>(&*element) && "Type token NT_" #name " does not match type!"); \
					return visit ## name (*(StaticCast<const name>()(&element)));

					// take all nodes ...
					#include "ast_nodes.def"

			#undef CONCRETE

		}

		// fail => invalid node type!
		assert ( false && "Cannot dispatch unknown node type!" );
		return ReturnType();
	}

	// ------------------ protected visitor methods -----------------------

protected:

	/**
	 * By default, every visitXXX method is just forwarding the call to the visitYYY method
	 * where YYY is the direct parent class of XXX. Subclasses of this generic visitor may
	 * override selected visitZZZ methods to tap into the visit processing.
	 *
	 * The visitXXX() methods are protected to avoid externally from inadvertently invoking
	 * a specific visit method instead of the dispatching visit(...) method.
	 */
	#define IS_A(CLASS, PARENT) \
		inline virtual ReturnType visit ## CLASS(const Ptr<const CLASS>& ptr) { \
			assert ( !!ptr && "Cannot visit NULL pointer!"); \
			return visit ## PARENT(ptr); \
		}
		#include "ast_nodes.def"
	#undef IS_A

	/**
	 * Implements a the base not visit. In case none of the visitXXX methods along the forwarding
	 * chain have been overridden, this method will be reached. By default, it returns an instance
	 * of a default constructed element of the return type.
	 */
	virtual ReturnType visitNode(const Ptr<const Node>&) {
		// by default, do nothing
		return ReturnType();
	}

};


/**
 * A visitor implementation operating on node addresses instead of node
 * pointer.
 */
template<typename ReturnType = void>
class AddressVisitor : public ASTVisitor<ReturnType, Address, StaticAddressCast> { };

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
 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
 * 				   post order will be enforced.
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
 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
 * 				   post order will be enforced.
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




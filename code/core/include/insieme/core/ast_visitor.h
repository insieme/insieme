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

#include "insieme/utils/functional_utils.h"

#include "insieme/core/ast_pointer.h"
#include "insieme/core/ast_address.h"
#include "insieme/core/ast_node.h"
#include "insieme/core/expressions.h"
#include "insieme/core/program.h"
#include "insieme/core/statements.h"
#include "insieme/core/types.h"

// Once more, gcc sucks
#if defined WIN32
#define TEMP_OP operator()
#else
#define TEMP_OP template operator()
#endif

namespace insieme {
namespace core {

template<
	typename ReturnType = void,
	template<class Target> class Ptr = Pointer
>
class ASTVisitor {

public:

	/**
	 * A member type describing the return type of this visitor.
	 */
	typedef ReturnType return_type;

	/**
	 * Instructs this visitor to visit / process the given element.
	 *
	 * @param element the element to be visited / processed
	 * @return the result of the visiting process
	 */
	virtual ReturnType visit(const Ptr<const Node>& element) {
		assert ( element && "Cannot visit NULL element!");

		// create cast functor instance
		typename Ptr<const Node>::StaticCast cast;

		// dispatch to correct visit method
		switch(element->getNodeType()) {

			// Generate all cases using node definition file
			#define CONCRETE(name) \
				case NT_ ## name : \
					assert(dynamic_cast<const name*>(&*element) && "Type token NT_" #name " does not match type!"); \
					return visit ## name (cast.TEMP_OP<const name>(element));

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
	 * Implements a the base node visit. In case none of the visitXXX methods along the forwarding
	 * chain have been overridden, this method will be reached. By default, it returns an instance
	 * of a default constructed element of the return type.
	 */
	virtual ReturnType visitNode(const Ptr<const Node>&) {
		// by default, do nothing
		return ReturnType();
	}

};

#undef TEMP_OP

/**
 * A visitor implementation operating on node addresses instead of node
 * pointer.
 */
template<typename ReturnType = void>
class AddressVisitor : public ASTVisitor<ReturnType, Address> { };

/**
 * TODO: comment
 */
template<
	typename Lambda,
	typename ResultType = void,
	template<class Target> class Ptr = Pointer
>
class LambdaVisitor : public ASTVisitor<ResultType, Ptr> {

	/**
	 * The lambda to be applied to all nodes ...
	 */
	Lambda lambda;

public:

	/**
	 * Create a new visitor based on the given lambda.
	 * @param lambda the lambda to be applied on all identified nodes
	 */
	LambdaVisitor(Lambda& lambda) : lambda(lambda) {};

	/**
	 * Visits the given node and applies it to the maintained lambda.
	 */
	ResultType visitNode(const Ptr<const Node>& node) {
		// simply apply lambda ...
		return lambda(node);
	}
};

/**
 * Creates a visitor where each node is passed as an argument to the given
 * lambda function.
 *
 * @param lambda the lambda function to which all visited nodes shell be passed.
 * @return the resulting visitor.
 */
template<
	template<class Target> class Ptr,
	typename Lambda
>
inline LambdaVisitor<Lambda, typename lambda_traits<Lambda>::result_type, Ptr> makeLambdaVisitor(Lambda lambda) {
	return LambdaVisitor<Lambda, typename lambda_traits<Lambda>::result_type, Ptr>(lambda);
};

/**
 * Creates a visitor where each node is passed as an argument to the given
 * lambda function.
 *
 * @param lambda the lambda function to which all visited nodes shell be passed.
 * @return the resulting visitor.
 */
template<typename Lambda>
inline LambdaVisitor<Lambda, typename lambda_traits<Lambda>::result_type, Pointer> makeLambdaPtrVisitor(Lambda lambda) {
	return makeLambdaVisitor<Pointer, Lambda>(lambda);
};

/**
 * Creates a visitor where each node is passed as an argument to the given
 * lambda function via a address.
 *
 * @param lambda the lambda function to which all visited nodes shell be passed.
 * @return the resulting visitor.
 */
template<typename Lambda>
inline LambdaVisitor<Lambda, typename lambda_traits<Lambda>::result_type, Address> makeLambdaAddressVisitor(Lambda lambda) {
	return makeLambdaVisitor<Address, Lambda>(lambda);
};

/**
 * The RecursiveProgramVisitor provides a wrapper around an ordinary visitor which
 * will recursively iterated depth first, pre-order through every visited node. Thereby,
 * within every node, the sub-visitor's visit method will be invoked. Further, the results
 * of the visited nodes may be combined using a generic result combinator.
 */
template<
	typename SubVisitorResultType,
	template<class Target> class Ptr = Pointer
>
class RecursiveASTVisitor : public ASTVisitor<void, Ptr> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	ASTVisitor<SubVisitorResultType, Ptr>& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

	/**
	 * The child factory to be used to create pointer to child nodes.
	 */
	typename Ptr<const Node>::ChildFactory childFactory;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	RecursiveASTVisitor(ASTVisitor<SubVisitorResultType, Ptr>& subVisitor, bool preorder = true)
		: subVisitor(subVisitor), preorder(preorder) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	void visitNode(const Ptr<const Node>& node) {

		// visit current (in case of a pre-order)
		if (preorder) {
			subVisitor.visit(node);
		}

		// recursively visit all sub-nodes
		const Node::ChildList& children = node->getChildList();
		for(std::size_t i=0; i<children.size(); i++) {
			this->visit(childFactory(node, i));
		}

		// visit current (in case of a post-order)
		if (!preorder) {
			subVisitor.visit(node);
		}
	}
};

/**
 * A special variant of a recursive visitor which can be stopped during its recursive
 * processing of an IR graph. To interrupt the recursive visitor, the handed in sub-visitor
 * may simply return false. The result of the interrupted visitor is false if not interrupted,
 * true otherwise.
 */
template<
	template<class Target> class Ptr = Pointer
>
class RecursiveInterruptableASTVisitor : public ASTVisitor<bool, Ptr> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	ASTVisitor<bool, Ptr>& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

	/**
	 * The child factory to be used to create pointer to child nodes.
	 */
	typename Ptr<const Node>::ChildFactory childFactory;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	RecursiveInterruptableASTVisitor(ASTVisitor<bool, Ptr>& subVisitor, bool preorder = true)
		: subVisitor(subVisitor), preorder(preorder) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	bool visitNode(const Ptr<const Node>& node) {

		// init interruption state
		bool interrupted = false;

		// create and run the actual visitor visiting the node using a lambda
		ASTVisitor<bool, Ptr>* inner;
		auto innerVisitor = makeLambdaVisitor<Ptr>([&, this](const Ptr<const Node>& cur)->bool {
			// visit current (in case of a pre-order)
			if (this->preorder) {
				interrupted = interrupted || !this->subVisitor.visit(cur);
			}

			// recursively visit all sub-nodes
			const Node::ChildList& children = cur->getChildList();
			for(std::size_t i=0; i<children.size(); i++) {
				interrupted = interrupted || inner->visit(this->childFactory(cur, i));
			}

			// visit current (in case of a post-order)
			if (!this->preorder) {
				interrupted = interrupted || !this->subVisitor.visit(cur);
			}

			// return interruption state
			return interrupted;
		});

		// close recursive cycle
		inner = &innerVisitor;

		// trigger inner visitor
		innerVisitor.visit(node);

		// return result
		return interrupted;
	}
};



/**
 * The RecursiveProgramVisitor provides a wrapper around an ordinary visitor which
 * will recursively iterated depth first, pre-order through every visited node. Thereby,
 * within every node, the sub-visitor's visit method will be invoked. Further, the results
 * of the visited nodes may be combined using a generic result combinator.
 */
template<
	typename SubVisitorResultType,
	template<class Target> class Ptr = Pointer
>
class BreadthFirstASTVisitor : public ASTVisitor<void, Ptr> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	ASTVisitor<SubVisitorResultType, Ptr>& subVisitor;

	/**
	 * The child factory to be used to create pointer to child nodes.
	 */
	typename Ptr<const Node>::ChildFactory childFactory;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	BreadthFirstASTVisitor(ASTVisitor<SubVisitorResultType, Ptr>& subVisitor) : subVisitor(subVisitor) {};

	/**
	 * Visits the given node by recursively, depth-first, pre-order visiting of the entire
	 * subtree rooted at this node.
	 */
	void visitNode(const Ptr<const Node>& node) {

		std::queue<Ptr<const Node>> queue;

		ASTVisitor<void>* visitor;
		auto lambdaVisitor = makeLambdaVisitor<Ptr>([&queue, &visitor, this](const Ptr<const Node>& node) {

			// visit the current node
			this->subVisitor.visit(node);

			// add children of current node to the queue
			auto children = node->getChildList();
			for(std::size_t i = 0; i<children.size(); i++) {
				queue.push(this->childFactory(node, i));
			}

			// proceed with next node in the queue
			if (!queue.empty()) {
				Ptr<const Node> next = queue.front();
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
template<
	typename SubVisitorResultType,
	template<class Target> class Ptr = Pointer
>
class VisitOnceASTVisitor : public ASTVisitor<void, Ptr> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	ASTVisitor<SubVisitorResultType, Ptr>& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

	/**
	 * The child factory to be used to create pointer to child nodes.
	 */
	typename Ptr<const Node>::ChildFactory childFactory;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	VisitOnceASTVisitor(ASTVisitor<SubVisitorResultType, Ptr>& subVisitor, bool preorder = true) : subVisitor(subVisitor), preorder(preorder) {};


	/**
	 * The entry point for the visiting process.
	 */
	virtual void visit(const Ptr<const Node>& node) {

		std::unordered_set<Ptr<const Node>, hash_target<Ptr<const Node>>, equal_target<Ptr<const Node>>> all;
		ASTVisitor<void, Ptr>* visitor;
		auto lambdaVisitor = makeLambdaVisitor<Ptr>([&all, &visitor, this](const Ptr<const Node>& node) {
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
			for(std::size_t i = 0; i<children.size(); i++) {
				visitor->visit(this->childFactory(node, i));
			}

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
 * This visitor is visiting all nodes within the AST in a recursive manner. Thereby,
 * the
 */
template<
	template<class Target> class Ptr = Pointer
>
class VisitOnceInterruptableASTVisitor : public ASTVisitor<bool, Ptr> {

	/**
	 * The sub-visitor visiting all nodes recursively.
	 */
	ASTVisitor<bool, Ptr>& subVisitor;

	/**
	 * The order in which nodes are processed.
	 */
	bool preorder;

	/**
	 * The child factory to be used to create pointer to child nodes.
	 */
	typename Ptr<const Node>::ChildFactory childFactory;

public:

	/**
	 * Create a new visitor based on the given sub-visitor.
	 */
	VisitOnceInterruptableASTVisitor(ASTVisitor<bool, Ptr>& subVisitor, bool preorder = true)
		: subVisitor(subVisitor), preorder(preorder) {};


	/**
	 * The entry point for the visiting process.
	 */
	virtual bool visit(const Ptr<const Node>& node) {

		// init interrupt flag
		bool interrupted = false;

		std::unordered_set<Ptr<const Node>, hash_target<Ptr<const Node>>, equal_target<Ptr<const Node>>> all;
		ASTVisitor<void, Ptr>* visitor;
		auto lambdaVisitor = makeLambdaVisitor<Ptr>([&interrupted, &all, &visitor, this](const Ptr<const Node>& node) {

			// quick shortcut
			if (interrupted) {
				return;
			}

			// add current node to set ..
			bool isNew = all.insert(node).second;
			if (!isNew) {
				return;
			}

			if (this->preorder) {
				// visit current node
				interrupted = interrupted || !this->subVisitor.visit(node);
			}

			// visit all child nodes recursively
			const Node::ChildList& children = node->getChildList();
			for(std::size_t i = 0; i<children.size(); i++) {
				visitor->visit(this->childFactory(node, i));
			}

			if (!this->preorder) {
				// visit current node
				interrupted = interrupted || !this->subVisitor.visit(node);
			}
		});

		// update pointer ..
		visitor = &lambdaVisitor;

		// trigger the visit (only once)
		visitor->visit(node);

		return interrupted;
	}
};

/**
 * A factory method creating recursive visitors based on a predefined visitor.
 *
 * @param visitor the visitor to be based on
 * @param preorder allows to chose between pre- or postorder depth first visiting
 * @return a recursive visitor encapsulating the given visitor
 */
template<typename Result, template<class Target> class Ptr>
RecursiveASTVisitor<Result, Ptr> makeRecursiveVisitor(ASTVisitor<Result,Ptr>& visitor, bool preorder=true) {
	return RecursiveASTVisitor<Result, Ptr>(visitor, preorder);
}

/**
 * A factory method creating visit once visitor based on a predefined visitor.
 *
 * @param visitor the visitor to be based on
 * @return a recursive visitor encapsulating the given visitor
 */
template<template<class Target> class Ptr>
RecursiveInterruptableASTVisitor<Ptr> makeRecursiveInterruptableVisitor(ASTVisitor<bool,Ptr>& visitor, bool preorder=true) {
	return RecursiveInterruptableASTVisitor<Ptr>(visitor, preorder);
}

/**
 * A factory method creating breadth first visitor visitors based on a predefined visitor.
 *
 * @param visitor the visitor to be based on
 * @return a recursive visitor encapsulating the given visitor
 */
template<typename Result, template<class Target> class Ptr>
BreadthFirstASTVisitor<Result, Ptr> makeBreadthFirstVisitor(ASTVisitor<Result,Ptr>& visitor) {
	return BreadthFirstASTVisitor<Result, Ptr>(visitor);
}

/**
 * A factory method creating visit once visitor based on a predefined visitor.
 *
 * @param visitor the visitor to be based on
 * @return a recursive visitor encapsulating the given visitor
 */
template<typename Result, template<class Target> class Ptr>
VisitOnceASTVisitor<Result, Ptr> makeVisitOnceVisitor(ASTVisitor<Result,Ptr>& visitor, bool preorder=true) {
	return VisitOnceASTVisitor<Result, Ptr>(visitor, preorder);
}

/**
 * A factory method creating visit once visitor based on a predefined visitor.
 *
 * @param visitor the visitor to be based on
 * @return a recursive visitor encapsulating the given visitor
 */
template<template<class Target> class Ptr>
VisitOnceInterruptableASTVisitor<Ptr> makeVisitOnceInterruptableVisitor(ASTVisitor<bool,Ptr>& visitor, bool preorder=true) {
	return VisitOnceInterruptableASTVisitor<Ptr>(visitor, preorder);
}

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
template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAll(const Ptr<Node>& root, ASTVisitor<Result, Ptr>&& visitor, bool preorder = true) {
	makeRecursiveVisitor(visitor, preorder).visit(root);
}

// same as above, however it is accepting visitors by reference
template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAll(const Ptr<Node>& root, ASTVisitor<Result, Ptr>& visitor, bool preorder = true) {
	makeRecursiveVisitor(visitor, preorder).visit(root);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If the given visitor returns false, the visiting will be interrupted.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
 * 				   post order will be enforced.
 * @return returns true if interrupted, false otherwise
 */
template<typename Node, template<class Target> class Ptr>
inline bool visitAllInterruptable(const Ptr<Node>& root, ASTVisitor<bool, Ptr>&& visitor, bool preorder = true) {
	return makeRecursiveInterruptableVisitor(visitor, preorder).visit(root);
}

// same as above, however it is accepting visitors by reference
template<typename Node, template<class Target> class Ptr>
inline bool visitAllInterruptable(const Ptr<Node>& root, ASTVisitor<bool, Ptr>& visitor, bool preorder = true) {
	return makeRecursiveInterruptableVisitor(visitor, preorder).visit(root);
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
template<template<class Target> class Ptr, typename Node, typename Lambda>
inline void visitAllNodes(const Ptr<Node>& root, Lambda lambda, bool preorder = true) {
	auto visitor = makeLambdaVisitor<Ptr>(lambda);
	visitAll(root, visitor, preorder);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * only once.
 *
 * NOTE: if used based on Addresses, only the first address referencing a shared node
 * 		 is visited.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 * @param preorder a flag indicating whether nodes should be visited in pre or post order
 */
template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAllOnce(const Ptr<Node>& root, ASTVisitor<Result, Ptr>&& visitor, bool preorder = true) {
	makeVisitOnceVisitor(visitor, preorder).visit(root);
}

template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAllOnce(const Ptr<Node>& root, ASTVisitor<Result, Ptr>& visitor, bool preorder = true) {
	makeVisitOnceVisitor(visitor, preorder).visit(root);
}


/**
 * The given lambda is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * only once.
 *
 * NOTE: if used based on Addresses, only the first address referencing a shared node
 * 		 is visited.
 *
 * @param root the root not to start the visiting from
 * @param lambda the lambda to be applied to all the nodes
 * @param preorder a flag indicating whether nodes should be visited in pre or post order
 */
template<typename Node, typename Lambda, template<class Target> class Ptr>
inline void visitAllNodesOnce(const Ptr<Node>& root, Lambda lambda, bool preorder = true) {
	auto visitor = makeLambdaVisitor<Ptr>(lambda);
	visitAllOnce(root, visitor, preorder);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If the given visitor returns false, the visiting will be interrupted.
 *
 * NOTE: if used based on Addresses, only the first address referencing a shared node
 * 		 is visited.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
 * 				   post order will be enforced.
 */
template<typename Node, template<class Target> class Ptr>
inline bool visitAllOnceInterruptable(const Ptr<Node>& root, ASTVisitor<bool, Ptr>&& visitor, bool preorder = true) {
	return makeVisitOnceInterruptableVisitor(visitor, preorder).visit(root);
}

// same as above, however it is accepting visitors by reference
template<typename Node, template<class Target> class Ptr>
inline bool visitAllOnceInterruptable(const Ptr<Node>& root, ASTVisitor<bool, Ptr>& visitor, bool preorder = true) {
	return makeVisitOnceInterruptableVisitor(visitor, preorder).visit(root);
}

/**
 * The given visitor is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param visitor the visitor to be visiting all the nodes
 */
template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAllBreadthFirst(const Ptr<Node>& root, ASTVisitor<Result, Ptr>&& visitor) {
	makeBreadthFirstVisitor(visitor).visit(root);
}

template<typename Node, typename Result, template<class Target> class Ptr>
inline void visitAllBreadthFirst(const Ptr<Node>& root, ASTVisitor<Result, Ptr>& visitor) {
	makeBreadthFirstVisitor(visitor).visit(root);
}

/**
 * The given lambda is recursively applied to all nodes reachable starting from the
 * given root node. If nodes are shared within the AST, those nodes will be visited
 * multiple times.
 *
 * @param root the root not to start the visiting from
 * @param lambda the lambda to be applied to all the nodes
 */
template<typename Node, typename Lambda, template<class Target> class Ptr>
inline void visitAllNodesBreadthFirst(const Ptr<Node>& root, Lambda lambda) {
	auto visitor = makeLambdaVisitor<Ptr>(lambda);
	visitAllBreadthFirst(root, visitor);
}

} // end namespace core
} // end namespace insieme




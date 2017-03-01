/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#pragma once

#include <algorithm>
#include <cassert>
#include <unordered_set>
#include <queue>
#include <functional>

#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/is_polymorphic.hpp>

#include "insieme/utils/functional_utils.h"

#include "insieme/core/ir.h"


// syntax accepted by visual studio is not accepted by gcc
#if defined WIN32
#define TEMP_OP operator()
#else
#define TEMP_OP template operator()
#endif

namespace insieme {
namespace core {


	/**
	 * The following struct is used as a type trait to map input
	 * data to visitor instances to the actual output value type.
	 *
	 * In most cases, visitors will return always the same type of
	 * value like booleans, integers or void. However, in some cases
	 * visitors may return different results when visiting different
	 * node types. This struct enables the user of the visitor to
	 * describe this relation ship. By specializing the struct, new
	 * relations can be formed. By default, the standard return
	 * type is used.
	 *
	 * @tparam ReturnType the return type defined within the visitor
	 * @tparam Ptr the pointer type used by the visitor
	 * @tparam NodeType the node type to be visited
	 * @tparam Context the parameters passed to the visiting step
	 */
	template <typename ReturnType, template <class Target> class Ptr, typename NodeType, typename... Context>
	struct ir_visitor_return_type_trait {
		/**
		 * The type of the result to be returned when visiting a
		 * node instance of the given type.
		 */
		typedef ReturnType type;

		/**
		 * A operation describing the default conversion when obtaining
		 * a result for a child-node type by using the visitor function of
		 * the parent node.
		 */
		template <typename Child, typename Parent>
		ReturnType operator()(const ReturnType& value) const {
			return value;
		}
	};

	/**
	 * A specialization of this struct dealing with void-return values.
	 */
	template <template <class Target> class Ptr, typename NodeType, typename... Context>
	struct ir_visitor_return_type_trait<void, Ptr, NodeType, Context...> {
		/**
		 * The type of the result to be returned when visiting a
		 * node instance of the given type.
		 */
		typedef void type;
	};


	/**
	 * A flag-type to be used as the instantiate a visitor preserving the
	 * node type of the visited node. When visiting a CallExprPtr, the result
	 * will be a CallExprPtr. The same holds for addresses.
	 */
	struct preserve_node_type {};

	template <template <class Target> class Ptr, typename NodeType, typename... Context>
	struct ir_visitor_return_type_trait<preserve_node_type, Ptr, NodeType, Context...> {
		/**
		 * The return type specification.
		 */
		typedef Ptr<const NodeType> type;


		/**
		 * The implicit static cast when resolving nodes by using the visitor method of
		 * the parent type.
		 */
		template <typename Child, typename Parent>
		Ptr<const Child> operator()(const Ptr<const Parent>& ptr) const {
			// re-interpreting should be sufficient (consistency has to be ensured by developer)
			return ptr.template reinterpret<const Child>();
		}
	};

	namespace detail {

		/**
		 * The core of the visitor is implemented by the IRVisitorDispatcher struct. This
		 * struct is defining the virtual visitXY(..) methods to be implemented by sub-classes
		 * of the visitor class.
		 *
		 * Due to the restrictions imposed by the void return type, a partial template specialization
		 * of this class had been necessary. That's why this struct had to be separated from the
		 * basic IRVisitor class.
		 */
		template <typename Derived, typename ReturnType, template <class Target> class Ptr, typename... P>
		struct IRVisitorDispatcher {
			/**
			 * Supports the visiting of particular types of nodes, thereby obtaining node-type specific
			 * return values as they are defined by the ir_visitor_return_type_trait struct.
			 */
			template <typename T>
			typename ir_visitor_return_type_trait<ReturnType, Ptr, T, P...>::type visit(const Ptr<const T>& element, P... context) {
				typedef ir_visitor_return_type_trait<ReturnType, Ptr, T, P...> Converter;
				static const Converter converter = Converter();
				return converter.TEMP_OP<T, Node>(((Derived*)this)->visit(Ptr<const Node>(element), context...));
			}

		  protected:
			// ------------------ protected visitor methods -----------------------

			/**
			 * By default, every visitXXX method is just forwarding the call to the visitYYY method
			 * where YYY is the direct parent class of XXX. Subclasses of this generic visitor may
			 * override selected visitZZZ methods to tap into the visit processing.
			 *
			 * The visitXXX() methods are protected to avoid externally from inadvertently invoking
			 * a specific visit method instead of the dispatching visit(...) method.
			 */
			#define IS_A(CLASS, PARENT)                                                                                                                        \
				inline virtual                                                                                                                                 \
				    typename ir_visitor_return_type_trait<ReturnType, Ptr, CLASS, P...>::type visit##CLASS(const Ptr<const CLASS>& ptr, P... context) {        \
					typedef ir_visitor_return_type_trait<ReturnType, Ptr, CLASS, P...> Converter;                                                              \
					static const Converter converter = Converter();                                                                                            \
					assert_false(!ptr) << "Cannot visit NULL pointer!";                                                                                        \
					return converter.TEMP_OP<CLASS, PARENT>(this->visit##PARENT(ptr, context...));                                                             \
				}
			#include "ir_nodes.def"
			#undef IS_A

			/**
			 * Implements a the base node visit. In case none of the visitXXX methods along the forwarding
			 * chain have been overridden, this method will be reached. By default, it returns an instance
			 * of a default constructed element of the return type.
			 */
			virtual typename ir_visitor_return_type_trait<ReturnType, Ptr, Node, P...>::type visitNode(const Ptr<const Node>&, P... context) {
				typedef typename ir_visitor_return_type_trait<ReturnType, Ptr, Node, P...>::type ResType;

				// by default, do nothing
				return ResType();
			}
		};

		/**
		 * The same as above, just specialized for the void return type.
		 */
		template <typename Derived, template <class Target> class Ptr, typename... P>
		struct IRVisitorDispatcher<Derived, void, Ptr, P...> {
			/**
			 * Supports the visiting of particular types of nodes, thereby obtaining node-type specific
			 * return values as they are defined by the ir_visitor_return_type_trait struct.
			 */
			template <typename T>
			void visit(const Ptr<const T>& element, P... context) {
				((Derived*)this)->visit(Ptr<const Node>(element), context...);
			}

		  protected:
			// ------------------ protected visitor methods -----------------------

			#define IS_A(CLASS, PARENT)                                                                                                                        \
				inline virtual void visit##CLASS(const Ptr<const CLASS>& ptr, P... context) {                                                                  \
					assert_false(!ptr) << "Cannot visit NULL pointer!";                                                                                        \
					this->visit##PARENT(ptr, context...);                                                                                                      \
				}
			#include "ir_nodes.def"
			#undef IS_A

			virtual void visitNode(const Ptr<const Node>&, P... context) {
				return;
			}
		};
	}

	template <typename ReturnType = void, template <class Target> class Ptr = Pointer, typename... P>
	class IRVisitor : public detail::IRVisitorDispatcher<IRVisitor<ReturnType, Ptr, P...>, ReturnType, Ptr, P...> {
		/**
		 * The type of the dispatcher.
		 */
		typedef detail::IRVisitorDispatcher<IRVisitor<ReturnType, Ptr, P...>, ReturnType, Ptr, P...> Dispatcher;

		/**
		 * A flag allowing to prune all types from the tree going to be visited
		 * by this visitor. Since a large portion of all nodes are types, pruning those
		 * saves a considerable amount of time when iterating through a IR DAG.
		 */
		const bool visitTypeNodes;

		/**
		 * A functor used to perform static casts on the handled pointer type.
		 */
		typename Ptr<const Node>::StaticCast cast;

	  public:
		/**
		 * A member type describing the return type of this visitor.
		 */
		typedef ReturnType return_type;

		/**
		 * A constructor for this basic visitor implementation.
		 *
		 * @param a flag determining whether the resulting visitor will visit types or not.
		 */
		IRVisitor(bool visitTypes = false) : visitTypeNodes(visitTypes), cast() {}

		/**
		 * A virtual destructor to support handling proper sub-types.
		 */
		virtual ~IRVisitor(){};

		/**
		 * Instructs this visitor to visit / process the given element.
		 *
		 * @param element the element to be visited / processed
		 * @param p the context parameters for the visiting process
		 * @return the result of the visiting process
		 */
		virtual typename ir_visitor_return_type_trait<ReturnType, Ptr, Node, P...>::type visit(const Ptr<const Node>& element, P... context) {
			typedef typename ir_visitor_return_type_trait<ReturnType, Ptr, Node, P...>::type ResType;

			assert_true(element) << "Cannot visit NULL element!";

			// avoid visiting types if not necessary
			if(!visitTypeNodes && element->getNodeCategory() == NC_Type) { return ResType(); }

			// dispatch to correct visit method
			switch(element->getNodeType()) {
			// Generate all cases using node definition file
			#define CONCRETE(name)                                                                                                                             \
				case NT_##name:                                                                                                                                \
					assert_true(dynamic_cast<const name*>(&*element)) << "Type token NT_" #name " does not match type!";                                       \
					return this->visit##name(cast.TEMP_OP<const name>(element), context...);

			// take all nodes ...
			#include "ir_nodes.def"

			#undef CONCRETE
			}

			// fail => invalid node type!
			assert_fail() << "Cannot dispatch unknown node type!";
			return ResType();
		}

		template <typename T>
		typename ir_visitor_return_type_trait<ReturnType, Ptr, T, P...>::type visit(const Ptr<const T>& element, P... context) {
			return Dispatcher::visit(element, context...);
		}

		/**
		 * Instructs this visitor to visit / process every element of the given list.
		 *
		 * @param list the list of elements to be visited / processed
		 * @param context the context parameters for the visiting process
		 */
		virtual void visitAll(const vector<Ptr<const Node>>& list, P... context) {
			// just visit all nodes within the list
			for(auto cur : list) {
				visit(cur, context...);
			}
		}

		/**
		 * A generic variant of the visitAll method supporting abitrary node type lists.
		 *
		 * @param T the type of nodes stored insiede the list
		 * @param list the list of elements to be visited / processed
		 * @param context the context parameters for the visiting process
		 */
		template <typename T>
		void visitAllGen(const vector<Ptr<const T>>& list, P... context) {
			visitAll(convertList<Node>(list), context...);
		}

		/**
		 * An overload of the call operator redirected to the visit entry point.
		 */
		typename ir_visitor_return_type_trait<ReturnType, Ptr, Node, P...>::type operator()(const Ptr<const Node>& element, P... context) {
			return visit(element, context...);
		}

		/**
		 * Determines whether this visitor is visiting types or not.
		 */
		bool isVisitingTypes() const {
			return visitTypeNodes;
		}
	};


	/**
	 * A lambda visitor is a wrapper for visitors consisting of a single visiting callback-routine. The routine
	 * may accept a pointer to an arbitrary sub-type of the node hierarchy and will only be invoked for the corresponding
	 * type.
	 *
	 * The lambda visitor mainly aims on offering a light-weight mean to create a visitor within user code.
	 */
	template <typename Lambda, typename ResultType = void, template <class Target> class Ptr = Pointer, typename TargetType = Node,
	          typename Filter = AcceptAll<const Ptr<const Node>&>, typename... P>
	class LambdaVisitor : public IRVisitor<ResultType, Ptr, P...> {
		/**
		 * A functor used to perform dynamic casts on the handled pointer type.
		 */
		typename Ptr<const Node>::DynamicCast cast;

		/**
		 * Instantiate the filter to be applied before visiting a node using the
		 * given lambda visitor.
		 */
		Filter filter;

		/**
		 * The lambda to be applied to all nodes ...
		 */
		Lambda lambda;

	  public:
		/**
		 * Create a new visitor based on the given lambda.
		 * @param lambda the labmda to be applied on all adequate and accepted nodes
		 * @param visitTypes to determine whether types should be visited as well
		 */
		LambdaVisitor(Lambda& lambda, bool visitTypes) : IRVisitor<ResultType, Ptr, P...>(visitTypes), lambda(lambda){};

		/**
		 * Create a new visitor based on the given lambda.
		 * @param filter a filter allowing to filter out nodes not to be visited
		 * @param lambda the labmda to be applied on all adequate and accepted nodes
		 * @param visitTypes to determine whether types should be visited as well
		 */
		LambdaVisitor(Filter& filter, Lambda& lambda, bool visitTypes) : IRVisitor<ResultType, Ptr, P...>(visitTypes), filter(filter), lambda(lambda){};

		/**
		 * This method is overwritten since no dispatching has to be applied to nodes
		 * visited by the lambda visitor.
		 */
		inline virtual typename ir_visitor_return_type_trait<ResultType, Ptr, Node, P...>::type visit(const Ptr<const Node>& element, P... context) {
			return LambdaVisitor::visitNode(element, context...);
		}

		/**
		 * Visits the given node and applies it to the maintained lambda.
		 */
		inline typename ir_visitor_return_type_trait<ResultType, Ptr, Node, P...>::type visitNode(const Ptr<const Node>& node, P... context) {
			typedef typename ir_visitor_return_type_trait<ResultType, Ptr, Node, P...>::type ResType;

			// check whether current node is of correct type
			if(auto element = cast.TEMP_OP<const TargetType>(node)) {
				// check filter and ...
				if(filter(element)) {
					// ... forward call if matching.
					return lambda(element, context...);
				}
			}

			// the element type does not match => lambda invocation is skipped
			return ResType();
		}
	};

	#undef TEMP_OP

	namespace detail {

		/**
		 * A trait supporting the identification of the type of a lambda and the resulting
		 * visitor instance. The trait is only defined via its specializations.
		 */
		template <typename Filter, typename FilterFun, typename Lambda, typename LambdaFun>
		struct lambda_visitor_trait_helper;

		/**
		 * A specialization handling lambdas dealing with node pointers, addresses and other stuff.
		 */
		template <typename Filter, typename Lambda, typename R, typename C1, typename C2, typename T, typename... P, template <typename> class Ptr>
		struct lambda_visitor_trait_helper<Filter, bool (C1::*)(const Ptr<const T>&) const, Lambda, R (C2::*)(const Ptr<const T>&, P...) const> {
			typedef LambdaVisitor<Lambda, R, Ptr, T, Filter, P...> visitorType;
		};

		/**
		 * A trait deducing the type of the resulting lambda visitor based on a given lambda.
		 */
		template <typename Filter, typename Lambda>
		struct lambda_visitor_trait : public lambda_visitor_trait_helper<Filter, decltype(&Filter::operator()), Lambda, decltype(&Lambda::operator())> {};
	}


	/**
	 * Creates a visitor where each node is passed as an argument to the given
	 * lambda function.
	 *
	 * @param lambda the lambda function to which all visited nodes shell be passed.
	 * @param visitTypes a flag determine whether the resulting visitor is visiting types as well
	 * @return the resulting visitor.
	 */
	template <typename Lambda, typename Filter = AcceptAll<typename lambda_traits<Lambda>::arg1_type>,
	          typename R = typename detail::lambda_visitor_trait<Filter, Lambda>::visitorType>
	inline R makeLambdaVisitor(Lambda lambda, bool visitTypes = false) {
		return R(lambda, visitTypes);
	};

	/**
	 * Creates a visitor where each node is passed as an argument to the given
	 * filter and if accepted, to the given lambda function.
	 *
	 * @param filter the filter to be applied before visiting the nodes
	 * @param lambda the lambda function to which all visited nodes shell be passed.
	 * @param visitTypes a flag determine whether the resulting visitor is visiting types as well
	 * @return the resulting visitor.
	 */
	template <typename Filter, typename Lambda, typename Switch = typename boost::disable_if<boost::is_same<Lambda, bool>>::type,
	          typename R = typename detail::lambda_visitor_trait<Filter, Lambda>::visitorType>
	inline R makeLambdaVisitor(Filter filter, Lambda lambda, bool visitTypes = false) {
		return R(filter, lambda, visitTypes);
	};


	/**
	 * The DepthFirstProgramVisitor provides a wrapper around an ordinary visitor which
	 * will DepthFirstly iterated depth first, pre-order through every visited node. Thereby,
	 * within every node, the sub-visitor's visit method will be invoked. Further, the results
	 * of the visited nodes may be combined using a generic result combinator.
	 */
	template <typename SubVisitorResultType, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstIRVisitor : public IRVisitor<void, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor;

		/**
		 * The order in which nodes are processed.
		 */
		bool preorder;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstIRVisitor(IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor, bool preorder = true)
		    : IRVisitor<void, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor), preorder(preorder){};

		/**
		 * Visits the given node by DepthFirstly, depth-first, pre-order visiting of the entire
		 * subtree rooted at this node.
		 */
		void visitNode(const Ptr<const Node>& node, P... context) {
			// visit current (in case of a pre-order)
			if(preorder) { subVisitor.visit(node, context...); }

			// DepthFirstly visit all sub-nodes
			this->visitAll(node->getChildList(), context...);

			// visit current (in case of a post-order)
			if(!preorder) { subVisitor.visit(node, context...); }
		}
	};

	/**
	 * A special variant of a DepthFirst visitor which can be stopped during its DepthFirst
	 * processing of an IR graph. To interrupt the DepthFirst visitor, the handed in sub-visitor
	 * may simply return *true*. The result of the interrupted visitor is false if not interrupted,
	 * true otherwise.
	 */
	template <typename Action, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstInterruptibleIRVisitor : public IRVisitor<bool, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<Action, Ptr, P...>& subVisitor;

		/**
		 * The order in which nodes are processed.
		 */
		bool preorder;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstInterruptibleIRVisitor(IRVisitor<Action, Ptr, P...>& subVisitor, bool preorder = true)
		    : IRVisitor<bool, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor), preorder(preorder){};

		/**
		 * Visits the given node by DepthFirstly, depth-first, pre-order visiting of the entire
		 * subtree rooted at this node.
		 */
		bool visitNode(const Ptr<const Node>& node, P... context) {
			// init interruption state
			bool interrupted = false;

			// create and run the actual visitor visiting the node using a lambda
			IRVisitor<bool, Ptr>* inner;
			auto innerVisitor = makeLambdaVisitor([&, this](const Ptr<const Node>& cur, P... context) -> bool {
				// visit current (in case of a pre-order)
				if(this->preorder) { interrupted = interrupted || this->subVisitor.visit(cur, context...); }

				// DepthFirstly visit all sub-nodes
				const vector<Ptr<const Node>>& children = cur->getChildList();
				for(auto it = children.begin(); it != children.end(); ++it) {
					interrupted = interrupted || inner->visit(*it, context...);
				}

				// visit current (in case of a post-order)
				if(!this->preorder) { interrupted = interrupted || this->subVisitor.visit(cur, context...); }

				// return interruption state
				return interrupted;
			}, this->isVisitingTypes());

			// close DepthFirst cycle
			inner = &innerVisitor;

			// trigger inner visitor
			innerVisitor.visit(node);

			// return result
			return interrupted;
		}
	};

	/**
	 * The options for visitors passed to the pruning visitor wrapper to decide
	 * whether the visitor should decent into the subtree or prune the subtree from the visiting
	 * process.
	 */
	enum Action {
		Prune = true,
		Descent = false,
		Interrupt = true,
		Continue = false
	};

	/**
	 * The DepthFirstPrunableIRVisitor provides a wrapper around an ordinary visitor which
	 * will be iterated depth first, pre-order through every visited node.
	 * Within every node, the sub-visitor's visit method will be invoked. The result of the
	 * visit will be used to determine whether its child nodes should be visited.
	 */
	template <typename Action, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstPrunableIRVisitor : public IRVisitor<void, Ptr, P...> {

		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<Action, Ptr, P...>& subVisitor;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstPrunableIRVisitor(IRVisitor<Action, Ptr, P...>& subVisitor)
		    : IRVisitor<void, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor){};

		/**
		 * Visits the given node by DepthFirstly, depth-first, pre-order visiting of the entire
		 * subtree rooted at this node.
		 */
		void visitNode(const Ptr<const Node>& node, P... context) {
			// visit current node
			if((bool)subVisitor.visit(node, context...)) {
				// => visiting sub-nodes is not required
				return;
			}

			// DepthFirstly visit all sub-nodes
			this->visitAll(node->getChildList(), context...);
		}
	};


	/**
	 * The BreadthFirstIRVisitor provides a wrapper around an ordinary visitor which
	 * will iterate breadth first, pre-order through every visited node.
	 */
	template <typename SubVisitorResultType, template <class Target> class Ptr = Pointer, typename... P>
	class BreadthFirstIRVisitor : public IRVisitor<void, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		BreadthFirstIRVisitor(IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor)
		    : IRVisitor<void, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor){};

		/**
		 * Visits the given node by DepthFirstly, depth-first, pre-order visiting of the entire
		 * subtree rooted at this node.
		 */
		void visitNode(const Ptr<const Node>& node, P... context) {
			// create a worklist
			std::queue<Ptr<const Node>> queue;

			// start with this node
			queue.push(node);

			// process until the queue is empty
			while(!queue.empty()) {
				// get the current node
				auto cur = queue.front();
				queue.pop();

				// avoid visiting types if not necessary
				if(!this->isVisitingTypes() && cur->getNodeCategory() == NC_Type) { continue; }

				// visit current node
				subVisitor.visit(cur, context...);

				// add children of current node to the queue
				for(const auto& child : cur->getChildList()) {
					queue.push(child);
				}
			}
		}
	};

	/**
	 * The BreadthFirstInterruptibleIRVisitor provides a wrapper around an ordinary visitor which
	 * will iterate breadth first, pre-order through every visited node and can be interrupted at
	 * any point by the sub-visitor.
	 */
	template <typename Action, template <class Target> class Ptr = Pointer, typename... P>
	class BreadthFirstInterruptibleIRVisitor : public IRVisitor<bool, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<Action, Ptr, P...>& subVisitor;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		BreadthFirstInterruptibleIRVisitor(IRVisitor<Action, Ptr, P...>& subVisitor)
		    : IRVisitor<bool, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor){};

		bool visitNode(const Ptr<const Node>& node, P... context) {
			// init interrupt flag
			bool interrupted = false;
			std::queue<Ptr<const Node>> queue;

			IRVisitor<bool, Ptr>* visitor;
			auto lambdaVisitor = makeLambdaVisitor([&](const Ptr<const Node>& node, P... context) -> bool {

				// visit the current node
				interrupted = interrupted || this->subVisitor.visit(node, context...);

				if(!interrupted) {
					// add children of current node to the queue
					for_each(node->getChildList(), [&](const Ptr<const Node>& cur) { queue.push(cur); });

					// proceed with next node in the queue
					if(!queue.empty()) {
						Ptr<const Node> next = queue.front();
						queue.pop();
						visitor->visit(next);
					}
				}
				return interrupted;
			}, this->isVisitingTypes());

			// update pointer ..
			visitor = &lambdaVisitor;

			// trigger the visit (only once)
			visitor->visit(node);

			return interrupted;
		}
	};


	/**
	 * This visitor is visiting all nodes within the IR in a DepthFirst manner. Thereby,
	 * the
	 */
	template <typename SubVisitorResultType, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstOnceIRVisitor : public IRVisitor<void, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor;

		/**
		 * The order in which nodes are processed.
		 */
		bool preorder;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstOnceIRVisitor(IRVisitor<SubVisitorResultType, Ptr, P...>& subVisitor, bool preorder = true)
		    : IRVisitor<void, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor), preorder(preorder){};

		/**
		 * The entry point for the visiting process.
		 */
		virtual void visit(const Ptr<const Node>& node, P... context) {
			utils::set::PointerSet<Ptr<const Node>> all;
			IRVisitor<void, Ptr>* visitor;
			auto lambdaVisitor = makeLambdaVisitor([&](const Ptr<const Node>& node, P... context) {
				// add current node to set ..
				bool isNew = all.insert(node).second;
				if(!isNew) { return; }

				if(this->preorder) {
					// visit current node
					this->subVisitor.visit(node, context...);
				}

				// visit all child nodes DepthFirstly
				visitor->visitAll(node->getChildList(), context...);

				if(!this->preorder) {
					// visit current node
					this->subVisitor.visit(node, context...);
				}
			}, this->isVisitingTypes());

			// update pointer ..
			visitor = &lambdaVisitor;

			// trigger the visit (only once)
			visitor->visit(node);
		}
	};

	/**
	 * This visitor is visiting all nodes within the IR in a DepthFirst manner and can be aborted at any time.
	 */
	template <typename Action, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstOnceInterruptibleIRVisitor : public IRVisitor<bool, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<Action, Ptr, P...>& subVisitor;

		/**
		 * The order in which nodes are processed.
		 */
		bool preorder;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstOnceInterruptibleIRVisitor(IRVisitor<Action, Ptr, P...>& subVisitor, bool preorder = true)
		    : IRVisitor<bool, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor), preorder(preorder){};


		/**
		 * The entry point for the visiting process.
		 */
		virtual bool visit(const Ptr<const Node>& node, P... context) {
			// init interrupt flag
			bool interrupted = false;

			utils::set::PointerSet<Ptr<const Node>> all;
			IRVisitor<void, Ptr>* visitor;
			auto lambdaVisitor = makeLambdaVisitor([&](const Ptr<const Node>& node, P... context) {

				// quick shortcut
				if(interrupted) { return; }

				// add current node to set ..
				bool isNew = all.insert(node).second;
				if(!isNew) { return; }

				if(this->preorder) {
					// visit current node
					interrupted = interrupted || this->subVisitor.visit(node, context...);
				}

				// visit all child nodes DepthFirstly
				visitor->visitAll(node->getChildList(), context...);

				if(!this->preorder) {
					// visit current node
					interrupted = interrupted || this->subVisitor.visit(node, context...);
				}
			}, this->isVisitingTypes());

			// update pointer ..
			visitor = &lambdaVisitor;

			// trigger the visit (only once)
			visitor->visit(node);

			return interrupted;
		}
	};


	/**
	 * This visitor is visiting all nodes within the IR in a DepthFirst manner. Thereby,
	 * the DepthFirst visiting of sub-trees can be pruned at any time.
	 */
	template <typename Action, template <class Target> class Ptr = Pointer, typename... P>
	class DepthFirstOncePrunableIRVisitor : public IRVisitor<void, Ptr, P...> {
		/**
		 * The sub-visitor visiting all nodes DepthFirstly.
		 */
		IRVisitor<Action, Ptr, P...>& subVisitor;

	  public:
		/**
		 * Create a new visitor based on the given sub-visitor.
		 */
		DepthFirstOncePrunableIRVisitor(IRVisitor<Action, Ptr, P...>& subVisitor)
		    : IRVisitor<void, Ptr, P...>(subVisitor.isVisitingTypes()), subVisitor(subVisitor){};


		/**
		 * The entry point for the visiting process.
		 */
		virtual void visit(const Ptr<const Node>& node, P... context) {
			utils::set::PointerSet<Ptr<const Node>> all;
			IRVisitor<void, Ptr>* visitor;
			auto lambdaVisitor = makeLambdaVisitor([&](const Ptr<const Node>& node, P... context) {

				// add current node to set ..
				bool isNew = all.insert(node).second;
				if(!isNew) { return; }

				// visit current node
				if(this->subVisitor.visit(node, context...)) {
					// visitor decided not to visit child nodes
					return;
				}

				// visit all child nodes DepthFirstly
				visitor->visitAll(node->getChildList(), context...);

			}, this->isVisitingTypes());

			// update pointer ..
			visitor = &lambdaVisitor;

			// trigger the visit (only once)
			visitor->visit(node, context...);
		}
	};


	/**
	 * A factory method creating DepthFirst visitors based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @param preorder allows to chose between pre- or postorder depth first visiting
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <typename Result, template <class Target> class Ptr, typename... P>
	DepthFirstIRVisitor<Result, Ptr, P...> makeDepthFirstVisitor(IRVisitor<Result, Ptr, P...>& visitor, bool preorder = true) {
		return DepthFirstIRVisitor<Result, Ptr, P...>(visitor, preorder);
	}

	/**
	 * A factory method creating visit once visitor based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <typename Action, template <class Target> class Ptr, typename... P>
	DepthFirstInterruptibleIRVisitor<Action, Ptr, P...> makeDepthFirstInterruptibleVisitor(IRVisitor<Action, Ptr, P...>& visitor, bool preorder = true) {
		return DepthFirstInterruptibleIRVisitor<Action, Ptr, P...>(visitor, preorder);
	}

	/**
	 * A factory method creating a DepthFirst visitor capable of Pruning the iteration space.
	 *
	 * @param visitor the visitor to be based on
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <typename Action, template <class Target> class Ptr, typename... P>
	DepthFirstPrunableIRVisitor<Action, Ptr, P...> makeDepthFirstPrunableVisitor(IRVisitor<Action, Ptr, P...>& visitor) {
		return DepthFirstPrunableIRVisitor<Action, Ptr, P...>(visitor);
	}

	/**
	 * A factory method creating breadth first visitor visitors based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a BreadthFirst visitor encapsulating the given visitor
	 */
	template <typename Result, template <class Target> class Ptr, typename... P>
	BreadthFirstIRVisitor<Result, Ptr, P...> makeBreadthFirstVisitor(IRVisitor<Result, Ptr, P...>& visitor) {
		return BreadthFirstIRVisitor<Result, Ptr, P...>(visitor);
	}

	/**
	 * A factory method creating breadth first interruptible visitor visitors based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a BreadthFirstInterruptible visitor encapsulating the given visitor
	 */
	template <typename Action, template <class Target> class Ptr, typename... P>
	BreadthFirstInterruptibleIRVisitor<Action, Ptr, P...> makeBreadthFirstInterruptibleVisitor(IRVisitor<Action, Ptr, P...>& visitor) {
		return BreadthFirstInterruptibleIRVisitor<Action, Ptr, P...>(visitor);
	}

	/**
	 * A factory method creating visit once visitor based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <typename Result, template <class Target> class Ptr, typename... P>
	DepthFirstOnceIRVisitor<Result, Ptr, P...> makeDepthFirstOnceVisitor(IRVisitor<Result, Ptr, P...>& visitor, bool preorder = true) {
		return DepthFirstOnceIRVisitor<Result, Ptr, P...>(visitor, preorder);
	}

	/**
	 * A factory method creating visit once visitor based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <template <class Target> class Ptr, typename Action, typename... P>
	DepthFirstOnceInterruptibleIRVisitor<Action, Ptr, P...> makeDepthFirstOnceInterruptibleVisitor(IRVisitor<Action, Ptr, P...>& visitor, bool preorder = true) {
		return DepthFirstOnceInterruptibleIRVisitor<Action, Ptr, P...>(visitor, preorder);
	}

	/**
	 * A factory method creating visit once visitor based on a predefined visitor.
	 *
	 * @param visitor the visitor to be based on
	 * @return a DepthFirst visitor encapsulating the given visitor
	 */
	template <template <class Target> class Ptr, typename Action, typename... P>
	DepthFirstOncePrunableIRVisitor<Action, Ptr, P...> makeDepthFirstOncePrunableVisitor(IRVisitor<Action, Ptr, P...>& visitor) {
		return DepthFirstOncePrunableIRVisitor<Action, Ptr, P...>(visitor);
	}


	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If nodes are shared within the IR, those nodes will be visited
	 * multiple times.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
	 * 				   post order will be enforced.
	 */
	template <typename Node, typename Result, template <class Target> class Ptr>
	inline void visitDepthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr>& visitor, bool preorder = true) {
		makeDepthFirstVisitor(visitor, preorder).visit(root);
	}

	// same as above, however it is accepting visitors by r-value reference
	template <typename Node, typename Result, template <class Target> class Ptr>
	inline void visitDepthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr>&& visitor, bool preorder = true) {
		makeDepthFirstVisitor(visitor, preorder).visit(root);
	}

	template <typename Node, typename Result, template <class Target> class Ptr, typename H, typename... P>
	inline void visitDepthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr, H, P...>& visitor, bool preorder, H first, P... rest) {
		makeDepthFirstVisitor(visitor, preorder).visit(root, first, rest...);
	}

	template <typename Node, typename Result, template <class Target> class Ptr, typename H, typename... P>
	inline void visitDepthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr, H, P...>&& visitor, bool preorder, H first, P... rest) {
		makeDepthFirstVisitor(visitor, preorder).visit(root, first, rest...);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline void visitDepthFirst(const Ptr<Node>& root, Lambda lambda, bool preorder = true, bool visitTypes = false) {
		visitDepthFirst(root, makeLambdaVisitor(lambda, visitTypes), preorder);
	}

	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If the given visitor returns false, the visiting will be interrupted.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
	 * 				   post order will be enforced.
	 * @return returns true if interrupted, false otherwise
	 */
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline bool visitDepthFirstInterruptible(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>&& visitor, bool preorder = true) {
		return makeDepthFirstInterruptibleVisitor(visitor, preorder).visit(root);
	}

	// same as above, however it is accepting visitors by reference
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline bool visitDepthFirstInterruptible(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>& visitor, bool preorder = true) {
		return makeDepthFirstInterruptibleVisitor(visitor, preorder).visit(root);
	}


	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline bool visitDepthFirstInterruptible(const Ptr<Node>& root, Lambda lambda, bool preorder = true, bool visitTypes = false) {
		return visitDepthFirstInterruptible(root, makeLambdaVisitor(lambda, visitTypes), preorder);
	}


	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If the given visitor returns true, the corresponding sub-tree will be pruned.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 */
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstPrunable(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>&& visitor) {
		makeDepthFirstPrunableVisitor(visitor).visit(root);
	}

	// same as above, however it is accepting visitors by reference
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstPrunable(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>& visitor) {
		makeDepthFirstPrunableVisitor(visitor).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline void visitDepthFirstPrunable(const Ptr<Node>& root, Lambda lambda, bool visitTypes = false) {
		visitDepthFirstPrunable(root, makeLambdaVisitor(lambda, visitTypes));
	}

	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If nodes are shared within the IR, those nodes will be visited
	 * only once.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 * @param preorder a flag indicating whether nodes should be visited in pre or post order
	 */
	template <typename Node, typename Result, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstOnce(const Ptr<Node>& root, IRVisitor<Result, Ptr, P...>&& visitor, bool preorder = true) {
		makeDepthFirstOnceVisitor(visitor, preorder).visit(root);
	}

	template <typename Node, typename Result, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstOnce(const Ptr<Node>& root, IRVisitor<Result, Ptr, P...>& visitor, bool preorder = true) {
		makeDepthFirstOnceVisitor(visitor, preorder).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline void visitDepthFirstOnce(const Ptr<Node>& root, Lambda lambda, bool preorder = true, bool visitTypes = false) {
		visitDepthFirstOnce(root, makeLambdaVisitor(lambda, visitTypes), preorder);
	}


	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If the given visitor returns true, the visiting will be interrupted.
	 *
	 * NOTE: if used based on Addresses, only the first address referencing a shared node
	 * 		 is visited.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 * @param preorder if set to true, nodes will be visited in preorder (parent node first), otherwise
	 * 				   post order will be enforced.
	 */
	template <typename Node, template <class Target> class Ptr, typename... P>
	inline bool visitDepthFirstOnceInterruptible(const Ptr<Node>& root, IRVisitor<bool, Ptr, P...>&& visitor, bool preorder = true) {
		return makeDepthFirstOnceInterruptibleVisitor(visitor, preorder).visit(root);
	}

	// same as above, however it is accepting visitors by reference
	template <typename Node, template <class Target> class Ptr, typename... P>
	inline bool visitDepthFirstOnceInterruptible(const Ptr<Node>& root, IRVisitor<bool, Ptr, P...>& visitor, bool preorder = true) {
		return makeDepthFirstOnceInterruptibleVisitor(visitor, preorder).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline bool visitDepthFirstOnceInterruptible(const Ptr<Node>& root, Lambda lambda, bool preorder = true, bool visitTypes = false) {
		return visitDepthFirstOnceInterruptible(root, makeLambdaVisitor(lambda, visitTypes), preorder);
	}

	/**
	 * The given visitor is DepthFirstly applied to all nodes reachable starting from the
	 * given root node. If the given visitor returns true, the corresponding sub-tree will be pruned.
	 *
	 * NOTE: if used based on Addresses, only the first address referencing a shared node
	 * 		 is visited.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 */
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstOncePrunable(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>&& visitor) {
		makeDepthFirstOncePrunableVisitor(visitor).visit(root);
	}

	// same as above, however it is accepting visitors by reference
	template <typename Node, typename Action, template <class Target> class Ptr, typename... P>
	inline void visitDepthFirstOncePrunable(const Ptr<Node>& root, IRVisitor<Action, Ptr, P...>& visitor) {
		makeDepthFirstOncePrunableVisitor(visitor).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline void visitDepthFirstOncePrunable(const Ptr<Node>& root, Lambda lambda, bool visitTypes = false) {
		return visitDepthFirstOncePrunable(root, makeLambdaVisitor(lambda, visitTypes));
	}

	/**
	 * The given visitor is BreadthFirstly applied to all nodes reachable starting from the
	 * given root node. If nodes are shared within the AST, those nodes will be visited
	 * multiple times.
	 *
	 * @param root the root not to start the visiting from
	 * @param visitor the visitor to be visiting all the nodes
	 */
	template <typename Node, typename Result, template <class Target> class Ptr, typename... P>
	inline void visitBreadthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr, P...>&& visitor) {
		makeBreadthFirstVisitor(visitor).visit(root);
	}

	template <typename Node, typename Result, template <class Target> class Ptr, typename... P>
	inline void visitBreadthFirst(const Ptr<Node>& root, IRVisitor<Result, Ptr, P...>& visitor) {
		makeBreadthFirstVisitor(visitor).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline void visitBreadthFirst(const Ptr<Node>& root, Lambda lambda, bool visitTypes = false) {
		visitBreadthFirst(root, makeLambdaVisitor(lambda, visitTypes));
	}

	template <typename Node, template <class Target> class Ptr, typename... P>
	inline bool visitBreadthFirstInterruptible(const Ptr<Node>& root, IRVisitor<bool, Ptr, P...>&& visitor) {
		return makeBreadthFirstInterruptibleVisitor(visitor).visit(root);
	}

	template <typename Node, template <class Target> class Ptr, typename... P>
	inline bool visitBreadthFirstInterruptible(const Ptr<Node>& root, IRVisitor<bool, Ptr, P...>& visitor) {
		return makeBreadthFirstInterruptibleVisitor(visitor).visit(root);
	}

	template <template <class Target> class Ptr, typename Node, typename Lambda,
	          typename Enable = typename boost::disable_if<boost::is_polymorphic<Lambda>, void>::type>
	inline bool visitBreadthFirstInterruptible(const Ptr<Node>& root, Lambda lambda, bool visitTypes = false) {
		return visitBreadthFirstInterruptible(root, makeLambdaVisitor(lambda, visitTypes));
	}

} // end namespace core
} // end namespace insieme

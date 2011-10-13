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

#include <vector>

#include <boost/variant.hpp>
#include <boost/utility.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/function_types/result_type.hpp>

#include "insieme/utils/annotation.h"
#include "insieme/utils/hash_utils.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/pointer.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace core {

	using std::vector;
	using std::pair;

	class Node;
	typedef Ptr<Node> NodePtr;

	typedef vector<NodePtr> NodeList;

	template<typename T> class Adr;


	template<
		typename T = Node, typename S,
		typename boost::enable_if<boost::is_base_of<T,S>,int>::type = 0
	>
	const vector<Ptr<T>>& toNodeList(const vector<Ptr<S>>& list) {
		// use a C-like cast since structurally the data is correct
		return (const vector<Ptr<T>>&)list;
	}

	class NodeAnnotation : public utils::Annotation {};

	enum NodeType {
		NT_TupleType = 5, NT_Literal, NT_If, NT_Compound
	};


	namespace detail {


		inline void hash_combine(std::size_t& seed) { }

		template<typename ... Rest>
		inline void hash_combine(std::size_t& seed, const NodePtr& node, const Rest& ... rest) {
			boost::hash_combine(seed, *node);
			hash_combine(seed, rest...);
		}

		template<typename ... Nodes>
		std::size_t hash(NodeType type, const Nodes& ... nodes) {
			std::size_t seed;
			boost::hash_combine(seed, type);
			hash_combine(seed, nodes...);
			return seed;
		}
	}



	class Node : public utils::HashableImmutableData<Node>, public utils::Annotatable<NodeAnnotation>, public utils::Printable {

	public:

			typedef boost::variant<bool,int,std::size_t,string> Value;

	protected:

		typedef boost::variant<pair<NodeType, NodeList>, Value> NodeData;

		NodeData data;

	public:

		Node() : HashableImmutableData<Node>(0) {};

		Node(const Value& value);

		Node(NodeType type, const NodeList& children);

		template<typename ... Nodes>
		Node(NodeType type, const Ptr<Nodes>& ... children)
			: HashableImmutableData<Node>(detail::hash(type, children...)),
			  data(std::make_pair(type, toVector<NodePtr>(children...))) {}

		bool isValue() const;

		NodeType getNodeType() const {
			return boost::get<pair<NodeType, NodeList>>(data).first;
		}

		const NodeList& getChildList() const {
			return boost::get<pair<NodeType, NodeList>>(data).second;
		}

		virtual std::ostream& printTo(std::ostream& out) const;

	protected:

		virtual bool equals(const Node& other) const { return this == &other; }

	};


	template<typename ... Children>
	class FixedSizeNode {

		const NodeList& list;

	public:

		FixedSizeNode(const NodeList& list) : list(list) {};

		template<
			unsigned index,
			typename Res = typename type_at<index, type_list<Children...>>::type
		>
		const Ptr<Res> get() const {
			return static_pointer_cast<Res>(list[index]);
		}

	};

	template<typename ValueType>
	class ListNode {

		const NodeList& list;

	public:
		ListNode(const NodeList& list) : list(list) {};

		template<unsigned index>
		const Ptr<ValueType> get() const {
			return static_pointer_cast<ValueType>(list[index]);
		}
	};



	template<typename Node, unsigned index>
	struct node_child_type {
		typedef decltype(((Node*)0)->template get<index>()) ptr_type;
		typedef typename ptr_type::element_type type;
	};




	template<typename T>
	struct EmptyBind {
		EmptyBind(const T& adr) {}
	};

	class Type : public Node {
	public:
		typedef EmptyBind<Type> bind_type;

		Type(NodeType type, const NodeList& list) : Node(type, list) {};
	};

	class Statement : public Node {
	public:

		typedef EmptyBind<Statement> bind_type;

		template<typename ... Children>
		Statement(NodeType type, const Children& ... child) : Node(type, child ...) {};
		Statement(NodeType type, const NodeList& list) : Node(type, list) {};
	};

	class Expression : public Statement {
	public:

		typedef EmptyBind<Adr<Expression>> bind_type;

		template<typename ... Children>
		Expression(NodeType type, const Children& ... child) : Statement(type, child ...) {};
		Expression(NodeType type, const NodeList& list) : Statement(type, list) {};
	};


	typedef Ptr<Type> TypePtr;
	typedef vector<TypePtr> TypeList;

	typedef Ptr<Statement> StatementPtr;
	typedef Adr<Statement> StatementAdr;
	typedef vector<StatementPtr> StatementList;

	typedef Ptr<Expression> ExpressionPtr;
	typedef Adr<Expression> ExpressionAdr;
	typedef vector<ExpressionPtr> ExpressionList;


	class If;
	typedef Ptr<If> IfPtr;
	typedef Adr<If> IfAdr;

	class TupleType;
	typedef Ptr<TupleType> TupleTypePtr;


	class TupleType : public Type, public ListNode<Type> {

	public:

		typedef EmptyBind<TupleType> bind_type;

		TupleType(const vector<TypePtr>& list)
			: Type(NT_TupleType, toNodeList(list)), ListNode<Type>(getChildList()) {}

	};

	class Literal : public Expression, public FixedSizeNode<> {
	public:

		typedef EmptyBind<Literal> bind_type;

		Literal() : Expression(NT_Literal), FixedSizeNode<>(getChildList()) {}
	};


	template<typename T> class Adr;
	template<typename T> struct IfBind;

	class If : public Statement, public FixedSizeNode<Expression, Statement, Statement> {

	public:

		typedef IfBind<Adr<If>> bind_type;

		If(const ExpressionPtr& condition, const StatementPtr& thenStmt, const StatementPtr& elseStmt)
			: Statement(NT_If, condition, thenStmt, elseStmt), FixedSizeNode<Expression, Statement, Statement>(getChildList()) {}

		ExpressionPtr getCondition() const {
			return get<0>();
		}
	};


	template<
		typename T
	>
	struct Adr {

		NodeList list;

		typename T::bind_type bind;

		Adr(NodePtr root) : list(toVector(root)), bind(*this) {}

		Adr(NodeList list) : list(list), bind(*this) {}

		Ptr<T> getAddressedNode() const {
			return static_pointer_cast<T>(list.back());
		}

		template<
			unsigned index,
			typename B = typename node_child_type<T,index>::type
		>
		Adr<B> get() const {
			NodeList newList = list;
			Ptr<T> target = getAddressedNode();
			newList.push_back(target->template get<index>());
			return Adr<B>(newList);
		}

		const T& operator*() const {
			return list.back();
		}

		const typename T::bind_type* operator->() const {
			return &bind; // return subst
		}
	};

	template<typename T>
	struct IfBind {
		const T& adr;
		IfBind(const T& adr) : adr(adr) {}
		ExpressionAdr getCondition() const { return adr.template get<0>(); }
		StatementAdr getThenStatement() const { return adr.template get<1>(); }
		StatementAdr getElseStatement() const { return adr.template get<2>(); }
	};

} // end namespace core
} // end namespace insieme

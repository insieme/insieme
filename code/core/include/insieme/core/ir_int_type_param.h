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
#include <vector>
#include <limits>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_values.h"

// ---------------------------------------- Integer Type Parameters ------------------------------

namespace insieme {
namespace core {

	/**
	 * Instances of this class represent the integer-type parameters.
	 *
	 * The type system supports two types of generic type parameters - other types and integers.
	 * Integer parameters may be concrete values, variables (equal to type variables) or the infinite.
	 */
	class IntTypeParam : public Node {
	protected:

		/**
		 * A constructor for this kind of nodes ensuring that every sub-class is a member of the
		 * IntTypeParam node category.
		 *
		 * @param nodeType the actual type the resulting node will be representing
		 * @param children the child nodes to be contained
		 */
		template<typename ... Nodes>
		IntTypeParam(const NodeType nodeType, const Pointer<const Nodes>& ... children)
			: Node(nodeType, NC_IntTypeParam, children ...) { }

		/**
		 * A constructor creating a new instance of this type based on a given child-node list.
		 *
		 * @param nodeType the type of the newly created node
		 * @param children the child nodes to be used to create the new node
		 */
		IntTypeParam(const NodeType nodeType, const NodeList& children)
			: Node(nodeType, NC_IntTypeParam, children) { }

	public:

		/**
		 * A virtual less-than operator allowing to compare int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const =0;

	};

	template<typename D,template<typename T> class P>
	struct IntTypeParamAccessor : public NodeAccessor<D,P> {};




	// **********************************************************************************
	// 							Concrete Int-Type Parameters
	// **********************************************************************************

	/**
	 * The accessor associated to a concrete int-type parameter.
	 */
	IR_NODE_ACCESSOR(ConcreteIntTypeParam, IntTypeParam, UIntValue)
		/**
		 * Obtains the child node representing the concrete value represented by this parameter.
		 */
		IR_NODE_PROPERTY(UIntValue, Param, 0);

		/**
		 * Overloads the get value for this node, allowing to access the represented value directly.
		 */
		unsigned getValue() const { return getParam()->getValue(); }
	};

	/**
	 * A node type representing concrete int-type parameters.
	 */
	IR_NODE(ConcreteIntTypeParam, IntTypeParam)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << getParam()->getValue();
		}

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static ConcreteIntTypeParamPtr get(NodeManager& manager, std::size_t value);

		/**
		 * Compares this concrete int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};




	// **********************************************************************************
	// 							Variable Int-Type Parameters
	// **********************************************************************************

	/**
	 * The accessor associated to a variable int-type parameter.
	 */
	IR_NODE_ACCESSOR(VariableIntTypeParam, IntTypeParam, CharValue)
		/**
		 * Obtains the child node representing the symbol represented by this parameter.
		 */
		IR_NODE_PROPERTY(CharValue, Symbol, 0);
	};

	/**
	 * A node type representing variable int-type parameters.
	 */
	IR_NODE(VariableIntTypeParam, IntTypeParam)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "#" << getSymbol()->getValue();
		}

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static VariableIntTypeParamPtr get(NodeManager& manager, char symbol);

		/**
		 * Compares this variable int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};




	// **********************************************************************************
	// 							Infinite Int-Type Parameters
	// **********************************************************************************

	/**
	 * The accessor associated to a concrete int-type parameter.
	 */
	IR_NODE_ACCESSOR(InfiniteIntTypeParam, IntTypeParam)
	};

	/**
	 * A node type representing infinite int-type parameters.
	 */
	IR_NODE(InfiniteIntTypeParam, IntTypeParam)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "inf";
		}

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static InfiniteIntTypeParamPtr get(NodeManager& manager);

		/**
		 * Compares this infinite int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};




	// **********************************************************************************
	// 							Int-Type Parameter List
	// **********************************************************************************


	/**
	 * The accessor associated to an int-type parameter list.
	 */
	IR_LIST_NODE_ACCESSOR(IntTypeParams, Support, Parameters, IntTypeParam)
	};

	/**
	 * A node type representing a list of int-type parameters.
	 */
	IR_NODE(IntTypeParams, Support)
	protected:

		/**
		 * Prints a string representation of this node to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << join(",", getChildList(), print<deref<NodePtr>>()) << "]";
		}

	public:

		/**
		 * This static factory method allows to construct a int-type parameter list based
		 * on the given int-type parameters.
		 *
		 * @param manager the manager used for maintaining instances of this class
		 * @param params the list of parameters to be included
		 * @return the requested instance managed by the given manager
		 */
		static IntTypeParamsPtr get(NodeManager& manager, const IntParamList& params) {
			return manager.get(IntTypeParams(convertList(params)));
		}
	};


} // end namespace core
} // end namespace insieme

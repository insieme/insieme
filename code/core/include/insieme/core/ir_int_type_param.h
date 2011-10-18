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
#include "insieme/core/values.h"

// ---------------------------------------- Integer Type Parameters ------------------------------

namespace insieme {
namespace core {
namespace new_core {

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
	IR_NODE_ACCESSOR(ConcreteIntTypeParam, UIntValue) {
		/**
		 * Obtains the child node representing the concrete value represented by this parameter.
		 */
		IR_NODE_PROPERTY(UIntValue, Param, 0);
	};

	/**
	 * A node type representing concrete int-type parameters.
	 */
	IR_NODE(ConcreteIntTypeParam, IntTypeParam)

		/**
		 * A constructor for this type of int type parameters accepting the value
		 * to be represented.
		 *
		 * @param value the value to be represented.
		 */
		ConcreteIntTypeParam(const UIntValuePtr& value);

		/**
		 * A constructor creating a new instance if this node type based on the
		 * given child list.
		 *
		 * @param children the list of children to be used
		 */
		ConcreteIntTypeParam(const NodeList& children);

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
	IR_NODE_ACCESSOR(VariableIntTypeParam, CharValue) {
		/**
		 * Obtains the child node representing the symbol represented by this parameter.
		 */
		IR_NODE_PROPERTY(CharValue, Symbol, 0);
	};

	/**
	 * A node type representing variable int-type parameters.
	 */
	IR_NODE(VariableIntTypeParam, IntTypeParam)

		/**
		 * A constructor for this type of int type parameters accepting the value
		 * to be represented.
		 *
		 * @param symbol the symbol to be used to represent the resulting variable
		 */
		VariableIntTypeParam(const CharValuePtr& symbol);

		VariableIntTypeParam(const NodeList& children);

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
	IR_NODE_ACCESSOR(InfiniteIntTypeParam) {};

	/**
	 * A node type representing infinite int-type parameters.
	 */
	IR_NODE(InfiniteIntTypeParam, IntTypeParam)

		/**
		 * A constructor for this type of int type parameter.
		 */
		InfiniteIntTypeParam();

		InfiniteIntTypeParam(const NodeList& children);

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


} // end namespace new_core
} // end namespace core
} // end namespace insieme

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

#include "insieme/core/ast_node.h"

#include "insieme/utils/instance_manager.h"

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
		 * A constructor for this kind of node.
		 *
		 * @param nodeType the actual type this node is representing
		 * @param hash the hash code for the resulting node
		 */
		IntTypeParam(NodeType nodeType, std::size_t hash)
			: Node(nodeType, NC_Support, hash) {};


		/**
		 * Since no int-type parameters has any child nodes, this method produces an empty child list.
		 *
		 * @return an empty child list
		 */
		virtual OptionChildList getChildNodes() const {
			// not int type parameter has a child node
			return std::make_shared<ChildList>();
		}

	public:

		/**
		 * A virtual less-than operator allowing to compare int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const =0;

	};


	/**
	 * A node type representing concrete int-type parameters.
	 */
	class ConcreteIntTypeParam : public IntTypeParam {

		/**
		 * The value represented by the concrete type parameter.
		 */
		const std::size_t value;

		/**
		 * A constructor for this type of int type parameters accepting the value
		 * to be represented.
		 *
		 * @param value the value to be represented.
		 */
		ConcreteIntTypeParam(std::size_t value);

	private:

		/**
		 * Creates a copy of this node using the given mapper.
		 *
		 * @param mapper the mapper to to be used for the copying process (will be ignored by this implementation)
		 * @return a pointer to a clone of this int type parameter node.
		 */
		virtual Node* createCopyUsing(NodeMapping& mapper) const {
			return new ConcreteIntTypeParam(value);
		}

	protected:

		/**
		 * Compares this node with the given node.
		 */
		bool equals(const Node& other) const;

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static ConcreteIntTypeParamPtr get(NodeManager& manager, std::size_t value);

		/**
		 * Prints the name of this identifier to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << value;
		}

		/**
		 * Obtains the value of this concrete int-type parameter.
		 */
		std::size_t getValue() const {
			return value;
		}

		/**
		 * Compares this concrete int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};

	/**
	 * A node type representing variable int-type parameters.
	 */
	class VariableIntTypeParam : public IntTypeParam {

		/**
		 * The symbol used to represent this integer type variable.
		 */
		const char symbol;

		/**
		 * A constructor for this type of int type parameters accepting the value
		 * to be represented.
		 *
		 * @param symbol the symbol to be used to represent the resulting variable
		 */
		VariableIntTypeParam(const char symbol);

	private:

		/**
		 * Creates a copy of this node using the given mapper.
		 *
		 * @param mapper the mapper to to be used for the copying process (will be ignored by this implementation)
		 * @return a pointer to a clone of this int type parameter node.
		 */
		virtual Node* createCopyUsing(NodeMapping& mapper) const {
			return new VariableIntTypeParam(symbol);
		}

	protected:

		/**
		 * Compares this node with the given node.
		 */
		bool equals(const Node& other) const;

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static VariableIntTypeParamPtr get(NodeManager& manager, char symbol);

		/**
		 * Prints the name of this identifier to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "#" << symbol;
		}

		/**
		 * Obtains the symbol used to represent this variable int-type parameter.
		 *
		 * @return the symbol representing this variable (making it unique).
		 */
		char getSymbol() const {
			return symbol;
		}

		/**
		 * Compares this variable int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};

	/**
	 * A node type representing infinite int-type parameters.
	 */
	class InfiniteIntTypeParam : public IntTypeParam {

		/**
		 * A constructor for this type of int type parameter.
		 */
		InfiniteIntTypeParam();

	private:

		/**
		 * Creates a copy of this node using the given mapper.
		 *
		 * @param mapper the mapper to to be used for the copying process (will be ignored by this implementation)
		 * @return a pointer to a clone of this int type parameter node.
		 */
		virtual Node* createCopyUsing(NodeMapping& mapper) const {
			return new InfiniteIntTypeParam();
		}

	protected:

		/**
		 * Compares this node with the given node.
		 */
		bool equals(const Node& other) const;

	public:

		/**
		 * A static factory method for instances of this type.
		 */
		static InfiniteIntTypeParamPtr get(NodeManager& manager);

		/**
		 * Prints the name of this identifier to the given output stream.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "Inf";
		}

		/**
		 * Compares this infinite int type parameter with other int type parameters.
		 */
		virtual bool operator<(const IntTypeParam& other) const;

	};


} // end namespace core
} // end namespace insieme

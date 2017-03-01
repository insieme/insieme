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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {

	template <typename D, template <typename T> class P>
	struct Accessor<Value,D,P> : public Accessor<Node, D, P> {
		/**
		 * Obtains a reference to the value represented by this node if
		 * it is representing a value. This method is overloading the version
		 * within the NodeAccessor - thereby increasing visibility.
		 *
		 * @return a reference to the internally maintained value
		 */
		const NodeValue& getValue() const {
			// forward call to protected parent method
			return Accessor<Node, D, P>::getNodeValue();
		}
	};

	/**
	 * A node forming a common base type for all value nodes.
	 */
	class Value : public Node {
	  protected:
		/**
		 * A constructor limiting subclasses to a value-node construction.
		 *
		 * @param type the type of the resulting node
		 * @param value the value to be represented
		 */
		Value(const NodeType type, const NodeValue& value) : Node(type, value) {}
	};


	/**
	 * A macro defining value nodes based on a name and the type of value to be presented.
	 */
	#define VALUE_NODE(NAME, TYPE)                                                                                                                             \
                                                                                                                                                               \
		template <typename D, template <typename T> class P>                                                                                                   \
		struct Accessor<NAME##Value,D,P> : public Accessor<Value, D, P> {                                                                                      \
			const TYPE& getValue() const {                                                                                                                     \
				return boost::get<TYPE>(this->getNodeValue());                                                                                                 \
			}                                                                                                                                                  \
		};                                                                                                                                                     \
                                                                                                                                                               \
		class NAME##Value : public Value, public Accessor<NAME##Value, NAME##Value, Pointer> {                                                                 \
			NAME##Value(const TYPE value) : Value(NT_##NAME##Value, value) {}                                                                                  \
                                                                                                                                                               \
		  public:                                                                                                                                              \
			static NAME##ValuePtr get(NodeManager& manager, const TYPE value) {                                                                                \
				return manager.get(NAME##Value(value));                                                                                                        \
			}                                                                                                                                                  \
			static NAME##ValuePtr get(NodeManager& manager, const NodeList& children) {                                                                        \
				assert_fail() << "Value nodes must not be constructed via their child node list!";                                                             \
				return NAME##ValuePtr();                                                                                                                       \
			}                                                                                                                                                  \
			operator const TYPE&() const {                                                                                                                     \
				return boost::get<TYPE>(Node::getNodeValue());                                                                                                 \
			}                                                                                                                                                  \
			const TYPE& getValue() const {                                                                                                                     \
				return boost::get<TYPE>(Node::getNodeValue());                                                                                                 \
			}                                                                                                                                                  \
			bool operator<(const NAME##Value& other) const {                                                                                                   \
				return getValue() < other.getValue();                                                                                                          \
			}                                                                                                                                                  \
                                                                                                                                                               \
		  protected:                                                                                                                                           \
			virtual Node* createInstanceUsing(const NodeList& children) const {                                                                                \
				assert_true(children.empty()) << "Value nodes must no have children!";                                                                         \
				return new NAME##Value(*this);                                                                                                                 \
			}                                                                                                                                                  \
			virtual std::ostream& printTo(std::ostream& out) const {                                                                                           \
				return out << getValue();                                                                                                                      \
			}                                                                                                                                                  \
		};

	/**
	 * The BoolValue node represents a single, boolean value.
	 */
	VALUE_NODE(Bool, bool);

	/**
	 * The CharValue node represents a character value within the IR structure.
	 */
	VALUE_NODE(Char, char);

	/**
	 * The IntValue node represents an integer value within the IR structure.
	 */
	VALUE_NODE(Int, int);

	/**
	 * The UIntValue node representing an unsigned integer value within the IR structure.
	 */
	VALUE_NODE(UInt, unsigned);

	/**
	 * The StringValue node represents a string value e.g. naming a type or an identifer within
	 * the IR structure.
	 */
	VALUE_NODE(String, string);


	#undef VALUE_NODE

} // end namespace core
} // end namespace insieme

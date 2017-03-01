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

#include <vector>
#include <boost/variant.hpp>

#include "insieme/utils/set_utils.h"
#include "insieme/utils/map_utils.h"

namespace insieme {
namespace core {

	using std::string;
	using std::pair;
	using std::vector;

	// forward declaration of pointer and address templates
	template <typename T>
	class Pointer;
	template <typename T>
	class Address;

	/**
	 * Adds forward declarations for all AST node types. Further, for each
	 * type a type definition for a corresponding annotated pointer is added.
	 */
	#define NODE(NAME)                                                                                                                                         \
		class NAME;                                                                                                                                            \
		typedef Pointer<const NAME> NAME##Ptr;                                                                                                                 \
		typedef Address<const NAME> NAME##Address;                                                                                                             \
		typedef std::vector<NAME##Ptr> NAME##List;                                                                                                             \
		typedef utils::set::PointerSet<NAME##Ptr> NAME##Set;																								   \
		typedef std::vector<NAME##Address> NAME##AddressList;                                                                                                  \


	// take all nodes from within the definition file
	#include "insieme/core/ir_nodes.def"

	#undef NODE


	/**
	 * The union of all the values which can directly be represented using nodes. If
	 * a node represents a value, it is representing a value of this type.
	 */
	typedef boost::variant<bool, char, int, unsigned, string> NodeValue;


	// Supporting Utilities
	class IRBuilder;
	class NodeManager;
	template <typename Context = int>
	class NodeMapping;
	class SimpleNodeMapping;
	class NodeAnnotation;

	namespace lang {
		class BasicGenerator;
	} // end namespace lang


	/**
	 * Typedefs for some widely used base type collections.
	 */

	typedef utils::map::PointerMap<NodePtr, NodePtr> NodeMap;
	typedef utils::map::PointerMap<TypePtr, TypePtr> TypeMap;
	typedef utils::map::PointerMap<ExpressionPtr, ExpressionPtr> ExpressionMap;
	typedef utils::map::PointerMap<StatementPtr, StatementPtr> StatementMap;
	typedef utils::map::PointerMap<VariablePtr, VariablePtr> VariableMap;
	typedef utils::map::PointerMap<VariablePtr, ExpressionPtr> VarExprMap;
	typedef utils::map::PointerMap<ExpressionPtr, VariablePtr> ExprVarMap;
	typedef utils::map::PointerMap<LambdaReferencePtr, LambdaPtr> LambdaBindingMap;
	typedef utils::map::PointerMap<TagTypeReferencePtr, RecordPtr> TagTypeBindingMap;

	typedef std::function<bool(NodePtr)> NodeFilter;

} // end namespace core
} // end namespace insieme



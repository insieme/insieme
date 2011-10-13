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

#include "insieme/core/ir_node.h"

namespace insieme {
namespace core {


	// **********************************************************************************
	// 							    Abstract Node Base
	// **********************************************************************************

	namespace detail {

		/**
		 * A visitor realizing the hashing for the value type potentially stored
		 * within a node.
		 */
		struct HashVisitor : public boost::static_visitor<std::size_t> {
			template<typename T>
			std::size_t operator()(const T& value) const {
				return boost::hash<T>()(value);
			}
		};

		/**
		 * Obtains a hash value for the given value instance.
		 *
		 * @param value the value to be hashed
		 * @return the hash code for the given value object
		 */
		inline std::size_t hash(const NodeType type, const Node::Value& value) {
			std::size_t seed = 0;
			boost::hash_combine(seed, type);
			boost::hash_combine(seed, boost::apply_visitor(HashVisitor(), value));
			return seed;
		}

		/**
		 * A static visitor determining whether an element within a boost::variant
		 * is a value or not.
		 */
		struct IsValueVisitor : public boost::static_visitor<bool> {
			bool operator()(const Node::Value& value) const { return true; }
			template<typename T> bool operator()(const T& other) const { return false; }
		};

	}

	Node::Node(const NodeType nodeType, const Value& value)
		: HashableImmutableData(detail::hash(nodeType, value)),
		  nodeType(nodeType), value(value), nodeCategory(NC_Value), equalityID(0) {

		// make sure value nodes are extending the value node type
		assert(dynamic_cast<ValueNode*>(this) && "Value node not being of proper sub-type encountered!");
	}

} // end namespace core
} // end namespace insieme

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

#include "insieme/core/ir_node_tryout.h"

namespace insieme {
namespace core {

	namespace detail {

		struct HashVisitor : public boost::static_visitor<std::size_t> {
			template<typename T>
			std::size_t operator()(const T& value) const {
				return boost::hash<T>()(value);
			}
		};

		std::size_t hash(const Node::Value& value) {
			return boost::apply_visitor(HashVisitor(), value);
		}

		std::size_t hash(NodeType type, const NodeList& children) {
			std::size_t seed;
			boost::hash_combine(seed, type);
			insieme::utils::hashList(seed, children, deref<NodePtr>());
			return seed;
		}


		struct IsValueVisitor : public boost::static_visitor<bool> {
			bool operator()(const Node::Value& value) const { return true; }
			template<typename T> bool operator()(const T& other) const { return false; }
		};

	}

	Node::Node(const Value& value)
		: HashableImmutableData<Node>(detail::hash(value)), data(value) {}

	Node::Node(NodeType type, const NodeList& children)
		: HashableImmutableData<Node>(detail::hash(type, children)), data(std::make_pair(type, children)) {}


	bool Node::isValue() const {
		return boost::apply_visitor(detail::IsValueVisitor(), data);
	}


	std::ostream& Node::printTo(std::ostream& out) const {
		return out << "node";
	}

} // end namespace core
} // end namespace insieme

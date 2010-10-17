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

#include <cassert>

#include "type_utils.h"
#include "map_utils.h"

namespace insieme {
namespace core {

namespace {

class SubstitutionMapper : public NodeMapping {

	NodeManager& manager;
	const Substitution::Mapping& mapping;

public:

	SubstitutionMapper(NodeManager& manager, const Substitution::Mapping& mapping)
		: manager(manager), mapping(mapping) {};

	const NodePtr mapElement(const NodePtr& element) {
		// quick check - only variables are substituted
		if (element->getNodeType() != NT_TypeVariable) {
			return element->substitute(manager, *this);
		}

		// lookup current variable within the mapping
		auto pos = mapping.find(static_pointer_cast<TypeVariable>(element));
		if (pos != mapping.end()) {
			// found! => replace
			return (*pos).second;
		}

		// not found => return current node
		// (since nothing within a variable node may be substituted)
		return element;
	}
};


}

Substitution::Substitution(const TypeVariablePtr& var, const TypePtr& type) {
	mapping.insert(std::make_pair(var, type));
};


TypePtr Substitution::applyTo(NodeManager& manager, const TypePtr& type) const {
	// perform substitution
	SubstitutionMapper mapper(manager, mapping);
	return mapper.map(type);
}

void Substitution::addMapping(const TypeVariablePtr& var, const TypePtr& type) {
	auto element = std::make_pair(var,type);
	auto res = mapping.insert(element);
	if (!res.second) {
		mapping.erase(var);
		res = mapping.insert(element);
		assert( res.second && "Insert was not successful!" );
	}
}

void Substitution::remMappingOf(const TypeVariablePtr& var) {
	mapping.erase(var);
}

Substitution Substitution::compose(NodeManager& manager, const Substitution& a, const Substitution& b) {

	typedef Substitution::Mapping::value_type Entry;

	// copy substitution b
	Substitution res(b);

	// apply substitution a to all mappings in b
	for_each(res.mapping, [&manager, &b](Entry& cur) {
		cur.second = b.applyTo(manager, cur.second);
	});

	// add remaining mappings of a
	Substitution::Mapping& resMapping = res.mapping;
	for_each(a.mapping, [&resMapping](const Entry& cur) {
		if (resMapping.find(cur.first) == resMapping.end()) {
			resMapping.insert(cur);
		}
	});

	return res;
}


bool occurs(const NodePtr& x, const NodePtr& term) {
	// it occurs if it is the current node or one of its child contains the node
	return *x==*term || any(term->getChildList(), [&x](const NodePtr& cur) {
		return occurs(x, cur);
	});
}

boost::optional<Substitution> unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list) {
	typedef std::pair<TypePtr, TypePtr> Pair;
	typedef std::list<Pair> List;

	// create result
	Substitution res;

	while(!list.empty()) {

		// get current element
		Pair cur = list.front();
		list.pop_front();

		const TypePtr a = cur.first;
		const TypePtr b = cur.second;

		const NodeType typeOfA = a->getNodeType();
		const NodeType typeOfB = b->getNodeType();

		// 1) if (a == b) ignore pair ..
		if (*a==*b) {
			continue;
		}

		// 2) swap if b is a variable but a is not ..
		if (typeOfA != NT_TypeVariable && typeOfB == NT_TypeVariable) {
			// add swapped pair to front of list
			list.push_front(std::make_pair(b, a));
			continue;
		}

		// 3) handle variables on left hand side ...
		if (typeOfA == NT_TypeVariable) {
			if (occurs(a, b)) {
				// not unifiable (e.g. X = type<X> cannot be unified)
				return boost::optional<Substitution>();
			}

			// convert current pair into substitution
			Substitution mapping(
					static_pointer_cast<TypeVariable>(a),
					static_pointer_cast<Type>(b)
			);

			// apply substitution to remaining pairs
			for_each(list, [&mapping, &manager](Pair& cur) {
				cur.first = mapping.applyTo(manager, cur.first);
				cur.second = mapping.applyTo(manager, cur.second);
			});

			// compose current mapping with overall result
			res = Substitution::compose(manager, res, mapping);
			continue;
		}

		// 4) function types / generic types / tuples / structs / unions / recursive types
		if (typeOfA != typeOfB) {
			// => not unifiable
			return boost::optional<Substitution>();
		}

		if (typeOfA == NT_RecType) {
			// TODO: implement
			assert ("RECURSIVE TYPE SUPPORT NOT IMPLEMENTED!");
		}

		// => check family of generic type
		if (typeOfA == NT_GenericType) {
			if (static_pointer_cast<GenericType>(a)->getFamilyName() != static_pointer_cast<GenericType>(b)->getFamilyName()) {
				return boost::optional<Substitution>();
			}
		}

		// => check all child nodes
		auto childrenA = a->getChildList();
		auto childrenB = b->getChildList();
		std::for_each(
				make_paired_iterator(childrenA.begin(), childrenB.begin()),
				make_paired_iterator(childrenA.end(), childrenB.end()),

				[&list](const std::pair<NodePtr, NodePtr>& cur) {
					list.push_front(std::make_pair(
							static_pointer_cast<Type>(cur.first),
							static_pointer_cast<Type>(cur.second)
					));
		});
	}

	// done
	return boost::optional<Substitution>(res);
}


} // end namespace core
} // end namespace insieme


std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution) {
	return out << "{" << join(",", substitution.getMapping(), [](std::ostream& out, const insieme::core::Substitution::Mapping::value_type& cur)->std::ostream& {
		return out << *cur.first << "->" << *cur.second;
	}) << "}";
	//return out << substitution.getMapping();
}


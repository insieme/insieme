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

#include <queue>

#include <boost/optional/optional.hpp>
#include <boost/unordered_map.hpp>

#include "types.h"


namespace insieme {
namespace core {


class Substitution {

public:

	typedef boost::unordered_map<TypeVariablePtr, TypePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> Mapping;

private:

	/**
	 * The mapping this substitution is representing.
	 */
	Mapping mapping;

public:

	Substitution() {};

	Substitution(const TypeVariablePtr& var, const TypePtr& type);

	TypePtr applyTo(NodeManager& manager, const TypePtr& type) const;

	void addMapping(const TypeVariablePtr& var, const TypePtr& type);

	void remMappingOf(const TypeVariablePtr& var);

	const Mapping& getMapping() const {
		return mapping;
	}

	static Substitution compose(NodeManager& manager, const Substitution& a, const Substitution& b);

};

/**
 * Checks whether the given node x is referenced directly or indirectly
 * within the given term.
 *
 * @param x the node to be searched within the given term
 * @param term the term to be searched
 * @return true if present, false otherwise
 */
bool occurs(const NodePtr& x, const NodePtr& term);



boost::optional<Substitution> unifyAll(NodeManager& manager, std::list<std::pair<TypePtr, TypePtr>>& list);

template<typename Container>
boost::optional<Substitution> unifyAll(NodeManager& manager, const Container& listA, const Container& listB) {

	// check length of lists ...
	if (listA.size() != listB.size()) {
		// ... if not equal => not unifyable
		return boost::optional<Substitution>();
	}

	// define some types ...
	typedef std::pair<TypePtr, TypePtr> Pair;
	typedef std::list<Pair> List;

	// delegate work to non-template method within cpp file
	List list(
				make_paired_iterator(listA.begin(), listB.begin()),
				make_paired_iterator(listA.end(), listB.end())
	);
	return unifyAll(manager, list);
}

boost::optional<Substitution> unify(NodeManager& manager, const TypePtr& typeA, const TypePtr& typeB) {
	return unifyAll(manager, toVector<TypePtr>(typeA), toVector<TypePtr>(typeB));
}

bool isUnifyable(const TypePtr& typeA, const TypePtr& typeB) {
	NodeManager tmp; // requires only temporary manager
	return typeA==typeB || unify(tmp, typeA, typeB);
}

template<typename Container>
bool isUnifyable(const Container& listA, const Container& listB) {
	NodeManager tmp; // requires only temporary manager
	// exploits implicit boolean conversion of boost::optional
	return unify(tmp, listA, listB);
}

}
}

/**
 * Allows substitutions to be printed to a stream (especially useful during debugging and
 * within test cases where equals expects values to be printable).
 */
std::ostream& operator<<(std::ostream& out, const insieme::core::Substitution& substitution);


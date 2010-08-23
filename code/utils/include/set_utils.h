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

#include <algorithm>
#include <memory>
#include <string>
#include <unordered_set>
#include <vector>

#include <boost/algorithm/string/join.hpp>

#include "string_utils.h"

// TODO: add hash function to arguments ...

template<typename E>
std::unique_ptr<std::unordered_set<E>> merge(const std::unordered_set<E>& setA, const std::unordered_set<E>& setB) {
	// merging by simply adding all elements to one set ...
	std::unique_ptr<std::unordered_set<E>> res(new std::unordered_set<E>());
	res->insert(setA.cbegin(), setA.cend());
	res->insert(setB.cbegin(), setB.cend());
	return res;
}

// ------------- BEGIN: just for experimenting with return type - will be removed -----------------------------------
template<typename E>
std::unordered_set<E> merge_V(const std::unordered_set<E>& setA, const std::unordered_set<E>& setB) {
	// merging by simply adding all elements to one set ...
	std::unique_ptr<std::unordered_set<E>> res(new std::unordered_set<E>());
	res->insert(setA.cbegin(), setA.cend());
	res->insert(setB.cbegin(), setB.cend());
	return res;
}

template<typename E>
void merge_R(std::unordered_set<E>& res, const std::unordered_set<E>& setA, const std::unordered_set<E>& setB) {
	// merging by simply adding all elements to one set ...
	res.clear();
	res->insert(setA.cbegin(), setA.cend());
	res->insert(setB.cbegin(), setB.cend());
	return res;
}
// ------------- END: just for experimenting with return type - will be removed -----------------------------------

template<typename E>
std::unique_ptr<std::unordered_set<E>> intersect(const std::unordered_set<E>& setA, const std::unordered_set<E>& setB) {
	// intersection by iterating through one set and checking membership within the other
	std::unique_ptr<std::unordered_set<E>> res(new std::unordered_set<E>());
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const E& cur) {
		if (setB.find(cur) != setB.cend()) {
			res->insert(cur);
		}
	});
	return res;
}

template<typename E>
std::unique_ptr<std::unordered_set<E>> difference(const std::unordered_set<E>& setA, const std::unordered_set<E>& setB) {
	// intersection by iterating through one set and checking membership within the other
	std::unique_ptr<std::unordered_set<E>> res(new std::unordered_set<E>());
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const E& cur) {
		if (setB.find(cur) == setB.cend()) {
			res->insert(cur);
		}
	});
	return res;
}

/**
 * Allows to print unoredered sets including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element>
std::ostream& operator<<(std::ostream& out, const std::unordered_set<Element>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.cbegin(), container.cend(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

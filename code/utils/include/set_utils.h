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
#include <boost/unordered_set.hpp>

#include "string_utils.h"


namespace insieme {
namespace utils {
namespace set {

/**
 * A factory method to obtain a set containing a single element. The type of set to be generated
 * has to be specified using a template parameter.
 *
 * @tparam Set the type of set to be created (has to be manually specified)
 * @tparam Element the type of element to be contained (should be automatically derived)
 * @param element the element to be present within the resulting set
 * @return a set containing the given element
 */
template<typename Set, typename Element>
const Set toSet(const Element& element) {
	Set res;
	res.insert(element);
	return res;
}

/**
 * Computes the set-union (merge) of the given sets.
 *
 * NOTE: this function should not be used to add elements to an existing set.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return the union of setA and setB
 */
template<typename Set>
Set merge(const Set& setA, const Set& setB) {
	// merging by simply adding all elements to one set ...
	Set res;
	res.insert(setA.cbegin(), setA.cend());
	res.insert(setB.cbegin(), setB.cend());
	return res;
}

/**
 * Computes the set-intersection of the given sets.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return the intersection of setA and setB
 */
template<typename Set>
Set intersect(const Set& setA, const Set& setB) {
	typedef typename Set::value_type Element;

	// intersection by iterating through one set and checking membership within the other
	Set res;
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const Element& cur) {
		if (setB.find(cur) != setB.cend()) {
			res.insert(cur);
		}
	});
	return res;
}

/**
 * Computes the set-difference of two sets.
 *
 * NOTE: this function should not be used to remove elements from an existing set.
 *
 * @tparam set the type of set to be handled
 * @param setA the first set
 * @param setB the second set
 * @return a set containing all elements of the first set which are not members of the second set.
 */
template<typename Set>
Set difference(const Set& setA, const Set& setB) {
	typedef typename Set::value_type Element;

	// intersection by iterating through one set and checking membership within the other
	Set res;
	std::for_each(setA.cbegin(), setA.cend(), [&res,&setB](const Element& cur) {
		if (setB.find(cur) == setB.cend()) {
			res.insert(cur);
		}
	});
	return res;
}

} // end namespace: set
} // end namespace: utils
} // end namespace: insieme

/**
 * Allows to print unoredered sets including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const std::unordered_set<Element, Hash, Pred, Alloc>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.cbegin(), container.cend(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

/**
 * Allows to print unoredered sets including printable elements.
 *
 * @param out the stream to which the given vector should be printed to
 * @param container the vector to be printed
 * @return the handed in ostream to chain operation invocations.
 */
template<typename Element, typename Hash, typename Pred, typename Alloc>
std::ostream& operator<<(std::ostream& out, const boost::unordered_set<Element, Hash, Pred, Alloc>& container) {

	// convert elements into strings
	std::vector<std::string> list;
	std::transform(container.cbegin(), container.cend(), back_inserter(list), &toString<Element>);

	// print and done
	return out << "{" << boost::join(list, ",") << "}";
}

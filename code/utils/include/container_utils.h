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

#include <vector>
#include <functional>

#include <boost/type_traits/is_convertible.hpp>
#include <boost/utility/enable_if.hpp>


using std::vector;
using std::function;

using boost::is_convertible;
using boost::enable_if;


/**
 * Creates a vector containing (a copy of) the single element provided as an argument.
 *
 * @tparam the type of element to be contained within the singleton vector
 * @param element the element to be included within the new vector
 *
 * @return a new vector instance containing a single element
 */
template<typename T>
vector<T> toVector(T element) {
	return vector<T> (1, element);
}

/**
 * A small utility function capable of appending all elements of one vector to another.
 *
 * @tparam T the element type of the target vector
 * @tparam B the element type of the source vector, needs to be a sub-type of T
 * @param target the vector to which elements should be appended
 * @param source the vector from which the element should be taken
 */
template<typename T, typename B>
typename enable_if<is_convertible<B,T>, void>::type addAll(vector<T>& target, const vector<B>& source) {
	// add all elements of the source to the end of the target
	copy(source.cbegin(), source.cend(), back_inserter(target));
}


template<class InputIterator, class Function>
bool all(InputIterator first, InputIterator last, const Function& predicate)
{
	while (first != last) {
		if (!predicate(*first)) {
			return false;
		}
		++first;
	}
	return true;
}

template<class T, class Function>
bool all(const vector<T>& list, const Function& predicate) {
	return all(list.cbegin(), list.cend(), predicate);
}

template<class InputIterator, class Function>
bool any(InputIterator first, InputIterator last, const Function& predicate)
{
	while (first != last) {
		if (predicate(*first)) {
			return true;
		}
		++first;
	}
	return false;
}

template<class T, class Function>
bool any(const vector<T>& list, const Function& predicate) {
	return any(list.cbegin(), list.cend(), predicate);
}

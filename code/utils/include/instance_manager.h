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
#include <unordered_set>
#include <functional>

#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_const.hpp>
#include <boost/utility/enable_if.hpp>

#include "instance_ptr.h"
#include "container_utils.h"

/**
 * This utility struct definition defines a predicate comparing two pointers
 * based on the value they are pointing to.
 *
 * @tparam Tp the type of pointer to be compared
 */
template<typename Tp>
struct pointing_to_equal: public std::binary_function<Tp, Tp, bool> {
	/**
	 * Performs the actual comparison by using the operator== of the generic
	 * type Tp.
	 *
	 * @param x the pointer to the first element to be compared
	 * @param y the pointer to the second element to be compared
	 */
	bool operator()(const Tp& x, const Tp& y) const {
		return *x == *y;
	}
};

/**
 * This utility struct defines the function used to compute hash codes for pointers.
 * Thereby, the hash code is not computed using the pointer themselves. Instead, the
 * target they are pointing to is used to compute the value. In case the pointer is null,
 * 0 is returned as a hash value.
 *
 * @tparam T the type of element the used pointers are pointing to
 */
template<typename T>
struct target_hash: public std::unary_function<T, std::size_t> {

	/**
	 * This function is used to compute the hash of the actual target.
	 */
	const boost::hash<T> hasher;

	/**
	 * Explicit Default constructor required by VC.
	 */
	target_hash() : hasher() {	}

	/**
	 * Computes the hash value of the given pointer based on the target it is pointing to.
	 */
	std::size_t operator()(const T* p) const {
		if (p) {
			return hasher(*p);
		}
		return 0;
	}
};

/**
 * An instance manager is capable of handling a set of instances of a generic type T. Instances
 * representing the same value are shared. Hence, to avoid altering the instances referenced by
 * others, the handled types have to be constant (which is enforced).
 *
 * @tparam T the type of elements managed by the concrete manager instance. The type has to
 * 			 be a constant type.
 */
template<
	typename T,
	typename R = InstancePtr<T>,
	typename boost::enable_if<boost::is_const<T>,int>::type = 0,
	typename boost::enable_if<boost::is_base_of<InstancePtr<T>, R>,int>::type = 0
	>
class InstanceManager {

	/**
	 * The storage used to maintain instances. It is based on a unordered set which
	 * is modified to support operations based on pointers. All the elements stored
	 * within this set will be automatically deleted when this instance manager instance
	 * is destroyed.
	 */
	std::unordered_set<T*, target_hash<T>, pointing_to_equal<T*>> storage;

public:

	/**
	 * The destructor of this instance manager freeing all elements within the store.
	 */
	~InstanceManager() {
		// delete all elements maintained by the manager
		std::for_each(storage.begin(), storage.end(),
				[](T* cur) { delete cur; }
		);
	}

	// true if new, false otherwise ...
	std::pair<R,bool> add(T* instance) {

		// test whether there is already an identical element
		auto res = storage.find(instance);
		if (res != storage.end()) {
			// use included element
			return std::make_pair(R(*res), false);
		}

		// copy element (to ensure private copy)
		T* newElement = instance->clone();
		storage.insert(newElement);
		return std::make_pair(R(newElement), true);

	}

	std::pair<R,bool> add(T& instance) {
		return add(&instance);
	}


	/**
	 * Obtains an instance managed by this instance manager referencing the master
	 * copy of the given instance. If so such instance is present yet, a copy of the
	 * handed in instance will be created and added to the internal store. The returned
	 * reference will point to the new master copy.
	 *
	 * @param instance the instance to be looking for within this instance manager.
	 * @return a reference to the new master copy of the handed in instance
	 *
	 * @see get(T&)
	 */
	R get(T* instance) {
		return add(instance).first;
	}

	/**
	 * Obtains an instance managed by this instance manager referencing the master
	 * copy of the given instance. If so such instance is present yet, a copy of the
	 * handed in instance will be created and added to the internal store. The returned
	 * reference will point to the new master copy.
	 *
	 * @param instance the instance to be looking for within this instance manager.
	 * @return a reference to the new master copy of the handed in instance
	 *
	 * @see get(T*)
	 */
	std::pair<R,bool> get(T& instance) {
		return this->get(&instance);
	}

	/**
	 * Retrieves the number of elements handled by this instance manager.
	 *
	 * @return the total number of elements currently managed
	 */
	int size() {
		return storage.size();
	}

	/**
	 * Checks whether all the instance pointer within the given vector are pointing
	 * to elements maintained by this manager. This method is mainly intended for assertions
	 * and test cases and should not be required within release code.
	 *
	 * @param pointers the list of pointers to be checked
	 * @return true if all are referencing to elements within this manager, false otherwise
	 */
	bool containsAll(const vector<R>& pointers) const {
		return all(pointers, [&](const R& cur)->bool {

			// search for entry (based on value)
			auto entry = this->storage.find(&*cur);

			// check whether something has been found
			if (entry == this->storage.cend()) {
				// not contained
				return false;
			}

			// check whether entry is pointing to same location as cur
			return &*cur == *entry;
		});
	}
};


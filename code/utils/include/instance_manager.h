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
#include <boost/utility.hpp>

#include "functional_utils.h"
#include "instance_ptr.h"
#include "container_utils.h"

#include <iostream>

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
	boost::hash<T> hasher;

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
	template<class C> class R = InstancePtr,
	typename boost::enable_if<boost::is_base_of<InstancePtr<T>, R<T> >,int>::type = 0
	>
class InstanceManager : private boost::noncopyable {

	/**
	 * The storage used to maintain instances. It is based on a unordered set which
	 * is modified to support operations based on pointers. All the elements stored
	 * within this set will be automatically deleted when this instance manager instance
	 * is destroyed.
	 */
	std::unordered_set<const T*, target_hash<const T>, pointing_to_equal<const T*>> storage;


	/**
	 * A private method used to clone instances to be managed by this type.
	 *
	 * @tparam S the type of the the instance to be cloned (the same type will be returned)
	 * @param instance a pointer to the instance to be cloned
	 * @return a pointer to a clone of the given instance of the same type
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>,S*>::type clone(const S* instance) {
		//  step 1 - cast to base type (since only this one allows us to clone it)
		const T* orig = instance;
		//  step 2 - clone
		T* clone = orig->clone(*static_cast<typename S::Manager*>(this));

		// make sure clone is valid
		assert( hash_value(*instance) == hash_value(*clone) );
		assert( *orig == *clone );

		// step 2 - cast back to original type
		return dynamic_cast<S*>(clone);
	}

public:

	/**
	 * The destructor of this instance manager freeing all elements within the store.
	 */
	virtual ~InstanceManager() {
		// delete all elements maintained by the manager
		std::for_each(storage.begin(), storage.end(),
				[](const T* cur) { delete cur; }
		);
	}

	/**
	 * Adds the given instance to this manager if not already present.
	 *
	 * @tparam S the type of the instance to be added (has to be a sub-type of the managed type)
	 * @param instance the instance to be added (will be cloned if not already present)
	 * @return a pair including a reference to the internally maintained instance and a boolean flag
	 * 			indicating whether an insertion actually occurred. If true, the given element was cloned
	 *			and added, if false a identical element was already present.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, std::pair<R<const S>,bool>>::type add(const S* instance) {
		// test whether there is already an identical element
		auto res = storage.find(instance);
		if (res != storage.end()) {
			// use included element
			return std::make_pair(R<const S>(dynamic_cast<const S*>(*res)), false);
		}

		// clone element (to ensure private copy)
		S* newElement = clone(instance);
		auto check = storage.insert(newElement);

		// ensure the element has really been added (hash and equals is properly implemented)
		assert ( check.second );

		// ensure the element can be found again
		assert ( check.first == storage.find(instance) );

//if (!check.second) {
//
//	target_hash<const T> hasher;
//
//	std::cout << "ERROR adding \"" << *instance << "\" failed!" << std::endl;
//	std::cout << "   Hash Instance:  " << hasher(instance) << std::endl;
//	std::cout << "   Hash Clone:     " << hasher(newElement) << std::endl;
//	std::cout << "   Equals:         " << (bool)(*instance == *newElement) << std::endl;
//	std::cout << "   Found Instance: " << (bool)(storage.find(instance) != storage.end()) << std::endl;
//	std::cout << "   Found Clone:    " << (bool)(storage.find(newElement) != storage.end()) << std::endl;
//
//	std::for_each(storage.cbegin(), storage.cend(),
//			[&instance, &hasher, &newElement](const T* cur) {
//				std::cout << "     - " << *cur << " - hash: " << hasher(cur) << " - " << (bool)(*cur == *newElement) << std::endl;
//	});
//	std::cout << "   Found Instance: " << (bool)(storage.find(instance) != storage.end()) << std::endl;
//	std::cout << "   Found Clone:    " << (bool)(storage.find(newElement) != storage.end()) << std::endl;
//
//	std::cout << std::endl;
//}

		return std::make_pair(R<const S>(newElement), true);
	}

	/**
	 * Adds the given instance to this manager if not already present.
	 *
	 * @tparam S the type of the instance to be added (has to be a sub-type of the managed type)
	 * @param instance the instance to be added (will be cloned if not already present)
	 * @return a pair including a reference to the internally maintained instance and a boolean flag
	 * 			indicating whether an insertion actually occurred. If true, the given element was cloned
	 *			and added, if false a identical element was already present.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, std::pair<R<const S>,bool>>::type add(const S& instance) {
		return add(&instance);
	}

	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, std::pair<R<const S>,bool>>::type add(const R<const S>& pointer) {
		if (!!pointer) {
			return this->add(*pointer);
		}
		// create null instance (potentially erasing annotations)
		return std::make_pair(R<const S>(NULL), false);
	}

	/**
	 * Obtains an instance managed by this instance manager referencing the master
	 * copy of the given instance. If so such instance is present yet, a copy of the
	 * handed in instance will be created and added to the internal store. The returned
	 * reference will point to the new master copy.
	 *
	 * @tparam S the type of instance to be obtained
	 * @param instance the instance to be looking for within this instance manager.
	 * @return a reference to the new master copy of the handed in instance
	 *
	 * @see get(T&)
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, R<const S>>::type get(const S* instance) {
		return add(instance).first;
	}

	/**
	 * Obtains an instance managed by this instance manager referencing the master
	 * copy of the given instance. If so such instance is present yet, a copy of the
	 * handed in instance will be created and added to the internal store. The returned
	 * reference will point to the new master copy.
	 *
	 * @tparam S the type of instance to be obtained
	 * @param instance the instance to be looking for within this instance manager.
	 * @return a reference to the new master copy of the handed in instance
	 *
	 * @see get(T*)
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, R<const S>>::type get(const S& instance) {
		return this->get(&instance);
	}

	/**
	 * Obtains an instance managed by this instance manager referencing the counterpart
	 * of the object referenced by the given pointer. If so such instance is present yet,
	 * a copy of the handed in instance will be created and added to the internal store.
	 * The returned reference will point to the new master copy.
	 *
	 * @tparam S the type of instance to be obtained
	 * @param instance the instance to be looking for within this instance manager.
	 * @return a reference to the new master copy of the handed in instance
	 *
	 * @see get(T*)
	 */
	template<class S>
	R<const S> get(const R<const S>& pointer) {
		if (!!pointer) {
			return this->get(*pointer);
		}
		// create null instance (potentially erasing annotations)
		return R<const S>(NULL);
	}

	template<typename InIter, typename OutIter>
	void getAll(InIter start, InIter end, OutIter out) {
		// NOTE: by any reason, std::transform is not working (functions_test.h)
		// FIXME: figure out why this is not working
		for (; start != end; ++start) {
			const typename std::iterator_traits<InIter>::value_type& cur = *start;
			*out = this->get(cur);
		}

//		std::transform(start, end, out,
//			[&](const typename std::iterator_traits<InIter>::value_type& cur) {
//				return this->get(cur);
//		});
	}

	/**
	 * Obtains references to all the elements within the given container and returns
	 * the resulting list within a new container of the same type. The order of elements
	 * is thereby preserved. In case elements a not present within this container, the
	 * corresponding instances will be added.
	 *
	 * @param container the container of references to be added.
	 * @return a container of the same size including references to the master copies
	 * 		   of the handed in elements.
	 */
	template<typename Container>
	Container getAll(const Container& container) {
		Container res;
		getAll(container.cbegin(), container.cend(), inserter(res, res.end()));
		return res;
	}

	/**
	 * Looks up the given instance within this manager. If found, a corresponding
	 * pointer will be returned. Otherwise, the retrieved pointer will point to NULL.
	 * Unlike the get methods, lookups will never change the content of the instance manager.
	 *
	 * @tparam S the type of the instance to be looked up (has to be a sub-type of the managed type)
	 * @param instance the instance to be looked up
	 * @return a pointer to the internally maintained copy of the corresponding object or a NULL pointer if not found.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, R<const S>>::type lookup(const S* instance) const {
		// test whether there is already an identical element
		auto res = storage.find(instance);
		if (res != storage.end()) {
			// use included element
			return R<const S>(dynamic_cast<const S*>(*res));
		}

		// not found => return null
		return R<const S>(NULL);
	}

	/**
	 * Looks up the given instance within this manager. If found, a corresponding
	 * pointer will be returned. Otherwise, the retrieved pointer will point to NULL.
	 * Unlike the get methods, lookups will never change the content of the instance manager.
	 *
	 * @tparam S the type of the instance to be looked up (has to be a sub-type of the managed type)
	 * @param instance the instance to be looked up
	 * @return a pointer to the internally maintained copy of the corresponding object or a NULL pointer if not found.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, R<const S>>::type lookup(const S& instance) const {
		return lookup(&instance);
	}

	/**
	 * Looks up the element referenced by the given pointer within this manager. If found, a corresponding
	 * pointer will be returned. Otherwise, the retrieved pointer will point to NULL.
	 * Unlike the get methods, lookups will never change the content of the instance manager.
	 *
	 * @tparam S the type of the instance to be looked up (has to be a sub-type of the managed type)
	 * @param instance the instance to be looked up
	 * @return a pointer to the internally maintained copy of the corresponding object or a NULL pointer if not found.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, R<const S>>::type lookup(const R<const S>& pointer) const {
		if (!!pointer) {
			return this->lookup(*pointer);
		}
		// create null instance (potentially erasing annotations)
		return R<const S>(NULL);
	}

	/**
	 * Looks up all the references within the given range and writs the results to the given
	 * output iterator.
	 *
	 * @param start the iterator pointing to the start of the range
	 * @param end the iterator pointing to the end of the range
	 * @param out the output iterator used to output results
	 */
	template<typename InIter, typename OutIter>
	void lookupAll(InIter start, InIter end, OutIter out) const {
		// NOTE: by any reason, std::transform is not working (functions_test.h)
		// FIXME: figure out why std::transform is not working
		for (; start != end; ++start) {
			const typename std::iterator_traits<InIter>::value_type& cur = *start;
			*out = this->lookup(cur);
		}
	}

	/**
	 * Obtains references to all the elements within the given container and returns
	 * the resulting list within a new container of the same type. The order of elements
	 * is thereby preserved.
	 *
	 * @param container the container of references to be looked up.
	 * @return a container of the same size including references to the master copies
	 * 		   of the handed in elements.
	 */
	template<typename Container>
	Container lookupAll(const Container& container) const {
		Container res;
		lookupAll(container.cbegin(), container.cend(), inserter(res, res.end()));
		return res;
	}

	/**
	 * Checks whether a clone or the referenced element itself is present within
	 * this instance manager.
	 *
	 * @tparam S the type of the element to be looking for
	 * @param element the element to be looking for
	 * @return true if present, false otherwise
	 */
	template<class S>
	bool contains(const S* element) const {
		return element==NULL || storage.find(element) != storage.cend();
	}

	/**
	 * Checks whether a clone or the referenced element itself is present within
	 * this instance manager.
	 *
	 * @tparam S the type of the element to be looking for
	 * @param element the element to be looking for
	 * @return true if present, false otherwise
	 */
	template<class S>
	bool contains(const S& element) const {
		return contains(&element);
	}

	/**
	 * Checks whether the element referenced by the given pointer or
	 * a clone of it is present within this instance manager.
	 *
	 * @tparam S the type of the element to be looking for
	 * @param ref a reference to the element to be checked
	 * @return true if present, false otherwise
	 */
	template<class S>
	bool contains(const R<S>& ref) const {
		// contained if NULL or actually contained
		return !ref || contains(*ref);
	}

	/**
	 * Checks whether all references within the given range are referencing elements
	 * for which a copy/clone is present within this manager.
	 *
	 * @param start the start of the range
	 * @param end the end of the range
	 * @return true if all are present, false otherwise
	 */
	template<typename InIter>
	bool containsAll(InIter start, InIter end) const {
		typedef typename std::iterator_traits<InIter>::value_type Element;
		return all(start, end, [&](const Element& cur) { return this->contains(cur); });
	}

	/**
	 * Checks whether all the instance pointer within the given vector are pointing
	 * to elements maintained by this manager. This method is mainly intended for assertions
	 * and test cases and should not be required within release code.
	 *
	 * @param pointers the list of pointers to be checked
	 * @return true if all are referencing to elements within this manager, false otherwise
	 */
	template<class S>
	bool containsAll(const vector<R<S>>& pointers) const {
		return containsAll(pointers.cbegin(), pointers.cend());
	}

	/**
	 * Checks whether the given reference is addressing a locally maintained
	 * data element.
	 *
	 * NOTE: unlike the contain functions, it is checking not only the value,
	 * also the location within the memory.
	 *
	 * @tparam S the type of element referenced by the given pointer
	 * @param ptr the pointer which should be tested
	 * @return true if it is referencing a locally maintained element,
	 * 		   false otherwise.
	 */
	template<class S>
	bool addressesLocal(const R<S>& ptr) const {
		// NULL pointer is always local
		if (!ptr) {
			return true;
		}

		// check whether a corresponding element is present
		auto local = storage.find(&*ptr);
		if (local == storage.cend()) {
			// not present => not local
			return false;
		}

		// compare pointer (need to point to same location)
		return &*ptr == *local;
	}

	/**
	 * Checks whether all the pointers within the given range are referencing elements
	 * managed by this manager.
	 *
	 * @param start the start of the range
	 * @param end the end of the range
	 * @return true if all are present, false otherwise
	 */
	template<typename InIter>
	bool addressesLocalAll(InIter start, InIter end) const {
		typedef typename std::iterator_traits<InIter>::value_type Element;
		return all(start, end, [&](const Element& cur) { return this->addressesLocal(cur); });
	}

	/**
	 * Checks whether all the instance pointer within the given vector are pointing
	 * to elements managed by this manager.
	 *
	 * @param pointers the list of pointers to be checked
	 * @return true if all are referencing to elements within this manager, false otherwise
	 */
	template<class S>
	bool addressesLocalAll(const vector<R<S>>& pointers) const {
		return addressesLocalAll(pointers.cbegin(), pointers.cend());
	}

	/**
	 * Retrieves the number of elements handled by this instance manager.
	 *
	 * @return the total number of elements currently managed
	 */
	int size() const {
		return storage.size();
	}
};


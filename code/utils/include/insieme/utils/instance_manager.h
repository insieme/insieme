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

#include <iostream>

#include <boost/functional/hash.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_const.hpp>
#include <boost/utility/enable_if.hpp>
#include <boost/utility.hpp>
#include <boost/iterator/transform_iterator.hpp>

#include "insieme/utils/pointer.h"
#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/utils/unused.h"

/**
 * A functor imposing no default clone action.
 */
template<typename T>
struct no_post_add_action {
	void operator()(const T* original, const T* copy) const { /* do nothing */ }

};

/**
 * An instance manager is capable of handling a set of instances of a generic type T. Instances
 * representing the same value are shared. Hence, to avoid altering the instances referenced by
 * others, the handled types have to be constant (which is enforced).
 *
 * Instance managers can be chained to form hierarchies of sharing domains. The derived manager
 * extends the base manager and thereby "inherits" all elements.
 *
 * @tparam T the type of elements managed by the concrete manager instance. The type has to
 * 			 be a constant type.
 */
template<
	typename T,
	template<class C> class R = Ptr,
	typename PostAddAction = no_post_add_action<T>,
	typename boost::enable_if<boost::is_base_of<Ptr<T>, R<T> >,int>::type = 0
	>
class InstanceManager : private boost::noncopyable {

	/**
	 * The type of storage used internally.
	 */
	typedef std::unordered_set<const T*, hash_target<const T*>, equal_target<const T*>> storage_type;

	/**
	 * The storage used to maintain instances. It is based on a unordered set which
	 * is modified to support operations based on pointers. All the elements stored
	 * within this set will be automatically deleted when this instance manager instance
	 * is destroyed.
	 */
	storage_type storage;

	/**
	 * The base manager of this manager, null if not present. This field is realizing
	 * the instance manager chaining. Instances within the base manager are considered
	 * to be covered by this manager.
	 */
	InstanceManager* base;

	/**
	 * A private method used to clone instances to be managed by this type.
	 *
	 * @tparam S the type of the the instance to be cloned (the same type will be returned)
	 * @param instance a pointer to the instance to be cloned
	 * @return a pointer to a clone of the given instance of the same type
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>,const S*>::type clone(const S* instance) {
		//  step 1 - cast to base type (since only this one allows us to clone it)
		const T* orig = instance;
		//  step 2 - clone
		const T* clone = orig->cloneTo(*static_cast<typename S::Manager*>(this));

		// make sure clone is valid
//		assert( hash_value(*instance) == hash_value(*clone) && "Incorrect hash value of clone!" );
//		assert( orig != clone && "Not realy cloned!");
//		assert( *orig == *clone && "Clone not equivalent to original!" );

		// step 2 - cast back to original type
		return static_cast<const S*>(clone);
	}

public:

	/**
	 * The default constructor initializing an empty instance manager outside any
	 * inheritance hierarchy.
	 */
	InstanceManager() : base(0) {}

	/**
	 * A constructor creating an instance manager extending the given manager. The life
	 * cycle of the given manager has to be at least as long as the life cycle of the newly
	 * constructed manager.
	 */
	explicit InstanceManager(InstanceManager& manager) : base(&manager) {}

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
	 * Obtains a pointer to the base manager this instance manager is linked to.
	 *
	 * @return a pointer to the base manager this manager is linked to; Null if there is no such manager
	 */
	InstanceManager* getBaseManager() const {
		return base;
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
		static const PostAddAction postAddAction;

		assert ( instance && "Instance must not be NULL!");

		// test whether there is already an identical element
		auto res = lookupPlain(instance);
		if (res) {
			// use included element
			return std::make_pair(R<const S>(dynamic_cast<const S*>(res)), false);
		}

		// clone element (to ensure private copy)
		const S* newElement = clone(instance);
		__unused auto check = storage.insert(newElement);

		// ensure this is a clone
		assert ( instance != newElement );

		// ensure the element has really been added (hash and equals is properly implemented)
		assert ( *check.first == newElement || check.second );

		// ensure the element can be found again
		assert ( check.first == storage.find(instance) && "Unable to add clone - value already present!" );

		// apply post-insert action
		postAddAction(instance, newElement);

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
	 * @param pointer a pointer to the instance to be looking for within this instance manager.
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
		// just convert one by one
		typedef typename std::iterator_traits<InIter>::value_type Element;
		std::transform(start, end, out, [&](const Element& cur) {
			return this->get(cur);
		});
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
	 * Looks up the given instance within this manager (including its base manager). If found, a
	 * pointer referencing the instance will be returned. Otherwise the retrieved pointer will point
	 * to NULL. Unlike the get methods, lookups will never change the content of the instance manager.
	 *
	 * @tparam S the type of the instance to be looked up (has to be a sub-type of the managed type)
	 * @param instance the instance to be looked up
	 * @return a pointer to the internally maintained copy of the corresponding object or a NULL pointer if not found.
	 */
	template<class S>
	typename boost::enable_if<boost::is_base_of<T,S>, const S*>::type lookupPlain(const S* instance) const {

		// first, check whether there is an instance within the base manager
		if (base) {
			auto res = base->lookupPlain(instance);
			if (res) { return res; }
		}

		// check local storage
		auto res = storage.find(instance);
		if (res != storage.end()) {
			// found locally
			return static_cast<const S*>(*res);
		}


		// not found => return null
		return NULL;
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
		// just wrap result of general lookup implementation
		return R<const S>(lookupPlain(instance));
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
	 * @param pointer a pointer to the instance to be looked up
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
		return element==NULL || storage.find(element) != storage.cend() || (base && base->contains(element));
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
	std::size_t size() const {
		return storage.size();
	}

	// --- offer an iterator over all elements within this instance manager ---

private:

	/**
	 * A functor required for realizing a transforming iterator. This functor
	 * is simply wrapping a pointer to some internally maintained data element
	 * into a pointer instances as it is expected to be offered by this instance
	 * manager.
	 */
	struct PtrWrapper : public std::unary_function<const T*, R<const T>> {
		R<const T> operator()(const T* ptr) const { return R<const T>(ptr); }
	};

public:

	/**
	 * The type of constant iterator offered by an instance manager to iterate over
	 * all contained elements.
	 */
	typedef boost::transform_iterator<PtrWrapper,typename storage_type::const_iterator> const_iterator;

	/**
	 * Obtains an iterator referencing the first element maintained by this instance
	 * manager. There is no guarantee of any order of the contained elements.
	 *
	 * The range obtained by [begin(),end()) is containing all the elements maintained
	 * by this instance manager.
	 *
	 * @return a reference to the first element stored internally
	 */
	const_iterator begin() const {
		return boost::make_transform_iterator<PtrWrapper>(storage.begin());
	}

	/**
	 * Obtains an iterator referencing the end element maintained by this instance
	 * manager. There is no guarantee of any order of the contained elements.
	 *
	 * The range obtained by [begin(),end()) is containing all the elements maintained
	 * by this instance manager.
	 *
	 * @return a reference to the end element referencing the end of the internally stored nodes
	 */
	const_iterator end() const {
		return boost::make_transform_iterator<PtrWrapper>(storage.end());
	}

};


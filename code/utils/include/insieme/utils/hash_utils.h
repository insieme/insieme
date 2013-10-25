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

#include <boost/functional/hash.hpp>

#include <boost/utility/enable_if.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_convertible.hpp>

#include <insieme/utils/functional_utils.h>

namespace insieme {
namespace utils {

/**
 * This marker class is indicating that classes extending have a hash function to be utilized
 * for hashing operation.
 */
struct Hashable {

	// This signature (or equivalent) is expected to be implemented.
//	std::size_t hash() const { ... };

};

/**
 * Integrates the hash code computation for nodes into the boost hash code framework.
 *
 * @param instance the instance for which a hash code should be obtained.
 * @return the hash code of the given instance
 */
template<typename T>
typename std::enable_if<std::is_base_of<Hashable, T>::value, std::size_t>::type
hash_value(const T& instance) {
	return instance.hash();
}

/**
 * A base class for hashing mutable data objects.
 */
template<class Derived>
class HashableMutableData {

	/**
	 * A flag indicating whether the currently stored hash code is valid.
	 */
	mutable bool accurate;

	/**
	 * A
	 */
	std::size_t hashCode;

public:

	HashableMutableData() : accurate(false) {}

	void invalidateHash() {
		accurate = false;
	}

	std::size_t hash() const {
		if (!accurate) {
			hashCode = static_cast<const Derived*>(this)->updateHash();
			accurate = true;
		}
		return hashCode;
	}

};

/**
 * A base type of plain, immutable data elements which should be maintained within a hash
 * based container.
 *
 * It provides efficient implementations of hash functions and comparison operations. Further
 * it provides means for using instances of this type within unordered sets derived from the
 * boost or std library.
 */
template<class Derived>
class HashableImmutableData : public Hashable {

	/**
	 * The hash value of this data element derived once during its construction. This value will be required
	 * frequently, hence evaluating it once and reusing it helps reducing computational overhead. Since
	 * instances are immutable, the hash does not have to be altered after the creation of a instance.
	 */
	std::size_t hashCode;

protected:

//	/**
//	 * A hooker method to be implemented by sub-classes to compare instances with other
//	 * node instances.
//	 *
//	 * @param other the instance to be compared to. The handed in element will already be checked for
//	 * 				identity and its hash value. Hence, simple checks may be omitted within
//	 * 				the implementation of this method.
//	 * @return true if equivalent, false otherwise.
//	 */
//	bool equals(const Derived& other) const = 0;

public:

	/**
	 * Creates a new instance of this base type build upon the given hash code.
	 *
	 * @param hashCode the hash code of this immutable hashable data element
	 */
	HashableImmutableData(std::size_t hashCode) : hashCode(hashCode) {};

	/**
	 * Computes a hash code for this node. The actual computation has to be conducted by
	 * subclasses. This class will only return the hash code passed on through the constructor.
	 *
	 * Note: this function is not virtual, so it should not be overridden in sub-classes.
	 *
	 * @return the hash code derived for this type.
	 */
	std::size_t hash() const {
		// retrieve cached hash code
		return hashCode;
	}


	/**
	 * Compares two instances of this type by checking for their identity (same
	 * memory location) and hash code. If those tests are not conclusive, the virtual
	 * function equals will be invoked.
	 */
	bool operator==(const Derived& other) const {
		// test for identity
		if (this == &other) {
			return true;
		}

		// fast hash code test
		if (hashCode != other.hash()) {
			return false;
		}

		// use virtual equals method
		return static_cast<const Derived*>(this)->equals(other);
	}

	/**
	 * Implementing the not-equal operator for AST nodes by negating the equals
	 * operator.
	 */
	bool operator!=(const Derived& other) const {
		return !(*this == other);
	}
};

/**
 * An extended version of the hashable immutable data object for virtual classes.
 */
template<typename Derived>
class VirtualHashableImmutableData : public HashableImmutableData<Derived> {
protected:

	/**
	 * Creates a new instance of this base type build upon the given hash code.
	 *
	 * @param hashCode the hash code of this immutable hashable data element
	 */
	VirtualHashableImmutableData(std::size_t hashCode) : HashableImmutableData<Derived>(hashCode) {};

public:

	/**
	 * A hooker method to be implemented by sub-classes to compare instances with other
	 * node instances.
	 *
	 * @param other the instance to be compared to. The handed in element will already be checked for
	 * 				identity and its hash value. Hence, simple checks may be omitted within
	 * 				the implementation of this method.
	 * @return true if equivalent, false otherwise.
	 */
	virtual bool equals(const Derived& other) const = 0;
};

/**
 * The terminal case for the hash combine operation (where no values are left).
 *
 * @param seed the seed to which no additional hash value should be appended to
 * @return the resulting hash value (the handed in seed)
 */
inline std::size_t appendHash(std::size_t& seed) {
	// nothing to do
	return seed;
}

/**
 * An alternative terminal case for the hash append function accepting specified
 * template parameters.
 */
template<
	template <typename H> class Extractor = id
>
inline std::size_t appendHash(std::size_t& seed) {
	// nothing to do
	return seed;
}

/**
 * The generic implementation of the hash combine operation.
 * @param seed the hash seed to which the hash values of the given arguments should be appended to
 * @param first the first of the elements to be hashed and appended
 * @param rest the remaining elements to be hashed and appended
 * @return the resulting hash value
 */
template<
	template <typename H> class Extractor = id,
	typename T,
	typename... Args
>
inline std::size_t appendHash(std::size_t& seed, const T& first, const Args&... rest) {
	static Extractor<T> ext;
	boost::hash_combine(seed, ext(first));
	return appendHash<Extractor>(seed, rest...);
}


/**
 * The terminal case for the variadic template based implementation of the combineHashes function.
 *
 * @return the initial hash seed corresponding to an empty tuple (0)
 */
inline std::size_t combineHashes() {
	return 0;
}

/**
 * A generic hash combining function computing a hash value for the given list of elements.
 *
 * @param first the first of the elements to be hashed
 * @param rest the remaining elements to be hashed
 * @return the resulting hash value
 */
template<
	template <typename H> class Extractor = id,
	typename T,
	typename ... Args
>
inline std::size_t combineHashes(const T& first, const Args&... rest) {
	// initialize hash seed
	std::size_t seed = 0;

	// append all the hash values
	appendHash<Extractor>(seed, first, rest...);
	return seed;
}

// --------------------------------------------------------------------------------------------------------------
// 												Hashing Containers
// --------------------------------------------------------------------------------------------------------------

/**
 * This functor can be used to print elements to an output stream.
 */
template<typename Extractor>
struct hash : public std::unary_function<const typename Extractor::argument_type&, std::size_t> {
	Extractor extractor;
	std::size_t operator()(const typename Extractor::argument_type& cur) const {
		boost::hash<typename Extractor::argument_type> hasher;
		return hasher(cur);
	}
};

/**
 * A generic method capable of computing a hash value for a ordered container (list, trees, ...).
 *
 * @param container the container for which a hash value should be computed
 * @return the computed hash value
 */
template<typename Container>
std::size_t hashList(const Container& container) {
	typedef typename Container::value_type Element;
	return hashList(container, hash<id<const Element&>>());
}

/**
 * A generic method capable of computing a hash value for a ordered container (list, trees, ...).
 *
 * @param container the container for which a hash value should be computed
 * @param hasher the hasher used for deriving a hash value for each element within the container
 * @return the computed hash value
 */
template<typename Container, typename Hasher>
std::size_t hashList(const Container& container, Hasher hasher) {
	typedef typename Container::value_type Element;

	std::size_t seed = 0;
	hashList(seed, container, hasher);
	return seed;
}

/**
 * A generic method capable of computing a hash value for a ordered container (list, trees, ...).
 *
 * @param seed the hash value to be manipulated by introducing the elements of the list
 * @param container the container for which a hash value should be computed
 * @param hasher the hasher used for deriving a hash value for each element within the container
 */
template<typename Container, typename Hasher>
void hashList(std::size_t& seed, const Container& container, Hasher hasher) {
	typedef typename Container::value_type Element;

	// combine hashes of all elements within the container
	std::for_each(container.begin(), container.end(), [&](const Element& cur) {
		boost::hash_combine(seed, hasher(cur));
	});
}

} // end namespace utils
} // end namespace insieme

namespace std
{

	/**
	 * Integrates the exposition of hashable immutable data into the std::hash infrastructure.
	 *
	 * @param instance the instance for which a hash code should be obtained.
	 * @return the hash code of the given instance
	 */
	template<>
	struct hash<insieme::utils::Hashable> {
		template<typename T>
		size_t operator()(const T& instance) const {
			return instance.hash();
		}
	};


	/**
	 * Add hash support for pairs.
	 */
	template<typename A, typename B>
	struct hash<pair<A,B>> {
		size_t operator()(const pair<A,B>& instance) const {
			return boost::hash_value(instance); // bridge to boost solution
		}
	};

	/**
	 * Add hash support for tuples.
	 */
	template<typename ... R>
	struct hash<tuple<R...>> {
		size_t operator()(const tuple<R...>& instance) const {
			return boost::hash_value(instance);
		}
	};

	/**
	 * Add hash support for vectors.
	 */
	template<typename T, typename A>
	struct hash<vector<T,A>> {
		size_t operator()(const vector<T,A>& instance) const {
			return boost::hash_value(instance); // bridge to boost solution
		}
	};


	/**
	 * Add hash support for vectors.
	 */
	template<typename K, typename C, typename A>
	struct hash<set<K,C,A>> {
		size_t operator()(const set<K,C,A>& instance) const {
			return boost::hash_value(instance); // bridge to boost solution
		}
	};


	// --------- support for shared pointer in boost -------------

	template<typename T> class shared_ptr;

	template<typename T>
	size_t hash_value(const shared_ptr<T>& ptr) {
		// forward call to std-shared ptr
		return hash<shared_ptr<T>>()(ptr);
	}

}



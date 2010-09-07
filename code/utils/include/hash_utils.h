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

namespace insieme {
namespace utils {

/**
 * A base type of plain, immutable data elements which should be maintained within a hash
 * based container.
 *
 * It provides efficient implementations of hash functions and comparison operations. Further
 * it provides means for using instances of this type within unordered sets derived from the
 * boost or std library.
 */
template<class Derived>
class HashableImmutableData {

protected:

	/**
	 * The hash value of this data element derived once during its construction. This value will be required
	 * frequently, hence evaluating it once and reusing it helps reducing computational overhead. Since
	 * instances are immutable, the hash does not have to be altered after the creation of a instance.
	 */
	const std::size_t hashCode;

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

public:

	/**
	 * Creates a new instance of this base type build upon the given hash code.
	 *
	 * @param hashCode the hash code of this immutable hashable data element
	 */
	HashableImmutableData(std::size_t hashCode) : hashCode(hashCode) {};

	/**
	 * A virtual destructor to clean up objects properly after usage.
	 */
	virtual ~HashableImmutableData() {};

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
		if (hashCode != other.hashCode) {
			return false;
		}

		// use virtual equals method
		return equals(other);
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
 * Integrates the hash code computation for nodes into the boost hash code framework.
 *
 * @param instance the instance for which a hash code should be obtained.
 * @return the hash code of the given instance
 */
template<typename Derived>
inline std::size_t hash_value(const insieme::utils::HashableImmutableData<Derived>& instance) {
	return instance.hash();
}

} // end namespace utils
} // end namespace insieme

namespace std
{
	/**
	 * A specialization of the std-hash struct to be used for realizing hashing of
	 * HashableImmutableData within the std-based hashing data structures.
	 */
	template <typename Derived>
	struct hash<insieme::utils::HashableImmutableData<Derived>> {
		size_t operator()(const insieme::utils::HashableImmutableData<Derived>& instance) {
			return instance.hash();
		}
	};
}



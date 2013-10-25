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
#include <set>
#include <utility>
#include <vector>
#include <memory>
#include <typeindex>

#include <boost/operators.hpp>

#include "insieme/core/ir.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/hash_utils.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * 		D ::= { E* }
	 * 		E ::= A | { [I,D]* }
	 *
	 * D ... Data
	 * E ... Elements
	 * A ... Atomic Values
	 * I ... Index Values
	 */


	// some forward declarations
	template<typename ElementType> class DataManager;
	template<typename ElementType> class Set;
	template<typename ElementType> class Element;


	// some forward declarations of operations

	template<typename ElementType>
	Set<ElementType> setUnion(const Set<ElementType>& a, const Set<ElementType>& b);

	template<typename ElementType>
	Set<ElementType> setIntersect(const Set<ElementType>& a, const Set<ElementType>& b);

//	template<typename ElementType>
//	Set<ElementType> setDiff(const Set<ElementType>& a, const Set<ElementType>& b);

	template<typename ElementType>
	bool empty(const Set<ElementType>& a);

//	template<typename ElementType>
//	bool isSubset(const Set<ElementType>& sub, const Set<ElementType>& super);
//
//	template<typename ElementType>
//	bool isEqual(const Set<ElementType>& a, const Set<ElementType>& b);

	// -------------------------------------------------------------------------------------------------

	template<typename ElementType>
	bool isMember(const Set<ElementType>& a, const Element<ElementType>& b);

	// -------------------------------------------------------------------------------------------------


	namespace {

		template<typename ElementType> struct SetEntry;
		template<typename ElementType> struct ElementEntry;

		namespace {

			template<typename ElementType>
			std::size_t hash_set(const std::set<Element<ElementType>>& data) {
				std::size_t hash = 0;
				for(const auto& cur : data) {
					hash += cur.hash();
				}
				return hash;
			}

			template<typename IndexType, typename ElementType>
			std::size_t hash_map(const std::map<IndexType, Set<ElementType>>& map) {
				std::size_t hash = 0;
				for(const std::pair<IndexType, Set<ElementType>>& cur : map) {
					hash += utils::combineHashes(cur.first, cur.second);
				}
				return hash;
			}

		}

		template<typename ElementType, typename DerivedType>
		struct Entry :
				public boost::equality_comparable<DerivedType>,
				public utils::VirtualPrintable {

			DataManager<ElementType>& mgr;
			const std::size_t hash;
			mutable std::size_t equality_class;

			Entry(DataManager<ElementType>& mgr, std::size_t hash) : mgr(mgr), hash(hash), equality_class(0) {}
			virtual ~Entry() {};

		};

		template<typename ElementType, typename DerivedType>
		inline std::size_t hash_value(const Entry<ElementType, DerivedType>& entry) {
			return entry.hash;
		}

		template<typename ElementType>
		struct SetEntry : public Entry<ElementType, SetEntry<ElementType>> {

			typedef std::set<Element<ElementType>> set_type;
			set_type data;

			SetEntry(DataManager<ElementType>& mgr, const set_type& data)
				: Entry<ElementType, SetEntry<ElementType>>(mgr, hash_set(data)), data(data) {
				assert(!data.empty());
				assert(!any(data, [](const Element<ElementType>& cur) { return cur.empty(); }));
			}

			SetEntry(DataManager<ElementType>& mgr, std::size_t hash, const set_type& data)
				: Entry<ElementType, SetEntry<ElementType>>(mgr, hash), data(data) {
				assert_eq(hash, hash_set(data)) << "Hashes not matching!";
				assert(!data.empty());
				assert(!any(data, [](const Element<ElementType>& cur) { return cur.empty(); }));
			}

			virtual ~SetEntry() {};

			bool isSingle() const {
				return data.size() == 1u && data.begin()->isSingle();
			}

			bool operator==(const SetEntry<ElementType>& other) const {
				// important: only structural equivalence
				return this == &other || (this->hash == other.hash && data == other.data);
			}

			bool operator==(const set_type& other) const {
				return data == other;
			}

			bool operator<(const SetEntry<ElementType>& other) const {
				return this != &other && data < other.data;
			}

		protected:

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << data;
			}

		};

		template<typename ElementType>
		struct ElementEntry : public Entry<ElementType, ElementEntry<ElementType>> {

			ElementEntry(DataManager<ElementType>& mgr, std::size_t hash)
				: Entry<ElementType, ElementEntry<ElementType>>(mgr, hash) {}

			// the equality operator is operating on the structural equality - equal to the hash
			virtual bool operator==(const ElementEntry<ElementType>& other) const =0;

			// a less-than operator regarding the structural equality - compatible to the hash and == operator
			virtual bool operator<(const ElementEntry<ElementType>& other) const =0;

			// check whether this element represents an empty cross-product
			virtual bool empty() const =0;

			// check whether this element is representing a "single" element - hence it can not be separated into multiple elements
			virtual bool isSingle() const =0;

			// check whether the set represented by this element contains the given element
			virtual bool contains(const ElementEntry<ElementType>& other) const =0;

			// implement the intersection between two element entries
			virtual const ElementEntry<ElementType>* intersect(const ElementEntry<ElementType>& other) const =0;

			// implement the set difference between two element entries (which needs to be a set of elements)
			virtual std::vector<Element<ElementType>> diff(const ElementEntry<ElementType>& other) const =0;
		};

		template<typename ElementType>
		struct AtomicEntry : public ElementEntry<ElementType> {

			const ElementType data;

			AtomicEntry(DataManager<ElementType>& mgr, const ElementType& element)
				: ElementEntry<ElementType>(mgr, std::hash<ElementType>()(element)), data(element) {}

			AtomicEntry(DataManager<ElementType>& mgr, std::size_t hash, const ElementType& element)
				: ElementEntry<ElementType>(mgr, hash), data(element) {
				assert_eq(hash, std::hash<ElementType>()(element)) << "Hashes not matching!";
			}

			virtual ~AtomicEntry() {}

			virtual bool operator==(const ElementEntry<ElementType>& other) const {
				// same type, same data
				return this == &other || (typeid(*this) == typeid(other) && data == static_cast<const AtomicEntry<ElementType>&>(other).data);
			}

			bool operator==(const ElementType& other) const {
				return data == other;
			}

			virtual bool operator<(const ElementEntry<ElementType>& other) const {
				return this != &other && data < cast(other).data;
			}

			virtual bool empty() const {
				return false;
			}

			virtual bool isSingle() const {
				return true;	// atomic elements are singles by definition
			}

			virtual bool contains(const ElementEntry<ElementType>& other) const {
				return *this == other;	// here just an equality comparison is necessary
			}

			virtual const ElementEntry<ElementType>* intersect(const ElementEntry<ElementType>& other) const {
				// unless equal, the result is empty
				return (*this == other) ? this : nullptr;
			}

			virtual std::vector<Element<ElementType>> diff(const ElementEntry<ElementType>& other) const {
				// for atomic elements it is pretty simple
				std::vector<Element<ElementType>> res;
				if (*this != other) res.push_back(this);
				return res;
			}

		protected:

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << data;
			}

		private:

			const AtomicEntry<ElementType>& cast(const ElementEntry<ElementType>& other) const {
				assert(dynamic_cast<const AtomicEntry<ElementType>*>(&other));
				return static_cast<const AtomicEntry<ElementType>&>(other);
			}

		};

		template<typename IndexType, typename ElementType>
		struct CompoundEntry : public ElementEntry<ElementType> {

			typedef std::map<IndexType, Set<ElementType>> map_type;
			const map_type data;

			CompoundEntry(DataManager<ElementType>& mgr, const map_type& data)
				: ElementEntry<ElementType>(mgr, hash_map(data)), data(data) {}

			CompoundEntry(DataManager<ElementType>& mgr, std::size_t hash, const map_type& data)
				: ElementEntry<ElementType>(mgr, hash), data(data) {
				assert_eq(hash, hash_map(data)) << "Externally computed is invalid!";
			}

			virtual ~CompoundEntry() {}

			virtual bool operator==(const ElementEntry<ElementType>& other) const {
				// same type, same content
				return this == &other || (typeid(*this) == typeid(other) && data == static_cast<const CompoundEntry<IndexType, ElementType>&>(other).data);
			}

			bool operator==(const map_type& other) const {
				return data == other;
			}

			virtual bool operator<(const ElementEntry<ElementType>& other) const {
				return this != &other && data < cast(other).data;
			}

			virtual bool empty() const {
				return data.empty() || any(data, [](const typename map_type::value_type& cur) { return cur.second.empty(); });
			}

			virtual bool isSingle() const {
				// it is a single if all sets are singles
				return !empty() && all(data, [](const typename map_type::value_type& cur) { return cur.second.isSingle(); });
			}

			virtual bool contains(const ElementEntry<ElementType>& other) const {
				// the other element has to be of the same type
				assert_true((dynamic_cast<const CompoundEntry<IndexType, ElementType>*>(&other)));

				// cast type
				const CompoundEntry<IndexType, ElementType>& b = static_cast<const CompoundEntry<IndexType, ElementType>&>(other);

				// all fields must be contained
				for(const auto& cur : b.data) {
					// get values of of same field in current set
					const Set<ElementType>& s = extract(data, cur.first);
					if (!isMember(s, cur.second.getSingleEntry())) return false;
				}

				// it is contained
				return true;
			}

			virtual const ElementEntry<ElementType>* intersect(const ElementEntry<ElementType>& other) const {
				const static ElementEntry<ElementType>* empty = nullptr;
				// the other element has to be of the same type
				assert_true((dynamic_cast<const CompoundEntry<IndexType, ElementType>*>(&other)));

				// check equality
				if (*this == other) return this;

				// cast type
				const CompoundEntry<IndexType, ElementType>& b = static_cast<const CompoundEntry<IndexType, ElementType>&>(other);

				// intersect all elements
				std::map<IndexType, Set<ElementType>> res;
				for(const IndexType& cur : cross(data, b.data)) {

					// extract sets of both sides and intersect those
					const Set<ElementType>& sA = extract(data, cur);
					const Set<ElementType>& sB = extract(b.data, cur);

					// intersect element sets
					Set<ElementType> r = setIntersect(sA, sB);

					// check whether it is empty (we can stop in this case)
					if (r.empty()) return empty;
					res[cur] = r;
				}

				// build result
				return this->mgr.compound(res).getPtr();
			}

			virtual std::vector<Element<ElementType>> diff(const ElementEntry<ElementType>& other) const {

				assert_not_implemented() << "This operation is not supported!";
				return std::vector<Element<ElementType>>();

//				// start with empty result
//				std::vector<Element<ElementType>> res;
//
//				// check equality
//				if (*this == other) return res;	// an empty list
//
//				// check empty cases
//				if (empty() || other.empty()) {
//					res.push_back(this);
//					return res;
//				}
//
//				// the other element has to be of the same type
//				assert_true((dynamic_cast<const CompoundEntry<IndexType, ElementType>*>(&other)));
//
//				// cast type
//				const CompoundEntry<IndexType, ElementType>& b = static_cast<const CompoundEntry<IndexType, ElementType>&>(other);
//
//				// create a copy with the full index space
//				std::map<IndexType, Set<ElementType>> full;
//				auto indices = cross(data, b.data);
//				for(const IndexType& cur : indices) {
//					full[cur] = setUnion(extract(data, cur));
//				}
//
//				// now create one copy where each indexed element is reduced once
//				for(const IndexType& cur : cross(data, b.data)) {
//
//					// extract sets of both sides and intersect those
//					Set<ElementType> sA = setUnion(extract(data, cur));
//					Set<ElementType> sB = setUnion(extract(b.data, cur));
//
//					// intersect element sets
//					Set<ElementType> r = setDiff(sA, sB);
//
//					// if result is empty => done
//					if (r.empty()) continue;
//
//					// insert current copy
//					Set<ElementType> old = full[cur];
//					full[cur] = r;
//					res.push_back(this->mgr.compound(full));
//					full[cur] = old;
//				}
//
//				// done
//				return res;
			}

		protected:

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << "[" << join(",", data, [](std::ostream& out, const typename map_type::value_type& cur) {
					out << cur.first << "=" << cur.second;
				}) << "]";
			}

		private:

			const CompoundEntry<IndexType,ElementType>& cast(const ElementEntry<ElementType>& other) const {
				assert((dynamic_cast<const CompoundEntry<IndexType,ElementType>*>(&other)));
				return static_cast<const CompoundEntry<IndexType,ElementType>&>(other);
			}

		};
	}

	template<typename IndexType, typename ElementType>
	std::pair<IndexType, Set<ElementType>> entry(const IndexType& i, const Set<ElementType>& e) {
		return std::make_pair(i, e);
	}

	template<typename ElementType>
	class DataManager {

		typedef SetEntry<ElementType>* SetEntryPtr;
		typedef AtomicEntry<ElementType>* AtomicEntryPtr;
		typedef ElementEntry<ElementType>* CompoundEntryPtr;

		std::unordered_map<std::size_t, std::vector<SetEntryPtr>> setCache;
		std::unordered_map<std::size_t, std::vector<AtomicEntryPtr>> atomicCache;
		std::unordered_map<std::size_t, std::vector<CompoundEntryPtr>> compoundCache;

	public:

		~DataManager() {
			// free all cached elements
			for(const auto& cur : setCache) {
				for(const auto& entry : cur.second) delete entry;
			}
			for(const auto& cur : atomicCache) {
				for(const auto& entry : cur.second) delete entry;
			}
			for(const auto& cur : compoundCache) {
				for(const auto& entry : cur.second) delete entry;
			}
		}

		Set<ElementType> set(const std::set<Element<ElementType>>& elements) {

			// handle empty sets
			if (elements.empty()) return Set<ElementType>();

			// compute hash
			std::size_t hash = hash_set(elements);

			// get element list
			auto& list = setCache[hash];

			// check whether element is already present
			for(auto cur : list) {
				if (*cur == elements) return cur;
			}

			// add new element
			auto res = new SetEntry<ElementType>(*this, hash, elements);
			list.push_back(res);
			return res;
		}

		template<typename ... Elements>
		Set<ElementType> set(const Elements& ... elements) {
			return set(utils::set::toSet<std::set<Element<ElementType>>>(elements...));
		}

		Element<ElementType> atomic(const ElementType& element) {

			// compute hash
			std::size_t hash = std::hash<ElementType>()(element);

			// get element list
			auto& list = atomicCache[hash];

			// check whether element is already present
			for(auto cur : list) {
				if (*cur == element) return cur;
			}

			// add new element
			auto res = new AtomicEntry<ElementType>(*this, hash, element);
			list.push_back(res);
			return res;
		}

		template<typename IndexType>
		Element<ElementType> compound(const std::map<IndexType, Set<ElementType>>& map) {

			// compute hash
			std::size_t hash = hash_map(map);

			// get element list
			auto& list = compoundCache[hash];

			// check whether element is already present
			for(auto cur : list) {
				if (auto entry = dynamic_cast<const CompoundEntry<IndexType, ElementType>*>(cur)) {
					if (*entry == map) return cur;
				}
			}

			// add new element
			auto res = new CompoundEntry<IndexType, ElementType>(*this, hash, map);
			list.push_back(res);
			return res;
		}

		template<typename IndexType, typename ... Elements>
		Element<ElementType> compound(const std::pair<IndexType, Set<ElementType>>& first, const Elements& ... elements) {
			return compound(utils::map::toMap(first, elements ...));
		}

	};


	template<typename ElementType>
	class Set :
			public boost::equality_comparable<Set<ElementType>>,
			public boost::partially_ordered<Set<ElementType>> {

		typedef const SetEntry<ElementType>* ptr_type;
		ptr_type data;

	public:

		Set(ptr_type ptr = nullptr) : data(ptr) {}

		Set(const Element<ElementType>& cur)
			: data(cur.getPtr()->mgr.set(cur).data) {}

		ptr_type getPtr() const {
			return data;
		}

		bool operator==(const Set<ElementType>& other) const {
			return data == other.data || (data && other.data && *data == *other.data);
		}

		bool operator<(const Set<ElementType>& other) const {
			return (data) ? (*data < *other.data) : (other.data == nullptr);
		}

		bool empty() const {
			return !data;
		}

		bool isSingle() const {
			return !empty() && data->isSingle();
		}

		const Element<ElementType>& getSingleEntry() const {
			assert(isSingle());
			return *data->data.begin();
		}

		bool contains(const Element<ElementType>& a) const {
			assert(a.isSingle());
			return !empty() && any(data->data, [&](const Element<ElementType>& cur)->bool { return cur.contains(a); });
		}

		Set merge(const Set<ElementType>& other) const {

			// handle empty sets
			if (!data) return other;
			if (!other.data) return *this;

			// merge sets and return result
			std::set<Element<ElementType>> sum = data->data;
			sum.insert(other.data->data.begin(), other.data->data.end());
			return getManager().set(sum);
		}

		Set intersect(const Set<ElementType>& other) const {

			// if sets are equivalent, the result is the set itself
			if (*this == other) return other;

			// if one set is empty, the result is empty
			if (empty()) return *this;
			if (other.empty()) return other;

			// otherwise we have to compute the intersection
			//  - since the elements represent sets, we have to intersect each element of
			//	  this set with every element of the other set and collect the resulting non-empty pieces

			std::set<Element<ElementType>> res;
			for(const Element<ElementType>& a : data->data) {
				for(const Element<ElementType>& b : other.data->data) {
					Element<ElementType> i = a.intersect(b);
					if (!i.empty()) res.insert(i);
				}
			}

			// return intersected set
			return getManager().set(res);
		}

		Set diff(const Set<ElementType>& other) const {

			// handle special cases where one operator is empty
			if (empty() || other.empty()) return *this;

			// otherwise: subtract all elements of other from all elements of this set and collect the bits
			Set<ElementType> res;
			for(const Element<ElementType>& a : data->data) {
				res = setUnion(res, a.diff(other));
			}

			// return the resulting set
			return res;
		}

		Set diff(const Element<ElementType>& other) const {

			// quick check - empty
			if (empty() || other.empty()) return *this;

			std::set<Element<ElementType>> res;
			for(const Element<ElementType>& element : data->data) {
				for(const auto& fragment : element.getPtr()->diff(*other.getPtr())) {
					if (!fragment.empty()) res.insert(fragment);
				}
			}

			return getManager().set(res);
		}

		DataManager<ElementType>& getManager() const {
			return data->mgr;
		}

		std::size_t hash() const {
			return (data) ? data->hash : 0;
		}
	};

	/**
	 * Adds hashing support for the data facade.
	 */
	template<typename ElementType>
	inline std::size_t hash_value(const Set<ElementType>& data) {
		return data.hash();
	}


	template<typename ElementType>
	class Element :
			public boost::equality_comparable<Element<ElementType>>,
			public boost::partially_ordered<Set<ElementType>> {

		typedef const ElementEntry<ElementType>* ptr_type;
		ptr_type data;

	public:

		Element(ptr_type ptr = nullptr) : data((!ptr || ptr->empty()) ? nullptr : ptr) {
			assert_true(!data || !data->empty()) << "Pointer must be null or not empty!";
		}

		bool operator==(const Element<ElementType>& other) const {
			return data == other.data || (data && other.data && *data == *other.data);
		}

		bool operator<(const Element<ElementType>& other) const {
			return (data) ? (*data < *other.data) : (other.data == nullptr);
		}

		bool empty() const {
			return !data;
		}

		bool isSingle() const {
			return !empty() && data->isSingle();
		}

		bool contains(const Element<ElementType>& a) const {
			assert(a.isSingle());
			return !empty() && data->contains(*a.data);
		}

		Element<ElementType> intersect(const Element<ElementType>& other) const {
			return data->intersect(*other.data);
		}

		Set<ElementType> diff(const Set<ElementType>& other) const {

			// handle empty element
			if (empty()) return Set<ElementType>();

			// if there is nothing to remove => done
			if (other.empty()) return *this;

			Set<ElementType> res = *this;
			for(const auto& cur : other.getPtr()->data) {
				res = res.diff(Element<ElementType>(cur));
			}

			// done
			return res;
		}

		DataManager<ElementType>& getManager() const {
			return data->mgr;
		}

		// for unit-testing
		ptr_type getPtr() const {
			return data;
		}

		std::size_t hash() const {
			return data->hash;
		}
	};

	/**
	 * Adds hashing support for the element facade.
	 */
	template<typename ElementType>
	inline std::size_t hash_value(const Element<ElementType>& element) {
		return element.hash();
	}


	// -------------------------------------------------------------------------------------------------
	// 											Operations
	// -------------------------------------------------------------------------------------------------

	template<typename ElementType>
	Set<ElementType> setUnion(const Set<ElementType>& a, const Set<ElementType>& b) {
		return a.merge(b);
	}

	template<typename ElementType>
	Set<ElementType> setUnion(const std::vector<Set<ElementType>>& list) {
		Set<ElementType> res;
		for(const auto& cur : list) res = setUnion(res, cur);
		return res;
	}

	template<typename ElementType>
	Set<ElementType> setIntersect(const Set<ElementType>& a, const Set<ElementType>& b) {
		return a.intersect(b);
	}

	// this operation is not supported in general
//	template<typename ElementType>
//	Set<ElementType> setDiff(const Set<ElementType>& a, const Set<ElementType>& b) {
//		return a.diff(b);
//	}

	template<typename ElementType>
	bool empty(const Set<ElementType>& a) {
		return a.empty();
	}

	// not supported yet
//	template<typename ElementType>
//	bool isSubset(const Set<ElementType>& sub, const Set<ElementType>& super);
//
//	template<typename ElementType>
//	bool isEqual(const Set<ElementType>& a, const Set<ElementType>& b) {
//		// TODO: form equality classes here!
//		return a == b || (isSubset(a,b) && isSubset(b,a));
//	}

	template<typename ElementType>
	bool isMember(const Set<ElementType>& a, const Element<ElementType>& e) {
		assert(e.isSingle());
		return a.contains(e);
	}



} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	template<typename ElementType>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::Set<ElementType>& data) {
		if (data.getPtr() == nullptr) return out << "{}";
		return out << *data.getPtr();
	}

	template<typename ElementType>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::Element<ElementType>& data) {
		return out << *data.getPtr();
	}

} // end namespace std

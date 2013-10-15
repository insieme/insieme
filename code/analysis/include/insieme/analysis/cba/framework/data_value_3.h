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
	template<typename ElementType> class Data;
	template<typename ElementType> class Element;


	// some forward declarations of operations

	template<typename ElementType>
	Data<ElementType> setUnion(const Data<ElementType>& a, const Data<ElementType>& b);

	template<typename ElementType>
	Data<ElementType> setIntersect(const Data<ElementType>& a, const Data<ElementType>& b);

	template<typename ElementType>
	Data<ElementType> setDiff(const Data<ElementType>& a, const Data<ElementType>& b);

	template<typename ElementType>
	bool empty(const Data<ElementType>& a);

	template<typename ElementType>
	bool isSubset(const Data<ElementType>& sub, const Data<ElementType>& super);

	template<typename ElementType>
	bool isEqual(const Data<ElementType>& a, const Data<ElementType>& b);


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
			std::size_t hash_map(const std::map<IndexType, Data<ElementType>>& map) {
				std::size_t hash = 0;
				for(const std::pair<IndexType, Data<ElementType>>& cur : map) {
					hash += utils::combineHashes(cur.first, cur.second);
				}
				return hash;
			}

		}

		template<typename ElementType, typename DerivedType>
		struct Entry :
				public boost::equality_comparable<DerivedType>,
				public utils::Printable {

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

			typedef std::map<IndexType, Data<ElementType>> map_type;
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
	std::pair<IndexType, Data<ElementType>> entry(const IndexType& i, const Data<ElementType>& e) {
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

		Data<ElementType> set(const std::set<Element<ElementType>>& elements) {

			// handle empty sets
			if (elements.empty()) return Data<ElementType>();

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
		Data<ElementType> set(const Elements& ... elements) {
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
		Element<ElementType> compound(const std::map<IndexType, Data<ElementType>>& map) {

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
		Element<ElementType> compound(const std::pair<IndexType, Data<ElementType>>& first, const Elements& ... elements) {
			return compound(utils::map::toMap(first, elements ...));
		}

	};


	template<typename ElementType>
	class Data :
			public boost::equality_comparable<Data<ElementType>>,
			public boost::partially_ordered<Data<ElementType>> {

		typedef SetEntry<ElementType>* ptr_type;
		ptr_type data;

	public:

		Data(ptr_type ptr = nullptr) : data(ptr) {}

		Data(const Element<ElementType>& cur)
			: data(cur.getPtr()->mgr.set(cur).data) {}

		ptr_type getPtr() const {
			return data;
		}

		bool operator==(const Data<ElementType>& other) const {
			return data == other.data || (data && other.data && *data == *other.data);
		}

		bool operator<(const Data<ElementType>& other) const {
			return (data) ? (*data < *other.data) : (other.data == nullptr);
		}

		bool empty() const {
			return !data;
		}

		Data merge(const Data<ElementType>& other) const {

			// handle empty sets
			if (!data) return other;
			if (!other.data) return *this;

			// merge sets and return result
			std::set<Element<ElementType>> sum = data->data;
			sum.insert(other.data->data.begin(), other.data->data.end());
			return getManager().set(sum);
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
	inline std::size_t hash_value(const Data<ElementType>& data) {
		return data.hash();
	}


	template<typename ElementType>
	class Element :
			public boost::equality_comparable<Element<ElementType>>,
			public boost::partially_ordered<Data<ElementType>> {

		typedef ElementEntry<ElementType>* ptr_type;
		ptr_type data;

	public:

		Element(ptr_type ptr) : data(ptr) {
			assert_true(data) << "This pointer must not be null!";
		}

		bool operator==(const Element<ElementType>& other) const {
			return data == other.data || *data == *other.data;
		}

		bool operator<(const Element<ElementType>& other) const {
			return *data < *other.data;
		}

		bool empty() const {
			return data->empty();
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
	Data<ElementType> setUnion(const Data<ElementType>& a, const Data<ElementType>& b) {
		return a.merge(b);
	}

	template<typename ElementType>
	Data<ElementType> setIntersect(const Data<ElementType>& a, const Data<ElementType>& b);

	template<typename ElementType>
	Data<ElementType> setDiff(const Data<ElementType>& a, const Data<ElementType>& b);

	template<typename ElementType>
	bool empty(const Data<ElementType>& a) {
		return a.empty();
	}

	template<typename ElementType>
	bool isSubset(const Data<ElementType>& sub, const Data<ElementType>& super);

	template<typename ElementType>
	bool isEqual(const Data<ElementType>& a, const Data<ElementType>& b) {
		// TODO: form equality classes here!
		return a == b || (isSubset(a,b) && isSubset(b,a));
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	template<typename ElementType>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::Data<ElementType>& data) {
		if (data.getPtr() == nullptr) return out << "{}";
		return out << *data.getPtr();
	}

	template<typename ElementType>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::Element<ElementType>& data) {
		return out << *data.getPtr();
	}

} // end namespace std

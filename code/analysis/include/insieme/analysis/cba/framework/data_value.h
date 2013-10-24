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

#include "insieme/utils/constraint/lattice.h"

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * Data value instances are constructs utilized for abstracting the value of structured
	 * data (structs, unions, tuples, vectors, arrays, ...) based on an abstraction for atomic
	 * values. They are realized as lattice transformers.
	 *
	 * Several different implementations might be considered. They differ in the accuracy of the
	 * resulting value approximations. This ranges from all values are the summarized to a single
	 * abstract value (smashing the structure), over the proper preservation of the element values
	 * (with the exception of arrays) up to supporting the preservation of inter-field relations
	 * (e.g. the two fields of a struct have the same value). The goal is to make the actual
	 * implementation interchangeable.
	 *
	 * Within this file three different implementations are offered:
	 * 		- Unit, every field is summerized to a single value
	 * 		- First-Order, fields are preserved (to some extend), inter-field relations not
	 * 		- Second-Order, fields are even more accurately preserved as well as some inter-field relations
	 */

	template<
		// the lattice this strcuture is based on
		typename _base_lattice,

		// a manager handling value instances
		typename _manager_type,

		// the type used to model a value in the resulting lattice
		typename _value_type,

		// an operation to retrieve sub-values
		typename _projection_op,

		// an operation converting a value into a base-lattice value
		typename _ground_op,

		// the meet operator for the resulting lattice
		typename _meet_assign_op_type,

		// the less operator for the resulting lattice
		typename _less_op_type = utils::constraint::meet_assign_based_less_op<_meet_assign_op_type>,

		// the meet operator for the resulting lattice
		typename _meet_op_type = utils::constraint::meet_assign_based_meet_op<_meet_assign_op_type>
	>
	struct DataStructureLattice :
			public utils::constraint::Lattice<_value_type, _meet_assign_op_type, _less_op_type, _meet_op_type> {
		typedef _base_lattice base_lattice;
		typedef _manager_type manager_type;
		typedef _projection_op projection_op_type;
		typedef _ground_op ground_op_type;
	};


	// -------------------------------------------------------------------------------------------
	//									Unit Data Structure
	// -------------------------------------------------------------------------------------------

	/**
	 * A unit data structure is representing the value of a composed data object by a single instance
	 * of the underlying lattice. It is therefore smashing the structure of data objects and represents
	 * its value as a single summary value (which is therefore necessarily imprecise).
	 */

	namespace unit {

		template<typename BaseLattice>
		class DataManager;

		template<typename BaseLattice>
		struct projection_op {
			typedef typename BaseLattice::value_type value_type;
			template<typename IndexType>
			const value_type& operator()(const value_type& value, const IndexType& index) const {
				return value;
			}
		};

	}

	template<typename BaseLattice>
	struct UnionStructureLattice :
			public DataStructureLattice<
				BaseLattice,
				unit::DataManager<BaseLattice>,
				typename BaseLattice::value_type,
				unit::projection_op<BaseLattice>,
				id<typename BaseLattice::value_type>,
				typename BaseLattice::meet_assign_op_type,
				typename BaseLattice::less_op_type,
				typename BaseLattice::meet_op_type
			> {};


	// -------------------------------------------------------------------------------------------
	//								First-Order Data Structure
	// -------------------------------------------------------------------------------------------

	/**
	 * A first order data structure is representing values of composed data objects by trees
	 * modeling the hierarchical structure of the object itself. Within the leaves an instance
	 * of the underlying lattice set is utilized to model the abstract properties to be represented.
	 */

	namespace first_order {

		template<typename BaseLattice>
		class Data;

		template<typename BaseLattice>
		class DataManager;

		template<typename BaseLattice>
		struct projection_op;

		template<typename BaseLattice>
		struct ground_op;

		template<typename BaseLattice>
		struct meet_assign_op;

		template<typename BaseLattice>
		struct less_op;
	}

	template<typename BaseLattice>
	struct FirstOrderStructureLattice :
			public DataStructureLattice<
				BaseLattice,
				first_order::DataManager<BaseLattice>,
				first_order::Data<BaseLattice>,
				first_order::projection_op<BaseLattice>,
				first_order::ground_op<BaseLattice>,
				first_order::meet_assign_op<BaseLattice>,
				first_order::less_op<BaseLattice>
			> {};


	// -------------------------------------------------------------------------------------------
	//								Second-Order Data Structure
	// -------------------------------------------------------------------------------------------

	namespace second_order {

		template<typename BaseLattice>
		class Data;

		template<typename BaseLattice>
		class DataManager;

		template<typename BaseLattice>
		struct projection_op;

		template<typename BaseLattice>
		struct ground_op;

		template<typename BaseLattice>
		struct meet_assign_op;

		template<typename BaseLattice>
		struct less_op;
	}

	template<typename BaseLattice>
	struct SecondOrderStructureLattice :
			public DataStructureLattice<
				BaseLattice,
				second_order::DataManager<BaseLattice>,
				second_order::Data<BaseLattice>,
				second_order::projection_op<BaseLattice>,
				second_order::ground_op<BaseLattice>,
				second_order::meet_assign_op<BaseLattice>,
				second_order::less_op<BaseLattice>
			> {};



	// ###########################################################################################

	// -------------------------------------------------------------------------------------------
	//									Utilities
	// -------------------------------------------------------------------------------------------

	template<typename IndexType, typename ValueType>
	std::pair<IndexType, ValueType> entry(const IndexType& i, const ValueType& e) {
		return std::make_pair(i, e);
	}

	namespace {

		template<typename IndexType, typename ValueType>
		std::size_t hash_map(const std::map<IndexType, ValueType>& map) {
			std::size_t hash = 0;
			for(const std::pair<IndexType, ValueType>& cur : map) {
				hash += utils::combineHashes(cur.first, cur.second);
			}
			return hash;
		}
	}


	namespace detail {

		template<typename _value_type, typename Derived>
		class BaseDataManager : public boost::noncopyable {

		public:

			typedef _value_type value_type;

			template<typename IndexType, typename ... Elements>
			value_type compound(const std::pair<IndexType, value_type>& first, const Elements& ... elements) {
				return static_cast<Derived*>(this)->compound(utils::map::toMap(first, elements ...));
			}

		};

	}


	// -------------------------------------------------------------------------------------------
	//									Unit Data Structure
	// -------------------------------------------------------------------------------------------

	namespace unit {

		template<typename BaseLattice>
		class DataManager : public detail::BaseDataManager<typename BaseLattice::value_type, DataManager<BaseLattice>> {

			typedef detail::BaseDataManager<typename BaseLattice::value_type, DataManager<BaseLattice>> super;
			typedef typename BaseLattice::value_type value_type;
			typedef typename BaseLattice::meet_assign_op_type meet_assign_op_type;

		public:

			using super::compound;

			const value_type& atomic(const value_type& value) const {
				return value;
			}

			template<typename IndexType>
			value_type compound(const std::map<IndexType, value_type>& map) {
				const static meet_assign_op_type meet_assign_op;
				value_type res;
				for(const auto& cur : map) {
					meet_assign_op(res, cur.second);
				}
				return res;
			}

		};

	}


	// -------------------------------------------------------------------------------------------
	//									First Order Data Structure
	// -------------------------------------------------------------------------------------------

	namespace first_order {

		/**
		 * 		T ::= L | I->T
		 *
		 * T ... Tree structure modeling structured data
		 * L ... Lattice being extended into a hierarchy
		 * I ... Index Values
		 */

		namespace internal {

			template<typename BaseLattice>
			struct Entry :
					public boost::equality_comparable<Entry<BaseLattice>>,
					public utils::Printable {

				DataManager<BaseLattice>& mgr;
				const std::size_t hash;

				// a constructor covering a reference to responsible manager and a hash value
				Entry(DataManager<BaseLattice>& mgr, std::size_t hash) : mgr(mgr), hash(hash) {}

				// a virtual destructor
				virtual ~Entry() {};

				// the equality operator is operating on the structural equality - equal to the hash
				virtual bool operator==(const Entry<BaseLattice>& other) const =0;

				// a less-than operator regarding the structural equality - compatible to the hash and == operator
				virtual bool operator<(const Entry<BaseLattice>& other) const =0;

				// check whether the set represented by this element contains the given element
				virtual bool contains(const Entry<BaseLattice>& other) const =0;

				// compute the meet value between this and the handed in entry
				virtual Data<BaseLattice> meet(const Entry<BaseLattice>& other) const =0;
			};

			template<typename BaseLattice>
			inline std::size_t hash_value(const Entry<BaseLattice>& entry) {
				return entry.hash;
			}

			template<typename BaseLattice>
			class AtomicEntry : public Entry<BaseLattice> {

				typedef typename BaseLattice::value_type value_type;
				const value_type value;

			public:

				AtomicEntry(DataManager<BaseLattice>& mgr, const value_type& value)
					: Entry<BaseLattice>(mgr, std::hash<value_type>()(value)), value(value) {}

				AtomicEntry(DataManager<BaseLattice>& mgr, std::size_t hash, const value_type& value)
					: Entry<BaseLattice>(mgr, hash), value(value) {
					assert_eq(hash, std::hash<value_type>()(value)) << "Hashes not matching!";
				}

				virtual ~AtomicEntry() {}

				const value_type& getValue() const {
					return value;
				}

				virtual bool operator==(const Entry<BaseLattice>& other) const {
					// same type, same data
					return this == &other || (typeid(*this) == typeid(other) && value == cast(other).value);
				}

				bool operator==(const value_type& other) const {
					return value == other;
				}

				virtual bool operator<(const Entry<BaseLattice>& other) const {
					return this != &other && value < cast(other).value;
				}

				virtual bool contains(const Entry<BaseLattice>& other) const {
					const typename BaseLattice::less_op_type less_op;
					return less_op(cast(other).value, value);
				}

				virtual bool contains(const typename BaseLattice::value_type& element) const {
					const typename BaseLattice::less_op_type less_op;
					return less_op(element, value);
				}

				virtual Data<BaseLattice> meet(const Entry<BaseLattice>& other) const {
					const static typename BaseLattice::meet_op_type meet_op;
					if (*this == other) return this;
					return this->mgr.atomic(meet_op(value, cast(other).value));
				}

			protected:

				virtual std::ostream& printTo(std::ostream& out) const {
					return out << value;
				}

			private:

				const AtomicEntry<BaseLattice>& cast(const Entry<BaseLattice>& other) const {
					assert(dynamic_cast<const AtomicEntry<BaseLattice>*>(&other));
					return static_cast<const AtomicEntry<BaseLattice>&>(other);
				}

			};


			template<typename IndexType, typename BaseLattice>
			class CompoundEntry : public Entry<BaseLattice> {

				typedef typename BaseLattice::value_type value_type;
				typedef std::map<IndexType, Data<BaseLattice>> map_type;
				const map_type data;

			public:

				CompoundEntry(DataManager<BaseLattice>& mgr, const map_type& data)
					: Entry<BaseLattice>(mgr, hash_map(data)), data(data) {}

				CompoundEntry(DataManager<BaseLattice>& mgr, std::size_t hash, const map_type& data)
					: Entry<BaseLattice>(mgr, hash), data(data) {
					assert_eq(hash, hash_map(data)) << "Externally computed hash is invalid!";
				}

				virtual ~CompoundEntry() {}

				virtual bool operator==(const Entry<BaseLattice>& other) const {
					// same type, same content
					return this == &other || (typeid(*this) == typeid(other) && data == cast(other).data);
				}

				bool operator==(const map_type& other) const {
					return data == other;
				}

				virtual bool operator<(const Entry<BaseLattice>& other) const {
					return this != &other && data < cast(other).data;
				}

				const Data<BaseLattice>& operator[](const IndexType& index) const {
					return extract(data, index);
				}

				virtual bool contains(const Entry<BaseLattice>& other) const {
					// the other element has to be of the same type
					assert_true((dynamic_cast<const CompoundEntry<IndexType, BaseLattice>*>(&other)));

					// quick check
					if (*this == other) return true;

					// cast type
					const CompoundEntry<IndexType, BaseLattice>& b = cast(other);

					// all fields must be contained
					for(const auto& cur : b.data) {
						// get values of of same field in current set
						const Data<BaseLattice>& sub = extract(data, cur.first);
						if (!sub.contains(cur.second)) return false;
					}

					// it is contained
					return true;
				}

				virtual Data<BaseLattice> meet(const Entry<BaseLattice>& other) const {

					// check equality
					if (*this == other) return this;

					// cast type
					const CompoundEntry<IndexType, BaseLattice>& b = cast(other);

					// intersect all elements
					std::map<IndexType, Data<BaseLattice>> res;
					for(const IndexType& cur : cross(data, b.data)) {

						// extract sets of both sides and meet those
						Data<BaseLattice> sA = extract(data, cur);
						Data<BaseLattice> sB = extract(b.data, cur);

						// meet entries
						sA.meetAssign(sB);

						// collect entries
						res[cur] = sA;
					}

					// build result
					return this->mgr.compound(res);
				}

			protected:

				virtual std::ostream& printTo(std::ostream& out) const {
					return out << "[" << join(",", data, [](std::ostream& out, const typename map_type::value_type& cur) {
						out << cur.first << "=" << cur.second;
					}) << "]";
				}

			private:

				const CompoundEntry<IndexType,BaseLattice>& cast(const Entry<BaseLattice>& other) const {
					assert((dynamic_cast<const CompoundEntry<IndexType,BaseLattice>*>(&other)));
					return static_cast<const CompoundEntry<IndexType,BaseLattice>&>(other);
				}

			};

		}

		template<typename BaseLattice>
		class Data :
				public boost::equality_comparable<Data<BaseLattice>> {

			typedef const internal::Entry<BaseLattice>* ptr_type;

			ptr_type data;

		public:

			Data(ptr_type ptr = nullptr) : data(ptr) {}

			bool operator==(const Data<BaseLattice>& other) const {
				return data == other.data || (data && other.data && *data == *other.data);
			}

			bool operator<(const Data<BaseLattice>& other) const {
				return (data) ? (*data < *other.data) : (other.data != nullptr);
			}

			template<typename IndexType>
			const Data<BaseLattice>& operator[](const IndexType& index) const {
				// handle empty
				if (!data) return *this;

				// check type
				assert_true((dynamic_cast<const internal::CompoundEntry<IndexType, BaseLattice>*>(data)));

				// access field
				return static_cast<const internal::CompoundEntry<IndexType, BaseLattice>&>(*data)[index];
			}

			const typename BaseLattice::value_type& ground() const {
				const static typename BaseLattice::value_type empty;

				// handle empty
				if (!data) return empty;

				// check type
				assert_true((dynamic_cast<const internal::AtomicEntry<BaseLattice>*>(data)));

				// access field
				return static_cast<const internal::AtomicEntry<BaseLattice>&>(*data).getValue();
			}

			bool meetAssign(const Data<BaseLattice>& other) {
				// compute meet data
				auto newData = (!data) ? other.data : data->meet(*other.data).data;
				// check whether there was a change
				if (data == newData) return false;
				data = newData;
				return true;
			}

			bool contains(const Data<BaseLattice>& other) const {
				if (data == other.data) return true;
				if (!data) return false;
				if (!other.data) return true;
				return data->contains(*other.data);
			}

			bool contains(const typename BaseLattice::value_type& value) const {
				// handle empty
				if (!data) return false;

				// check type
				assert_true((dynamic_cast<const internal::AtomicEntry<BaseLattice>*>(data)));

				// check membership
				return static_cast<const internal::AtomicEntry<BaseLattice>&>(*data).contains(value);
			}

			std::ostream& printTo(std::ostream& out) const {
				if (data == nullptr) return out << "[]";
				return out << *data;
			}

			std::size_t hash() const {
				return (data) ? data->hash : 0;
			}

			ptr_type getPtr() const {
				return data;
			}

		};

		/**
		 * Adds hashing support for the element facade.
		 */
		template<typename BaseLattice>
		inline std::size_t hash_value(const Data<BaseLattice>& data) {
			return data.hash();
		}

		template<typename BaseLattice>
		class DataManager : public detail::BaseDataManager<Data<BaseLattice>, DataManager<BaseLattice>> {

			typedef detail::BaseDataManager<Data<BaseLattice>, DataManager<BaseLattice>> super;

			typedef typename BaseLattice::value_type base_value_type;
			typedef Data<BaseLattice> value_type;

			typedef internal::AtomicEntry<BaseLattice>* AtomicEntryPtr;
			typedef internal::Entry<BaseLattice>* CompoundEntryPtr;

			std::unordered_map<std::size_t, std::vector<AtomicEntryPtr>> atomicCache;
			std::unordered_map<std::size_t, std::vector<CompoundEntryPtr>> compoundCache;
		public:

			~DataManager() {
				// free all cached elements
				for(const auto& cur : atomicCache) {
					for(const auto& entry : cur.second) delete entry;
				}
				for(const auto& cur : compoundCache) {
					for(const auto& entry : cur.second) delete entry;
				}
			}

			value_type atomic(const base_value_type& value) {

				// compute hash
				std::size_t hash = std::hash<base_value_type>()(value);

				// get element list
				auto& list = atomicCache[hash];

				// check whether element is already present
				for(auto cur : list) {
					if (*cur == value) return cur;
				}

				// add new element
				auto res = new internal::AtomicEntry<BaseLattice>(*this, hash, value);
				list.push_back(res);
				return res;
			}

			template<typename IndexType>
			value_type compound(const std::map<IndexType, value_type>& map) {

				// compute hash
				std::size_t hash = hash_map(map);

				// get element list
				auto& list = compoundCache[hash];

				// check whether element is already present
				for(auto cur : list) {
					if (auto entry = dynamic_cast<const internal::CompoundEntry<IndexType, BaseLattice>*>(cur)) {
						if (*entry == map) return cur;
					}
				}

				// add new element
				auto res = new internal::CompoundEntry<IndexType, BaseLattice>(*this, hash, map);
				list.push_back(res);
				return res;
			}

			using super::compound;

		};

		template<typename BaseLattice>
		struct projection_op {
			template<typename IndexType>
			const Data<BaseLattice>& operator()(const Data<BaseLattice>& value, const IndexType& index) const {
				return value[index];
			}
		};

		template<typename BaseLattice>
		struct ground_op {
			typedef typename BaseLattice::value_type base_value_type;
			const base_value_type& operator()(const Data<BaseLattice>& value) const {
				return value.ground();
			}
		};

		template<typename BaseLattice>
		struct meet_assign_op {
			bool operator()(Data<BaseLattice>& a, const typename BaseLattice::value_type& b) const {
				return a.meetAssign(a.getPtr()->mgr.atomic(b));
			}
			bool operator()(Data<BaseLattice>& a, const Data<BaseLattice>& b) const {
				return a.meetAssign(b);
			}
		};

		template<typename BaseLattice>
		struct less_op {
			bool operator()(const typename BaseLattice::value_type& a, const Data<BaseLattice>& b) const {
				return b.contains(a);
			}
			bool operator()(const Data<BaseLattice>& a, const Data<BaseLattice>& b) const {
				return b.contains(a);
			}
		};
	}



	// -------------------------------------------------------------------------------------------
	//									Second Order Data Structure
	// -------------------------------------------------------------------------------------------


	namespace second_order {

		/**
		 * 		D ::= 2^T
		 * 		T ::= L | I->D
		 *
		 * D ... the type modeling the data
		 * T ... Tree modeling structure of data
		 * L ... Lattice being extended into a hierarchy
		 * I ... Index Values
		 */

		namespace internal {

			template<typename BaseLattice, typename DerivedType>
			struct Entry :
					public boost::equality_comparable<DerivedType>,
					public utils::Printable {

				DataManager<BaseLattice>& mgr;
				const std::size_t hash;

				Entry(DataManager<BaseLattice>& mgr, std::size_t hash) : mgr(mgr), hash(hash) {}
				virtual ~Entry() {};

			};

			template<typename BaseLattice, typename DerivedType>
			inline std::size_t hash_value(const Entry<BaseLattice, DerivedType>& entry) {
				return entry.hash;
			}

			template<typename BaseLattice> class TreeEntry;

			template<typename BaseLattice>
			std::size_t hash_set(const std::set<TreeEntry<BaseLattice>*>& data) {
				std::size_t hash = 0;
				for(const auto& cur : data) {
					hash += cur->hash;
				}
				return hash;
			}

			template<typename BaseLattice>
			class SetEntry : public Entry<BaseLattice, SetEntry<BaseLattice>> {

				typedef TreeEntry<BaseLattice>* tree_ptr;
				typedef std::set<tree_ptr> set_type;
				typedef typename set_type::const_iterator const_iterator;

				set_type data;

			public:

				SetEntry(DataManager<BaseLattice>& mgr, const set_type& data)
					: Entry<BaseLattice, SetEntry<BaseLattice>>(mgr, hash_set(data)), data(data) {
					assert(!data.empty());
				}

				SetEntry(DataManager<BaseLattice>& mgr, std::size_t hash, const set_type& data)
					: Entry<BaseLattice, SetEntry<BaseLattice>>(mgr, hash), data(data) {
					assert_eq(hash, hash_set(data)) << "Hashes not matching!";
					assert(!data.empty());
				}

				virtual ~SetEntry() {};

				bool operator==(const SetEntry<BaseLattice>& other) const {
					// important: only structural equivalence
					return this == &other || (this->hash == other.hash && data == other.data);
				}

				bool operator==(const set_type& other) const {
					return data == other;
				}

				bool operator<(const SetEntry<BaseLattice>& other) const {
					return this != &other && data < other.data;
				}

				bool contains(const SetEntry<BaseLattice>& other) const {
					// any entry of the other set has to be contained in some entry of the this set
					for(const auto& a : other.data) {
						if (!any(data, [&](tree_ptr b)->bool { return b->contains(*a); })) {
							return false;
						}
					}
					return true;
				}

				Data<BaseLattice> meet(const SetEntry<BaseLattice>& other) const {
					// just compute union of both sets
					set_type res = data;
					res.insert(other.data.begin(), other.data.end());
					return this->mgr.set(res);
				}

				const_iterator begin() const {
					return data.begin();
				}

				const_iterator end() const {
					return data.end();
				}

			protected:

				virtual std::ostream& printTo(std::ostream& out) const {
					return out << "{" << join(",", data, print<deref<tree_ptr>>()) << "}";
				}

			};

			template<typename BaseLattice>
			struct TreeEntry : public Entry<BaseLattice, TreeEntry<BaseLattice>> {

				TreeEntry(DataManager<BaseLattice>& mgr, std::size_t hash)
					: Entry<BaseLattice, TreeEntry<BaseLattice>>(mgr, hash) {}

				// the equality operator is operating on the structural equality - equal to the hash
				virtual bool operator==(const TreeEntry<BaseLattice>& other) const =0;

				// a less-than operator regarding the structural equality - compatible to the hash and == operator
				virtual bool operator<(const TreeEntry<BaseLattice>& other) const =0;

				// check whether the set represented by this element contains the given element
				virtual bool contains(const TreeEntry<BaseLattice>& other) const =0;

			};

			template<typename BaseLattice>
			class AtomicEntry : public TreeEntry<BaseLattice> {

				typedef typename BaseLattice::value_type value_type;
				const value_type value;

			public:

				AtomicEntry(DataManager<BaseLattice>& mgr, const value_type& value)
					: TreeEntry<BaseLattice>(mgr, std::hash<value_type>()(value)), value(value) {}

				AtomicEntry(DataManager<BaseLattice>& mgr, std::size_t hash, const value_type& value)
					: TreeEntry<BaseLattice>(mgr, hash), value(value) {
					assert_eq(hash, std::hash<value_type>()(value)) << "Hashes not matching!";
				}

				virtual ~AtomicEntry() {}

				const value_type& getValue() const {
					return value;
				}

				virtual bool operator==(const TreeEntry<BaseLattice>& other) const {
					// same type, same value
					return this == &other || (typeid(*this) == typeid(other) && value == cast(other).value);
				}

				bool operator==(const value_type& other) const {
					return value == other;
				}

				virtual bool operator<(const TreeEntry<BaseLattice>& other) const {
					return this != &other && value < cast(other).value;
				}

				virtual bool contains(const TreeEntry<BaseLattice>& other) const {
					const typename BaseLattice::less_op_type less_op;
					return less_op(cast(other).value, value);
				}

				virtual bool contains(const typename BaseLattice::value_type& element) const {
					const typename BaseLattice::less_op_type less_op;
					return less_op(element, value);
				}

			protected:

				virtual std::ostream& printTo(std::ostream& out) const {
					return out << value;
				}

			private:

				const AtomicEntry<BaseLattice>& cast(const TreeEntry<BaseLattice>& other) const {
					assert(dynamic_cast<const AtomicEntry<BaseLattice>*>(&other));
					return static_cast<const AtomicEntry<BaseLattice>&>(other);
				}

			};

			template<typename IndexType, typename BaseLattice>
			class CompoundEntry : public TreeEntry<BaseLattice> {

				typedef typename BaseLattice::value_type value_type;
				typedef std::map<IndexType, Data<BaseLattice>> map_type;
				const map_type data;

			public:

				CompoundEntry(DataManager<BaseLattice>& mgr, const map_type& data)
					: TreeEntry<BaseLattice>(mgr, hash_map(data)), data(data) {}

				CompoundEntry(DataManager<BaseLattice>& mgr, std::size_t hash, const map_type& data)
					: TreeEntry<BaseLattice>(mgr, hash), data(data) {
					assert_eq(hash, hash_map(data)) << "Externally computed hash is invalid!";
				}

				virtual ~CompoundEntry() {}

				virtual bool operator==(const TreeEntry<BaseLattice>& other) const {
					// same type, same content
					return this == &other || (typeid(*this) == typeid(other) && data == cast(other).data);
				}

				bool operator==(const map_type& other) const {
					return data == other;
				}

				virtual bool operator<(const TreeEntry<BaseLattice>& other) const {
					return this != &other && data < cast(other).data;
				}

				const Data<BaseLattice>& operator[](const IndexType& index) const {
					return extract(data, index);
				}

				virtual bool contains(const TreeEntry<BaseLattice>& other) const {
					// the other element has to be of the same type
					assert_true((dynamic_cast<const CompoundEntry<IndexType, BaseLattice>*>(&other)));

					// quick check
					if (*this == other) return true;

					// cast type
					const CompoundEntry<IndexType, BaseLattice>& b = cast(other);

					// all fields must be contained
					for(const auto& cur : b.data) {
						// get values of of same field in current set
						const Data<BaseLattice>& sub = extract(data, cur.first);
						if (!sub.contains(cur.second)) return false;
					}

					// it is contained
					return true;
				}

			protected:

				virtual std::ostream& printTo(std::ostream& out) const {
					return out << "[" << join(",", data, [](std::ostream& out, const typename map_type::value_type& cur) {
						out << cur.first << "=" << cur.second;
					}) << "]";
				}

			private:

				const CompoundEntry<IndexType,BaseLattice>& cast(const TreeEntry<BaseLattice>& other) const {
					assert((dynamic_cast<const CompoundEntry<IndexType,BaseLattice>*>(&other)));
					return static_cast<const CompoundEntry<IndexType,BaseLattice>&>(other);
				}

			};
		}

		template<typename BaseLattice>
		class Data : public boost::equality_comparable<Data<BaseLattice>> {

			typedef const internal::SetEntry<BaseLattice>* ptr_type;

			ptr_type data;

		public:

			Data(ptr_type ptr = nullptr) : data(ptr) {}

			bool operator==(const Data<BaseLattice>& other) const {
				return data == other.data || (data && other.data && *data == *other.data);
			}

			bool operator<(const Data<BaseLattice>& other) const {
				return (data) ? (*data < *other.data) : (other.data != nullptr);
			}

			template<typename IndexType>
			Data<BaseLattice> operator[](const IndexType& index) const {
				// handle empty
				if (!data) return *this;

				// check whether element types are properly typed
				assert_true((dynamic_cast<const internal::CompoundEntry<IndexType, BaseLattice>*>(*data->begin())));

				// collect union of all projections of all elements within the set
				Data<BaseLattice> res;
				for(const auto& cur : *data) {
					res.meetAssign(static_cast<const internal::CompoundEntry<IndexType, BaseLattice>&>(*cur)[index]);
				}

				// done
				return res;
			}

			typename BaseLattice::value_type ground() const {
				static const typename BaseLattice::meet_assign_op_type meet_assign_op;

				// aggregate the values of the set of atomics
				typename BaseLattice::value_type res;
				for(const auto& cur : *data) {
					meet_assign_op(res, static_cast<const internal::AtomicEntry<BaseLattice>&>(*cur).getValue());
				}

				// done
				return res;
			}

			bool meetAssign(const Data<BaseLattice>& other) {
				// compute meet data
				auto newData = (!data) ? other.data : data->meet(*other.data).data;
				// check whether there was a change
				if (data == newData) return false;
				data = newData;
				return true;
			}

			bool contains(const Data<BaseLattice>& other) const {
				if (data == other.data) return true;
				if (!data) return false;
				if (!other.data) return true;
				return data->contains(*other.data);
			}

			bool contains(const typename BaseLattice::value_type& value) const {
				// handle empty
				if (!data) return false;

				// check whether value is covered by any set
				for(const auto& cur : *data) {
					if (static_cast<const internal::AtomicEntry<BaseLattice>&>(*cur).contains(value)) return true;
				}

				// not found
				return false;
			}

			std::ostream& printTo(std::ostream& out) const {
				if (data == nullptr) return out << "{}";
				return out << *data;
			}

			std::size_t hash() const {
				return (data) ? data->hash : 0;
			}

			ptr_type getPtr() const {
				return data;
			}
		};

		/**
		 * Adds hashing support for the element facade.
		 */
		template<typename BaseLattice>
		inline std::size_t hash_value(const Data<BaseLattice>& data) {
			return data.hash();
		}

		template<typename BaseLattice>
		class DataManager : public detail::BaseDataManager<Data<BaseLattice>, DataManager<BaseLattice>> {

			typedef detail::BaseDataManager<Data<BaseLattice>, DataManager<BaseLattice>> super;

			typedef typename BaseLattice::value_type base_value_type;
			typedef Data<BaseLattice> value_type;

			typedef internal::TreeEntry<BaseLattice>* TreeEntryPtr;
			typedef internal::SetEntry<BaseLattice>* SetEntryPtr;
			typedef internal::AtomicEntry<BaseLattice>* AtomicEntryPtr;
			typedef internal::TreeEntry<BaseLattice>* CompoundEntryPtr;

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

			value_type set() {
				return value_type();
			}

			template<typename ... Trees>
			value_type set(const TreeEntryPtr& first, const Trees& ... trees) {
				return set(utils::set::toSet<std::set<TreeEntryPtr>>(first, trees...));
			}

			value_type set(const std::set<TreeEntryPtr>& trees) {

				// handle empty sets
				if (trees.empty()) return value_type();

				// compute hash
				std::size_t hash = hash_set(trees);

				// get element list
				auto& list = setCache[hash];

				// check whether element is already present
				for(auto cur : list) {
					if (*cur == trees) return cur;
				}

				// add new element
				auto res = new internal::SetEntry<BaseLattice>(*this, hash, trees);
				list.push_back(res);
				return res;
			}

			value_type atomic(const base_value_type& value) {

				// compute hash
				std::size_t hash = std::hash<base_value_type>()(value);

				// get element list
				auto& list = atomicCache[hash];

				// check whether element is already present
				for(auto cur : list) {
					if (*cur == value) return set(cur);
				}

				// add new element
				auto res = new internal::AtomicEntry<BaseLattice>(*this, hash, value);
				list.push_back(res);
				return set(res);
			}


			template<typename IndexType>
			value_type compound(const std::map<IndexType, value_type>& map) {

				// compute hash
				std::size_t hash = hash_map(map);

				// get element list
				auto& list = compoundCache[hash];

				// check whether element is already present
				for(auto cur : list) {
					if (auto entry = dynamic_cast<const internal::CompoundEntry<IndexType, BaseLattice>*>(cur)) {
						if (*entry == map) return set(cur);
					}
				}

				// add new element
				auto res = new internal::CompoundEntry<IndexType, BaseLattice>(*this, hash, map);
				list.push_back(res);
				return set(res);
			}

			using super::compound;

		};

		template<typename BaseLattice>
		struct projection_op {
			template<typename IndexType>
			Data<BaseLattice> operator()(const Data<BaseLattice>& value, const IndexType& index) const {
				return value[index];
			}
		};

		template<typename BaseLattice>
		struct ground_op {
			typedef typename BaseLattice::value_type base_value_type;
			base_value_type operator()(const Data<BaseLattice>& value) const {
				return value.ground();
			}
		};

		template<typename BaseLattice>
		struct meet_assign_op {
			bool operator()(Data<BaseLattice>& a, const typename BaseLattice::value_type& b) const {
				return a.meetAssign(a.getPtr()->mgr.atomic(b));
			}
			bool operator()(Data<BaseLattice>& a, const Data<BaseLattice>& b) const {
				return a.meetAssign(b);
			}
		};

		template<typename BaseLattice>
		struct less_op {
			bool operator()(const typename BaseLattice::value_type& a, const Data<BaseLattice>& b) const {
				return b.contains(a);
			}
			bool operator()(const Data<BaseLattice>& a, const Data<BaseLattice>& b) const {
				return b.contains(a);
			}
		};
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	template<typename L>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::first_order::Data<L>& data) {
		return data.printTo(out);
	}

	template<typename L>
	std::ostream& operator<<(std::ostream& out, const insieme::analysis::cba::second_order::Data<L>& data) {
		return data.printTo(out);
	}

} // end namespace std


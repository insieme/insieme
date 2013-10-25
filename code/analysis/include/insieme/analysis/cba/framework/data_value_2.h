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

namespace insieme {
namespace analysis {
namespace cba {

	/**
	 * Within this file the data structures modeling values for the data flow analysis
	 * within the CBA (constraint based analysis) framework.
	 *
	 * Values are structured as trees where each level is a step along the data path
	 * of a structured data object. For instance, a data value instance representing
	 * an array of structs will consist of a 2-level tree - the first modeling the
	 * array index step while the second is modeling the members of the structs.
	 *
	 * Since within a CBA analysis values are frequently duplicated due to a large
	 * number of temporary states, values are represented by pointers to shared value
	 * instances, which are therefore necessarily immutable.
	 *
	 * Summary:
	 *
	 * 		- instances are modeling data values in CBA
	 * 		- all are referenced by pointers (shared pointers)
	 * 		- all are immutable once created
	 *
	 */



	// ------------------------------------------------------------------------
	//							type definitions
	// ------------------------------------------------------------------------

	// forward declaration of the base type
	template<typename ElementType> class Data;
	template<typename ElementType> class DataBase;
	template<typename ElementType> class SetData;


	// TODO move all factory declarations to a common place
	template<typename ET> Data<ET> toSet();
	template<typename ET> Data<ET> atomic(const ET& element);


	// the type of pointer to be used for handling data values
	template<typename ElementType>
	class Data :
			public boost::equality_comparable<Data<ElementType>>,
			public boost::partially_ordered<Data<ElementType>>,
			public utils::Printable {
	public:

		std::shared_ptr<DataBase<ElementType>> data;

		Data() : data(std::make_shared<SetData<ElementType>>()) {}

		Data(const std::shared_ptr<DataBase<ElementType>>& data)
			: data(data) { assert(data); }

		// set difference - TODO: rename
		Data<ElementType> operator-(const Data<ElementType>& other) const {
			return *data - *other.data;
		}

		// set union
		Data<ElementType> operator+(const Data<ElementType>& other) const {
			return *data + *other.data;
		}

		bool operator==(const Data<ElementType>& other) const {
			return data == other.data || *data == *other.data;
		}

		bool operator<(const Data<ElementType>& other) const {
			return *this != other && *data < *other.data;
		}

		bool empty() const {
			return data->empty();
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << *data;
		}

	};

	template<typename Type, typename ... Args>
	Data<typename Type::element_type> make_ptr(const Args& ... args) {
		return Data<typename Type::element_type>(std::make_shared<Type>(args...));
	}


	template<typename ET>
	bool isSubset(const Data<ET>& a, const Data<ET>& b);

	template<typename ET>
	Data<ET> setDiff(const Data<ET>& a, const Data<ET>& b);

	// ------------------------------------------------------------------------
	//						the abstract node base type
	// ------------------------------------------------------------------------

	template<typename ET>
	bool isSubset(const DataBase<ET>& a, const DataBase<ET>& b);

	enum DataKind {
		Atomic,
		Composite,
		Set
	};

	template<typename ElementType>
	class DataBase : public utils::VirtualPrintable {

	public:

		typedef ElementType element_type;

	private:

		DataKind kind;

	public:

		DataBase(const DataKind& kind) : kind(kind) {}

		virtual ~DataBase() {}

		// TODO: remove this member!
		DataKind getKind() const { return kind; }

		bool operator==(const DataBase& other) const {
			return this == &other || (typeid(*this) == typeid(other) && isSubset(*this, other) && isSubset(other, *this));
		}

		bool operator<(const DataBase& other) const {
			auto typeA = std::type_index(typeid(*this));
			auto typeB = std::type_index(typeid(other));
			if (typeA != typeB) return typeA < typeB;
			return lessThan(other);
		}

		virtual Data<ElementType> operator-(const DataBase<ElementType>& other) const =0;
		virtual Data<ElementType> operator+(const DataBase<ElementType>& other) const =0;

		virtual bool isSubsetOf(const DataBase<ElementType>& other) const =0;

		virtual bool empty() const =0;

	protected:

		virtual std::ostream& printTo(std::ostream& out) const =0;

		virtual bool equals(const DataBase& other) const =0;

		virtual bool lessThan(const DataBase& other) const =0;
	};


	// ------------------------------------------------------------------------
	//							the leaf node type
	// ------------------------------------------------------------------------


	template<typename ElementType>
	class AtomicData : public DataBase<ElementType> {

		ElementType data;

	public:

		AtomicData(const ElementType& element)
			: DataBase<ElementType>(DataKind::Atomic), data(element) {}

		virtual ~AtomicData() {}

		virtual Data<ElementType> operator-(const DataBase<ElementType>& other) const {
			// TODO: improve the performance of this implementation!!
			if (*this == other) {
				return toSet<ElementType>();
			}
			return atomic(data);
		}

		virtual Data<ElementType> operator+(const DataBase<ElementType>& other) const {
			const auto& otherAtomic = cast(other);
			if (*this == otherAtomic) return atomic(data);
			return toSet(atomic(data), atomic(otherAtomic.data));
		}

		virtual bool isSubsetOf(const DataBase<ElementType>& other) const {
			// only if equivalent
			return data == cast(other).data;
		}

		virtual bool empty() const {
			return false;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

		virtual bool equals(const DataBase<ElementType>& other) const {
			assert(dynamic_cast<const AtomicData<ElementType>*>(&other));
			return data == static_cast<const AtomicData<ElementType>&>(other).data;
		}

		virtual bool lessThan(const DataBase<ElementType>& other) const {
			assert(dynamic_cast<const AtomicData<ElementType>*>(&other));
			return data < static_cast<const AtomicData<ElementType>&>(other).data;
		}

		static const AtomicData<ElementType>& cast(const DataBase<ElementType>& other) {
			assert(dynamic_cast<const AtomicData<ElementType>*>(&other));
			return static_cast<const AtomicData<ElementType>&>(other);
		}
	};


	// -- Factory Functions --------------

	template<typename ET>
	Data<ET> atomic(const ET& element) {
		return make_ptr<AtomicData<ET>>(element);
	}


	// ------------------------------------------------------------------------
	//				a node type for sets (disjunctions of other data)
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class SetData : public DataBase<ElementType> {

		typedef std::set<Data<ElementType>> set_type;
		set_type data;

	public:

		SetData(const set_type& data = set_type())
			: DataBase<ElementType>(DataKind::Set), data(data) {
			// just check that there are no empty members
			assert_true(check()) << "Encountered empty members: " << *this << "\n";
		}

		virtual ~SetData() {}

		virtual Data<ElementType> operator-(const DataBase<ElementType>& other) const {

			const auto& otherData = cast(other).data;

			Data<ElementType> res;
			for(const Data<ElementType>& t : data) {
				auto rest = t;
				for(const Data<ElementType>& s : otherData) {
					rest = setDiff(rest, s);
				}
				res = res + rest;
			}

			return res;
		}

		virtual Data<ElementType> operator+(const DataBase<ElementType>& other) const {
			const auto& otherSet = cast(other);

			set_type res = data;
			res.insert(otherSet.data.begin(), otherSet.data.end());
			return Data<ElementType>(std::make_shared<SetData<ElementType>>(res));
		}

		virtual bool isSubsetOf(const DataBase<ElementType>& superSet) const {
			const SetData<ElementType>& other = cast(superSet);

			// { T1, T2, ... , Tn } is subset of { S1, S2, ... , Sm }
			// if for all Tx we have
			//    Tx \ S1 \ S2 \ ... \ Sm = 0 	// empty

			for (const Data<ElementType>& t : data) {
				auto rest = t;
				for(const Data<ElementType>& s : other.data) {
					rest = setDiff(rest, s);
				}

				// if there is still something left => not a subset
				if (!rest.empty()) return false;
			}

			// all fine
			return true;
		}

		virtual bool empty() const {
			return data.empty();
		}

//		bool operator==(const DataSet<ElementType>& other) const {
//			return this == &other || (subSet(other) && other.subSet(*this));
//		}
//
//		bool operator<(const DataSet<ElementType>& other) const {
//			return *this != other && data < other.data;
//		}
//
//		bool subSet(const DataSet<ElementType>& other) const {
//			return this == &other || std::includes(data.begin(), data.end(), other.data.begin(), other.data.end());
//		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

		virtual bool equals(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const SetData<ElementType>*>(&other)));
			return data == static_cast<const SetData<ElementType>&>(other).data;
		}

		virtual bool lessThan(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const SetData<ElementType>*>(&other)));
			return data < static_cast<const SetData<ElementType>&>(other).data;
		}

	private:

		bool check() {
			// must not contain an empty data element
			for(const auto& cur : data) {
				if (cur.empty()) return false;
			}
			return true;
		}

		static const SetData<ElementType>& cast(const DataBase<ElementType>& other) {
			assert(dynamic_cast<const SetData<ElementType>*>(&other));
			return static_cast<const SetData<ElementType>&>(other);
		}
	};

	template<typename ET>
	Data<ET> toSet() {
		return make_ptr<SetData<ET>>(std::set<Data<ET>>());
	}

	template<typename ET, typename ... Rest>
	Data<ET> toSet(const Data<ET>& first, const Rest& ... rest) {
		return make_ptr<SetData<ET>>(utils::set::toSet<std::set<Data<ET>>>(first, rest...));
	}


	// ------------------------------------------------------------------------
	//			a node type for composed structures (cross-product)
	// ------------------------------------------------------------------------

	template<typename ElementType, typename IndexType>
	class CompositeData : public DataBase<ElementType> {

		typedef std::map<IndexType, Data<ElementType>> data_type;
		typedef typename data_type::value_type data_value_type;

		const data_type data;

	public:

		CompositeData(const data_type& data)
			: DataBase<ElementType>(DataKind::Composite), data(data) {
			assert_true(checkDuplicates()) << "Encountered duplicated indices: " << data;
		}

		virtual ~CompositeData() {}

		Data<ElementType> getValuesOf(const IndexType& index) const {
			// TODO: hand this over to index type
			for(const data_value_type& cur : data) {
				if (cur.first == index) return cur.second;
			}
			return Data<ElementType>();
		}

		virtual Data<ElementType> operator-(const DataBase<ElementType>& other) const {
			data_type res;

			const data_type& otherData = cast(other).data;
			for(const auto& cur : data) {
				auto pos = otherData.find(cur.first);
				if (pos != otherData.end()) {
					res[cur.first] = cur.second - pos->second;
				} else {
					res[cur.first] = cur.second;
				}
			}

			return Data<ElementType>(std::make_shared<CompositeData<ElementType,IndexType>>(res));
		}

		virtual Data<ElementType> operator+(const DataBase<ElementType>& other) const {
			const auto& otherComposite = cast(other);

			return toSet(
					Data<ElementType>(std::make_shared<CompositeData<ElementType, IndexType>>(data)),
					Data<ElementType>(std::make_shared<CompositeData<ElementType, IndexType>>(otherComposite.data))
			);

		}

		virtual bool isSubsetOf(const DataBase<ElementType>& superSet) const {
			const auto& other = cast(superSet);

			for(const auto& cur : data) {
				if (!isSubset(cur.second, other.getValuesOf(cur.first))) return false;
			}

			return true;
		}

		virtual bool empty() const {
			return any(data, [](const data_value_type& other) { return other.second.empty(); });
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, [](std::ostream& out, const std::pair<IndexType, Data<ElementType>>& cur) {
				out << "[" << cur.first << "]=" << cur.second;
			}) << "}";
		}

		virtual bool equals(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const CompositeData<ElementType,IndexType>*>(&other)));
			return data == static_cast<const CompositeData<ElementType,IndexType>&>(other).data;
		}

		virtual bool lessThan(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const CompositeData<ElementType,IndexType>*>(&other)));
			return data < static_cast<const CompositeData<ElementType,IndexType>&>(other).data;
		}

	private:

		bool checkDuplicates() const {
			for(const auto& curA : data) {
				for(const auto& curB : data) {
					if (&curA != &curB && curA.first == curB.first) {
						return false;
					}
				}
			}
			return true;
		}

		static const CompositeData<ElementType, IndexType>& cast(const DataBase<ElementType>& other) {
			assert((dynamic_cast<const CompositeData<ElementType, IndexType>*>(&other)));
			return static_cast<const CompositeData<ElementType, IndexType>&>(other);
		}


	};

	template<typename ET, typename IT>
	std::pair<IT,Data<ET>>
	element(const IT index, const Data<ET>& values) {
		return std::make_pair(index, values);
	}

	template<typename ET, typename IT>
	Data<ET> compositeData(const std::map<IT, Data<ET>>& data) {
		return make_ptr<CompositeData<ET,IT>>(data);
	}

	template<typename ET, typename IT, typename ... Rest>
	Data<ET> compositeData(const std::pair<IT, Data<ET>>& first, const Rest& ... rest) {
		return compositeData<ET,IT>(utils::map::toMap(first, rest...));
	}


	namespace {

		template<typename ET>
		bool checkSameType(const DataBase<ET>& a, const DataBase<ET>& b) {
			return std::type_index(typeid(a)) == std::type_index(typeid(b));
		}
	}

	// ------------------------------------------------------------------------
	//								subset relation
	// ------------------------------------------------------------------------

	template<typename ET>
	bool isSubset(const Data<ET>& a, const Data<ET>& b) {
		return isSubset(*a.data, *b.data);
	}

	template<typename ET>
	bool isSubset(const DataBase<ET>& a, const DataBase<ET>& b) {
		// make sure it is the same type of data node
		assert(checkSameType(a, b));

		// equal sets are always subsets
		if (&a == &b) return true;

		// the type has to be equivalent
		if (a.getKind() != b.getKind()) return false;

		// just call the member function
		return a.isSubsetOf(b);
	}


	// ------------------------------------------------------------------------
	//									union
	// ------------------------------------------------------------------------

	template<typename ET>
	Data<ET> setUnion(const Data<ET>& a, const Data<ET>& b) {
		return a + b;
	}

	// ------------------------------------------------------------------------
	//								set-diff operation
	// ------------------------------------------------------------------------

	template<typename ET>
	Data<ET> setDiff(const Data<ET>& a, const Data<ET>& b) {
		return a - b;
	}


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

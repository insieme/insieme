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
	template<typename ElementType> class DataBase;

	// the type of pointer to be used for handling data values
	template<typename ElementType>
	class Data :
			public boost::equality_comparable<Data<ElementType>>,
			public boost::partially_ordered<Data<ElementType>>,
			public utils::Printable {

		std::shared_ptr<DataBase<ElementType>> data;

	public:

		Data(const std::shared_ptr<DataBase<ElementType>>& data)
			: data(data) { assert(data); }

		bool operator==(const Data<ElementType>& other) const {
			return data == other.data || *data == *other.data;
		}

		bool operator<(const Data<ElementType>& other) const {
			return *this != other && *data < *other.data;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << *data;
		}

	};

	template<typename Type, typename ... Args>
	Data<typename Type::element_type> make_ptr(const Args& ... args) {
		return Data<typename Type::element_type>(std::make_shared<Type>(args...));
	}

	template<typename ElementType>
	class DataSet :
			public boost::equality_comparable<DataSet<ElementType>>,
			public boost::partially_ordered<DataSet<ElementType>>,
			public utils::Printable {

		std::set<Data<ElementType>> data;

	public:

		DataSet(const std::set<Data<ElementType>>& data)
			: data(data) { }

		bool operator==(const DataSet<ElementType>& other) const {
			return this == &other || data == other.data;
		}

		bool operator<(const DataSet<ElementType>& other) const {
			return *this != other && data < other.data;
		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}

	};

	template<typename ET, typename ... Rest>
	DataSet<ET> toSet(const Data<ET>& first, const Rest& ... rest) {
		return utils::set::toSet<std::set<Data<ET>>>(first, rest...);
	}


	// ------------------------------------------------------------------------
	//						the abstract node base type
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class DataBase : public utils::Printable {

	public:

		typedef ElementType element_type;

		virtual ~DataBase() {}

		bool operator==(const DataBase& other) {
			return typeid(*this) == typeid(other) && equals(other);
		}

		bool operator<(const DataBase& other) const {
			auto typeA = std::type_index(typeid(*this));
			auto typeB = std::type_index(typeid(other));
			if (typeA != typeB) return typeA < typeB;
			return lessThan(other);
		}

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

		AtomicData(const ElementType& element) : data(element) {}

		virtual ~AtomicData() {}

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
	};


	// -- Factory Functions --------------

	template<typename ET>
	Data<ET> atomic(const ET& element) {
		return make_ptr<AtomicData<ET>>(element);
	}


	// ------------------------------------------------------------------------
	//							a node type for structs
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class StructData : public DataBase<ElementType> {

		typedef std::map<core::StringValuePtr, DataSet<ElementType>> data_map_type;
		typedef typename data_map_type::value_type data_map_value_type;

		data_map_type data;

	public:

		StructData(const data_map_type& data = data_map_type()) : data(data) {}

		virtual ~StructData() {}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, [](std::ostream& out, const std::pair<core::StringValuePtr, DataSet<ElementType>>& cur) {
				out << *(cur.first) << "=" << cur.second;
			}) << "}";
		}

		virtual bool equals(const DataBase<ElementType>& other) const {
			assert(dynamic_cast<const StructData<ElementType>*>(&other));
			return data == static_cast<const StructData<ElementType>&>(other).data;
		}

		virtual bool lessThan(const DataBase<ElementType>& other) const {
			assert(dynamic_cast<const StructData<ElementType>*>(&other));
			return data < static_cast<const StructData<ElementType>&>(other).data;
		}
	};

	template<typename ET>
	std::pair<core::StringValuePtr, DataSet<ET>>
	member(const core::StringValuePtr& name, const DataSet<ET>& data) {
		return std::make_pair(name, data);
	}

	template<typename ET>
	Data<ET> structData(const std::map<core::StringValuePtr, DataSet<ET>>& data) {
		return make_ptr<StructData<ET>>(data);
	}

	template<typename ET, typename ... Rest>
	Data<ET> structData(const std::pair<core::StringValuePtr, DataSet<ET>>& first, const Rest& ... rest) {
		return structData(utils::map::toMap(first, rest...));
	}


	// ------------------------------------------------------------------------
	//							a node type for arrays
	// ------------------------------------------------------------------------

	template<typename ElementType, typename IndexType>
	class ArrayData : public DataBase<ElementType> {

		typedef std::vector<std::pair<IndexType, DataSet<ElementType>>> data_type;
		typedef typename data_type::value_type data_value_type;

		const data_type data;

	public:

		ArrayData(const data_type& data) : data(data) {
			assert_true(checkDuplicates()) << "Encountered duplicated indices: " << data;

		}

		virtual ~ArrayData() {}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, [](std::ostream& out, const std::pair<IndexType, DataSet<ElementType>>& cur) {
				out << "[" << cur.first << "]=" << cur.second;
			}) << "}";
		}

		virtual bool equals(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const ArrayData<ElementType,IndexType>*>(&other)));
			return data == static_cast<const ArrayData<ElementType,IndexType>&>(other).data;
		}

		virtual bool lessThan(const DataBase<ElementType>& other) const {
			assert((dynamic_cast<const ArrayData<ElementType,IndexType>*>(&other)));
			return data < static_cast<const ArrayData<ElementType,IndexType>&>(other).data;
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

	};

	template<typename ET, typename IT>
	std::pair<IT,DataSet<ET>>
	element(const IT index, const DataSet<ET>& values) {
		return std::make_pair(index, values);
	}

	template<typename ET, typename IT>
	Data<ET> arrayData(const std::vector<std::pair<IT, DataSet<ET>>>& data) {
		return make_ptr<ArrayData<ET,IT>>(data);
	}

	template<typename ET, typename IT, typename ... Rest>
	Data<ET> arrayData(const std::pair<IT, DataSet<ET>>& first, const Rest& ... rest) {
		return arrayData<ET,IT>(toVector(first, rest...));
	}

} // end namespace cba
} // end namespace analysis
} // end namespace insieme

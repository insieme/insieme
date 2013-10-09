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

	// a type trait for the pointer type to be utilized
	template<typename ElementType> struct data_ptr {
		typedef std::shared_ptr<Data<ElementType>> type;
	};

	template<typename Type, typename ... Args>
	typename data_ptr<typename Type::element_type>::type make_ptr(const Args& ... args) {
		return std::make_shared<Type>(args...);
	}


	// ------------------------------------------------------------------------
	//						the abstract node base type
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class Data : public utils::Printable {

	public:

		typedef ElementType element_type;

		virtual ~Data() {}

		// an operator for merging data representations
//		virtual Data& operator+(const Data& other) const =0;

	protected:

		virtual std::ostream& printTo(std::ostream& out) const =0;
	};

//	template<typename ET>
//	data_ptr<ET>::type operator+(const data_ptr<ET>::type& a, const data_ptr<ET>::type& b) {
//		if (!a) return b;
//
//	}
//
//	template<typename ElementType>
//	void operator+=(DataPtr& a, const DataPtr& b) const {
//		if (a) {
//			(*a) += b;
//		} else {
//			a = b->clone();
//		}
//	}


	// ------------------------------------------------------------------------
	//							the leaf node type
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class ElementData : public Data<ElementType> {

		std::set<ElementType> data;

		typedef std::shared_ptr<Data<ElementType>> DataPtr;
		typedef std::shared_ptr<ElementData<ElementType>> ElementDataPtr;

	public:

//		ElementData(const ElementType& element) : data(utils::set::toSet<std::set>(element)) {}
		ElementData(const std::set<ElementType>& data = std::set<ElementType>()) : data(data) {}
//		ElementData(const ElementData& other) : data(other.data) {}

		virtual ~ElementData() {}

//		ElementDataPtr clone() const {
//			return std::make_shared<ElementData<ElementType>>(*this);
//		}
//
//		virtual DataPtr operator+(const DataPtr& otherData) const {
//			// combine data containers
//			ElementDataPtr res = clone();
//			(*res) += otherData;
//			return res;
//		}
//
//		virtual void operator+=(const DataPtr& otherData) const {
//			// check type
//			assert(dynamic_pointer_cast<ElementData<ElementType>>(otherData));
//
//			// get other data node
//			ElementDataPtr other = static_pointer_cast<ElementData<ElementType>>(otherData);
//
//			// merge the two data sets
//			data.insert(other->data.begin(), other->data.end());
//		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << data;
		}
	};


	// -- Factory Functions --------------

	template<typename ET, typename ... Rest>
	typename data_ptr<ET>::type elements(const ET& element, const Rest& ... rest) {
		return make_ptr<ElementData<ET>>(utils::set::toSet<std::set<ET>>(element, rest...));
	}

	template<typename ET>
	typename data_ptr<ET>::type element(const ET& element) {
		return elements(element);
	}

	// ------------------------------------------------------------------------
	//							a node type for structs
	// ------------------------------------------------------------------------

	template<typename ElementType>
	class StructData : public Data<ElementType> {

		typedef std::shared_ptr<Data<ElementType>> DataPtr;
		typedef std::shared_ptr<StructData<ElementType>> StructDataPtr;

		typedef std::map<core::StringValuePtr,DataPtr> data_map_type;
		data_map_type data;

	public:

		StructData(const data_map_type& data = data_map_type()) : data(data) {}

		virtual ~StructData() {}

//		StructDataPtr clone() const {
//			return std::make_shared<StructData<ElementType>>(*this);
//		}
//
//		virtual DataPtr operator+(const DataPtr& otherData) const {
//			// combine data containers
//			StructDataPtr res = std::make_shared<StructData<ElementType>>(*this);
//			(*res) += otherData;
//			return res;
//		}
//
//		virtual void operator+=(const DataPtr& otherData) const {
//			// check type
//			assert(dynamic_pointer_cast<ElementData<ElementType>>(otherData));
//
//			// get other data node
//			ElementDataPtr other = static_pointer_cast<ElementData<ElementType>>(otherData);
//
//			// merge the two data sets
//			for(const std::pair<core::LiteralPtr, DataPtr>& cur : other->data) {
//				res->data[cur.first] += cur.second;
//			}
//		}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "{" << join(",", data, [](std::ostream& out, const std::pair<core::StringValuePtr, DataPtr>& cur) {
				out << *(cur.first) << "=" << *(cur.second);
			}) << "}";
		}
	};

	template<typename ET>
	std::pair<core::StringValuePtr, typename data_ptr<ET>::type> member(const core::StringValuePtr& name, const std::shared_ptr<Data<ET>>& data) {
		return std::make_pair(name, data);
	}


	template<typename ET>
	typename data_ptr<ET>::type structData(const std::map<core::StringValuePtr, std::shared_ptr<Data<ET>>>& data) {
		return make_ptr<StructData<ET>>(data);
	}

	template<typename ET, typename ... Rest>
	typename data_ptr<ET>::type structData(const std::pair<core::StringValuePtr, std::shared_ptr<Data<ET>>>& first, const Rest& ... rest) {
		return structData(utils::map::toMap(first, rest...));
	}


	// ------------------------------------------------------------------------
	//							a node type for arrays
	// ------------------------------------------------------------------------

	template<typename ElementType, typename IndexType>
	class ArrayData : public Data<ElementType> {

		typedef std::shared_ptr<Data<ElementType>> DataPtr;

		std::vector<std::pair<IndexType, DataPtr>> data;
		DataPtr other;

	public:

		virtual ~ArrayData() {}

	protected:

		virtual std::ostream& printTo(std::ostream& out) const {
			out << "{" << join(",", data, [](std::ostream& out, const std::pair<IndexType, DataPtr>& cur) {
				out << cur.first << "=" << *(cur.second);
			});

			if (!data.empty() && other) out << ",";
			if (other) "else=" << *other;
			return out;
		}

	};


} // end namespace cba
} // end namespace analysis
} // end namespace insieme

namespace std {

	template<typename T>
	std::ostream& operator<<(std::ostream& out, const std::shared_ptr<insieme::analysis::cba::Data<T>>& data) {
		return out << *data;
	}

} // end namespace data

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

#include <memory>
#include <vector>
#include <string>
#include <array>

#include <boost/variant.hpp>
#include <boost/utility.hpp>
#include <boost/type_traits/is_base_of.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace transform {
namespace parameter {

	using std::vector;
	using std::string;
	using std::array;

	class Type;
	typedef std::shared_ptr<Type> TypePtr;

	class Value;
	typedef std::shared_ptr<Value> ValuePtr;


	class Type : public utils::Printable, boost::noncopyable {

	};

	template<
		typename T,
		typename boost::enable_if<boost::is_base_of<Type, T>,int>::type = 1
	>
	const T& createType() {
		static const T instance;
		return instance;
	}


	// --- Atomic Types -------------------------------------------------------------

	class AtomicType : public Type {};

	namespace detail {

		template<typename T>
		class atomic_type_helper : public AtomicType {
			const string name;
		public:
			atomic_type_helper(const string& name) : name(name) {};

			virtual std::ostream& printTo(std::ostream& out) const {
				return out << name;
			}
		};

	}


	template<typename T> struct atomic_type;

	#define ATOMIC_TYPE(TYPE) \
		template<> struct atomic_type<TYPE> : detail::atomic_type_helper<TYPE> { \
			atomic_type() : detail::atomic_type_helper<TYPE>(#TYPE) {}; \
		};

	ATOMIC_TYPE(int);
	ATOMIC_TYPE(unsigned);
	ATOMIC_TYPE(string);

	#undef ATOMIC_TYPE


	// --- Composed Types -------------------------------------------------------------

	class ComposedType : public Type {};


	template<typename ... T>
	class tuple_type : public Type {

		BOOST_STATIC_CONSTANT(unsigned, length=type_list<T...>::length);

		typedef array<const Type*,length> TypeList;

		TypeList elementTypes;

	public:

		tuple_type() : elementTypes(createElements()){ fillList<0,T...>(elementTypes); };

		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "(" << join(",", elementTypes, print<deref<const Type*>>()) << ")";
		}

		static TypeList createElements() {
			TypeList res;
			fillList<0,T...>(res);
			return res;
		}

		template<
			int index,
			typename First,
			typename ... Rest
		>
		static void fillList(TypeList& list) {
			list[index] = &createType<First>();
			fillList<index+1,Rest...>(list);
		}

		template<int index>
		static void fillList(TypeList& list) {}

	};


	class Value {

		TypePtr type;

		typedef boost::variant<int, vector<ValuePtr>> value_type;

	};

} // end namespace parameter
} // end namespace transform
} // end namespace insieme

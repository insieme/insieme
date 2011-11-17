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

#include <boost/utility.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "insieme/core/forward_decls.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace transform {
namespace parameter {

	using std::vector;
	using std::string;
	using std::array;


	class Parameter;
	typedef std::shared_ptr<Parameter> ParameterPtr;

	class AtomicParameter;
	typedef std::shared_ptr<AtomicParameter> AtomicParameterPtr;

	class TupleParameter;
	typedef std::shared_ptr<TupleParameter> TupleParameterPtr;





	// ---------------------------------------------------------------------------------
	//     Values
	// ---------------------------------------------------------------------------------

	/**
	 * The type used to represent parameter values. The implementation is based on a
	 * recursive boost-variant type which allows to form recursive, type save union
	 * types.
	 *
	 * For more details, see boost documentation.
	 */
	typedef boost::make_recursive_variant<
			int,
			string,
			vector<boost::recursive_variant_>
	>::type Value;


	// -- type identifier --------------------------------------------------------------

	template<typename T>
	struct is_type_of : public boost::static_visitor<bool> {
		bool operator()(const T& cur) const { return true; }
		template<typename S>
		bool operator()(const S& cur) const { return false; }
	};

	template<typename T>
	bool isTypeOf(const Value& value) {
		static is_type_of<T> visitor;
		return boost::apply_visitor(visitor, value);
	}


	// -- Value utilities -------------------------------------------------------------

	inline Value makeValue(int value) {
		return Value(value);
	}

	inline Value makeValue(const string& value) {
		return Value(value);
	}

	template<typename ... T>
	inline Value makeComposedValue(const T& ... values) {
		return Value(toVector<Value>(values...));
	}

	template<typename T>
	inline T getValue(const Value& value) {
		assert(isTypeOf<T>(value) && "Tried reading value of incompatible Value instance!");
		return boost::get<T>(value);
	}

	template<>
	inline Value getValue<Value>(const Value& value) {
		return value;
	}

	template<typename T, typename ... Path>
	inline T getValue(const Value& value, int first, Path ... path) {
		assert(isTypeOf<vector<Value>>(value) && "Not nested value encountered!");
		return getValue<T>(boost::get<vector<Value>>(value)[first], path ...);
	}







	// ---------------------------------------------------------------------------------
	//     Parameters
	// ---------------------------------------------------------------------------------

	class Parameter : boost::noncopyable  {

		string description;
		vector<ParameterPtr> components;

	public:

		template<typename ... T>
		Parameter(const string& description, const T& ... components)
			: description(description), components(toVector<ParameterPtr>(components ...)) {}

		virtual ~Parameter() {};

		virtual bool isValid(const Value& value) const = 0;

		const string& getDescription() const {
			return description;
		}

		const vector<ParameterPtr>& getComponents() const {
			return components;
		}

		bool isAtomic() const;

	};


	// -- Atomic Parameters ---------------------------------------------

	class AtomicParameter : public Parameter {
		string typeName;
	public:
		AtomicParameter(const string& typeName, const string& description)
			: Parameter(description), typeName(typeName) {}

		const string& getTypeName() const {
			return typeName;
		}
	};

	class IntParameter : public AtomicParameter {
	public:
		IntParameter(const string& description = "") : AtomicParameter("int", description) {}
		virtual bool isValid(const Value& value) const {
			return isTypeOf<int>(value);
		}
	};

	class StringParameter : public AtomicParameter {
	public:
		StringParameter(const string& description = "") : AtomicParameter("string", description) {}
		virtual bool isValid(const Value& value) const {
			return isTypeOf<string>(value);
		}
	};

	class NodePtrParameter : public AtomicParameter {
	public:
		NodePtrParameter(const string& description = "") : AtomicParameter("NodePtr", description) {}
		virtual bool isValid(const Value& value) const {
			return isTypeOf<core::NodePtr>(value);
		}
	};


	// -- Composed Parameters ---------------------------------------------

	class TupleParameter : public Parameter {
	public:

		template<typename ... T>
		TupleParameter(const string& description, const T& ... elements)
			: Parameter(description, elements...) {}

		virtual bool isValid(const Value& value) const;
	};


	// -- Parameter Constructors -------------------------------------------------------

	template<typename T> AtomicParameterPtr atom(const string& desc = "");

	template<>
	AtomicParameterPtr atom<int>(const string& desc) {
		return std::make_shared<IntParameter>(desc);
	}

	template<>
	AtomicParameterPtr atom<string>(const string& desc) {
		return std::make_shared<StringParameter>(desc);
	}

	template<typename ... Params>
	TupleParameterPtr tuple(const string& desc, const Params& ... params) {
		return std::make_shared<TupleParameter>(desc, params...);
	}

	template<typename ... Params>
	TupleParameterPtr tuple(const char* desc, const Params& ... params) {
		return tuple(string(desc), params ...);
	}

	template<typename ... Params>
	TupleParameterPtr tuple(const Params& ... params) {
		return tuple(string(""), params ...);
	}

	extern const TupleParameterPtr no_parameters;


	// -- Parameter Printer ----------------------------------------------------------

	class ParameterInfo : public utils::Printable {

		ParameterPtr parameter;

	public:

		ParameterInfo(const ParameterPtr& parameter) : parameter(parameter) {}

		virtual std::ostream& printTo(std::ostream& out) const;

	};


} // end namespace parameter
} // end namespace transform
} // end namespace insieme

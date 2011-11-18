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

#include "insieme/utils/properties.h"

namespace insieme {
namespace analysis {
namespace features {


	/**
	 * Within this header file, the classes to represent feature values and their types
	 * will be defined. The implementation is based on the insieme utils properties library.
	 */

	using std::vector;
	using std::string;




	// ---------------------------------------------------------------------------------
	//     Values
	// ---------------------------------------------------------------------------------
	//
	// Values are implemented using a typedef referencing a recursive boost-variant type.
	// In addition, various utilities simplifying the handling of values are offered.
	//
	//  Usage:
	//			to create a value the factory methods
	//					- makeValue(..)
	//					- combineValue(..)
	//			should be used. For instance, to create a value representing the
	//			integer value 12, makeValue(12) should be used. To merge existing
	//			values a and b into a composed values, makeComposedValue(a,b) should be
	//			used.
	//

	/**
	 * The type used to represent feature values. The implementation is based on a
	 * recursive boost-variant type which allows to form recursive, type save union
	 * types.
	 *
	 * For more details, see boost documentation.
	 */
	typedef utils::properties::make_value_type<
			bool,						// a list of potential feature value types
			int,
			float,
			double
	>::type Value;


	// -- Value utilities -------------------------------------------------------------

	/**
	 * A generic factory method allowing to convert a actual value into a Value instance.
	 * For instance, makeValue(12) is producing a value instance containing the integer 12.
	 *
	 * @tparam T the type of value to be encoded (typically automatically deduced)
	 * @param value the value to be encoded
	 * @return the given value represented as a feature value
	 */
	template<typename T>
	inline Value makeValue(T value) {
		return utils::properties::makeValue<Value>(value);
	}

	/**
	 * A generic factory method allowing to combine predefined values into a new value.
	 *
	 * @tparam T the variadic template type used to enable variable length lists of values
	 * @param values the values to be combined
	 * @return the value representing the combination of the given values
	 */
	template<typename ... T>
	inline Value combineValues(const T& ... values) {
		return utils::properties::combineValues<Value>(values...);
	}


	/**
	 * This generic method extracts a value stored within a value instance. The type
	 * stored within the value has to be provided as a template argument. In case the
	 * given type argument does not correspond to the stored value, an assertion is triggered.
	 *
	 * @tparam the type of value to be extracted
	 * @param the value which should be read
	 * @return the value retrieved from the given encoded value
	 */
	template<typename T>
	inline T getValue(const Value& value) {
		return utils::properties::getValue<T>(value);
	}

	/**
	 * This generic method extracts a value form a composed Value instance. The types
	 * first and path can be used to specify a path selecting the value to be read.
	 *
	 * @tparam T the type of the value to be read from the given value
	 * @tparam Rest the variadic template types used to support an arbitrary path
	 * @param value the value to be read
	 * @param first the first step along the path
	 * @param rest the remaining steps along the path
	 * @return the extracted value
	 */
	template<typename T, typename ... Path>
	inline T getValue(const Value& value, Path ... rest) {
		return utils::properties::getValue<T>(value, rest...);
	}







	// ---------------------------------------------------------------------------------
	//     Feature Types
	// ---------------------------------------------------------------------------------
	// Feature types are used to specify the kind of value used to represent the value
	// of a feature. Features may be represented using simple, atomic value like a
	// single integer, a float or a boolean. However, features may also be more complex.
	// For instance, a feature may be a pair of values, a tuple of heterogeneous values
	// or a list of homogeneously typed values.
	//
	// The C++ type Type can be used to describe the structure of a feature value.
	//


	typedef utils::properties::Property<Value> Type;
	typedef typename Type::ptr TypePtr;

	typedef utils::properties::TupleProperty<Value> TupleType;
	typedef typename TupleType::ptr TupleTypePtr;

	typedef utils::properties::ListProperty<Value> ListType;
	typedef typename ListType::ptr ListTypePtr;



	/**
	 *  The declaration of a atom-type builder. The actual implementation
	 *  is specialized below.
	 */
	template<typename T> TypePtr atom(const string& desc = "");

	template<>
	inline TypePtr atom<bool>(const string& desc) {
		return utils::properties::atom<Value,bool>(desc);
	}

	template<>
	inline TypePtr atom<int>(const string& desc) {
		return utils::properties::atom<Value,int>(desc);
	}

	template<>
	inline TypePtr atom<float>(const string& desc) {
		return utils::properties::atom<Value,float>(desc);
	}

	template<>
	inline TypePtr atom<double>(const string& desc) {
		return utils::properties::atom<Value,double>(desc);
	}


	/**
	 * Creates a tuple type being composed of the given types using the given description.
	 *
	 * @param desc the description for the resulting type
	 * @param params the type to be combined
	 * @return the requested tuple type instance
	 */
	template<typename ... Params>
	inline TupleTypePtr tuple(const string& desc, const Params& ... params) {
		return utils::properties::tuple<Value>(desc, params ...);
	}

	/**
	 * Creates a tuple type being composed of the given types using the given description.
	 *
	 * @param desc the description for the resulting type
	 * @param params the types to be combined
	 * @return the requested tuple type instance
	 */
	template<typename ... Params>
	inline TupleTypePtr tuple(const char* desc, const Params& ... params) {
		return tuple(string(desc), params ...);
	}

	/**
	 * Creates a tuple type being composed of the given types without any description.
	 *
	 * @param params the types to be combined
	 * @return the requested tuple type instance
	 */
	template<typename ... Params>
	inline TupleTypePtr tuple(const Params& ... params) {
		return tuple(string(""), params ...);
	}

	/**
	 * Creates a new tuple type based on the given types.
	 */
	inline TupleTypePtr tuple(const vector<TypePtr>& types) {
		return utils::properties::toTuple<Value>("",types);
	}


	/**
	 * Creates a list type requesting lists of types of the given type.
	 *
	 * @param desc the description for the resulting type
	 * @param elementType the type of element to be stored within values being assigned to this type
	 * @return the requested list type instance
	 */
	inline ListTypePtr list(const string& desc, const TypePtr& elementType) {
		return std::make_shared<ListType>(desc, elementType);
	}

	/**
	 * Creates a list type requesting lists of types of the given type.
	 *
	 * @param elementType the type of element to be stored within values being assigned to this type
	 * @return the requested list type instance
	 */
	inline ListTypePtr list(const TypePtr& elementType) {
		return list("", elementType);
	}


} // end namespace features
} // end namespace analysis
} // end namespace insieme

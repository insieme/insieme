/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#pragma once

#include <memory>
#include <vector>
#include <string>

#include "insieme/core/forward_decls.h"

#include "insieme/utils/properties.h"
#include "insieme/transform/filter/filter.h"

namespace insieme {
namespace transform {

	// forward declarations for the transformations

	class Transformation;
	typedef std::shared_ptr<Transformation> TransformationPtr;

	namespace parameter {

		/**
		 * Transformations within the Insieme compiler can be parameterized. For instance,
		 * the loop unrolling factor of an unrolling transformation is represented by a
		 * parameter.
		 *
		 * To support an optimizer to generically compose and parameterize transformations,
		 * a meta level is required, enabling code to automatically deduce required parameters.
		 * Further, a data type for storing the actual values for the parameters needs to be
		 * provided.
		 *
		 * Within this header file, those two components - the utilities for describing
		 * transformation parameters on a meta-level and the ability to define values for
		 * parameters in a generic way - are defined and mostly implemented.
		 *
		 * The implementation is based on generalized properties infrastructure provided
		 * by the insieme utils.
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
		 * The type used to represent parameter values. The implementation is based on a
		 * recursive boost-variant type which allows to form recursive, type save union
		 * types.
		 *
		 * For more details, see boost documentation.
		 */
		typedef utils::properties::make_value_type<bool, // a list of potential property types
		                                           int, unsigned, TransformationPtr, filter::Filter, filter::TargetFilter>::type Value;


		// -- Value utilities -------------------------------------------------------------

		/**
		 * A constant representing an empty value. This value might be used to parameterize
		 * transformations which do not expose any parameters.
		 */
		extern const Value emptyValue;

		/**
		 * A generic factory method allowing to convert a actual value into a Value instance.
		 * For instance, makeValue(12) is producing a value instance containing the integer 12.
		 *
		 * @tparam T the type of value to be encoded (typically automatically deduced)
		 * @param value the value to be encoded
		 * @return the given value represented as a transformation parameter value
		 */
		template <typename T>
		inline Value makeValue(T value) {
			return utils::properties::makeValue<Value>(value);
		}

		/**
		 * A generic factory method allowing to combine predefined values into a new value.
		 *
		 * @tparam T the variadic template parameter used to enable variable length lists of values
		 * @param values the values to be combined
		 * @return the value representing the combination of the given values
		 */
		template <typename... T>
		inline Value combineValues(const T&... values) {
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
		template <typename T>
		inline T getValue(const Value& value) {
			return utils::properties::getValue<T>(value);
		}

		/**
		 * This generic method extracts a value form a composed Value instance. The parameters
		 * first and path can be used to specify a path selecting the value to be read.
		 *
		 * @tparam T the type of the value to be read from the given value
		 * @tparam Rest the variadic template parameters used to support an arbitrary path
		 * @param value the value to be read
		 * @param first the first step along the path
		 * @param rest the remaining steps along the path
		 * @return the extracted value
		 */
		template <typename T, typename... Path>
		inline T getValue(const Value& value, Path... rest) {
			return utils::properties::getValue<T>(value, rest...);
		}

		/**
		 * A generic utility function allowing to test whether a value is storing an
		 * element of a given type.
		 *
		 * @tparam T the type to be tested for
		 * @return true if value stores an instance of T, false otherwise
		 */
		template <typename T>
		bool isTypeOf(const Value& value) {
			return utils::properties::isTypeOf<T>(value);
		}


		// ---------------------------------------------------------------------------------
		//     Parameters
		// ---------------------------------------------------------------------------------
		// Parameters are used to describe the parameters required by transformations.
		// Parameters can be atomic values (integers, strings, NodePtrs, other transformations)
		// and compositions of those. Compositions might be tuples (fixed length, heterogeneous
		// type collection) or lists (variable length, homogeneous type collection).
		//
		// Parameters can be created using utility functions / constructors. To create an atomic
		// type the utility atom<X>() can be used, where X represents the type to be represented.
		// To generate a combination of a given list of types, the tuple(..) constructor should be
		// used.
		//
		// Each parameter may be equipped with an additional description. Finally, the ParameterPrinter
		// can be used to print the description of a set of parameters in a user-readable manner.
		//


		typedef utils::properties::Property<Value> Parameter;
		typedef typename Parameter::ptr ParameterPtr;

		typedef utils::properties::TupleProperty<Value> TupleParameter;
		typedef typename TupleParameter::ptr TupleParameterPtr;

		typedef utils::properties::ListProperty<Value> ListParameter;
		typedef typename ListParameter::ptr ListParameterPtr;


		/**
		 *  The declaration of a atom-parameter builder. The actual implementation
		 *  is specialized below.
		 */
		template <typename T>
		ParameterPtr atom(const string& desc = "");

		template <>
		inline ParameterPtr atom<bool>(const string& desc) {
			return utils::properties::atom<Value, bool>(desc);
		}

		template <>
		inline ParameterPtr atom<int>(const string& desc) {
			return utils::properties::atom<Value, int>(desc);
		}

		template <>
		inline ParameterPtr atom<unsigned>(const string& desc) {
			return utils::properties::atom<Value, unsigned>(desc);
		}

		template <>
		inline ParameterPtr atom<TransformationPtr>(const string& desc) {
			return utils::properties::atom<Value, TransformationPtr>(desc, "TransformationPtr");
		}

		template <>
		inline ParameterPtr atom<filter::Filter>(const string& desc) {
			return utils::properties::atom<Value, filter::Filter>(desc, "Filter");
		}

		template <>
		inline ParameterPtr atom<filter::TargetFilter>(const string& desc) {
			return utils::properties::atom<Value, filter::TargetFilter>(desc, "TargetFilter");
		}

		/**
		 * Creates a tuple parameter being composed of the given parameters using the given description.
		 *
		 * @param desc the description for the resulting parameter
		 * @param params the parameters to be combined
		 * @return the requested tuple parameter instance
		 */
		template <typename... Params>
		inline TupleParameterPtr tuple(const string& desc, const Params&... params) {
			return utils::properties::tuple<Value>(desc, params...);
		}

		/**
		 * Creates a tuple parameter being composed of the given parameters using the given description.
		 *
		 * @param desc the description for the resulting parameter
		 * @param params the parameters to be combined
		 * @return the requested tuple parameter instance
		 */
		template <typename... Params>
		inline TupleParameterPtr tuple(const char* desc, const Params&... params) {
			return tuple(string(desc), params...);
		}

		/**
		 * Creates a tuple parameter being composed of the given parameters without any description.
		 *
		 * @param params the parameters to be combined
		 * @return the requested tuple parameter instance
		 */
		template <typename... Params>
		inline TupleParameterPtr tuple(const Params&... params) {
			return tuple(string(""), params...);
		}


		/**
		 * Creates a list parameter requesting lists of parameters of the given type.
		 *
		 * @param desc the description for the resulting parameter
		 * @param elementType the type of element to be stored within values being assigned to this parameter
		 * @return the requested list parameter instance
		 */
		inline ListParameterPtr list(const string& desc, const ParameterPtr& elementType) {
			return std::make_shared<ListParameter>(desc, elementType);
		}

		/**
		 * Creates a list parameter requesting lists of parameters of the given type.
		 *
		 * @param elementType the type of element to be stored within values being assigned to this parameter
		 * @return the requested list parameter instance
		 */
		inline ListParameterPtr list(const ParameterPtr& elementType) {
			return list("", elementType);
		}

		/**
		 * A constructor producing a parameter description representing no parameters.
		 */
		inline ParameterPtr no_parameters() {
			return tuple();
		}


		// -- Value Factory Utility -----------------------------------------------------------


		typedef utils::properties::AtomicProperty<Value, bool> BoolParameter;
		typedef std::shared_ptr<BoolParameter> BoolParameterPtr;

		typedef utils::properties::AtomicProperty<Value, int> IntParameter;
		typedef std::shared_ptr<IntParameter> IntParameterPtr;

		typedef utils::properties::AtomicProperty<Value, unsigned> UIntParameter;
		typedef std::shared_ptr<UIntParameter> UIntParameterPtr;

		typedef utils::properties::AtomicProperty<Value, TransformationPtr> TransformationParameter;
		typedef std::shared_ptr<TransformationParameter> TransformationParameterPtr;

		typedef utils::properties::AtomicProperty<Value, filter::Filter> FilterParameter;
		typedef std::shared_ptr<FilterParameter> FilterParameterPtr;

		typedef utils::properties::AtomicProperty<Value, filter::TargetFilter> TargetFilterParameter;
		typedef std::shared_ptr<TargetFilterParameter> TargetFilterParameterPtr;

		typedef utils::properties::TupleProperty<Value> TupleParameter;
		typedef std::shared_ptr<TupleParameter> TupleParameterPtr;

		typedef utils::properties::ListProperty<Value> ListParameter;
		typedef std::shared_ptr<ListParameter> ListParameterPtr;

		namespace {

			template <typename ValueFactory>
			Value createValueInternal(const ParameterPtr& parameter, ValueFactory& factory) {
				if(parameter->isAtomic()) {
					// handle atomic types
					if(auto cur = dynamic_pointer_cast<BoolParameter>(parameter)) { return factory(cur); }
					if(auto cur = dynamic_pointer_cast<IntParameter>(parameter)) { return factory(cur); }
					if(auto cur = dynamic_pointer_cast<UIntParameter>(parameter)) { return factory(cur); }
					if(auto cur = dynamic_pointer_cast<TransformationParameter>(parameter)) { return factory(cur); }
					if(auto cur = dynamic_pointer_cast<FilterParameter>(parameter)) { return factory(cur); }
					if(auto cur = dynamic_pointer_cast<TargetFilterParameter>(parameter)) { return factory(cur); }
					assert_fail() << "Invalid Atomic Parameter Type encountered.";
				}

				if(auto cur = dynamic_pointer_cast<ListParameter>(parameter)) { return factory(cur); }
				if(auto cur = dynamic_pointer_cast<TupleParameter>(parameter)) { return factory(cur); }
				assert_fail() << "Invalid Parameter Type encountered.";
				return 0;
			}
		}


		/**
		 * A static visitor utility helping to construct values for parameter types using
		 * a generic factory function. The passed in factory has to accept Parameter pointers or
		 * pointers to specialized parameters and return instances of the corresponding values.
		 *
		 * @param parameter the parameter for which a value should be generated
		 * @param factory the factory to be used for generating the value
		 * @return a value forming a valid instantiation for the given parameters
		 */
		template <typename ValueFactory>
		Value createValue(const ParameterPtr& parameter, ValueFactory& factory) {
			Value res = createValueInternal(parameter, factory);
			assert_true(parameter->isValid(res)) << "Invalid value produced by factory!";
			return res;
		}

	} // end namespace parameter
} // end namespace transform
} // end namespace insieme

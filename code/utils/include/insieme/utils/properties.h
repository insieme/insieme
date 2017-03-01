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
 *
 */
#pragma once

#include <memory>
#include <vector>
#include <string>
#include <iostream>

#include <boost/utility.hpp>
#include <boost/type_traits/is_base_of.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/variant.hpp>
#include <boost/variant/recursive_variant.hpp>

#include "insieme/utils/printable.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace utils {
namespace properties {

	/**
	 * Within various places within the compiler, structured properties need to be
	 * represented. Examples for those are transformation parameters or extracted code
	 * features. The generic classes within this file provide utilities to build the
	 * necessary data structures to specify the format and values of such properties.
	 *
	 * Within this file, a generic mechanism to describe composed values and to define
	 * and document and verify the corresponding structure is implemented. Instantiations
	 * of the Value template can be used to represent arbitrarily composed and nested values
	 * build on top of a variable number of atomic types. Instances of the Property template
	 * and its sub-classes can be used to define and document required structures.
	 */


	using std::vector;
	using std::string;


	// ---------------------------------------------------------------------------------
	//     Values
	// ---------------------------------------------------------------------------------
	//
	// Values are implemented based on a recursive boost-variant type. The list of atomic
	// data types values should be based on is left open for the actual application.
	// In addition, various utilities simplifying the handling of values are offered.
	//
	//  Usage:
	//			to create a value the factory methods
	//					- makeValue(..)
	//					- combineValue(..)
	//					- emptyValue()
	//			should be used. For instance, to create a value representing the
	//			integer value 12, makeValue(12) should be used. To merge existing
	//			values a and b into a composed values, makeComposedValue(a,b) should be
	//			used.
	//


	// WORK-AROUND - START ----------------------

	namespace {

		/**
		 * This helper is required to circumvent a limitation in the current implementation of GCC.
		 * GCC is not able to expand variadic template parameters into a type with a fixed size argument list.
		 * Therefore, old-style variadic templates are required.
		 */
		template <typename... T>
		struct value_helper;

		template <typename T0>
		struct value_helper<T0> {
			typedef typename boost::make_recursive_variant<T0, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1>
		struct value_helper<T0, T1> {
			typedef typename boost::make_recursive_variant<T0, T1, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2>
		struct value_helper<T0, T1, T2> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2, typename T3>
		struct value_helper<T0, T1, T2, T3> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, T3, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2, typename T3, typename T4>
		struct value_helper<T0, T1, T2, T3, T4> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, T3, T4, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5>
		struct value_helper<T0, T1, T2, T3, T4, T5> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, T3, T4, T5, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6>
		struct value_helper<T0, T1, T2, T3, T4, T5, T6> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, T3, T4, T5, T6, vector<boost::recursive_variant_>>::type variant_type;
		};

		template <typename T0, typename T1, typename T2, typename T3, typename T4, typename T5, typename T6, typename T7>
		struct value_helper<T0, T1, T2, T3, T4, T5, T6, T7> {
			typedef typename boost::make_recursive_variant<T0, T1, T2, T3, T4, T5, T6, T7, vector<boost::recursive_variant_>>::type variant_type;
		};
	}

	// WORK-AROUND - END ------------------------

	/**
	 * The utility used to build value types. This type accepts a list of
	 * atomic values to be stored within the value. Internally, it is
	 * mapping the given element types to a recursive variant type.
	 *
	 * For more details, see boost documentation.
	 */
	template <typename First, typename... Rest>
	struct make_value_type {
		// not yet implemented within GCC => re-enable when supported
		//		typedef typename boost::make_recursive_variant<First, Rest..., vector<boost::recursive_variant_>>::type type;

		// workaround solution
		typedef typename value_helper<First, Rest...>::variant_type type;
	};


	// -- type identifier --------------------------------------------------------------

	namespace {

		/**
		 * A static visitor used to determine the value being stored within a
		 * boost variant. is_type_of<X>(y) is true only if y is a variant storing
		 * a value of type X.
		 */
		template <typename T>
		struct is_type_of : public boost::static_visitor<bool> {
			// the case in which the property is true => the type is correct.
			bool operator()(const T& /*cur*/) const {
				return true;
			}

			// all other cases are handled generically
			template <typename S>
			bool operator()(const S& /*cur*/) const {
				return false;
			}
		};
	}

	/**
	 * A generic utility function allowing to test whether a value is storing an
	 * element of a given type.
	 *
	 * @tparam T the type to be tested for
	 * @tparam C the atomic value types used within the value
	 * @param value the value to be tested
	 * @return true if value stores an instance of T, false otherwise
	 */
	template <typename T, typename Value>
	bool isTypeOf(const Value& value) {
		static const is_type_of<T> visitor = is_type_of<T>();
		return boost::apply_visitor(visitor, value);
	}


	// -- Value utilities -------------------------------------------------------------


	/**
	 * A generic factory method allowing to convert a actual value into a Value instance.
	 * For instance, makeValue(12) is producing a value instance containing the integer 12.
	 *
	 * @tparam Value the kind of value to be created
	 * @tparam T the type of value to be encoded (typically automatically deduced)
	 * @param value the value to be encoded
	 * @return the given value represented as a transformation property value
	 */
	template <typename Value, typename T>
	inline Value makeValue(T value) {
		return Value(value);
	}

	/**
	 * A generic factory method allowing to combine predefined values into a new value.
	 *
	 * @tparam Value the kind of value to be created
	 * @tparam T the variadic template property used to enable variable length lists of values
	 * @param values the values to be combined
	 * @return the value representing the combination of the given values
	 */
	template <typename Value, typename... T>
	inline Value combineValues(const T&... values) {
		return Value(toVector<Value>(values...));
	}

	/**
	 * A generic factory method allowing to combine predefined values into a new value.
	 *
	 * @tparam Value the kind of value to be created
	 * @param values the values to be combined
	 * @return the value representing the combination of the given values
	 */
	template <typename Value>
	inline Value combineValues(const vector<Value>& values) {
		return Value(values);
	}

	/**
	 * A generic factory method creating an empty value instance.
	 *
	 * @tparam Value the kind of value to be created
	 * @return the requested value instance
	 */
	template <typename Value>
	inline Value emptyValue() {
		return combineValues<Value>();
	}

	/**
	 * This generic method extracts a value stored within a value instance. The type
	 * stored within the value has to be provided as a template argument. In case the
	 * given type argument does not correspond to the stored value, an assertion is triggered.
	 *
	 * @tparam T the type of value to be extracted
	 * @tparam Value the kind of value to be read from
	 * @param the value which should be read
	 * @return the value retrieved from the given encoded value
	 */
	template <typename T, typename Value, typename boost::disable_if<boost::is_same<T, Value>, int>::type = 0>
	inline T getValue(const Value& value) {
		assert_true(isTypeOf<T>(value)) << "Tried reading value of incompatible Value instance!";
		return boost::get<T>(value);
	}

	/**
	 * A specialization of the getValue template function handling the extraction of a Value
	 * instance from a value. The operation is corresponding to the identity. It also is a
	 * potential terminal case for the generic getValue(..) function extracting values
	 * from any location within a composed value.
	 *
	 * @tparam Value the type of value to be read
	 * @param value the value to read
	 * @return the handed in value
	 */
	template <typename Value>
	inline Value getValue(const Value& value) {
		return value;
	}

	/**
	 * This generic method extracts a value form a composed Value instance. The properties
	 * first and path can be used to specify a path selecting the value to be read.
	 *
	 * @tparam T the type of the value to be read from the given value
	 * @tparam Value the kind of value to be read from
	 * @tparam Rest the variadic template properties used to support an arbitrary path
	 * @param value the value to be read
	 * @param first the first step along the path
	 * @param rest the remaining steps along the path
	 * @return the extracted value
	 */
	template <typename T, typename Value, typename... Rest>
	inline T getValue(const Value& value, int first, Rest... rest) {
		assert_true(isTypeOf<vector<Value>>(value)) << "Not nested value encountered!";
		return getValue<T>(boost::get<vector<Value>>(value)[first], rest...);
	}


	// ---------------------------------------------------------------------------------
	//     Property
	// ---------------------------------------------------------------------------------
	// Properties are used to describe the type structure of value instances.
	// Properties can be atomic values (boolean, integers, floats, strings, ...)
	// or compositions of those. Compositions might be tuples (fixed length, heterogeneous
	// type collection) or lists (variable length, homogeneous type collection).
	//
	// Properties can be created using utility functions / constructors. To create an atomic
	// type the utility atom<X>() can be used, where X represents the type to be represented.
	// To generate a combination of a given list of types, the tuple(..) constructor should be
	// used. To generate a list-property, the list(..) constructor can be used.
	//
	// Each property may be equipped with an additional description. Finally, the InfoPrinter
	// can be used to print the description of a set of properties in a user-readable manner.
	//

	/**
	 * The abstract base class for all kind of properties.
	 */
	template <typename Value>
	class Property : public utils::VirtualPrintable, boost::noncopyable {
	  public:
		/**
		 * A type definition defining the value type this property is associated to.
		 */
		typedef Value value_type;

		/**
		 * The type definition for a property pointer.
		 */
		typedef std::shared_ptr<Property<Value>> ptr;

	  private:
		/**
		 * A flag indicating whether this property is atomic or not.
		 */
		bool atomic;

		/**
		 * The description associated to this property.
		 */
		string description;

		/**
		 * The various sub-properties of this property (only present if it is a composed property).
		 */
		vector<typename Property<Value>::ptr> components;

	  public:
		/**
		 * Creates a new property using the given description and sub-properties.
		 *
		 * @param description the description to be attached to the resulting property
		 * @param components the components this property is consisting of
		 */
		Property(bool atomic, const string& description, const vector<typename Property<Value>::ptr>& components = vector<typename Property<Value>::ptr>())
		    : atomic(atomic), description(description), components(components) {}

		/**
		 * A virtual destructor required by this abstract virtual base class.
		 */
		virtual ~Property(){};

		/**
		 * Test whether the given value is a valid instantiation of this property set.
		 *
		 * @param value the value to be tested
		 * @return true if so, false otherwise
		 */
		virtual bool isValid(const Value& value) const = 0;

		/**
		 * Obtains a reference to the description attached to this property.
		 */
		const string& getDescription() const {
			return description;
		}

		/**
		 * Obtains a reference to the sub-properties this property is consisting of.
		 * The list will be empty if this property is an atomic property.
		 *
		 * @return the list of properties this property is combining
		 */
		const vector<typename Property<Value>::ptr>& getComponents() const {
			return components;
		}

		/**
		 * Tests whether this property is an atomic property or not.
		 */
		bool isAtomic() const {
			return atomic;
		}
	};


	// -- Atomic Properties ---------------------------------------------

	/**
	 * The abstract base-type of all atomic properties.
	 */
	template <typename Value, typename AtomicType>
	class AtomicProperty : public Property<Value> {
	  public:
		/**
		 * The type definition for a property pointer.
		 */
		typedef std::shared_ptr<AtomicProperty<Value, AtomicType>> ptr;

	  private:
		/**
		 * The name of the type expected for this property.
		 */
		string typeName;

	  public:
		/**
		 * Creates a new atomic property based on the given type name and description.
		 *
		 * @param typeName the name of the type of the property to be presented
		 * @param description the description to be attached to the resulting property
		 */
		AtomicProperty(const string& typeName, const string& description) : Property<Value>(true, description), typeName(typeName) {}

		/**
		 * A virtual destructor for this virtual class.
		 */
		virtual ~AtomicProperty(){};

		/**
		 * Obtains the name of the expected type of value used to instantiate the
		 * represented property.
		 *
		 * @return the name of the expected type
		 */
		const string& getTypeName() const {
			return typeName;
		}

		/**
		 * Provides a readable summary of this properties type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << typeName;
		}

		/**
		 * Tests whether the given value has is an instance of the represented type.
		 */
		virtual bool isValid(const Value& value) const {
			return isTypeOf<AtomicType>(value);
		}
	};


	// -- Composed Properties ---------------------------------------------

	/**
	 * The property type used for combining other properties.
	 */
	template <typename Value>
	class TupleProperty : public Property<Value> {
	  public:
		/**
		 * The type definition for a property pointer.
		 */
		typedef std::shared_ptr<TupleProperty<Value>> ptr;

		/**
		 * Creates a new property froming the combination of the givne properties.
		 *
		 * @param description the description of the resulting property
		 * @param elements the elements to be combined within the resulting property
		 */
		TupleProperty(const string& description, const vector<typename Property<Value>::ptr>& elements) : Property<Value>(false, description, elements) {}

		/**
		 * A virtual destructor for this virtual class.
		 */
		virtual ~TupleProperty(){};

		/**
		 * Tests whether the given value is representing a value being within the domain
		 * of this property.
		 *
		 * @param value the value to be tested
		 * @return true if it is valid, false otherwise
		 */
		virtual bool isValid(const Value& value) const {
			// test whether it is a list of values
			if(!isTypeOf<vector<Value>, Value>(value)) { return false; }

			// test type of values within list
			const vector<Value>& values = boost::get<vector<Value>>(value);
			const vector<typename Property<Value>::ptr>& params = TupleProperty<Value>::getComponents();
			if(values.size() != params.size()) { return false; }

			auto paired = make_paired_range(values, params);
			return ::all(paired, [](const std::pair<Value, typename Property<Value>::ptr>& cur) { return cur.second->isValid(cur.first); });
		}

		/**
		 * Provides a readable summary of this properties type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "(" << join(",", Property<Value>::getComponents(), print<deref<typename Property<Value>::ptr>>()) << ")";
		}
	};

	/**
	 * The property type used for forming properties containing a list of other properties.
	 */
	template <typename Value>
	class ListProperty : public Property<Value> {
	  public:
		/**
		 * The type definition for a property pointer.
		 */
		typedef std::shared_ptr<ListProperty<Value>> ptr;

		/**
		 * Creates a property forming a list of the given property type.
		 *
		 * @param description the description of the resulting property
		 * @param elementType the type of element to be present within the list
		 */
		ListProperty(const string& description, const typename Property<Value>::ptr& elementType)
		    : Property<Value>(false, description, toVector<typename Property<Value>::ptr>(elementType)) {}

		/**
		 * A virtual destructor for this virtual class.
		 */
		virtual ~ListProperty(){};

		/**
		 * Tests whether the given value is representing a value being within the domain
		 * of this property.
		 *
		 * @param value the value to be tested
		 * @return true if it is valid, false otherwise
		 */
		virtual bool isValid(const Value& value) const {
			// test whether it is a list of values
			if(!isTypeOf<vector<Value>, Value>(value)) { return false; }

			// test type of values within list
			const vector<Value>& values = boost::get<vector<Value>>(value);
			const typename Property<Value>::ptr& elementType = ListProperty<Value>::getElementType();

			return ::all(values, [&](const Value& cur) { return elementType->isValid(cur); });
		}

		/**
		 * Provides a readable summary of this properties type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << *ListProperty<Value>::getElementType() << "*]";
		}

		/**
		 * Obtains the element type stored within this list.
		 *
		 * @return a pointer to the type of element stored within lists of this property
		 */
		const typename Property<Value>::ptr& getElementType() const {
			assert_eq(Property<Value>::getComponents().size(), 1u) << "Invalid size of component vector!";
			return Property<Value>::getComponents()[0];
		}
	};

	// -- Property Constructors -------------------------------------------------------

	/**
	 * A factory method creating a empty property specification.
	 */
	template <typename Value>
	const typename TupleProperty<Value>::ptr emptyProperties() {
		return std::make_shared<TupleProperty<Value>>();
	}


	namespace detail {

		// predefined list of names for well known types ...
		template <typename T>
		inline const char* getName();

		template <>
		inline const char* getName<bool>() {
			return "bool";
		};
		template <>
		inline const char* getName<int>() {
			return "int";
		};
		template <>
		inline const char* getName<unsigned>() {
			return "unsigned";
		};
		template <>
		inline const char* getName<float>() {
			return "float";
		};
		template <>
		inline const char* getName<double>() {
			return "double";
		};

		template <>
		inline const char* getName<string>() {
			return "string";
		};

		template <>
		inline const char* getName<uint64_t>() {
			return "uint64";
		};
		template <>
		inline const char* getName<int64_t>() {
			return "int64";
		};
	}

	/**
	 * Creates a new atomic property based on the given parameters.
	 *
	 * @tparam AtomicValue the type to be represented
	 * @param desc an optional description of this property
	 * @param typeName the name of the represented type
	 * @return the requested parameter
	 */
	template <typename Value, typename AtomicType>
	inline typename AtomicProperty<Value, AtomicType>::ptr atom(const string& desc = "", const char* typeName = detail::getName<AtomicType>()) {
		return std::make_shared<AtomicProperty<Value, AtomicType>>(typeName, desc);
	}

	/**
	 * Creates a tuple property being composed of the given properties using the given description.
	 *
	 * @param desc the description for the resulting property
	 * @param elements the properties to be combined
	 * @return the requested tuple property instance
	 */
	template <typename Value>
	inline typename TupleProperty<Value>::ptr toTuple(const string& desc, const vector<typename Property<Value>::ptr>& elements) {
		return std::make_shared<TupleProperty<Value>>(desc, elements);
	}

	/**
	 * Creates a tuple property being composed of the given properties using the given description.
	 *
	 * @param desc the description for the resulting property
	 * @param params the properties to be combined
	 * @return the requested tuple property instance
	 */
	template <typename Value, typename... Params>
	inline typename TupleProperty<Value>::ptr tuple(const string& desc, const Params&... params) {
		typedef typename Property<Value>::ptr ptr;
		return std::make_shared<TupleProperty<Value>>(desc, toVector<ptr>(params...));
	}

	/**
	 * Creates a tuple property being composed of the given properties using the given description.
	 *
	 * @param desc the description for the resulting property
	 * @param params the properties to be combined
	 * @return the requested tuple property instance
	 */
	template <typename Value, typename... Params>
	inline typename TupleProperty<Value>::ptr tuple(const char* desc, const Params&... params) {
		return tuple<Value>(string(desc), params...);
	}

	/**
	 * Creates a tuple property being composed of the given properties without any description.
	 *
	 * @param params the properties to be combined
	 * @return the requested tuple property instance
	 */
	template <typename Value, typename... Params>
	inline typename TupleProperty<Value>::ptr tuple(const Params&... params) {
		return tuple<Value>(string(""), params...);
	}


	/**
	 * Creates a list property requesting lists of properties of the given type.
	 *
	 * @param desc the description for the resulting property
	 * @param elementType the type of element to be stored within values being assigned to this property
	 * @return the requested list property instance
	 */
	template <typename Value>
	inline typename ListProperty<Value>::ptr list(const string& desc, const typename Property<Value>::ptr& elementType) {
		return std::make_shared<ListProperty<Value>>(desc, elementType);
	}

	/**
	 * Creates a list property requesting lists of properties of the given type.
	 *
	 * @param elementType the type of element to be stored within values being assigned to this property
	 * @return the requested list property instance
	 */
	template <typename Value>
	inline typename ListProperty<Value>::ptr list(const typename Property<Value>::ptr& elementType) {
		return list("", elementType);
	}

	// -- Property Printer ----------------------------------------------------------

	/**
	 * A printer formating the information stored within a property to be presented
	 * to the user. It can also be used for debugging.
	 */
	template <typename Value>
	class InfoPrinter : public utils::Printable {
		typedef typename Property<Value>::ptr PropertyPtr;

		/**
		 * The properties to be printed.
		 */
		PropertyPtr property;

	  public:
		/**
		 * Creates a new instance printing the given property.
		 * @param the property to be printed.
		 */
		InfoPrinter(const typename Property<Value>::ptr& property) : property(property) {}

		/**
		 * Realizes the actual printing of the properties passed via the constructor.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			out << "Parameters: \n";
			printFormated(out, property, 4);
			out << "\n";
			return out;
		}

	  private:
		/**
		 * Conducts the actual printing (recursively).
		 */
		void printFormated(std::ostream& out, const PropertyPtr& ptr, unsigned indent) const {
			typedef typename TupleProperty<Value>::ptr TuplePropertyPtr;
			typedef typename ListProperty<Value>::ptr ListPropertyPtr;

			const int column = 20;

			string desc = ptr->getDescription();
			if(!desc.empty()) { desc = " ... " + desc; }

			if(ptr->isAtomic()) {
				const string& name = toString(*ptr);
				out << times(" ", indent) << name << times(" ", column - indent - name.length()) << desc << "\n";
				return;
			}

			if(TuplePropertyPtr tuple = std::dynamic_pointer_cast<TupleProperty<Value>>(ptr)) {
				out << times(" ", indent) << "( \n";
				for_each(ptr->getComponents(), [&](const PropertyPtr& cur) { this->printFormated(out, cur, indent + 4); });
				out << times(" ", indent) << ") " << times(" ", column - indent - 2) << desc << "\n";
				return;
			}

			if(ListPropertyPtr list = std::dynamic_pointer_cast<ListProperty<Value>>(ptr)) {
				out << times(" ", indent) << "[ \n";
				for_each(ptr->getComponents(), [&](const PropertyPtr& cur) { this->printFormated(out, cur, indent + 4); });
				out << times(" ", indent) << "]* " << times(" ", column - indent - 3) << desc << "\n";
				return;
			}

			std::cout << "ERROR: unexpeted parameter type: " << *ptr << "\n";
			assert_fail() << "Unexpected Parameter type encountered!";
		}
	};

	/**
	 * Creates a printable object presenting property information in a user-readable format.
	 *
	 * @param property the property to be printed
	 * @return a printable info instance presenting the given properties in a user-readable format
	 */
	template <typename Ptr, typename Value = typename Ptr::element_type::value_type>
	InfoPrinter<Value> printInfo(const Ptr& property) {
		return InfoPrinter<Value>(property);
	}


} // end namespace property
} // end namespace transform
} // end namespace insieme

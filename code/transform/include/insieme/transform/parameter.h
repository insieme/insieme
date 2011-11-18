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
	 */


	using std::vector;
	using std::string;
	using std::array;


	class Parameter;
	typedef std::shared_ptr<Parameter> ParameterPtr;

	class AtomicParameter;
	typedef std::shared_ptr<AtomicParameter> AtomicParameterPtr;

	class TupleParameter;
	typedef std::shared_ptr<TupleParameter> TupleParameterPtr;

	class ListParameter;
	typedef std::shared_ptr<ListParameter> ListParameterPtr;




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
	typedef boost::make_recursive_variant<
			int,
			string,
			TransformationPtr,
			vector<boost::recursive_variant_>
	>::type Value;


	// -- type identifier --------------------------------------------------------------

	namespace {

		/**
		 * A static visitor used to determine the value being stored within a
		 * boost variant. is_type_of<X>(y) is true only if y is a variant storing
		 * a value of type X.
		 */
		template<typename T>
		struct is_type_of : public boost::static_visitor<bool> {

			// the case in which the property is true => the type is correct.
			bool operator()(const T& cur) const { return true; }

			// all other cases are handled generically
			template<typename S>
			bool operator()(const S& cur) const { return false; }
		};

	}

	/**
	 * A generic utility function allowing to test whether a value is storing an
	 * element of a given type.
	 *
	 * @tparam T the type to be tested for
	 * @param value the value to be tested
	 * @return true if value stores an instance of T, false otherwise
	 */
	template<typename T>
	bool isTypeOf(const Value& value) {
		static is_type_of<T> visitor;
		return boost::apply_visitor(visitor, value);
	}


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
	template<typename T>
	inline Value makeValue(T value) {
		return Value(value);
	}

	/**
	 * A generic factory method allowing to combine predefined values into a new value.
	 *
	 * @tparam T the variadic template parameter used to enable variable length lists of values
	 * @param values the values to be combined
	 * @return the value representing the combination of the given values
	 */
	template<typename ... T>
	inline Value combineValues(const T& ... values) {
		return Value(toVector<Value>(values...));
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
		assert(isTypeOf<T>(value) && "Tried reading value of incompatible Value instance!");
		return boost::get<T>(value);
	}

	/**
	 * A specialization of the getValue template function handling the extraction of a Value
	 * instance from a value. The operation is corresponding to the identity. It also is a
	 * potential terminal case for the generic getValue(..) function extracting values
	 * from any location within a composed value.
	 *
	 * @param value the value to read
	 * @return the handed in value
	 */
	template<>
	inline Value getValue<Value>(const Value& value) {
		return value;
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
	template<typename T, typename ... Rest>
	inline T getValue(const Value& value, int first, Rest ... rest) {
		assert(isTypeOf<vector<Value>>(value) && "Not nested value encountered!");
		return getValue<T>(boost::get<vector<Value>>(value)[first], rest ...);
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


	/**
	 * The abstract base class for all kind of parameters.
	 */
	class Parameter : public utils::Printable, boost::noncopyable  {

		/**
		 * The description associated to this parameter.
		 */
		string description;

		/**
		 * The various sub-parameters of this parameter (only present if it is a composed parameter).
		 */
		vector<ParameterPtr> components;

	public:

		/**
		 * Creates a new parameter using the given description and sub-parameters.
		 *
		 * @tparam T used to support an arbitrary list of components
		 * @param description the description to be attached to the resulting parameter
		 * @param components the components this parameter is consisting of
		 */
		template<typename ... T>
		Parameter(const string& description, const T& ... components)
			: description(description), components(toVector<ParameterPtr>(components ...)) {}

		/**
		 * A virtual destructor required by this abstract virtual base class.
		 */
		virtual ~Parameter() {};

		/**
		 * Test whether the given value is a valid instantiation of this parameter set.
		 *
		 * @param value the value to be tested
		 * @return true if so, false otherwise
		 */
		virtual bool isValid(const Value& value) const = 0;

		/**
		 * Obtains a reference to the description attached to this parameter.
		 */
		const string& getDescription() const {
			return description;
		}

		/**
		 * Obtains a reference to the sub-parameters this parameter is consisting of.
		 * The list will be empty if this parameter is an atomic parameter.
		 *
		 * @return the list of parameters this parameter is combining
		 */
		const vector<ParameterPtr>& getComponents() const {
			return components;
		}

		/**
		 * Tests whether this parameter is an atomic parameter or not.
		 */
		bool isAtomic() const;

	};


	// -- Atomic Parameters ---------------------------------------------

	/**
	 * The abstract base-type of all atomic parameters.
	 */
	class AtomicParameter : public Parameter {

		/**
		 * The name of the type expected for this parameter.
		 */
		string typeName;

	protected:

		/**
		 * Creates a new atomic parameter based on the given type name and description.
		 *
		 * @param typeName the name of the type of the parameter to be presented
		 * @param description the description to be attached to the resulting parameter
		 */
		AtomicParameter(const string& typeName, const string& description)
			: Parameter(description), typeName(typeName) {}

	public:

		/**
		 * Obtains the name of the expected type of value used to instantiate the
		 * represented parameter.
		 *
		 * @return the name of the expected type
		 */
		const string& getTypeName() const {
			return typeName;
		}

		/**
		 * Provides a readable summary of this parameters type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << typeName;
		}
	};

	#define ATOMIC_PARAM(NAME,TYPE) \
		class NAME ## Parameter : public AtomicParameter { \
		public: \
			NAME ## Parameter(const string& description = "") : AtomicParameter(#TYPE, description) {} \
			virtual bool isValid(const Value& value) const { \
				return isTypeOf<TYPE>(value); \
			} \
		};

	/**
	 * An atomic parameter representing an integer.
	 */
	ATOMIC_PARAM(Int,int);

	/**
	 * An atomic parameter representing a string.
	 */
	ATOMIC_PARAM(String,string);

	/**
	 * An atomic parameter representing another transformation.
	 */
	ATOMIC_PARAM(Transformation,TransformationPtr);



	// -- Composed Parameters ---------------------------------------------

	/**
	 * The parameter type used for combining other parameters.
	 */
	class TupleParameter : public Parameter {
	public:

		/**
		 * Creates a parameter forming the combination of the given parameters.
		 *
		 * @param description the description of the resulting parameter
		 * @param elements the elements to be combined within the resulting parameter
		 */
		template<typename ... T>
		TupleParameter(const string& description, const T& ... elements)
			: Parameter(description, elements...) {}

		/**
		 * Tests whether the given value is representing a value being within the domain
		 * of this parameter.
		 *
		 * @param value the value to be tested
		 * @return true if it is valid, false otherwise
		 */
		virtual bool isValid(const Value& value) const;

		/**
		 * Provides a readable summary of this parameters type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "(" << join(",",getComponents(),print<deref<ParameterPtr>>()) << ")";
		}
	};

	/**
	 * The parameter type used for forming parameters containing a list of other parameters.
	 */
	class ListParameter : public Parameter {
	public:

		/**
		 * Creates a parameter forming a list of the given parameter type.
		 *
		 * @param description the description of the resulting parameter
		 * @param elementType the type of element to be present within the list
		 */
		ListParameter(const string& description, const ParameterPtr& elementType)
			: Parameter(description, elementType) {}

		/**
		 * Tests whether the given value is representing a value being within the domain
		 * of this parameter.
		 *
		 * @param value the value to be tested
		 * @return true if it is valid, false otherwise
		 */
		virtual bool isValid(const Value& value) const;

		/**
		 * Provides a readable summary of this parameters type.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return out << "[" << *getElementType() << "*]";
		}

		/**
		 * Obtains the element type stored within this list.
		 *
		 * @return a pointer to the type of element stored within lists of this parameter
		 */
		const ParameterPtr& getElementType() const {
			assert(getComponents().size() == 1u && "Invalid size of component vector!");
			return getComponents()[0];
		}
	};

	// -- Parameter Constructors -------------------------------------------------------

	/**
	 * A constant to be used when accepting no parameters at all.
	 */
	extern const TupleParameterPtr no_parameters;

	/**
	 *  The declaration of a atom-parameter builder. The actual implementation
	 *  is specialized below.
	 */
	template<typename T> AtomicParameterPtr atom(const string& desc = "");

	template<>
	inline AtomicParameterPtr atom<int>(const string& desc) {
		return std::make_shared<IntParameter>(desc);
	}

	template<>
	inline AtomicParameterPtr atom<string>(const string& desc) {
		return std::make_shared<StringParameter>(desc);
	}

	template<>
	inline AtomicParameterPtr atom<TransformationPtr>(const string& desc) {
		return std::make_shared<TransformationParameter>(desc);
	}

	/**
	 * Creates a tuple parameter being composed of the given parameters using the given description.
	 *
	 * @param desc the description for the resulting parameter
	 * @param params the parameters to be combined
	 * @return the requested tuple parameter instance
	 */
	template<typename ... Params>
	inline TupleParameterPtr tuple(const string& desc, const Params& ... params) {
		return std::make_shared<TupleParameter>(desc, params...);
	}

	/**
	 * Creates a tuple parameter being composed of the given parameters using the given description.
	 *
	 * @param desc the description for the resulting parameter
	 * @param params the parameters to be combined
	 * @return the requested tuple parameter instance
	 */
	template<typename ... Params>
	inline TupleParameterPtr tuple(const char* desc, const Params& ... params) {
		return tuple(string(desc), params ...);
	}

	/**
	 * Creates a tuple parameter being composed of the given parameters without any description.
	 *
	 * @param params the parameters to be combined
	 * @return the requested tuple parameter instance
	 */
	template<typename ... Params>
	inline TupleParameterPtr tuple(const Params& ... params) {
		return tuple(string(""), params ...);
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

	// -- Parameter Printer ----------------------------------------------------------

	/**
	 * A printer formating the information stored within a parameter to be presented
	 * to the user. It can also be used for debugging.
	 */
	class InfoPrinter : public utils::Printable {

		/**
		 * The parameters to be printed.
		 */
		ParameterPtr parameter;

	public:

		/**
		 * Creates a new instance printing the given parameter.
		 * @param the parameter to be printed.
		 */
		InfoPrinter(const ParameterPtr& parameter) : parameter(parameter) {}

		/**
		 * Realizes the actual printing of the parameters passed via the constructor.
		 */
		virtual std::ostream& printTo(std::ostream& out) const;

	};


} // end namespace parameter
} // end namespace transform
} // end namespace insieme

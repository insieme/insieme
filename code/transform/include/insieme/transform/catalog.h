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

#include <map>
#include <string>

#include "insieme/transform/parameter.h"
#include "insieme/transform/transformation.h"

#include "insieme/utils/container_utils.h"

namespace insieme {
namespace transform {

	/**
	 * Within this header file the transformation catalog infrastructure is defined. The catalog
	 * provides a list of transformations annotated with additional information enabling users / code
	 * to instantiated them. The corresponding factory mechanisms are as well offered.
	 *
	 * The catalog should be the main interface for an optimizer when interacting with the transformation
	 * environment of the Insieme Compiler core. It should shield the optimizer from the underlying details.
	 */

	using std::string;

	class Catalog;

	class TransformationType;
	typedef std::shared_ptr<TransformationType> TransformationTypePtr;


	/**
	 * Obtains a catalog containing a comprehensive list of transformations.
	 */
	Catalog getStandardCatalog();


	/**
	 * Within the Catalog, transformations are represented via transformation types. Instances of this class
	 * combine information and mechanisms required to instantiate transformations with additional information
	 * documenting the represented transformations.
	 *
	 * This abstract base class defines the interface offered to the optimizer to handle transformations in a
	 * generic way. For each transformation specialized sub-classes need to be implemented.
	 */
	class TransformationType {

		/**
		 * The name of the represented transformation. This name should be unique. It will be used within the
		 * catalog to index the maintained transformations.
		 */
		string name;

		/**
		 * A short description of the represented class of transformations.
		 */
		string description;

		/**
		 * The type of parameters required by the transformations represented by this type.
		 */
		parameter::ParameterPtr parameterInfo;

	public:

		/**
		 * Creates a new instance of this type based on the given parameters.
		 *
		 * @param name the name of the resulting type of transformation
		 * @param desc a short description of the resulting transformation
		 * @paramInfo the parameters supported by the represented transformation - by default no arguments are required.
		 */
		TransformationType(const string& name, const string& desc, const parameter::ParameterPtr& paramInfo = parameter::no_parameters)
			: name(name), description(desc), parameterInfo(paramInfo) {};

		/**
		 * A virtual destructor required by this abstract virtual base class.
		 */
		virtual ~TransformationType() {}

		/**
		 * The factory function to be used to create a new instance of the represented transformation.
		 *
		 * @param value the arguments to be used to set up the parameters for the resulting transformation.
		 * @return a pointer to an instance of the requested transformation
		 * @throws invalid_argument exception in case the given value does not fit the required parameters
		 */
		TransformationPtr createTransformation(const parameter::Value& value = parameter::emptyValue) const {
			// check the types of the handed in values
			if (!getParameterInfo()->isValid(value)) {
				throw std::invalid_argument("Handed in value not valid for this type of transformation!");
			}
			// use internal builder to produce result
			return buildTransformation(value);
		}

		/**
		 * Obtains the name of the represented transformation.
		 */
		const string& getName() const {
			return name;
		}

		/**
		 * Obtains the description associate to this transformation type.
		 */
		const string& getDescription() const {
			return description;
		}

		/**
		 * Obtains a pointer to the type of parameters expected by instances of the represented transformation type.
		 */
		const parameter::ParameterPtr& getParameterInfo() const {
			return parameterInfo;
		}

	protected:

		/**
		 * This internal member function needs to be implemented by all sub-classes. This function is conducting the actual
		 * construction of transformations of the represented type.
		 */
		virtual TransformationPtr buildTransformation(const parameter::Value& value) const = 0;

	};

	/**
	 * A macro simplifying the declaration of a transformation type.
	 */
	#define TRANSFORM_TYPE(NAME, DESC, PARAM_TYPE) \
		class NAME ## Type : public TransformationType { \
		public: \
			NAME ## Type() : TransformationType(#NAME, DESC, PARAM_TYPE) {} \
			virtual TransformationPtr buildTransformation(const parameter::Value& value) const; \
		};

	/**
	 * The Transformation catalog is an aggregation of Transformation-Meta-Information and the main
	 * utility to be used by the optimizer when selecting, instantiating and composing transformations
	 * to be applied on code within the Insieme Compiler.
	 */
	class Catalog {

		/**
		 * The container for the internally stored transformations.
		 */
		std::map<string, TransformationTypePtr> catalog;

	public:

		/**
		 * Adds a new transformation type to this catalog.
		 *
		 * @param newType the new transformation type to be added
		 */
		void add(const TransformationTypePtr& newType) {
			assert(catalog.find(newType->getName()) == catalog.end() && "Discoverd name collision!");
			catalog.insert(std::make_pair(newType->getName(), newType));
		}

		/**
		 * A generic variant which is creating and adding a new transformation type to this catalog.
		 *
		 * @tparam T the meta-type / factory type of the new transformation
		 * @param params the parameters to be forwarded to the construction of the new type
		 */
		template<
			typename T, typename ...P,
			typename boost::enable_if<boost::is_base_of<TransformationType,T>,int>::type = 0
		>
		void add(P ... params) { add(std::make_shared<T>(params...)); }

		/**
		 * Obtains the type registered to the given name.
		 *
		 * @param name the name of the transformation looking for
		 * @return the requested type or a null pointer if there is no such type
		 */
		TransformationTypePtr getTransformationType(const string& name) const {
			auto pos = catalog.find(name);
			if (pos != catalog.end()) {
				return pos->second;
			}
			return TransformationTypePtr();
		}

		/**
		 * Creates a new transformation. The given name is used to determine the type of the requested
		 * transformation and the given values are used to parameterize the result.
		 *
		 * @param name the name of the transformation for which a new instance is requested
		 * @param value the values to be used for setting up transformation parameters
		 */
		TransformationPtr createTransformation(const string& name, const parameter::Value& value = parameter::emptyValue) const {
			TransformationTypePtr type = getTransformationType(name);
			assert(type && "Unknown transformation type requested!");
			return type->createTransformation(value);
		}

		/**
		 * Obtains a reference to the internally maintained transformation type register.
		 */
		const std::map<string, TransformationTypePtr>& getRegister() const {
			return catalog;
		}

		/**
		 * Obtains a list of all names of the internally maintained transformations.
		 */
		vector<string> getAllTransformationNames() const {
			vector<string> res;
			projectToFirst(catalog, res);
			return res;
		}

		/**
		 * Obtains a list of all internally maintained transformations.
		 */
		vector<TransformationTypePtr> getAllTransformations() const {
			return projectToSecond(catalog);
		}

	};


} // end namespace transform
} // end namespace insieme

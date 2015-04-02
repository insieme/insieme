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
#include "insieme/core/ir_address.h"
#include "insieme/core/forward_decls.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/transform/parameter.h"

#include "insieme/utils/printable.h"
#include "insieme/utils/static_constant.h"

namespace insieme {
namespace transform {

	/**
	 * NOTES:
	 * This header file defines the basic interface for transformations and
	 * some of the routines to be used for applying those. Further header files
	 * are defining concrete transformations or transformation connectors.
	 */

	// forward declarations
	class Transformation;
	typedef std::shared_ptr<Transformation> TransformationPtr;

	class TransformationType;
	typedef const TransformationType* TransformationTypePtr;

	/**
	 * An exception which will be raised in case a transformation can not be applied to
	 * a certain target node.
	 */
	class InvalidTargetException : public std::invalid_argument {
	public:
		InvalidTargetException(const string& cause) : invalid_argument(cause) {};
		InvalidTargetException(const core::NodePtr& node);
		virtual ~InvalidTargetException() throw() { }
	};

	/**
	 * An exception which will be raised in case a transformation should be constructed
	 * using an invalid set of parameters.
	 */
	class InvalidParametersException : public std::invalid_argument {
	public:
		InvalidParametersException(const string& cause = "Cannot instantiate Transformation based on given Parameters!") : invalid_argument(cause) {};
		virtual ~InvalidParametersException() throw() { }
	};

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
		 * A flag indicating whether this transformation type represents a connector or
		 * an atomic transformation.
		 */
		bool connector;

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
		 * @param connector a flag indicating whether the resulting type is a connector type or not
		 * @paramInfo the parameters supported by the represented transformation - by default no arguments are required.
		 */
		TransformationType(const string& name, const string& desc, bool connector, const parameter::ParameterPtr& paramInfo)
			: name(name), description(desc), connector(connector), parameterInfo(paramInfo) {
			assert(paramInfo && "Parameter Information must be set!");
		};

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
		TransformationPtr createTransformation(const parameter::Value& value = parameter::emptyValue) const;

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
		 * Determines whether the represented transformation type is a transformation
		 * connector (true) or an atomic transformation (false).
		 *
		 * @return true if it is a connector, false otherwise
		 */
		bool isConnector() const {
			return connector;
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
	 * The common abstract base class / interface for all transformations handled
	 * within the Insieme Transformation Framework.
	 */
	class Transformation : public utils::Printable {

		/**
		 * A pointer to an instance of this transformation type.
		 */
		const TransformationType& type;

		/**
		 * The list of transformations this transformation is composed of.
		 */
		vector<TransformationPtr> subTransformations;

		/**
		 * The parameters used for instantiate this transformation with.
		 */
		parameter::Value parameters;

	public:

		/**
		 * The constructor to be used for all transformations not representing
		 * combinations of other transformations.
		 *
		 * @param type a reference to the class describing the type of this transformation
		 * @param params the parameters used to instantiate this transformation
		 */
		Transformation(const TransformationType& type, const parameter::Value& params)
			: type(type), subTransformations(), parameters(params) {
			if (!type.getParameterInfo()->isValid(params)) throw InvalidParametersException("Constructed using invalid parameters!");
		}

		/**
		 * The constructor to be used to form combined transformations.
		 *
		 * @param type a reference to the class describing the type of this transformation
		 * @param subTransformations the list of sub-transformations combined by this one
		 * @param params the parameters used to instantiate this transformation
		 */
		Transformation(const TransformationType& type, const vector<TransformationPtr>& subTransformations, const parameter::Value& params)
			: type(type), subTransformations(subTransformations), parameters(params) {
			assert(type.getParameterInfo()->isValid(params) && "Constructed using invalid parameters!");
		}

		/**
		 * A virtual destructor for this abstract base class.
		 */
		virtual ~Transformation() {};

		/**
		 * Indicates whether this transformation is a combination of additional
		 * transformations or not.
		 */
		bool isConnector() const {
			return type.isConnector();
		}

		/**
		 * Obtains a reference to the meta-class describing the type of this transformation.
		 *
		 * @return a reference to the transformation type description representing associated to this transformation
		 */
		const TransformationType& getType() const {
			return type;
		}

		/**
		 * Obtains the list of transformations composed by this transformation.
		 *
		 * @return the list of transformations composed by this transformation.
		 */
		const vector<TransformationPtr>& getSubTransformations() const {
			return subTransformations;
		}

		/**
		 * Obtains a reference to the parameters used to instantiate this transformation
		 * instance.
		 */
		const parameter::Value& getParameters() const {
			return parameters;
		}

		/**
		 * Creates a new instance of this transformation type based on the given parameters.
		 *
		 * @param params the parameters to be used when creating the instance
		 * @return a pointer to the resulting transformation instance
		 * @throws InvalidParametersException in case the given parameters are not suitable for this transformation
		 */
		TransformationPtr getInstanceUsing(const parameter::Value& params) const {
			return type.createTransformation(params);
		}

		/**
		 * Requests this transformation to be applied on the given target. The result
		 * should be the transformed version of the node. During the transformation, the
		 * node manager of the given node will be used to create new instances of IR nodes.
		 *
		 * The transformation conducted during this step is requested to be deterministic and pure.
		 * Hence, given the same input, the same result is produced and no global state will be able to
		 * effect the transformations behavior.
		 *
		 * @param target the node to be transformed
		 * @return the transformed node
		 * @throws InvalidTargetException if this transformation can not be applied to the given target
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const =0;

		/**
		 * A generic version of the method above which will preserve the type of the transformed node.
		 * The user has to pick a pointer type which will be generic enough to fit the target and resulting
		 * node type.
		 *
		 * In case the transformed node can not be referenced by the pointer type of the argument, an
		 * assertion will fail.
		 *
		 * @tparam T the type of node to be transformed and returned
		 * @param target the node to be transformed
		 * @return the transformed node
		 * @throws InvalidTargetException if this transformation can not be applied to the given target
		 */
		template<typename T>
		core::Pointer<const T> apply(const core::Pointer<const T>& target) const {
			return static_pointer_cast<const T>(apply(core::NodeAddress(target)).getAddressedNode());
		}

		/**
		 * A generic version of the method above which will preserve the type of the transformed node
		 * referenced via an address. The user has to pick a pointer type which will be generic enough
		 * to fit the target and resulting node type.
		 *
		 * In case the transformed node can not be referenced by the pointer type of the argument, an
		 * assertion will fail.
		 *
		 * @tparam T the type of node to be transformed and returned
		 * @param target the node to be transformed
		 * @return the transformed node
		 * @throws InvalidTargetException if this transformation can not be applied to the given target
		 */
		template<typename T>
		core::Address<const T> apply(const core::Address<const T>& target) const {
			return apply(core::NodeAddress(target)).as<core::Address<const T>>();
		}

		/**
		 * Enables this transformation to be printed to any kind of output stream. Transformations
		 * are printed in the shape of a hierarchy.
		 *
		 * @param out the output stream a string representation should be written to
		 * @return the handed in output stream
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			return printTo(out, Indent());
		}

		/**
		 * The mechanism to be implemented by all implementations for printing trees in a formated way.
		 *
		 * @param out the stream to be printed to
		 * @param indent the left-side indent to be respected.
		 * @return the handed in putput stream
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const =0;

		/**
		 * An equality operator to be implemented by all transformation implementations.
		 *
		 * @param other the transformation to be compared with
		 * @return true if equivalent, false otherwise
		 */
		virtual bool operator==(const Transformation& other) const {
			return &getType() == &other.getType() && getParameters() == other.getParameters();
		}

		/**
		 * An in-equality operator to be used for comparing transformations.
		 *
		 * @param other the transformation to be compared with
		 * @return true if not equivalent, false otherwise
		 */
		virtual bool operator!=(const Transformation& other) const {
			return !(*this == other); // default implementation based on == implementation
		}

		/**
		 * Defines a total order on transformations to enable transformations to be
		 * placed within maps. The total order is simply based on the type of the transformation
		 * (the address of the type token is used for comparison) and the parameters used for
		 * instantiating transformations of the same type.
		 *
		 * @param other the transformation to be compared with
		 */
		bool operator<(const Transformation& other) const;
	};


	// -- Transformation Utilities -----------------------------------

	/**
	 * Extends the < relation of the transformation to transformation pointers.
	 */
	inline bool operator<(const TransformationPtr& a, const TransformationPtr& b) {
		if (a == b) return false;
		if (!a || !b) return false;
		return *a < *b;
	}


	// -- Transformation Type Utilities ------------------------------


	template<typename Derived>
	struct AbstractTransformationType : public TransformationType, public utils::StaticConstant<Derived> {

		/**
		 * A constructor just forwarding parameters to the parent class.
		 */
		AbstractTransformationType(const string& name, const string& desc, bool connector, const parameter::ParameterPtr& paramInfo = parameter::no_parameters)
			: TransformationType(name, desc, connector, paramInfo) {};

	};

	/**
	 * A macro simplifying the declaration of a transformation type.
	 */
	#define TRANSFORMATION_TYPE(NAME, DESC, PARAM_TYPE) \
	class NAME; \
	class NAME ## Type : public AbstractTransformationType<NAME ## Type> { \
	public: \
		NAME ## Type() : AbstractTransformationType(#NAME, DESC, false, PARAM_TYPE) {} \
		virtual TransformationPtr buildTransformation(const parameter::Value& value) const { \
			return std::make_shared<NAME>(value); \
		} \
	};

	#define TRANSFORMATION_CONNECTOR_TYPE(NAME, DESC, PARAM_TYPE) \
	class NAME; \
	class NAME ## Type : public AbstractTransformationType<NAME ## Type> { \
	public: \
		NAME ## Type() : AbstractTransformationType(#NAME, DESC, true, PARAM_TYPE) {} \
		virtual TransformationPtr buildTransformation(const parameter::Value& value) const { \
			return std::make_shared<NAME>(value); \
		} \
	};

} // end namespace transform
} // end namespace insieme

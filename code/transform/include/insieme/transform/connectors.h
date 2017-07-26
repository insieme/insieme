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

#include <vector>
#include <limits>

#include "insieme/transform/transformation.h"
#include "insieme/transform/primitives.h"
#include "insieme/transform/filter/filter.h"
#include "insieme/transform/catalog.h"

#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace transform {

	/**
	 * Connectors:
	 * 	Transformations can be combined to form more complex transformations using
	 * 	various connectors. The connector is defining the relationship between the
	 * 	combined transformations. The following connectors are supported:
	 *
	 *		- pipeline ... simple, sequential execution of multiple transformations
	 *		- for_all  ... applies a transformation on a list of a-priory selected sub-nodes simultaneously
	 *		- for_each ... applies a transformation on each sub-node satisfying a certain property iteratively
	 *		- fixpoint ... applies a transformation repeatedly until a fixpoint is reached
	 *		- tryOtherwise ... tries one transformation, if it fails another
	 *		- condition ... determines based on a condition which of two transformations should be applied
	 *
	 *
	 *	Potential future Connectors: (ask if you need them!)
	 * 		- check ... a simple one-element wrapper adding additional pre / post-condition checks
	 * 		- to_first ... applies a transformation only to the first node satisfying a property
	 *
	 */


	// -- Transformation Implementations -----------

	/**
	 * A pipeline connects a list of transformations by executing them
	 * one after another.
	 */
	class Pipeline : public Transformation {
	  public:
		/**
		 * Creates a pipeline based on the given parameters.
		 *
		 * @param value the parameters to be used for creating the pipeline
		 */
		Pipeline(const parameter::Value& value);

		/**
		 * Applies the list of transformations this connector is based on the the given target.
		 * The transformations are applied one after another. The result is returned to the caller.
		 *
		 * @param target the target to be transformed
		 * @return the transformed code segment
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			// apply all transformations, one after another
			// if one is failing, the entire transformation is failing
			core::NodeAddress res = target;
			for_each(getSubTransformations(), [&](const TransformationPtr& cur) { res = cur->apply(res); });
			return res;
		}

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};

	/**
	 * A factory function creating a pipeline connector given a sequence of transformations.
	 *
	 * @param filter the filter to be used for selecting target nodes
	 * @param transform the transformation to be applied on the selected target nodes
	 */
	template <class... Params>
	inline TransformationPtr makePipeline(const TransformationPtr& first, const TransformationPtr& second, const Params&... rest) {
		assert_true(first) << "Transformation must be valid!";
		return std::make_shared<Pipeline>(parameter::combineValues(parameter::makeValue(first), parameter::makeValue(makePipeline(second, rest...))));
	}

	inline TransformationPtr makePipeline(const TransformationPtr& first) {
		return first;
	}


	inline TransformationPtr makePipeline(const std::vector<TransformationPtr>& trans_list) {
		std::vector<parameter::Value> values;
		for_each(trans_list, [&](const TransformationPtr& cur) { values.push_back(parameter::makeValue(cur)); });

		return std::make_shared<Pipeline>(parameter::combineValues(values));
	}

	/**
	 * The transformation type used as a factory for pipeline connectors.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(Pipeline, "Combines a list of Transformations into a Sequence",
	                              parameter::list("List of combined transformations", parameter::atom<TransformationPtr>()));

	/**
	 * The for-each connector is applying a given transformation to every node within a
	 * tree satisfying a given filter property.
	 */
	class ForEach : public Transformation {
		/**
		 * The filter to be used for selecting instances to be transformed.
		 */
		filter::Filter filter;

		/**
		 * A flag determining whether the transformation should be in pre- (true) or postorder (false).
		 */
		bool preorder;

		/**
		 * The maximal depth to be descending into the code.
		 */
		unsigned maxDepth;

	  public:
		ForEach(const parameter::Value& value);

		virtual core::NodeAddress apply(const core::NodeAddress& target) const;

		const filter::Filter& getFilter() const {
			return filter;
		}

		const TransformationPtr& getTransformation() const {
			return getSubTransformations()[0];
		}

		bool isPreOrder() const {
			return preorder;
		}

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;

	  private:
		/**
		 * The recursive function realizing the core of for-each transformation. It is decending
		 * recursively into a tree to be transformed, thereby applying a given transformation
		 * to each encountered node in pre/post order whenever the given node filter is satisfied.
		 *
		 * @param target the current node inspected
		 * @param depth the current level above the end of the recursive decent
		 * @return the transformed node
		 */
		core::NodePtr apply(const core::NodePtr& target, unsigned depth) const;
	};

	/**
	 * The transformation type used as a factory for for_each connectors.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(ForEach, "Applies a given transformation to all nodes within a code fragment satisfying a given property.",
	                              parameter::tuple(parameter::atom<filter::Filter>("the filter used to pick target nodes"),
	                                               parameter::atom<TransformationPtr>("the transformation to be applied"),
	                                               parameter::atom<bool>("whether the transformation should be applied in pre- or post order"),
	                                               parameter::atom<unsigned>("the maximal depth the for_each is descending into the code")));

	/**
	 * A factory function creating a for-each connector around a given transformation. The transformation
	 * will only be applied on nodes satisfying the given filter criteria.
	 *
	 * @param filter the filter to be used for selecting target nodes
	 * @param transform the transformation to be applied on the selected target nodes
	 */
	inline TransformationPtr makeForEach(const filter::Filter& filter, const TransformationPtr& transform, bool preorder = true,
	                                     unsigned depth = std::numeric_limits<unsigned>::max()) {
		return std::make_shared<ForEach>(parameter::combineValues(parameter::makeValue(filter), parameter::makeValue(transform), parameter::makeValue(preorder),
		                                                          parameter::makeValue(depth)));
	}

	/**
	 * The for-all connector is applying a given transformation to a list of
	 */
	class ForAll : public Transformation {
		/**
		 * The filter to be used for selecting instances to be transformed.
		 */
		filter::TargetFilter filter;

	  public:
		ForAll(const parameter::Value& value);

		/**
		 * Obtains a reference to the filter associated to this for-all node.
		 */
		const filter::TargetFilter& getFilter() const {
			return filter;
		}

		/**
		 * Obtains a reference to the transformation being applied on every selected node.
		 */
		const TransformationPtr& getTransformation() const {
			return getSubTransformations()[0];
		}

		/**
		 * Applies the represented transformation to the given target.
		 *
		 * @param target the target to be transformed
		 * @return the transformed node instance
		 * @throw InvalidTargetException in case on of the selected targets could not be transformed
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const;

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};

	/**
	 * The transformation type used as a factory for for_all connectors.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(ForAll, "Applies a given transformation to all nodes identified before transforming the target.",
	                              parameter::tuple(parameter::atom<filter::TargetFilter>("the filter used to pick target nodes"),
	                                               parameter::atom<TransformationPtr>("the transformation to be applied")));

	/**
	 * A factory method creating for-all transformation connectors based on the given arguments.
	 *
	 * @param filter the filter to be used to select regions to be transformed
	 * @param transformation the transformations to be applied to the regions selected by the filter
	 * @return the requested, combined transformation
	 */
	inline TransformationPtr makeForAll(const filter::TargetFilter& filter, const TransformationPtr& transform) {
		return std::make_shared<ForAll>(parameter::combineValues(parameter::makeValue(filter), parameter::makeValue(transform)));
	}

	/**
	 * A transformation connector repeating a given transformation until a fixpoint is reached
	 * or a given number of iterations have been conducted. Whether a result not representing
	 * a fixpoint but being obtained by iterating the maximal number of iterations is considered
	 * valid is decided by an extra flag.
	 */
	class Fixpoint : public Transformation {
		/**
		 * The maximal number of iterations processed before accepting the result.
		 */
		unsigned maxIterations;

		/**
		 * A flag indicating whether the result after aborting due to an iteration limit
		 * will be considered valid or not.
		 */
		bool acceptNonFixpoint;

	  public:
		/**
		 * Creates a new instance of this combined transformation based on the given parameters.
		 *
		 * @param value a value instance containing the targeted transformation, maximum number of iterations and a flag
		 * 			indicating whether or not approximations should be accepted or not
		 */
		Fixpoint(const parameter::Value& value);

		/**
		 * Obtains a reference to the transformation being applied on every selected node.
		 */
		const TransformationPtr& getTransformation() const {
			return getSubTransformations()[0];
		}

		/**
		 * Conducts the actual processing of the fixpoint.
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const;

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};

	/**
	 * The transformation type representation of the fixpoint connector.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(Fixpoint, "Obtains a fixpoint for a given transformation.",
	                              parameter::tuple(parameter::atom<TransformationPtr>("the transformation for which a fixpoint should be obtained"),
	                                               parameter::atom<unsigned>("the maximal number of iterations to be computed for approximating the fixpoint"),
	                                               parameter::atom<bool>("should an approximation also be accepted")));

	/**
	 * A factory method creating for-all transformation connectors based on the given arguments.
	 *
	 * @param transform the transformation for which a fixpoint should be established
	 * @param numIterations the upper limit for the total number of iterations to be considered
	 * @param acceptApproximation accept a fixpoint when the max number of iterations has been reached
	 * @return the requested, combined transformation
	 */
	inline TransformationPtr makeFixpoint(const TransformationPtr& transform, unsigned numIterations = 100, bool acceptApproximation = true) {
		return std::make_shared<Fixpoint>(
		    parameter::combineValues(parameter::makeValue(transform), parameter::makeValue(numIterations), parameter::makeValue(acceptApproximation)));
	}


	/**
	 * A condition connector allows to combine two transformations within an if .. then .. else .. endif construct.
	 * The condition used to decide which of the two involved transformations should actually be applied is
	 * defined by a filter.
	 */
	class Condition : public Transformation {
		/**
		 * The condition to be used to decide which transformation to be applied.
		 */
		filter::Filter condition;

		/**
		 * The transformation to be applied in case the condition is satisfied.
		 */
		TransformationPtr thenTransform;

		/**
		 * The transformation to be applied in case the condition is not satisfied.
		 */
		TransformationPtr elseTransform;

	  public:
		/**
		 * Creates a new instance of this connector combining the parameters encoded within
		 * the given value.
		 *
		 * @param value a value encoding a condition and two transformations. The condition is
		 * 			used to determine which of the two transformation should be applied. The first
		 * 			transformation is applied in case the condition is satisfied, the second otherwise.
		 */
		Condition(const parameter::Value& value);

		/**
		 * Realizes the actual transformation by evaluating the condition and applying the corresponding transformation.
		 *
		 * @param target the node to be transformed
		 * @return the transformed program code
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			return (condition(target)) ? thenTransform->apply(target) : elseTransform->apply(target);
		}

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};


	/**
	 * The transformation type representation of the condition connector.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(Condition, "Applies one out of two transformations depending on a given condition.",
	                              parameter::tuple(parameter::atom<filter::Filter>("the condition determining the transformation to be applied"),
	                                               parameter::atom<TransformationPtr>("the transformation applied when the condition is satisfied"),
	                                               parameter::atom<TransformationPtr>("the transformation applied when the condition is not satisfied")));


	/**
	 * A connector realizing a try ... otherwise ... end construct. The transformation will try to apply the
	 * first transformation. If successful, the corresponding result will be returned. If the first fails, the
	 * result of the second will be returned.
	 *
	 * This construct can be used to compensate for invalid transformations. By using "try <X> otherwise NoOp end"
	 * transformation X can be "safely" nested inside any other connector.
	 */
	class TryOtherwise : public Transformation {
		/**
		 * The first transformation to be tested.
		 */
		TransformationPtr tryTransform;

		/**
		 * The fallback transformation to be applied.
		 */
		TransformationPtr otherwiseTransform;

	  public:
		/**
		 * Creates a new instance of this combined transformation based on the parameters encoded
		 * within the given value argument.
		 *
		 * @param value the value encoding two transformations. The first will be tried. If it fails
		 * 			the second will be applied.
		 */
		TryOtherwise(const parameter::Value& value);

		/**
		 * Realizes the actual semantic of this transformation.
		 */
		virtual core::NodeAddress apply(const core::NodeAddress& target) const {
			try {
				return tryTransform->apply(target);
			} catch(const InvalidTargetException& ite) {
				// => first failed, use otherwise transformation
			}
			return otherwiseTransform->apply(target);
		}

		/**
		 * Compares this connector with the given transformation. It will only be the same
		 * if it is a transformation of the same type being instantiated using the same parameters.
		 */
		virtual bool operator==(const Transformation& other) const;

		/**
		 * Prints a readable representation of this transformation to the given output stream
		 * using the given indent.
		 */
		virtual std::ostream& printTo(std::ostream& out, const Indent& indent) const;
	};

	/**
	 * The transformation type representation of the TryOtherwise connector.
	 */
	TRANSFORMATION_CONNECTOR_TYPE(TryOtherwise, "Tries applying a transformation and in case it fails uses a backup transformation.",
	                              parameter::tuple(parameter::atom<TransformationPtr>("the transformation to be tried"),
	                                               parameter::atom<TransformationPtr>("the fallback transformation")));

	/**********************************************************************************************
	 * Utility functions for creating transformations
	 *********************************************************************************************/


	/**
	 * makeTryOtherwise: create a transformation which applies the 'first' transformation and in the
	 * case it fails to apply, it applies the second option
	 */
	template <class... Params>
	TransformationPtr makeTryOtherwise(const TransformationPtr& first, const TransformationPtr& second, const Params&... rest) {
		assert_true(first) << "Transformation must be valid!";
		return std::make_shared<TryOtherwise>(parameter::combineValues(parameter::makeValue(first), parameter::makeValue(makeTryOtherwise(second, rest...))));
	}

	TransformationPtr makeTryOtherwise(const TransformationPtr& first);

	TransformationPtr makeTry(const TransformationPtr& trans);

} // end namespace transform
} // end namespace insieme

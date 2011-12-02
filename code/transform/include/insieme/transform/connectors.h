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

#include <vector>

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
	 *		- for_each ... applies a transformation on each sub-node satisfying a certain property
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
	 * The transformation type used as a factory for pipeline connectors.
	 */
	TRANSFORM_TYPE(
			Pipeline,
			"Combines a list of Transformations into a Sequence",
			parameter::list("List of combined transformations", parameter::atom<TransformationPtr>())
	);

	/**
	 * A pipeline connects a list of transformations by executing them
	 * one after another.
	 */
	class Pipeline : public AbstractTransformation {

		/**
		 * The list of transformations to be processed in sequence.
		 */
		vector<TransformationPtr> transformations;

	public:

		/**
		 * Creates a new pipeline based on the given list of transformations.
		 *
		 * @param transformations the transformations to be included within the pipeline
		 */
		template<class ... T>
		Pipeline(const T& ... transformations)
			: transformations(toVector<TransformationPtr>(transformations ...)) {};

		/**
		 * Creates a new pipeline based on the given list of transformations.
		 *
		 * @param transformations the transformations to be included within the pipeline
		 */
		Pipeline(const vector<TransformationPtr>& transformations)
			: transformations(transformations) {}

		/**
		 * Applies the list of transformations this connector is based on the the given target.
		 * The transformations are applied one after another. The result is returned to the caller.
		 *
		 * @param target the target to be transformed
		 * @return the transformed code segment
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const {
			// apply all transformations, one after another
			// if one is failing, the entire transformation is failing
			core::NodePtr res = target;
			for_each(transformations, [&](const TransformationPtr& cur) {
				res = cur->apply(res);
			});
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
	 * The transformation type used as a factory for for_each connectors.
	 */
	TRANSFORM_TYPE(
			ForEach,
			"Applies a given transformation to all nodes within a code fragment satisfying a given property.",
			parameter::tuple(
					parameter::atom<filter::Filter>("the filter used to pick target nodes"),
					parameter::atom<TransformationPtr>("the transformation to be applied"),
					parameter::atom<bool>("whether the transformation should be applied in pre- or post order"),
					parameter::atom<unsigned>("the maximal depth the for_each is descending into the code")
			)
	);

	/**
	 * The for-each connector is applying a given transformation to every node within a
	 * tree satisfying a given filter property.
	 */
	class ForEach : public AbstractTransformation {

		/**
		 * The filter to be used for selecting instances to be transformed.
		 */
		filter::Filter filter;

		/**
		 * The transformation to be applied on selected instances.
		 */
		TransformationPtr transformation;

		/**
		 * A flag determining whether the transformation should be in pre- (true) or postorder (false).
		 */
		bool preorder;

		/**
		 * The maximal depth to be descending into the code.
		 */
		unsigned maxDepth;

	public:

		/**
		 * Crates a new for-each filter
		 */
		ForEach(const TransformationPtr& transform, bool preorder = true, unsigned maxDepth = 100)
			: filter(filter::all), transformation(transform), preorder(preorder), maxDepth(maxDepth) {}

		ForEach(const filter::Filter& filter, const TransformationPtr& transform, bool preorder = true, unsigned maxDepth = 100)
			: filter(filter), transformation(transform), preorder(preorder), maxDepth(maxDepth) {}

		virtual core::NodePtr apply(const core::NodePtr& target) const;

		const filter::Filter& getFilter() const {
			return filter;
		}

		const TransformationPtr& getTransformation() const {
			return transformation;
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
	 * The transformation type representation of the fixpoint connector.
	 */
	TRANSFORM_TYPE(
			Fixpoint,
			"Obtains a fixpoint for a given transformation.",
			parameter::tuple(
					parameter::atom<TransformationPtr>("the transformation for which a fixpoint should be obtained"),
					parameter::atom<unsigned>("the maximal number of iterations to be computed for approximating the fixpoint"),
					parameter::atom<bool>("should an approximation also be accepted")
			)
	);

	/**
	 * A transformation connector repeating a given transformation until a fixpoint is reached
	 * or a given number of iterations have been conducted. Whether a result not representing
	 * a fixpoint but being obtained by iterating the maximal number of iterations is considered
	 * valid is decided by an extra flag.
	 */
	class Fixpoint : public AbstractTransformation {

		/**
		 * The transformation for which a fixpoint should be derived.
		 */
		TransformationPtr transformation;

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
		 * @param transform the transformation for which a fixpoint should be obtained
		 * @param maxIterations the maximal number of iterations conducted before accepting the result to be a fixpoint
		 * @param acceptNonFixpoint determines whether a result obtained by iterating the maximum number of iterations will
		 * 			considered a valid result of the transformation or not
		 */
		Fixpoint(const TransformationPtr& transform, unsigned maxIterations = 100, bool accpetNonFixpoint = false)
			: transformation(transform), maxIterations(maxIterations), acceptNonFixpoint(accpetNonFixpoint) {}

		/**
		 * Conducts the actual processing of the fixpoint.
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const;

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
	TRANSFORM_TYPE(
			Condition,
			"Applies one out of two transformations depending on a given condition.",
			parameter::tuple(
					parameter::atom<filter::Filter>("the condition determining the transformation to be applied"),
					parameter::atom<TransformationPtr>("the transformation applied when the condition is satisfied"),
					parameter::atom<TransformationPtr>("the transformation applied when the condition is not satisfied")
			)
	);

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
		 * Creates a new instance of this connector combining the given parameters.
		 *
		 * @param condition the condition to be used to determine which transformation to be applied
		 * @param thenTrans the transformation to be applied in case the condition is satisfied
		 * @param elseTrans the transformation to be applied in case the condition is not satisfied
		 */
		Condition(const filter::Filter& condition, const TransformationPtr& thenTrans, const TransformationPtr& elseTrans)
			: condition(condition), thenTransform(thenTrans), elseTransform(elseTrans) { }

		/**
		 * Realizes the actual transformation by evaluating the condition and applying the corresponding transformation.
		 *
		 * @param target the node to be transformed
		 * @return the transformed program code
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const {
			return (condition(target)) ?
				thenTransform->apply(target) :
				elseTransform->apply(target);
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
	TRANSFORM_TYPE(
			TryOtherwise,
			"Tries applying a transformation and in case it fails uses a backup transformation.",
			parameter::tuple(
					parameter::atom<TransformationPtr>("the transformation to be tried"),
					parameter::atom<TransformationPtr>("the fallback transformation")
			)
	);

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
		 * Creates a new instance of this combined transformation based on the givne parameters.
		 *
		 * @param tryTransform the first transformation to be tested
		 * @param otherwiseTransform the fallback transformation to be applied in case the first fails
		 */
		TryOtherwise(const TransformationPtr& tryTransform, const TransformationPtr& otherwiseTransform)
			: tryTransform(tryTransform), otherwiseTransform(otherwiseTransform) { }

		/**
		 * Realizes the actual semantic of this transformation.
		 */
		virtual core::NodePtr apply(const core::NodePtr& target) const {
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


	/**********************************************************************************************
	 * Utility functions for creating transformations 
	 *********************************************************************************************/


	/**
	 * makeTryOtherwise: create a transformation which applies the 'first' transformation and in the
	 * case it fails to apply, it applies the second option
	 */
	template <class ...Params>
	TransformationPtr makeTryOtherwise (
			const TransformationPtr&  first, 
			const TransformationPtr&  second,
			const Params& ... rest ) 	
	{
		assert(first && "Transformation must be valid!");
		return std::make_shared<TryOtherwise>(first, makeTryOtherwise(second, rest...) );
	}

	TransformationPtr makeTryOtherwise ( const TransformationPtr&  first ) ;

	TransformationPtr makeTry (const TransformationPtr& trans );

} // end namespace transform
} // end namespace insieme

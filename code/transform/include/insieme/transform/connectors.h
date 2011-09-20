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
#include "insieme/utils/cache_utils.h"

namespace insieme {
namespace transform {

	/**
	 * Connectors:
	 *
	 * Connectors for expressive power:
	 * 		- pipeline ... (simply applies a sequence of transformations)
	 * 		- for_each ... (supports filters + recursion / bottom up / top down / ...)
	 * 		- fixpoint	... (repeats until fix-points has been obtained or a limit is reached)
	 * 		- try - otherwise
	 * 		- check ... a simple one-element wrapper adding additional checks
	 * 		- conditional ... (applies transformation if a certain condition is satisfied, false otherwise)
	 *
	 * Connectors for performance reasons:
	 * 		- cached transformation
	 *
	 */


	class Pipeline : public AbstractTransformation {

		vector<TransformationPtr> transformations;

	public:

		template<class ... T>
		Pipeline(T ... transforms) : transformations(toVector<TransformationPtr>(transforms ...)) {};

		Pipeline(const vector<TransformationPtr>& transformations)
			: transformations(transformations) {}


		virtual bool checkPreCondition(const core::NodePtr& target) const {
			//  check pre-condition of first transformation
			return transformations.empty() || (*transformations.begin())->checkPreCondition(target);
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const {
			// apply all transformations, one after another
			// if one is failing, the entire transformation is failing
			core::NodePtr res = target;
			for_each(transformations, [&](const TransformationPtr& cur) {
				res = cur->apply(res);
			});
			return target;
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			// check post-condition of last transformation
			return transformations.empty() || (*transformations.rbegin())->checkPostCondition(before, after);
		}

	};


	class ForEach : public AbstractTransformation {

		typedef std::function<bool(const core::NodePtr&)> Filter;

		Filter filter;

		TransformationPtr transformation;

		unsigned maxDepth;

	public:

		ForEach(const TransformationPtr& transform, unsigned maxDepth = std::numeric_limits<unsigned>::max())
			: filter(AcceptAll<const core::NodePtr&>()), transformation(transform), maxDepth(maxDepth) {}

		ForEach(const Filter& filter, const TransformationPtr& transform, unsigned maxDepth = std::numeric_limits<unsigned>::max())
			: filter(filter), transformation(transform), maxDepth(maxDepth) {}

		virtual core::NodePtr apply(const core::NodePtr& target) const;

		const Filter& getFilter() const {
			return filter;
		}

		const TransformationPtr& getTransformation() const {
			return transformation;
		}

	};


	class Fixpoint : public AbstractTransformation {

		unsigned maxIterations;

		TransformationPtr transformation;

	public:

		Fixpoint(const TransformationPtr& transform, unsigned maxIterations = 100)
			: maxIterations(maxIterations), transformation(transform) {}


		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return transformation->checkPreCondition(target);
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const;

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return transformation->checkPostCondition(before, after);
		}

	};



	class Condition : public Transformation {

	public:

		typedef std::function<bool(const core::NodePtr&)> condition_type;

	private:

		condition_type condition;

		TransformationPtr thenTransform;
		TransformationPtr elseTransform;

	public:

		Condition(const condition_type& condition, const TransformationPtr& thenTrans, const TransformationPtr& elseTrans)
			: condition(condition), thenTransform(thenTrans), elseTransform(elseTrans) { }

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return (condition(target)) ?
				thenTransform->checkPreCondition(target) :
				elseTransform->checkPreCondition(target);
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const {
			return (condition(target)) ?
				thenTransform->apply(target) :
				elseTransform->apply(target);
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return (condition(before)) ?
				thenTransform->checkPostCondition(before, after) :
				elseTransform->checkPostCondition(before, after);
		}

	};


	class Sequence : public Transformation {

		const std::vector<TransformationPtr> transformations;

	public:

		virtual bool checkPreCondition(const core::NodePtr& target) const;

		virtual core::NodePtr apply(const core::NodePtr& target) const;

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const;

	};

	class CachedTransformation : public Transformation {

		const TransformationPtr transformation;

		mutable utils::cache::PointerCache<core::NodePtr, bool> precondition;
		mutable utils::cache::PointerCache<core::NodePtr, core::NodePtr> transformed;

	public:

		CachedTransformation(const TransformationPtr& transform)
			: transformation(transform),
			  precondition(fun(*transform, &Transformation::checkPreCondition)),
			  transformed(fun(*transform, &Transformation::apply)) {}

		virtual bool checkPreCondition(const core::NodePtr& target) const {
			return precondition.get(target);
		}

		virtual core::NodePtr apply(const core::NodePtr& target) const throw (InvalidTargetException) {
			return transformed.get(target);
		}

		virtual bool checkPostCondition(const core::NodePtr& before, const core::NodePtr& after) const {
			return transformation->checkPostCondition(before, after);
		}

	};

//	class Sequence : public Transformation {
//
//		const std::vector<TransformationPtr> transformations;
//
//	public:
//		virtual core::NodePtr apply(const core::NodePtr& target);
//		virtual bool checkPrecondition(const core::NodePtr& target);
//		virtual bool checkPostcondition(const core::NodePtr& before, const core::NodePtr& after);
//
//	};
//
//
//	class Parallel : public Transformation {
//
//		const std::vector<TransformationPtr> transformations;
//
//	public:
//		virtual core::NodePtr apply(const core::NodePtr& target);
//		virtual bool checkPrecondition(const core::NodePtr& target);
//		virtual bool checkPostcondition(const core::NodePtr& before, const core::NodePtr& after);
//
//	};
//
//
//	class Pipeline : public Transformation {
//
//		const std::vector<TransformationPtr> transformations;
//
//	public:
//		virtual core::NodePtr apply(const core::NodePtr& target);
//		virtual bool checkPrecondition(const core::NodePtr& target);
//		virtual bool checkPostcondition(const core::NodePtr& before, const core::NodePtr& after);
//
//	};
//
//
//
//	class Conditional : public Transformation {
//
//		const TransformationPtr transformation;
//
//	public:
//		virtual core::NodePtr apply(const core::NodePtr& target);
//		virtual bool checkPrecondition(const core::NodePtr& target);
//		virtual bool checkPostcondition(const core::NodePtr& before, const core::NodePtr& after);
//
//	};
//
//
//
//	class ForAllChild : public Transformation {
//
//		const TransformationPtr transformation;
//
//	public:
//		virtual core::NodePtr apply(const core::NodePtr& target);
//		virtual bool checkPrecondition(const core::NodePtr& target);
//		virtual bool checkPostcondition(const core::NodePtr& before, const core::NodePtr& after);
//
//	};

}
}

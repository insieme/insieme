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

#include "insieme/transform/connectors.h"

#include "insieme/core/transform/node_mapper_utils.h"

namespace insieme {
namespace transform {



	TransformationPtr PipelineType::buildTransformation(const parameter::Value& value) const {
		vector<TransformationPtr> list;
		vector<parameter::Value> args = parameter::getValue<vector<parameter::Value>>(value);
		for_each(args, [&](const parameter::Value& cur) {
			list.push_back(parameter::getValue<TransformationPtr>(cur));
		});
		return std::make_shared<Pipeline>(list);
	}

	TransformationPtr ForEachType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<ForEach>(
				parameter::getValue<filter::Filter>(value, 0),
				parameter::getValue<TransformationPtr>(value, 1),
				parameter::getValue<bool>(value, 2),
				parameter::getValue<unsigned>(value, 3)
		);
	}

	TransformationPtr FixpointType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<Fixpoint>(
				parameter::getValue<TransformationPtr>(value,0),
				parameter::getValue<unsigned>(value,1),
				parameter::getValue<bool>(value,2)
		);
	}

	TransformationPtr ConditionType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<Condition>(
				parameter::getValue<filter::Filter>(value,0),
				parameter::getValue<TransformationPtr>(value,1),
				parameter::getValue<TransformationPtr>(value,2)
		);
	}

	TransformationPtr TryOtherwiseType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<TryOtherwise>(
				parameter::getValue<TransformationPtr>(value,0),
				parameter::getValue<TransformationPtr>(value,1)
		);
	}


	core::NodePtr ForEach::apply(const core::NodePtr& target) const {
		return apply(target, maxDepth);
	}


	core::NodePtr ForEach::apply(const core::NodePtr& target, unsigned depth) const {

		// terminate application of transformation at level zero
		if (depth == 0) {
			return target;
		}

		core::NodePtr res = target;

		// conduct transformation in pre-order if requested
		if (preorder && filter(res)) {
			res = transformation->apply(res);
		}

		// conduct recursive decent - by transforming children
		core::NodeList children = res->getChildList();
		for_each(children, [&](core::NodePtr& cur) {
			cur = apply(cur, depth-1);
		});

		// re-assemble transformed node from modified child list (if necessary)
		if (!equals(children, res->getChildList(), equal_target<core::NodePtr>())) {
			core::transform::ChildListMapping nodeMapper(children);
			res = res->substitute(res->getNodeManager(), nodeMapper);
		}

		// conduct transformation in post-order if requested
		if (!preorder && filter(res)) {
			res = transformation->apply(res);
		}

		// done
		return res;
	}


	core::NodePtr Fixpoint::apply(const core::NodePtr& target) const {
		// apply transformation until result represents a fix-point of the sub-transformation
		core::NodePtr cur = target;;
		core::NodePtr last;
		unsigned counter = 0;
		do {
			last = cur;
			cur = transformation->apply(last);
			counter++;
		} while (*cur != *last && counter <= maxIterations);

		// check whether fixpoint could be obtained
		if (!acceptNonFixpoint && *cur != *last) {
			// => no fixpoint found!
			throw InvalidTargetException("Fixpoint could not be obtained!");
		}

		// return fixpoint (or approximation)
		return cur;
	}

	TransformationPtr makeNoOp() { return std::make_shared<NoOp>();	}

	TransformationPtr makeTryOtherwise ( const TransformationPtr&  first ) 	{
		assert ( first && "Transformation must be valid!");
		return first;
	}

	TransformationPtr makeTry (const TransformationPtr& trans ) { 
		return makeTryOtherwise(trans, makeNoOp());
	}

} // namespace transform
} // namespace insieme

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


	bool Pipeline::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		const Pipeline* other = dynamic_cast<const Pipeline*>(&transform);
		return other && equals(getSubTransformations(), other->getSubTransformations(), equal_target<TransformationPtr>());
	}

	std::ostream& Pipeline::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "Pipeline";
		if (getSubTransformations().empty()) {
			return out << "()";
		}

		for_each(getSubTransformations(), [&](const TransformationPtr& cur) {
			out << "\n";
			cur->printTo(out, indent+1);
		});

		return out;
	}



	TransformationPtr ForEachType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<ForEach>(
				parameter::getValue<filter::Filter>(value, 0),
				parameter::getValue<TransformationPtr>(value, 1),
				parameter::getValue<bool>(value, 2),
				parameter::getValue<unsigned>(value, 3)
		);
	}

	bool ForEach::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		// compare field by field
		const ForEach* other = dynamic_cast<const ForEach*>(&transform);
		return other && filter == other->filter && maxDepth == other->maxDepth
				&& preorder == other->preorder && filter == other->filter
				&& *transformation == *other->transformation;
	}

	std::ostream& ForEach::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "For " << filter << " in " << ((preorder)?"preorder":"postorder") << " within " << maxDepth << " levels do\n";
		return transformation->printTo(out, indent+1);
	}




	TransformationPtr ForAllType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<ForAll>(
				parameter::getValue<filter::TargetFilter>(value, 0),
				parameter::getValue<TransformationPtr>(value, 1)
		);
	}

	core::NodePtr ForAll::apply(const core::NodePtr& target) const {

		// generate list of target nodes
		vector<core::NodeAddress> targets = filter(target);
		if (targets.empty()) {
			return target;
		}

		// generate replacement map
		std::map<core::NodeAddress, core::NodePtr> replacements;
		for_each(targets, [&](const core::NodeAddress& cur) {
			replacements[cur] = transformation->apply(cur.getAddressedNode());
		});

		// apply replacement
		return core::transform::replaceAll(target->getNodeManager(), replacements);
	}

	bool ForAll::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		// compare field by field
		const ForAll* other = dynamic_cast<const ForAll*>(&transform);
		return other && filter == other->filter && *transformation == *other->transformation;
	}

	std::ostream& ForAll::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "For " << filter << " do\n";
		return transformation->printTo(out, indent+1);
	}




	TransformationPtr FixpointType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<Fixpoint>(
				parameter::getValue<TransformationPtr>(value,0),
				parameter::getValue<unsigned>(value,1),
				parameter::getValue<bool>(value,2)
		);
	}

	bool Fixpoint::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		// compare field by field
		const Fixpoint* other = dynamic_cast<const Fixpoint*>(&transform);
		return other && maxIterations == other->maxIterations && acceptNonFixpoint == other->acceptNonFixpoint
				&& *transformation == *other->transformation;
	}

	std::ostream& Fixpoint::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "Fixpoint - max iterations: " << maxIterations << " - accepting approximation: " << ((acceptNonFixpoint)?"true":"false") << "\n";
		return transformation->printTo(out, indent+1);
	}


	TransformationPtr ConditionType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<Condition>(
				parameter::getValue<filter::Filter>(value,0),
				parameter::getValue<TransformationPtr>(value,1),
				parameter::getValue<TransformationPtr>(value,2)
		);
	}

	bool Condition::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		// compare field by field
		const Condition* other = dynamic_cast<const Condition*>(&transform);
		return other && condition == other->condition &&
				*thenTransform == *other->thenTransform &&
				*elseTransform == *other->elseTransform;
	}

	std::ostream& Condition::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "if (" << condition << ") then \n";
		thenTransform->printTo(out, indent+1);
		out << "\n" << indent << "else\n";
		elseTransform->printTo(out, indent+1);
		return out;
	}



	TransformationPtr TryOtherwiseType::buildTransformation(const parameter::Value& value) const {
		return std::make_shared<TryOtherwise>(
				parameter::getValue<TransformationPtr>(value,0),
				parameter::getValue<TransformationPtr>(value,1)
		);
	}

	bool TryOtherwise::operator==(const Transformation& transform) const {
		// check for identity
		if (this == &transform) {
			return true;
		}

		// compare field by field
		const TryOtherwise* other = dynamic_cast<const TryOtherwise*>(&transform);
		return other && *tryTransform == *other->tryTransform &&
			   *otherwiseTransform == *other->otherwiseTransform;
	}

	std::ostream& TryOtherwise::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "try \n";
		tryTransform->printTo(out, indent+1);
		out << "\n" << indent << "otherwise\n";
		otherwiseTransform->printTo(out, indent+1);
		return out;
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

	TransformationPtr makeTryOtherwise ( const TransformationPtr&  first ) 	{
		assert ( first && "Transformation must be valid!");
		return first;
	}

	TransformationPtr makeTry (const TransformationPtr& trans ) { 
		return makeTryOtherwise(trans, makeNoOp());
	}

} // namespace transform
} // namespace insieme

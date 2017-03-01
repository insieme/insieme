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
#include "insieme/transform/connectors.h"

#include "insieme/core/transform/node_mapper_utils.h"

namespace insieme {
namespace transform {

	namespace {

		// some convenience utilities

		TransformationPtr getTransform(const parameter::Value& value, unsigned index) {
			return parameter::getValue<TransformationPtr>(value, index);
		}

		vector<TransformationPtr> extractTransformationList(const parameter::Value& value) {
			vector<TransformationPtr> list;
			vector<parameter::Value> args = parameter::getValue<vector<parameter::Value>>(value);
			for_each(args, [&](const parameter::Value& cur) { list.push_back(parameter::getValue<TransformationPtr>(cur)); });
			return list;
		}
	}

	Pipeline::Pipeline(const parameter::Value& value) : Transformation(PipelineType::getInstance(), extractTransformationList(value), value) {}


	bool Pipeline::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		const Pipeline* other = dynamic_cast<const Pipeline*>(&transform);
		return other && equals(getSubTransformations(), other->getSubTransformations(), equal_target<TransformationPtr>());
	}

	std::ostream& Pipeline::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "Pipeline";
		if(getSubTransformations().empty()) { return out << "()"; }

		for_each(getSubTransformations(), [&](const TransformationPtr& cur) {
			out << "\n";
			cur->printTo(out, indent + 1);
		});

		return out;
	}


	ForEach::ForEach(const parameter::Value& value)
	    : Transformation(ForEachType::getInstance(), toVector<TransformationPtr>(getTransform(value, 1)), value),
	      filter(parameter::getValue<filter::Filter>(value, 0)), preorder(parameter::getValue<bool>(value, 2)),
	      maxDepth(parameter::getValue<unsigned>(value, 3)) {}

	bool ForEach::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		// compare field by field
		const ForEach* other = dynamic_cast<const ForEach*>(&transform);
		return other && filter == other->filter && maxDepth == other->maxDepth && preorder == other->preorder && filter == other->filter
		       && *getTransformation() == *other->getTransformation();
	}

	std::ostream& ForEach::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "For " << filter << " in " << ((preorder) ? "preorder" : "postorder") << " within " << maxDepth << " levels do\n";
		return getTransformation()->printTo(out, indent + 1);
	}


	ForAll::ForAll(const parameter::Value& value)
	    : Transformation(ForAllType::getInstance(), toVector<TransformationPtr>(getTransform(value, 1)), value),
	      filter(parameter::getValue<filter::TargetFilter>(value, 0)) {}


	core::NodeAddress ForAll::apply(const core::NodeAddress& targetAddress) const {
		auto target = targetAddress.as<core::NodePtr>();

		// generate list of target nodes
		vector<core::NodeAddress> targets = filter(target);
		if(targets.empty()) { return targetAddress; }

		// generate replacement map
		std::map<core::NodeAddress, core::NodePtr> replacements;
		for_each(targets, [&](const core::NodeAddress& cur) { replacements[cur] = getTransformation()->apply(cur.getAddressedNode()); });

		// apply replacement
		auto mod = core::transform::replaceAll(target->getNodeManager(), replacements);
		return core::transform::replaceAddress(target->getNodeManager(), targetAddress, mod);
	}

	bool ForAll::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		// compare field by field
		const ForAll* other = dynamic_cast<const ForAll*>(&transform);
		return other && filter == other->filter && *getTransformation() == *other->getTransformation();
	}

	std::ostream& ForAll::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "For " << filter << " do\n";
		return getTransformation()->printTo(out, indent + 1);
	}


	Fixpoint::Fixpoint(const parameter::Value& value)
	    : Transformation(FixpointType::getInstance(), toVector<TransformationPtr>(getTransform(value, 0)), value),
	      maxIterations(parameter::getValue<unsigned>(value, 1)), acceptNonFixpoint(parameter::getValue<bool>(value, 2)) {}

	bool Fixpoint::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		// compare field by field
		const Fixpoint* other = dynamic_cast<const Fixpoint*>(&transform);
		return other && maxIterations == other->maxIterations && acceptNonFixpoint == other->acceptNonFixpoint
		       && *getTransformation() == *other->getTransformation();
	}

	std::ostream& Fixpoint::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "Fixpoint - max iterations: " << maxIterations << " - accepting approximation: " << ((acceptNonFixpoint) ? "true" : "false") << "\n";
		return getTransformation()->printTo(out, indent + 1);
	}


	Condition::Condition(const parameter::Value& value)
	    : Transformation(ConditionType::getInstance(), toVector(getTransform(value, 1), getTransform(value, 2)), value),
	      condition(parameter::getValue<filter::Filter>(value, 0)), thenTransform(getTransform(value, 1)), elseTransform(getTransform(value, 2)) {}

	bool Condition::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		// compare field by field
		const Condition* other = dynamic_cast<const Condition*>(&transform);
		return other && condition == other->condition && *thenTransform == *other->thenTransform && *elseTransform == *other->elseTransform;
	}

	std::ostream& Condition::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "if (" << condition << ") then \n";
		thenTransform->printTo(out, indent + 1);
		out << "\n" << indent << "else\n";
		elseTransform->printTo(out, indent + 1);
		return out;
	}


	TryOtherwise::TryOtherwise(const parameter::Value& value)
	    : Transformation(TryOtherwiseType::getInstance(), toVector(getTransform(value, 0), getTransform(value, 1)), value),
	      tryTransform(getTransform(value, 0)), otherwiseTransform(getTransform(value, 1)) {}


	bool TryOtherwise::operator==(const Transformation& transform) const {
		// check for identity
		if(this == &transform) { return true; }

		// compare field by field
		const TryOtherwise* other = dynamic_cast<const TryOtherwise*>(&transform);
		return other && *tryTransform == *other->tryTransform && *otherwiseTransform == *other->otherwiseTransform;
	}

	std::ostream& TryOtherwise::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "try \n";
		tryTransform->printTo(out, indent + 1);
		out << "\n" << indent << "otherwise\n";
		otherwiseTransform->printTo(out, indent + 1);
		return out;
	}


	core::NodeAddress ForEach::apply(const core::NodeAddress& targetAddress) const {
		auto target = targetAddress.as<core::NodePtr>();
		auto mod = apply(target, maxDepth);
		return core::transform::replaceAddress(target->getNodeManager(), targetAddress, mod);
	}


	core::NodePtr ForEach::apply(const core::NodePtr& target, unsigned depth) const {
		// terminate application of transformation at level zero
		if(depth == 0) { return target; }

		core::NodePtr res = target;

		// conduct transformation in pre-order if requested
		if(preorder && filter(res)) { res = getTransformation()->apply(res); }

		// conduct recursive decent - by transforming children
		core::NodeList children = res->getChildList();
		for_each(children, [&](core::NodePtr& cur) { cur = apply(cur, depth - 1); });

		// re-assemble transformed node from modified child list (if necessary)
		if(!equals(children, res->getChildList(), equal_target<core::NodePtr>())) {
			core::transform::ChildListMapping nodeMapper(children);
			res = res->substitute(res->getNodeManager(), nodeMapper);
		}

		// conduct transformation in post-order if requested
		if(!preorder && filter(res)) { res = getTransformation()->apply(res); }

		// done
		return res;
	}


	core::NodeAddress Fixpoint::apply(const core::NodeAddress& targetAddress) const {
		auto target = targetAddress.as<core::NodePtr>();

		// apply transformation until result represents a fix-point of the sub-transformation
		core::NodePtr cur = target;
		;
		core::NodePtr last;
		unsigned counter = 0;
		do {
			last = cur;
			cur = getTransformation()->apply(last);
			counter++;
		} while(*cur != *last && counter <= maxIterations);

		// check whether fixpoint could be obtained
		if(!acceptNonFixpoint && *cur != *last) {
			// => no fixpoint found!
			throw InvalidTargetException("Fixpoint could not be obtained!");
		}

		// return fixpoint (or approximation)
		return core::transform::replaceAddress(target->getNodeManager(), targetAddress, cur);
	}

	TransformationPtr makeTryOtherwise(const TransformationPtr& first) {
		assert_true(first) << "Transformation must be valid!";
		return first;
	}

	TransformationPtr makeTry(const TransformationPtr& trans) {
		return makeTryOtherwise(trans, makeNoOp());
	}

} // namespace transform
} // namespace insieme

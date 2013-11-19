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

#include "insieme/transform/versioning.h"

#include <stdint.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"

namespace insieme {
namespace transform {

	namespace {

		vector<TransformationPtr> extract(const parameter::Value& value) {
			vector<TransformationPtr> res;
			auto valueList = parameter::getValue<vector<parameter::Value>>(value);
			for_each(valueList, [&](const parameter::Value& cur) {
				res.push_back(parameter::getValue<TransformationPtr>(cur));
			});
			return res;
		}

		parameter::Value encode(const vector<TransformationPtr>& list) {
			vector<parameter::Value> res;
			for_each(list, [&](const TransformationPtr& cur) {
				res.push_back(parameter::makeValue(cur));
			});
			return parameter::makeValue(res);
		}

	}

	Versioning::Versioning(const vector<TransformationPtr>& transformations)
		: Transformation(VersioningType::getInstance(), transformations, encode(transformations)) {
		if (getSubTransformations().empty()) {
			throw InvalidParametersException("Cannot create versioning transformation without versions!");
		}
	}

	Versioning::Versioning(const parameter::Value& params)
		: Transformation(VersioningType::getInstance(), extract(params), params) {
		if (getSubTransformations().empty()) {
			throw InvalidParametersException("Cannot create versioning transformation without versions!");
		}
	}

	core::NodeAddress Versioning::apply(const core::NodeAddress& targetAddress) const {
		auto target = targetAddress.as<core::NodePtr>();

		// check pre-condition
		if (target->getNodeCategory() != core::NC_Statement && target->getNodeCategory() != core::NC_Expression) {
			throw InvalidTargetException("Can only be applied to statements and expressions!");
		}

		// check for free break statements
		bool hasFreeBreak = false;
		visitDepthFirstOncePrunable(target, [&](const core::NodePtr& cur) {
			if (cur->getNodeType() == core::NT_ForStmt || cur->getNodeType() == core::NT_WhileStmt) {
				return true; // do not decent here
			}
			if (cur->getNodeType() == core::NT_BreakStmt) {
				hasFreeBreak = true;	// "free" break found
			}
			return hasFreeBreak;
		});
		if (hasFreeBreak) {
			throw InvalidTargetException("Transformation would capture a break. Break statements are not supported within switch statements!");
		}

		// get list of transformations to be used for versioning
		auto& transformations = getSubTransformations();

		// special handling for single-version node
		if (transformations.size() == 1u) {
			return transformations[0]->apply(targetAddress);
		}

		// create switch selecting versions
		core::IRBuilder builder(target->getNodeManager());

		vector<core::ExpressionPtr> index;
		vector<core::SwitchCasePtr> cases;

		uint16_t i = 0;
		core::TypePtr uint16 = builder.getLangBasic().getUInt2();
		for_each(transformations, [&](const TransformationPtr& cur) {
			core::LiteralPtr lit = builder.literal(uint16, toString(i));
			index.push_back(lit);
			cases.push_back(builder.switchCase(lit,
				static_pointer_cast<core::StatementPtr>(cur->apply(target)))
			);
			i++;
		});

		// create final switch stmt
		auto res = builder.switchStmt(builder.pickVariant(index), builder.switchCases(cases), builder.getNoOp());
		return core::transform::replaceAddress(target->getNodeManager(), targetAddress, res);
	}

	bool Versioning::operator==(const Transformation& transform) const {
		if (this == &transform) {
			return true;
		}

		// check type and compare list of contained transformations
		const Versioning* other = dynamic_cast<const Versioning*>(&transform);
		return other && equals(getSubTransformations(), other->getSubTransformations(), equal_target<TransformationPtr>());
	}

	std::ostream& Versioning::printTo(std::ostream& out, const Indent& indent) const {
		out << indent << "Versioning\n";
		for_each(getSubTransformations(), [&](const TransformationPtr& cur) {
			cur->printTo(out, indent+1); out << "\n";
		});
		return out;
	}


} // end namespace transform
} // end namespace insieme

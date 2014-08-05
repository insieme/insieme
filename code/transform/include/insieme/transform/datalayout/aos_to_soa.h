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

#include "insieme/core/ir_builder.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/pattern/pattern.h"

namespace insieme {
namespace transform {
namespace datalayout {

class AosToSoa {
	core::NodeManager& mgr;

	virtual utils::map::PointerMap<core::VariablePtr, core::RefTypePtr> findCandidates(core::NodePtr toTransform);
	virtual core::StructTypePtr createNewType(core::StructTypePtr oldType);

	core::ExpressionPtr updateInit(core::ExpressionPtr init, core::TypePtr oldType, core::TypePtr newType);
	void replaceAssignments(const core::VariableMap& varReplacements, const core::StructTypePtr& newStructType,
			const core::NodeAddress& toTransform, const core::pattern::TreePattern& allocPattern, core::ExpressionPtr& nElems,
			std::map<core::NodeAddress, core::NodePtr>& replacements);


	core::StatementPtr generateMarshalling(const core::VariablePtr& oldVar, const core::VariablePtr& newVar, const core::ExpressionPtr& start,
			const core::ExpressionPtr& end, const core::StructTypePtr& structType);
	std::vector<core::StatementAddress> addMarshalling(const core::VariableMap& varReplacements,
			const core::StructTypePtr& newStructType, const core::NodeAddress& toTransform, const core::ExpressionPtr& nElems,
			std::map<core::NodeAddress, core::NodePtr>& replacements);

	core::StatementPtr generateUnmarshalling(const core::VariablePtr& oldVar, const core::VariablePtr& newVar, const core::ExpressionPtr& start,
			const core::ExpressionPtr& end, const core::StructTypePtr& structType);
	std::vector<core::StatementAddress> addUnmarshalling(const core::VariableMap& varReplacements,
			const core::StructTypePtr& newStructType, const core::NodeAddress& toTransform, const core::StatementAddress& begin,
			const core::ExpressionPtr& nElems, std::map<core::NodeAddress, core::NodePtr>& replacements);

	core::ExpressionMap replaceAccesses(const core::VariableMap& varReplacements, const core::NodeAddress& toTransform,
			const std::vector<core::StatementAddress>& begin, const std::vector<core::StatementAddress>& end,
			std::map<core::NodeAddress, core::NodePtr>& replacements);

	void replaceScalarStructs(const core::pattern::AddressMatchOpt& match, const core::VariablePtr& newVar,
			std::map<core::NodeAddress, core::NodePtr>& replacements, core::ExpressionMap& structures);
	void updateScalarStructAccesses(core::NodePtr& toTransform);

	core::CompoundStmtPtr generateDel(const core::StatementAddress& stmt, const core::VariablePtr& oldVar, const core::VariablePtr& newVar,
			const core::StructTypePtr& newStructType);
	void addNewDel(const core::VariableMap& varReplacements, const core::NodeAddress& toTransform,
			const core::StructTypePtr& newStructType, std::map<core::NodeAddress, core::NodePtr>& replacements);
public:
	AosToSoa(core::NodePtr& toTransform);
	virtual ~AosToSoa() {}
};

class VariableAdder: public core::transform::CachedNodeMapping {
	core::NodeManager& mgr;
	core::VariableMap& varReplacements;

public:
	VariableAdder(core::NodeManager& mgr, core::VariableMap& varReplacements)
			: mgr(mgr), varReplacements(varReplacements) {}

	const core::NodePtr resolveElement(const core::NodePtr& element);

	core::VariableMap getVarMapping() { return varReplacements; }
};

} // datalayout
} // transform
} // insieme

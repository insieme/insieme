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

//#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/transform/datalayout/aos_to_taos.h"
#include "insieme/transform/datalayout/datalayout_utils.h"

#include "insieme/utils/annotation.h"

#include "insieme/analysis/defuse_collect.h"
#include "insieme/analysis/cba/analysis.h"
#include "insieme/analysis/scopes_map.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

AosToTaos::AosToTaos(core::NodePtr& toTransform) : AosToSoa(toTransform) {
	IRBuilder builder(mgr);
	genericTileSize = builder.variableIntTypeParam('t');
}

void AosToTaos::transform() {
	IRBuilder builder(mgr);

	std::vector<std::pair<ExpressionSet, RefTypePtr>> toReplaceLists = createCandidateLists();

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(mgr.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));


	for(std::pair<ExpressionSet, RefTypePtr> toReplaceList : toReplaceLists) {
		StructTypePtr oldStructType = toReplaceList.second->getElementType().as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();

		StructTypePtr newStructType = createNewType(oldStructType);
		ExpressionMap varReplacements;
		ExpressionMap nElems;
		std::map<NodeAddress, NodePtr> replacements;

		for(ExpressionPtr oldVar : toReplaceList.first) {
			TypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), toReplaceList.second,
					builder.refType(builder.arrayType(newStructType))).as<TypePtr>();
//std::cout << "NT: " << newStructType << " var " << oldVar << std::endl;

			// check if local or global variable
			LiteralPtr globalVar = oldVar.isa<LiteralPtr>();

			// create new variables, local or global
			varReplacements[oldVar] = globalVar ?
					builder.literal(globalVar->getStringValue() + "_soa", newType).as<ExpressionPtr>() :
					builder.variable(newType).as<ExpressionPtr>();
		}

		VariableAdder varAdd(mgr, varReplacements);
		toTransform = varAdd.mapElement(0, toTransform);

		NodeAddress tta(toTransform);

		addNewDecls(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);


//for(std::pair<NodeAddress, NodePtr> r : replacements) {
//	std::cout << "\nFRom:\n";
//	dumpPretty(r.first);
//	std::cout << "\nTo: \n";
//	dumpPretty(r.second);
//	std::map<NodeAddress, NodePtr> re;
//	re[r.first] = r.second;
////	core::transform::replaceAll(mgr, re);
//	std::cout << "\n------------------------------------------------------------------------------------------------------------------------\n";
//}

		if(!replacements.empty())
			toTransform = core::transform::replaceAll(mgr, replacements);

		toTransform = core::transform::replaceAll(mgr, toTransform, genericTileSize, builder.concreteIntTypeParam(512));
	}
}

StructTypePtr AosToTaos::createNewType(core::StructTypePtr oldType) {
	IRBuilder builder(mgr);

	NodeRange<NamedTypePtr> member = oldType->getElements();
	std::vector<NamedTypePtr> newMember;
	for(NamedTypePtr memberType : member) {
//			std::cout << "member: " << memberType << std::endl;
		newMember.push_back(builder.namedType(memberType->getName(), builder.vectorType(memberType->getType(), genericTileSize)));

	}

	return builder.structType(newMember);
}

StatementList AosToTaos::generateNewDecl(const ExpressionMap& varReplacements, const DeclarationStmtAddress& decl, const VariablePtr& newVar,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and it's initialization
	StatementList allDecls;

	allDecls.push_back(decl);
	allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), core::transform::replaceAllGen(mgr, decl->getInitialization().getAddressedNode(),
			oldStructType, newStructType, true)));

	return allDecls;
}

StatementList AosToTaos::generateNewAssigns(const ExpressionMap& varReplacements, const CallExprAddress& call,
		const ExpressionPtr& newVar, const StructTypePtr& newStructType) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	allAssigns.push_back(call);


	return allAssigns;
}


} // datalayout
} // transform
} // insieme


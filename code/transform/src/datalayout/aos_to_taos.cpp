/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/transform/datalayout/aos_to_taos.h"
#include "insieme/transform/datalayout/parallel_sec_transform.h"
#include "insieme/transform/datalayout/datalayout_utils.h"

#include "insieme/utils/annotation.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

namespace {

StatementPtr aosToTaosAllocTypeUpdate(const StatementPtr& stmt) {
	pattern::TreePattern oldStructTypePattern = pattern::aT(pattern::var("structType", pirp::structType(*pattern::any)));
	pattern::TreePattern newStructTypePattern = pattern::aT(pattern::var("structType", pirp::structType(*pattern::any)));

	return allocTypeUpdate(stmt, oldStructTypePattern, newStructTypePattern);
}

}

AosToTaos::AosToTaos(core::NodePtr& toTransform, CandidateFinder candidateFinder) : DatalayoutTransformer(toTransform, candidateFinder) {
	IRBuilder builder(mgr);
	genericTileSize = builder.variableIntTypeParam('t');
	genericTileSizeExpr = builder.uintLit(84537493);
}

void AosToTaos::transform() {
	IRBuilder builder(mgr);

	const NodeAddress toTransAddr(toTransform);
	std::vector<std::pair<ExprAddressSet, ArrayTypePtr>> toReplaceLists = createCandidateLists(toTransAddr);

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(mgr.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));

	for(std::pair<ExprAddressSet, ArrayTypePtr> toReplaceList : toReplaceLists) {
		StructTypePtr oldStructType = toReplaceList.second.as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();

		StructTypePtr newStructType = createNewType(oldStructType);
		ExprAddressMap varReplacements;
		std::map<StatementPtr, ExpressionPtr> nElems;
		std::map<NodeAddress, NodePtr> replacements;

		for(ExpressionAddress oldVar : toReplaceList.first) {
			// update root for in case it has been modified in a previous iteration
			oldVar = oldVar.switchRoot(toTransform);

			TypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), /*check this for src types*/builder.refType(toReplaceList.second),
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
		NodeAddress tta = varAdd.addVariablesToLambdas(toTransform);

		addNewDecls(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		// assignments to the entire struct should be ported to the new struct members
		replaceAssignments(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		//introducing marshalling
		std::vector<StatementAddress> begin = addMarshalling(varReplacements, newStructType, tta, nElems, replacements);

		//introducing unmarshalling
		std::vector<StatementAddress> end = addUnmarshalling(varReplacements, newStructType, tta, begin, nElems, replacements);

		//free memory of the new variables
		addNewDel(varReplacements, tta, newStructType, replacements);

//for(std::pair<NodeAddress, NodePtr> r : replacements) {
//	std::cout << "\nFRom:\n";
//	dumpPretty(r.first);
//	std::cout << "\nTo: \n";
//	dumpPretty(r.second);
//	std::map<NodeAddress, NodePtr> re;
//	re[r.first] = r.second;
//	core::transform::replaceAll(mgr, re);
//	std::cout << "\n------------------------------------------------------------------------------------------------------------------------\n";
//}
		updateTuples(varReplacements, newStructType, oldStructType, tta, replacements);

		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

		updateCopyDeclarations(varReplacements, newStructType, oldStructType, tta, replacements);

//for(std::pair<NodeAddress, NodePtr> r : replacements) {
//	std::cout << "\nFRom:\n";
//	dumpPretty(r.first);
//	std::cout << "\nTo: \n";
//	dumpPretty(r.second);
//	std::map<NodeAddress, NodePtr> re;
//	re[r.first] = r.second;
//	core::transform::replaceAll(mgr, re);
//	std::cout << "\n------------------------------------------------------------------------------------------------------------------------\n";
//}
//assert_fail();
		const ExprAddressMap kernelVarReplacements =
				replaceStructsInJobs(varReplacements, newStructType, oldStructType, toTransform, allocPattern, replacements);

		doReplacements(kernelVarReplacements, replacements, aosToTaosAllocTypeUpdate);

		NodeMap tilesize;
		tilesize[genericTileSizeExpr] = builder.uintLit(64);
		tilesize[genericTileSize] = builder.concreteIntTypeParam(64);
		toTransform = core::transform::replaceAll(mgr, toTransform, tilesize, false);
	}

	// remove all inserted compound expressions
	NewCompoundsRemover remComp(mgr);
	toTransform = remComp.mapElement(0, toTransform);
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

ExpressionPtr AosToTaos::updateAccess(const ExpressionPtr& oldAccess, const std::pair<ExpressionAddress, StatementPtr>& varInInit,
		const StringValuePtr& fieldName) {
	// replace accesses to old variable with accesses to field in new variable
	IRBuilder builder(mgr);
	ExpressionMap localReplacement;
	localReplacement[varInInit.first] = fieldName ?
			builder.accessMember(varInInit.second.as<ExpressionPtr>(), fieldName) : varInInit.second.as<ExpressionPtr>();

	core::pattern::TreePattern arrayAccess = pattern::aT(pirp::arrayRefElem1D(pattern::any, var("idx", pattern::any)));
	pattern::MatchOpt match = arrayAccess.matchPointer(oldAccess);

	if(match) {
		// replace index expression with one divided by tilesize
		// it has to be devidable, otherwise it won't produce correct code
		// TODO add check
		ExpressionPtr idxExpr = match.get()["idx"].getValue().as<ExpressionPtr>();
		localReplacement[idxExpr] = builder.div(idxExpr, genericTileSizeExpr);
	}

	return core::transform::fixTypesGen(mgr, oldAccess, localReplacement, true);
}

StatementList AosToTaos::generateNewDecl(const ExprAddressMap& varReplacements, const DeclarationStmtAddress& decl, const StatementPtr& newVar,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and its initialization
	StatementList allDecls;

	allDecls.push_back(decl);

	NodeMap inInitReplacementsInCaseOfNovarInInit = generateTypeReplacements(oldStructType, newStructType);
//	inInitReplacementsInCaseOfNovarInInit[oldStructType] = newStructType;
	// divide initialization size by tilesize
	if(nElems) inInitReplacementsInCaseOfNovarInInit[nElems] = builder.div(nElems, genericTileSizeExpr);

	allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), updateInit(varReplacements, decl->getInitialization(),
		inInitReplacementsInCaseOfNovarInInit)));

	return allDecls;
}

StatementList AosToTaos::generateNewAssigns(const ExprAddressMap& varReplacements, const CallExprAddress& call,
		const StatementPtr& newVar, const StructTypePtr& newStructType, const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	allAssigns.push_back(call);
	NodeMap inInitReplacementsInCaseOfNovarInInit = generateTypeReplacements(oldStructType, newStructType);
//	inInitReplacementsInCaseOfNovarInInit[oldStructType] = newStructType;
	// divide initialization size by tilesize
	if(nElems) inInitReplacementsInCaseOfNovarInInit[nElems] = builder.div(nElems, genericTileSizeExpr);

	allAssigns.push_back(builder.assign(newVar.as<ExpressionPtr>(), updateInit(varReplacements, call[1], inInitReplacementsInCaseOfNovarInInit)));

	return allAssigns;
}

StatementPtr AosToTaos::generateMarshalling(const ExpressionAddress& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	TypePtr boundaryType = start->getType();
	VariablePtr iterator = builder.variable(boundaryType);
	VariablePtr tiledIterator = builder.variable(boundaryType);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = genericTileSizeExpr;

	for(NamedTypePtr memberType : structType->getElements()) {
		ExpressionPtr globalIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.add(builder.mul(iterator, tilesize), tiledIterator));

		ExpressionPtr aosAccess = builder.deref(refAccess(oldVar, globalIdx, memberType->getName()));
		ExpressionPtr taosAccess = refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName(),
				builder.castExpr(builder.getLangBasic().getUInt8(), tiledIterator));

		loopBody.push_back(builder.assign(taosAccess, aosAccess));
	}

	StatementPtr innerLoop = builder.forStmt(builder.declarationStmt(tiledIterator, builder.castExpr(boundaryType, builder.uintLit(0))),
			builder.castExpr(boundaryType, tilesize), builder.literal(boundaryType, "1"),
			builder.compoundStmt(loopBody));

	return builder.forStmt(builder.declarationStmt(iterator, builder.castExpr(boundaryType, builder.div(start, tilesize))),
			builder.castExpr(boundaryType, builder.div(end, tilesize)), builder.literal(boundaryType, "1"), innerLoop);
}

StatementPtr AosToTaos::generateUnmarshalling(const ExpressionAddress& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	TypePtr boundaryType = start->getType();
	VariablePtr iterator = builder.variable(boundaryType);
	VariablePtr tiledIterator = builder.variable(boundaryType);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = genericTileSizeExpr;

	for(NamedTypePtr memberType : structType->getElements()) {
		ExpressionPtr globalIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.add(builder.mul(iterator, tilesize), tiledIterator));

		ExpressionPtr aosAccess = refAccess(oldVar, globalIdx, memberType->getName());
		ExpressionPtr taosAccess = builder.deref(refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName(),
				builder.castExpr(builder.getLangBasic().getUInt8(), tiledIterator)));

		loopBody.push_back(builder.assign(aosAccess, taosAccess));
	}

	StatementPtr innerLoop = builder.forStmt(builder.declarationStmt(tiledIterator, builder.castExpr(boundaryType, builder.uintLit(0))),
			builder.castExpr(boundaryType, tilesize), builder.literal(boundaryType, "1"),
			builder.compoundStmt(loopBody));

	return builder.forStmt(builder.declarationStmt(iterator, builder.castExpr(boundaryType, builder.div(start, tilesize))),
			builder.castExpr(boundaryType, builder.div(end, tilesize)), builder.literal(boundaryType, "1"), innerLoop);
}

StatementList AosToTaos::generateDel(const StatementAddress& stmt, const ExpressionAddress& oldVar, const ExpressionPtr& newVar,
		const StructTypePtr& newStructType) {
	StatementList deletes;

	pattern::TreePattern delVarPattern = pirp::refDelete(aT(pattern::var("oldVar", pattern::atom(oldVar))));
	pattern::AddressMatchOpt match = delVarPattern.matchAddress(stmt);

	if(match) {
		ExpressionAddress matchedOldVar = match.get()["oldVar"].getValue().as<ExpressionAddress>();
		if(!compareVariables(oldVar, matchedOldVar))
			return deletes;

		IRBuilder builder(mgr);
		deletes.push_back(core::transform::fixTypes(mgr, match.get().getRoot(), oldVar, newVar, true).as<ExpressionPtr>());
		deletes.push_back(stmt);
	}

	return deletes;
}

ExpressionPtr AosToTaos::generateNewAccesses(const ExpressionAddress& oldVar, const StatementPtr& newVar, const StringValuePtr& member, const ExpressionPtr& index,
		const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypesGen(mgr, oldStructAccess, oldVar, newVar.as<ExpressionPtr>(), false);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr arrayIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.div(index, genericTileSizeExpr));
	ExpressionPtr vectorIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.mod(index, genericTileSizeExpr));

	ExpressionPtr vectorAccess = refAccess(newStructAccess, arrayIdx, member, vectorIdx);

	return vectorAccess;
}

ExpressionPtr AosToTaos::generateByValueAccesses(const ExpressionPtr& oldVar, const StatementPtr& newVar, const core::StructTypePtr& newStructType,
		const ExpressionPtr& index, const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypesGen(mgr, oldStructAccess, oldVar, newVar.as<ExpressionPtr>(), false).as<ExpressionPtr>();

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr arrayIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.div(index, genericTileSizeExpr));
	ExpressionPtr vectorIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.mod(index, genericTileSizeExpr));

	vector<std::pair<StringValuePtr, ExpressionPtr>> values;
	for(NamedTypePtr memberType : newStructType->getElements()) {

		StringValuePtr memberName = memberType->getName();

		ExpressionPtr vectorAccess = builder.deref(refAccess(newStructAccess, arrayIdx, memberName, vectorIdx));
		values.push_back(std::make_pair(memberName, vectorAccess));
	}

	return builder.structExpr(values);
}

const ExprAddressMap AosToTaos::replaceStructsInJobs(ExprAddressMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
			NodePtr& toTransform, const pattern::TreePattern& allocPattern, std::map<NodeAddress, NodePtr>& replacements) {

	ParSecTransform<AosToTaos> psa(toTransform, varReplacements, replacements, newStructType, oldStructType);
	psa.transform();
	return psa.getKernelVarReplacements();
}

} // datalayout
} // transform
} // insieme


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
#include "insieme/core/types/subtyping.h"

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

StatementPtr allocTypeUpdate(const StatementPtr& stmt) {
	TypePtr oldType, newType;
	NodeManager& mgr = stmt->getNodeManager();

	pattern::TreePattern structTypePattern = pattern::aT(pattern::var("structType", pirp::structType(*pattern::any)));

	if(const DeclarationStmtPtr& decl = stmt.isa<DeclarationStmtPtr>()) {
		const VariablePtr& var = decl->getVariable();
		const CallExprPtr& init = decl->getInitialization().isa<CallExprPtr>();

		// check if init is a call and its type fits the variable
		if(!init | types::isSubTypeOf(var->getType(), init->getType()))
			return stmt;

		// if init is a call with wrong, try to fix the type
		pattern::MatchOpt initMatch = structTypePattern.matchPointer(init[0]->getType());
		pattern::MatchOpt varMatch = structTypePattern.matchPointer(var->getType());
		ExpressionPtr nElem;
		if(initMatch && varMatch) {
			oldType = initMatch.get()["structType"].getValue().as<TypePtr>();
			newType = varMatch.get()["structType"].getValue().as<TypePtr>();
		}
	}

	if(const CallExprPtr& assign = stmt.isa<CallExprPtr>()) {
		if(!core::analysis::isCallOf(assign, mgr.getLangBasic().getRefAssign()))
			return stmt;

		const ExpressionPtr& lhs = assign[0];
		const ExpressionPtr& rhs = assign[1];

		const RefTypePtr& lhsTy = lhs->getType().as<RefTypePtr>();

		// check if type is consistent
		if(lhsTy->getElementType() == rhs->getType())
			return stmt;

		// if type is not consistent, try to update the type
		pattern::MatchOpt rhsMatch = structTypePattern.matchPointer(rhs->getType());
		pattern::MatchOpt lhsMatch = structTypePattern.matchPointer(lhsTy->getElementType());
		ExpressionPtr nElem;
		if(rhsMatch && lhsMatch) {
			oldType = rhsMatch.get()["structType"].getValue().as<TypePtr>();
			newType = lhsMatch.get()["structType"].getValue().as<TypePtr>();
		}
	}

	if(oldType)
		return core::transform::replaceAllGen(mgr, stmt, oldType, newType, false);

	return stmt;
}

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

		// assignments to the entire struct should be ported to the new sturct members
		replaceAssignments(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		//introducing marshalling
		std::vector<StatementAddress> begin = addMarshalling(varReplacements, newStructType, tta, nElems, replacements);

		//introducing unmarshalling
		std::vector<StatementAddress> end = addUnmarshalling(varReplacements, newStructType, tta, begin, nElems, replacements);

		//free memory of the new variables
		addNewDel(varReplacements, tta, newStructType, replacements);

		//replace array accesses
		ExpressionMap structures = replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

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
		updateTuples(varReplacements, newStructType, oldStructType, tta, replacements, structures);

		if(!replacements.empty())
			toTransform = core::transform::replaceAll(mgr, replacements);

for(std::pair<ExpressionPtr, ExpressionPtr> s : structures) {
	std::cout<< "\nöjaflsssssssssssk\n" << s.first << "  ->  " << s.second << std::endl << std::endl;
}

		core::transform::TypeHandler handleTypesInAlloc = allocTypeUpdate;

		if(!structures.empty())
			toTransform = core::transform::fixTypes(mgr, toTransform, structures, false, handleTypesInAlloc);

		NodeMap tilesize;
		tilesize[builder.uintLit(84537493)] = builder.uintLit(64);
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

StatementList AosToTaos::generateNewDecl(const ExpressionMap& varReplacements, const DeclarationStmtAddress& decl, const VariablePtr& newVar,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and it's initialization
	StatementList allDecls;

	allDecls.push_back(decl);

	NodeMap inInitReplacementsInCaseOfNovarInInit;
	inInitReplacementsInCaseOfNovarInInit[oldStructType] = newStructType;
	// divide initialization size by tilesize
	if(nElems) inInitReplacementsInCaseOfNovarInInit[nElems] = builder.div(nElems, builder.uintLit(84537493));

	allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), updateInit(varReplacements, decl->getInitialization(),
		inInitReplacementsInCaseOfNovarInInit)));

	return allDecls;
}

StatementList AosToTaos::generateNewAssigns(const ExpressionMap& varReplacements, const CallExprAddress& call,
		const ExpressionPtr& newVar, const StructTypePtr& newStructType, const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	allAssigns.push_back(call);
	NodeMap inInitReplacementsInCaseOfNovarInInit;
	inInitReplacementsInCaseOfNovarInInit[oldStructType] = newStructType;
	// divide initialization size by tilesize
	if(nElems) inInitReplacementsInCaseOfNovarInInit[nElems] = builder.div(nElems, builder.uintLit(84537493));

	allAssigns.push_back(builder.assign(newVar, updateInit(varReplacements, call[1], inInitReplacementsInCaseOfNovarInInit)));

	return allAssigns;
}

StatementPtr AosToTaos::generateMarshalling(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	TypePtr boundaryType = start->getType();
	VariablePtr iterator = builder.variable(boundaryType);
	VariablePtr tiledIterator = builder.variable(boundaryType);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = builder.uintLit(84537493);

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

StatementPtr AosToTaos::generateUnmarshalling(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	TypePtr boundaryType = start->getType();
	VariablePtr iterator = builder.variable(boundaryType);
	VariablePtr tiledIterator = builder.variable(boundaryType);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = builder.uintLit(84537493);

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

StatementList AosToTaos::generateDel(const StatementAddress& stmt, const ExpressionPtr& oldVar, const ExpressionPtr& newVar,
		const StructTypePtr& newStructType) {
	StatementList deletes;

	pattern::TreePattern delVarPattern = pirp::refDelete(aT(pattern::atom(oldVar)));
	pattern::AddressMatchOpt match = delVarPattern.matchAddress(stmt);

	if(match) {
		IRBuilder builder(mgr);
		deletes.push_back(core::transform::fixTypes(mgr, match.get().getRoot(), oldVar, newVar, true).as<ExpressionPtr>());
		deletes.push_back(stmt);
	}

	return deletes;
}

ExpressionPtr AosToTaos::generateNewAccesses(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const StringValuePtr& member, const ExpressionPtr& index,
		const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypesGen(mgr, oldStructAccess, oldVar, newVar, false);

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = builder.uintLit(84537493);
	ExpressionPtr arrayIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.div(index, tilesize));
	ExpressionPtr vectorIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.mod(index, tilesize));

	ExpressionPtr vectorAccess = refAccess(newStructAccess, arrayIdx, member, vectorIdx);

	return vectorAccess;
}

ExpressionPtr AosToTaos::generateByValueAccesses(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const core::StructTypePtr& newStructType,
		const ExpressionPtr& index, const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypesGen(mgr, oldStructAccess, oldVar, newVar, false).as<ExpressionPtr>();

	// 84537493 is a placeholder for tilesize. Hopefully nobody else is going to use this number...
	ExpressionPtr tilesize = builder.uintLit(84537493);
	ExpressionPtr arrayIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.div(index, tilesize));
	ExpressionPtr vectorIdx = builder.castExpr(builder.getLangBasic().getUInt8(), builder.mod(index, tilesize));

	vector<std::pair<StringValuePtr, ExpressionPtr>> values;
	for(NamedTypePtr memberType : newStructType->getElements()) {

		StringValuePtr memberName = memberType->getName();

		ExpressionPtr vectorAccess = builder.deref(refAccess(newStructAccess, arrayIdx, memberName, vectorIdx));

		values.push_back(std::make_pair(memberName, vectorAccess));
	}

	return builder.structExpr(values);
}

} // datalayout
} // transform
} // insieme


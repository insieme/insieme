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

#include "insieme/transform/datalayout/aos_to_taos.h"
#include "insieme/transform/datalayout/parallelSecAtt.h"
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
		ExpressionMap structures;
		updateTuples(varReplacements, newStructType, oldStructType, tta, replacements, structures);

		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements, structures);

		updateCopyDeclarations(varReplacements, newStructType, oldStructType, tta, replacements, structures);

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
//assert(false);

		replaceStructsInJobs(varReplacements, newStructType, oldStructType, toTransform, allocPattern, replacements, structures);

		doReplacements(replacements, structures, aosToTaosAllocTypeUpdate);

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

void AosToTaos::replaceStructsInJobs(ExpressionMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
			NodePtr& toTransform, const pattern::TreePattern& allocPattern, std::map<NodeAddress, NodePtr>& replacements, ExpressionMap& structures) {

//	std::set<ExpressionPtr> varsToPropagate;
//	for(std::pair<ExpressionPtr, ExpressionPtr> oldToNew : varReplacements) {
//		varsToPropagate.insert(oldToNew.second);
//		std::cout << "ölkjasfdökljsfda " << oldToNew.second << std::endl;
//assert(false);
//	}

	ParSecAtt psa(toTransform, varReplacements);
#if 0
	ExpressionMap jobReplacements;
	IRBuilder builder(mgr);

	core::visitBreadthFirst(toTransform, [&](const ExpressionAddress& expr) {
		// adding arguments which use a tuple member expression as argument which's tuple member has been replaced already to replace list
		if(CallExprAddress call = expr.isa<CallExprAddress>()) {
			if(!core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getTupleMemberAccess()))
				return;
std::cout << "\nat the tuple member access\n";

			// check if tuple argument has a member which will be updated
			ExpressionPtr oldRootVar = getRootVariable(call, call->getArgument(0)).as<ExpressionPtr>();
			auto newRootVarIter = structures.find(oldRootVar);

			if(newRootVarIter != structures.end()) { // tuple has been updated, check if it was the current field
				ExpressionPtr newRootVar = newRootVarIter->second;

				RefTypePtr newType = getBaseType(newRootVar->getType()).as<TupleTypePtr>()->getElement(
						call->getArgument(1).as<LiteralPtr>()->getValueAs<unsigned>()).as<RefTypePtr>();
				TypePtr oldType = call->getArgument(2)->getType().as<GenericTypePtr>()->getTypeParameter(0);
std::cout << "Creating var of new type: " << newType << std::endl;

				// check if and update is needed
				if(newType == oldType)
					return;

				ExpressionAddress argument = call.getParentAddress(2).isa<CallExprAddress>();
				CallExprAddress parent = argument.getParentAddress(1).isa<CallExprAddress>();
				if(!parent)
					return;

				LambdaExprAddress lambda = parent->getFunctionExpr().isa<LambdaExprAddress>();

				if(!lambda)
					return;

				for_range(make_paired_range(parent->getArguments(), lambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
					if(pair.first == argument) {// found the argument which will be updated
						// create replacement for corresponding parameter
						TypePtr newParamType = newType->getElementType().as<ArrayTypePtr>()->getElementType();
						VariablePtr newParam = builder.variable(newParamType);

						// add corresponding parameter to update list
						jobReplacements[pair.second] = newParam;
std::cout << ": \nAdding: " << *pair.second << " - " << newParamType << std::endl;
					}
				});
			}
		}

		// propagating variables to be replaced through job expressions
		if(JobExprPtr job = expr.isa<JobExprPtr>()) {
			CallExprPtr parallelCall = job->getDefaultExpr().isa<BindExprPtr>()->getCall();

			if(!parallelCall)
				return;

			LambdaExprPtr parallelLambda = parallelCall->getFunctionExpr().isa<LambdaExprPtr>();
			if(!parallelLambda)
				return;

			for_range(make_paired_range(parallelCall->getArguments(), parallelLambda->getParameterList()->getElements()),
					[&](const std::pair<const ExpressionPtr, const VariablePtr>& pair) {
std::cout << "Looking for " << pair.first << std::endl;
				auto newArgIter = varReplacements.find(pair.first);
				if(newArgIter != varReplacements.end()) {
					jobReplacements[pair.second] = builder.variable(newArgIter->second->getType());
	std::cout << "Found in VARreplacements: " << pair.first << " -> " << jobReplacements[pair.second]->getType() << std::endl;
				}

				newArgIter = jobReplacements.find(pair.first);
				if(newArgIter != jobReplacements.end()) {
					jobReplacements[pair.second] = builder.variable(newArgIter->second->getType());
	std::cout << "Found in jobREPLACEMENTS: " << pair.first << " -> " << jobReplacements[pair.second]->getType() << std::endl;
				}
			});
		}
	});

	std::vector<StatementAddress> empty;

	ExpressionMap nElems;
std::cout << "\n---------------------------------------------------------------------\n\n";

	//replace array accesses
//	replaceAccesses(jobReplacements, newStructType, toTransform, empty, empty, replacements, structures);

	updateCopyDeclarations(jobReplacements, newStructType, oldStructType, toTransform, replacements, structures);

	// assignments to the entire struct should be ported to the new sturct members
	replaceAssignments(jobReplacements, newStructType, oldStructType, toTransform, allocPattern, nElems, replacements);

//	for(std::pair<NodeAddress, NodePtr> r : replacements) {
//		std::cout << "\nFRom:\n";
//		dumpPretty(r.first);
//		std::cout << "\nTo: \n";
//		dumpPretty(r.second);
//		std::cout << "\n------------------------------------------------------------------------------------------------------------------------\n";
//	}

//	doReplacements(replacements, structures, aosToTaosAllocTypeUpdate);

//	assert(false);
#endif
}

} // datalayout
} // transform
} // insieme


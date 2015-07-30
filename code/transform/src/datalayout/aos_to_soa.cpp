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

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/transform/datalayout/aos_to_soa.h"
#include "insieme/transform/datalayout/parallel_sec_soa.h"
#include "insieme/transform/datalayout/datalayout_utils.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

namespace {

	StatementPtr aosToSoaAllocTypeUpdate(const StatementPtr& stmt) {
		pattern::TreePattern oldStructTypePattern = pattern::aT(
				pattern::var("structType", pirp::arrayType( pirp::refType( pirp::structType(*pattern::any)))));
		pattern::TreePattern newStructTypePattern = pattern::aT(
				pattern::var("structType", pirp::structType(*pattern::any)));

		return allocTypeUpdate(stmt, oldStructTypePattern, newStructTypePattern);
	}
}

AosToSoa::AosToSoa(core::NodePtr& toTransform, CandidateFinder candidateFinder) :
		DatalayoutTransformer(toTransform, candidateFinder) {
}

void AosToSoa::transform() {

	IRBuilder builder(mgr);

	const NodeAddress toTransAddr(toTransform);
	CandidateList toReplaceLists =
			createCandidateLists(toTransAddr);

	pattern::TreePattern allocPattern = pattern::aT(
			pirp::callExpr(mgr.getLangBasic().getArrayCreate1D(),
					pattern::any << var("nElems", pattern::any)));

	for (std::pair<ExprAddressSet, StructTypePtr> toReplaceList : toReplaceLists) {
		StructTypePtr oldStructType =  toReplaceList.second;
//				toReplaceList.second.as<GenericTypePtr>()->getTypeParameter(0).as<RefTypePtr>()->getElementType().as<StructTypePtr>();


		StructTypePtr newStructType = createNewType(oldStructType);

		ExprAddressMap varReplacements;
		std::map<StatementPtr, ExpressionPtr> nElems;
		std::map<NodeAddress, NodePtr> replacements;
		pattern::TreePattern refTypePattern = pattern::aT(
				var("toBeReplaced", (pattern::atom(oldStructType))));

		for (ExpressionAddress oldVar : toReplaceList.first) {


			// update root for in case it has been modified in a previous iteration
			oldVar = oldVar.switchRoot(toTransform);

			TypeAddress ovta = oldVar->getType();
			NodePtr oldTypeToBeReplaced;

			pattern::MatchOpt typeToBeReplacedMatch = refTypePattern.matchPointer(ovta);
			assert_true(typeToBeReplacedMatch)
					<< " could not identify the type to be replaced in "
					<< *ovta;
//			if (typeToBeReplacedMatch) {
//				oldTypeToBeReplaced = typeToBeReplacedMatch.get()["toBeReplaced"].getValue();
//			}
//
//			TypePtr newType = core::transform::replaceAll(mgr,
//					oldVar->getType(), oldTypeToBeReplaced, /*check this for src types*/
//					newStructType).as<TypePtr>();

			TypePtr newType = core::transform::replaceAllGen(mgr, oldVar->getType().getAddressedNode(),
					generateTypeReplacements(typeToBeReplacedMatch.get()["toBeReplaced"].getValue().as<TypePtr>(), newStructType));
//std::cout << "NT: " << newType << " var " << oldTypeToBeReplaced << std::endl;

// check if local or global variable
			LiteralPtr globalVar = oldVar.isa<LiteralPtr>();

			// create new variables, local or global
			varReplacements[oldVar] =
					globalVar ?
							builder.literal(
									globalVar->getStringValue() + "_soa",
									newType).as<ExpressionPtr>() :
							builder.variable(newType).as<ExpressionPtr>();
		}

		VariableAdder varAdd(mgr, varReplacements);

		NodeAddress tta = varAdd.addVariablesToLambdas(toTransform);

		addNewDecls(varReplacements, newStructType, oldStructType, tta,
				allocPattern, nElems, replacements);

		// assignments to the entire struct should be ported to the new sturct members
		replaceAssignments(varReplacements, newStructType, oldStructType, tta,
				allocPattern, nElems, replacements);

		//introducing marshalling
		std::vector<StatementAddress> begin = addMarshalling(varReplacements,
				newStructType, tta, nElems, replacements);

		//introducing unmarshalling
		std::vector<StatementAddress> end = addUnmarshalling(varReplacements,
				newStructType, tta, begin, nElems, replacements);

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
		updateTuples(varReplacements, newStructType, toReplaceList.second, tta,
				replacements);

		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end,
				replacements);

		updateCopyDeclarations(varReplacements, newStructType, oldStructType,
				tta, replacements);

		const ExprAddressMap kernelVarReplacements = replaceStructsInJobs(
				varReplacements, newStructType, oldStructType, toTransform,
				allocPattern, replacements);

		doReplacements(kernelVarReplacements, replacements, aosToSoaAllocTypeUpdate);

//		replacements.clear();
//		tta = NodeAddress(toTransform);
	}

	// remove all inserted compound expressions
	NewCompoundsRemover remComp(mgr);
	toTransform = remComp.mapElement(0, toTransform);
//dumpPretty(toTransform);
}

NodeMap AosToSoa::generateTypeReplacements(const TypePtr& oldStructType, const TypePtr& newStructType) {
	IRBuilder builder(mgr);
	NodeMap tyReplace;
	tyReplace[builder.refType(builder.arrayType(oldStructType))] = newStructType;
	tyReplace[builder.refType(builder.arrayType(oldStructType), RK_SOURCE)] = newStructType; // TODO that for src types?

	return tyReplace;
}

StructTypePtr AosToSoa::createNewType(core::StructTypePtr oldType) {
	IRBuilder builder(mgr);

	NodeRange<NamedTypePtr> member = oldType->getElements();
	std::vector<NamedTypePtr> newMember;
	for (NamedTypePtr memberType : member) {
//			std::cout << "member: " << memberType << std::endl;
		newMember.push_back(
				builder.namedType(memberType->getName(),
						builder.refType(builder.arrayType(memberType->getType()))));

	}
	return builder.structType(newMember);
}

StatementList AosToSoa::generateNewDecl(const ExprAddressMap& varReplacements,
		const DeclarationStmtAddress& decl, const StatementPtr& newVar,
		const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and it's initialization
	StatementList allDecls;

//			if(memAlloc.match(decl->getInitialization())) { // if memory is allocated, allocate memory of new newVar
	allDecls.push_back(decl);
	allDecls.push_back(
			builder.declarationStmt(newVar.as<VariablePtr>(),
					builder.undefinedVar(newVar.as<VariablePtr>()->getType())));

	// split up initialization expressions
	for (NamedTypePtr memberType : newStructType->getElements()) {
//std::cout << "\nnewVar: " << *newVar << "\nnewVarType: " << *newVar.as<ExpressionPtr>()->getType() << std::endl;
//std::cout << "\ninitType: " << *decl->getInitialization()->getType() << std::endl;
//builder.refMember(newVar, memberType->getName());
//removeRefVar(decl->getInitialization());

		NodeMap inInitReplacementsInCaseOfNovarInInit; // = generateTypeReplacements(oldStructType, getBaseType(memberType->getType(), memberType->getName()));
		inInitReplacementsInCaseOfNovarInInit[oldStructType] = getBaseType(
				memberType->getType(), memberType->getName());

		allDecls.push_back(
				builder.assign(
						builder.refMember(newVar.as<VariablePtr>(),
								memberType->getName()),
						updateInit(varReplacements,
								removeRefVar(decl->getInitialization()),
								inInitReplacementsInCaseOfNovarInInit,
								memberType->getName())));
//			allDecls.push_back(removeRefVar(decl->getInitialization()));
	}
//			} else { // keep old initialization and hope it is a variable which will be replaced by one with the same type as the new one
//				allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), updateInit(decl->getInitialization(),
//						getBaseType(oldVar->getType(), StringValuePtr()),
//						getBaseType(newVar->getType(), StringValuePtr()))));
//			}

	return allDecls;
//	CompoundStmtPtr cmpDecls = builder.compoundStmt(allDecls);
//	cmpDecls.addAnnotation<RemoveMeAnnotation>();
//
//	return cmpDecls;
}

StatementList AosToSoa::generateNewAssigns(
		const ExprAddressMap& varReplacements, const CallExprAddress& call,
		const StatementPtr& newVar, const StructTypePtr& newStructType,
		const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	allAssigns.push_back(call);

	for (NamedTypePtr memberType : newStructType->getElements()) {
		NodeMap inInitReplacementsInCaseOfNovarInInit; // = generateTypeReplacements(oldStructType, removeRefArray(memberType->getType()));
		inInitReplacementsInCaseOfNovarInInit[oldStructType] = getBaseType(memberType->getType());

		allAssigns.push_back(
				builder.assign(
						builder.refMember(newVar.as<ExpressionPtr>(),
								memberType->getName()),
						updateInit(varReplacements, call[1],
								inInitReplacementsInCaseOfNovarInInit,
								memberType->getName())));
	}

	return allAssigns;
}

StatementPtr AosToSoa::generateMarshalling(const ExpressionAddress& oldVar,
		const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for (NamedTypePtr memberType : structType->getElements()) {
		ExpressionPtr aosAccess = builder.deref(
				refAccess(oldVar,
						builder.castExpr(builder.getLangBasic().getUInt8(),
								iterator), memberType->getName()));
		ExpressionPtr soaAccess = refAccess(newVar,
				builder.castExpr(builder.getLangBasic().getUInt8(), iterator),
				memberType->getName());
		loopBody.push_back(builder.assign(soaAccess, aosAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end,
			builder.literal(start->getType(), "1"),
			builder.compoundStmt(loopBody));
}

StatementPtr AosToSoa::generateUnmarshalling(const ExpressionAddress& oldVar,
		const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for (NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = refAccess(oldVar,
				builder.castExpr(builder.getLangBasic().getUInt8(), iterator),
				memberType->getName());
		ExpressionPtr soaAccess = builder.deref(
				refAccess(newVar,
						builder.castExpr(builder.getLangBasic().getUInt8(),
								iterator), memberType->getName()));
		loopBody.push_back(builder.assign(aosAccess, soaAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end,
			builder.literal(start->getType(), "1"),
			builder.compoundStmt(loopBody));

}

ExpressionPtr AosToSoa::generateNewAccesses(const ExpressionAddress& oldVar,
		const StatementPtr& newVar, const StringValuePtr& member,
		const ExpressionPtr& index, const ExpressionPtr& structAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, structAccess,
			oldVar, newVar.as<ExpressionPtr>(), false).as<ExpressionPtr>();

	ExpressionPtr replacement = refAccess(newStructAccess, index, member);

	return replacement;
}

ExpressionPtr AosToSoa::generateByValueAccesses(const ExpressionPtr& oldVar,
		const StatementPtr& newVar, const StructTypePtr& newStructType,
		const ExpressionPtr& index, const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);

	ExpressionPtr newStructAccess = core::transform::fixTypes(mgr,
			oldStructAccess, oldVar, newVar.as<ExpressionPtr>(), false).as<
			ExpressionPtr>();

	vector<std::pair<StringValuePtr, ExpressionPtr>> values;
	for (NamedTypePtr memberType : newStructType->getElements()) {
		StringValuePtr memberName = memberType->getName();
		ExpressionPtr arrayAccess = valueAccess(newStructAccess, index,
				memberName);
		values.push_back(std::make_pair(memberName, arrayAccess));
	}

	return builder.structExpr(values);
}

StatementList AosToSoa::generateDel(const StatementAddress& stmt,
		const ExpressionAddress& oldVar, const ExpressionPtr& newVar,
		const StructTypePtr& newStructType) {
	StatementList deletes;

	pattern::TreePattern delVarPattern = pirp::refDelete(
			aT(pattern::var("oldVar", pattern::atom(oldVar))));
	pattern::AddressMatchOpt match = delVarPattern.matchAddress(stmt);

	if (match) {
		ExpressionAddress matchedOldVar = match.get()["oldVar"].getValue().as<
				ExpressionAddress>();
		if (!compareVariables(oldVar, matchedOldVar))
			return deletes;

		IRBuilder builder(mgr);
		for (NamedTypePtr memberType : newStructType->getElements()) {
			deletes.push_back(
					builder.refDelete(
							refAccess(newVar, ExpressionPtr(),
									memberType->getName())));
		}
		deletes.push_back(stmt);

	}

	return deletes;

}

const ExprAddressMap AosToSoa::replaceStructsInJobs(
		ExprAddressMap& varReplacements, const StructTypePtr& newStructType,
		const StructTypePtr& oldStructType, NodePtr& toTransform,
		const pattern::TreePattern& allocPattern,
		std::map<NodeAddress, NodePtr>& replacements) {

	ParSecSoa pss(toTransform, varReplacements, replacements, newStructType,
			oldStructType);
	pss.transform();
	return pss.getKernelVarReplacements();
}
} // datalayout
} // transform
} // insieme

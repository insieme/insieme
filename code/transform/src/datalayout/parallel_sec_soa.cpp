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

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/manipulation_utils.h"

#include "insieme/transform/datalayout/parallel_sec_soa.h"
#include "insieme/transform/datalayout/datalayout_utils.h"
#include "insieme/transform/datalayout/aos_argument_unfolder.h"


namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;

core::NodeMap ParSecSoa::generateTypeReplacements(const core::TypePtr& oldStructType, const core::TypePtr& newStructType) {
	IRBuilder builder(mgr);
	NodeMap tyReplace;
	tyReplace[builder.refType(builder.arrayType(oldStructType))] = newStructType;
	tyReplace[builder.refType(builder.arrayType(oldStructType), RK_SOURCE)] = newStructType; // TODO that for src types?

	return tyReplace;
}

ExpressionPtr ParSecSoa::updateInit(const ExprAddressMap& varReplacements, core::ExpressionAddress init, core::NodeMap& backupReplacements,
		core::StringValuePtr fieldName) {
	// check for marshalled variables in init expression
	std::pair<ExpressionAddress, std::map<StringValuePtr, VariablePtr>> varInInit;
	visitBreadthFirstInterruptible(init, [&](const ExpressionAddress& currVar) {
		auto check = fieldReplacements.find(getDeclaration(currVar));
		if(check != fieldReplacements.end()) {
			varInInit = *check;
			return true;
		}
		return false;
	});

	if(varInInit.first) { // variable to be replaced found
		// replace accesses to old variable with accesses to new (non-struct) variable
		ExpressionMap localReplacement;
		IRBuilder builder(mgr);
		localReplacement[varInInit.first] = varInInit.second[fieldName];

		ExpressionPtr newInit = core::transform::fixTypesGen(mgr, init.getAddressedNode(), localReplacement, true);

		return newInit;
	}

	// backup, works for e.g. memory allocation
	return core::transform::replaceAll(mgr, init, backupReplacements, core::transform::globalReplacement).as<ExpressionPtr>();
}

StatementList ParSecSoa::generateNewDecl(const ExprAddressMap& varReplacements, const DeclarationStmtAddress& decl, const StatementPtr& newVars,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration of the new soa variables
	StatementList allDecls;

	// split up initialization expressions
	for(std::pair<StringValuePtr, VariablePtr> member : fieldReplacements[decl->getVariable()]) {

		NodeMap inInitReplacementsInCaseOfNovarInInit = generateTypeReplacements(oldStructType, getBaseType(member.second->getType(), member.first));
//		inInitReplacementsInCaseOfNovarInInit[oldStructType] = getBaseType(member.second->getType(), member.first);
//
//			allDecls.push_back(builder.assign(builder.accessMember(newVar, memberType->getName()),
//					updateInit(varReplacements, removeRefVar(decl->getInitialization()), inInitReplacementsInCaseOfNovarInInit, memberType->getName())));

		VariablePtr newVar = member.second.as<VariablePtr>();

		allDecls.push_back(builder.declarationStmt(newVar,
				updateInit(varReplacements, (decl->getInitialization()), inInitReplacementsInCaseOfNovarInInit, member.first)));
	}

	return allDecls;
}

StatementList ParSecSoa::generateNewAssigns(const ExprAddressMap& varReplacements, const CallExprAddress& call,
		const StatementPtr& newVar, const StructTypePtr& newStructType, const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	StatementList newVars = newVar.as<CompoundStmtPtr>()->getStatements();

	unsigned i = 0;
	for(NamedTypePtr memberType : newStructType->getElements()) {
		NodeMap inInitReplacementsInCaseOfNovarInInit = generateTypeReplacements(oldStructType, memberType->getType());
//		inInitReplacementsInCaseOfNovarInInit[oldStructType] = removeRefArray(memberType->getType());

		allAssigns.push_back(builder.assign(newVars[i++].as<ExpressionPtr>(),
				updateInit(varReplacements, call[1], inInitReplacementsInCaseOfNovarInInit, memberType->getName())));
	}
//	std::cout << "new assigns: \n";
//	dumpPretty(builder.compoundStmt(allAssigns));

	return allAssigns;
}
/*
void ParSecSoa::replaceAssignments(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, ExpressionMap& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const CompoundStmtPtr& newVars = vr.second.isa<CompoundStmtPtr>();

		if(newVars) visitDepthFirst(toTransform, [&](const CallExprAddress& call) {

			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {

				if(oldVar == getDeclaration(call[0])) {
					// check if the assignment does an allocation and try to extract the number of elements in that case
					pattern::MatchOpt match = allocPattern.matchPointer(call[1]);

					StatementList allAssigns = generateNewAssigns(varReplacements, call, newVars, newStructType, oldStructType);

					CompoundStmtPtr cmpAssigns = builder.compoundStmt(allAssigns);
std::cout << "new assigns: \n";
dumpPretty(call);
assert_fail();
					cmpAssigns.addAnnotation<RemoveMeAnnotation>();
					addToReplacements(replacements, call, cmpAssigns);
				}
			}
		});
	}
}
*/
core::ExpressionPtr ParSecSoa::generateNewAccesses(const ExpressionAddress& oldVar, const StatementPtr& newVar, const StringValuePtr& member,
		const ExpressionPtr& index, const ExpressionPtr& structAccess) {
	IRBuilder builder(mgr);

	// replace struct access with access to new variable, representing one field only
	ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, structAccess,
			oldVar, fieldReplacements[oldVar][member], false).as<ExpressionPtr>();
	ExpressionPtr replacement = refAccess(newStructAccess, index, StringValuePtr());
	return replacement;
}

void ParSecSoa::transform() {
	NodeManager& m = mgr;
	IRBuilder builder(m);
	NodeAddress tta(toTransform);

	std::vector<std::pair<ExprAddressSet, StructTypePtr>> toReplaceLists = createCandidateLists(tta);

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(m.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));
	VariableMap<std::map<LiteralPtr, ExpressionPtr>> map;

	pattern::TreePattern refTypePattern = pattern::aT(var("toBeReplaced", pirp::refType(pirp::arrayType(pattern::atom(oldStructType)))));

	for(std::pair<ExprAddressSet, StructTypePtr> toReplaceList : toReplaceLists) {
		std::map<StatementPtr, ExpressionPtr> nElems;

		std::vector<NamedTypePtr> newStructElemsTypes = newStructType->getElements();

		for(ExpressionAddress oldVar : toReplaceList.first) {
//std::cout << "NT: " << newStructType << " var " << oldVar << " " << *oldVar << std::endl;
			StatementList newVars;

			NodePtr oldTypeToBeReplaced;

			pattern::MatchOpt typeToBeReplacedMatch = refTypePattern.matchPointer(oldVar->getType());
			if(typeToBeReplacedMatch){
				oldTypeToBeReplaced = typeToBeReplacedMatch.get()["toBeReplaced"].getValue();
			}
			assert_true(oldTypeToBeReplaced) << " could not identify the type to be replaced";

			std::map<StringValuePtr, VariablePtr> fieldMap;
			for(NamedTypePtr elemTy : newStructType->getElements()) {
				TypePtr newType = core::transform::replaceAll(m, oldVar->getType(), oldTypeToBeReplaced, elemTy->getType()).as<TypePtr>();
				fieldMap[elemTy->getName()] = builder.variable(newType);
				newVars.push_back(fieldMap[elemTy->getName()]);
//std::cout << "NT: " << newType << " " << newVars.back() << " var " << *oldVar->getType() << " " << *oldVar << std::endl;
			}
//std::cout << "heyho " << oldVar << " " << *oldVar <<  "\n";
			// note, there are no global variables to be handled here
			varReplacements[oldVar] = builder.compoundStmt(newVars);
			fieldReplacements[oldVar] = fieldMap;

//			// create new variables, local or global
//			varReplacements[oldVar] = globalVar ?
//					builder.literal(globalVar->getStringValue() + "_soa", newType).as<ExpressionPtr>() :
//					builder.variable(newType).as<ExpressionPtr>();
		}

//for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
//	std::cout << *vr.first << " -> " << *vr.second << std::endl;
//	if(CompoundStmtPtr c = vr.second.isa<CompoundStmtPtr>())
//		for(StatementPtr s : c)
//			std::cout << s.as<ExpressionPtr>()->getType() << " " << *s << std::endl;
//}
		AosArgumentUnfolder ar(mgr, varReplacements, varsToPropagate, replacements, oldStructType, newStructType);

//		tta = ar.addVariablesToLambdas(toTransform);
		tta = ar.mapPtr(toTransform);

		// the argument replacer changed the transform -> all previously stored addresses are invalid. Trying to update them.
		mapKeySwitchRoot(varReplacements, tta);
		mapKeySwitchRoot(fieldReplacements, tta);

//for(std::pair<ExpressionAddress, StatementPtr> fr : varReplacements) {
//	std::cout << "there: " << *fr.first << " " << *fr.second << std::endl;
//}
//assert_fail();

		// replacing the declarations of the old variables with new ones
		addNewDecls(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		const std::vector<core::StatementAddress> begin, end;

		// assignments to the entire struct should be ported to the new struct members
		replaceAssignments(varReplacements, newStructType, oldStructType, tta, pattern::TreePattern(), nElems, replacements);
//std::cout << "\n\n-----------------------------------------------------------------------------\n\n";
		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements, true);

		std::map<core::NodeAddress, core::NodePtr> copy;
		for(std::pair<core::NodeAddress, core::NodePtr> r : replacements) {
//			std::cout << "\nfrom ";
//			dumpPretty(r.first);
//			std::cout << "to ";
//			dumpPretty(r.second);
			copy[r.first.switchRoot(toTransform)] = r.second;
		}

		replacements.clear();
		for(std::pair<core::NodeAddress, core::NodePtr> r : copy) {
			replacements[r.first] = r.second;
		}
	}

}

} // datalayout
} // transform
} // insieme

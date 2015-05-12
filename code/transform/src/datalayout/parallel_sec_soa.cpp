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
	return core::transform::replaceAll(mgr, init, backupReplacements, false).as<ExpressionPtr>();
}

StatementList ParSecSoa::generateNewDecl(const ExprAddressMap& varReplacements, const DeclarationStmtAddress& decl, const StatementPtr& newVars,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration of the new soa variables
	StatementList allDecls;

	// split up initialization expressions
	for(std::pair<StringValuePtr, VariablePtr> member : fieldReplacements[decl->getVariable()]) {

		NodeMap inInitReplacementsInCaseOfNovarInInit;
		inInitReplacementsInCaseOfNovarInInit[oldStructType] = getBaseType(member.second->getType(), member.first);
//
//			allDecls.push_back(builder.assign(builder.accessMember(newVar, memberType->getName()),
//					updateInit(varReplacements, removeRefVar(decl->getInitialization()), inInitReplacementsInCaseOfNovarInInit, memberType->getName())));

		VariablePtr newVar = member.second.as<VariablePtr>();

		allDecls.push_back(builder.declarationStmt(newVar, //builder.undefinedVar(newVar->getType())));
				updateInit(varReplacements, removeRefVar(decl->getInitialization()), inInitReplacementsInCaseOfNovarInInit, member.first)));
	}

	return allDecls;
}

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
dumpPretty(cmpAssigns);
assert_fail();
					cmpAssigns.addAnnotation<RemoveMeAnnotation>();
					addToReplacements(replacements, call, cmpAssigns);
				}
			}
		});
	}
}

core::ExpressionPtr ParSecSoa::generateNewAccesses(const ExpressionAddress& oldVar, const StatementPtr& newVar, const StringValuePtr& member,
		const ExpressionPtr& index, const ExpressionPtr& structAccess) {
	IRBuilder builder(mgr);

	CompoundStmtPtr c = (varReplacements[oldVar]).as<CompoundStmtPtr>();
	std::cout << c[0].as<ExpressionPtr>()->getType() << " " << c[0] << std::endl;

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

	std::vector<std::pair<ExprAddressSet, RefTypePtr>> toReplaceLists = createCandidateLists(tta);

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(m.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));
	VariableMap<std::map<LiteralPtr, ExpressionPtr>> map;

	for(std::pair<ExprAddressSet, RefTypePtr> toReplaceList : toReplaceLists) {
		ExpressionMap nElems;

		std::vector<NamedTypePtr> newStructElemsTypes;

		for(NamedTypePtr elemTy : newStructType->getElements()) {
			newStructElemsTypes.push_back(elemTy);
		}

		for(ExpressionAddress oldVar : toReplaceList.first) {
//std::cout << "NT: " << newStructType << " var " << oldVar << " " << *oldVar << std::endl;
			TypePtr newType;
			StatementList newVars;
			std::map<StringValuePtr, VariablePtr> fieldMap;
			for(NamedTypePtr elemTy : newStructType->getElements()) {
				newType = core::transform::replaceAll(m, oldVar->getType(), builder.refType(builder.arrayType(oldStructType)), elemTy->getType()).as<TypePtr>();
				fieldMap[elemTy->getName()] = builder.variable(newType);
				newVars.push_back(fieldMap[elemTy->getName()]);
//std::cout << "NT: " << newType << " " << newVars.back() << " var " << *oldVar->getType() << " " << *oldVar << std::endl;
			}
//std::cout << "heyho " << oldVar << " " << *oldVar << "\n";
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
		//replace array accesses
//		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

		// assignments to the entire struct should be ported to the new struct members
//		replaceAssignments(varReplacements, newStructType, oldStructType, tta, pattern::TreePattern(), nElems, replacements);
//std::cout << "\n\n-----------------------------------------------------------------------------\n\n";
		//replace array accesses
		replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

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


ExpressionList ArgumentReplacer::updateArguments(const ExpressionAddress& oldArg) {
	ExpressionList newArgs;
	pattern::AddressMatchOpt match = varWithOptionalDeref.matchAddress(oldArg);

	if(match) {
		ExpressionAddress oldVarDecl = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());

		auto varCheck = varsToReplace.find(oldVarDecl);
		if(varCheck != varsToReplace.end()) {
			ExpressionAddress oldVar = varCheck->first;
			CompoundStmtPtr newVars = varCheck->second.as<CompoundStmtPtr>();

			for(StatementPtr newVar : newVars->getStatements()) {
				newArgs.push_back(core::transform::fixTypesGen(mgr, oldArg.getAddressedNode(), oldVar, newVar.as<ExpressionPtr>(), false));
			}
		}
		return newArgs;
	}

	// "unfold" the tuple member access to get the individual pointers
	match = tupleMemberAccess.matchAddress(oldArg);

	if(match) {
		ExpressionAddress oldVarRoot = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());
		std::cout << "oldDecl: " << oldVarRoot << " " << *oldVarRoot << std::endl;
assert_fail();
		auto varCheck = varsToPropagate.find(oldVarRoot);
		if(varCheck != varsToPropagate.end()) {
			IRBuilder builder(mgr);
			ExpressionAddress oldVar = varCheck->first;
			ExpressionPtr newVar = varCheck->second.as<ExpressionPtr>();

std::cout << "oldVar: " << oldVar << " " << *oldVar << std::endl;
std::cout << "newVar: " << newVar->getType() << " " << *newVar << std::endl;
			ExpressionPtr tupleIndex = match.get()["tupleIndex"].getValue().as<LiteralPtr>();
			ExpressionPtr arrayIndex = match.get()["arrayIndex"].getValue().as<ExpressionPtr>();
			for(NamedTypePtr newType : newStructType) {

				ExpressionPtr accessTuple = builder.accessComponent(newVar, tupleIndex);
				newArgs.push_back(builder.deref(builder.accessMember(builder.arrayRefElem(accessTuple, arrayIndex), newType->getName())));
			}
		}
	}

	return newArgs;
}

ArgumentReplacer::ArgumentReplacer(core::NodeManager& mgr, ExprAddressMap& varReplacements, const ExprAddressMap& varsToPropagate,
		const StructTypePtr& newStructType)
	: VariableAdder(mgr, varReplacements), varsToPropagate(varsToPropagate), newStructType(newStructType),
	  tupleMemberAccess(pirp::refDeref(pirp::arrayRefElem1D(
			pirp::tupleMemberAccess(namedVariablePattern, var("tupleIndex", pattern::any), pattern::any), var("arrayIndex", pattern::any)))) {}

core::NodePtr ArgumentReplacer::generateNewCall(const CallExprAddress& oldCall, const StatementPtr& newVars, const int argIdx) {
	const LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().as<LambdaExprAddress>();

	ExpressionAddress oldArg = oldCall->getArgument(argIdx);

	// update to new variable which will be added to argument list;
	ExpressionList newArgs = updateArguments(oldArg);

	assert(!newArgs.empty() && "Parameter to be replaced doesn't have replaceable argument.");

	std::vector<ExpressionPtr> args(oldCall.getAddressedNode()->getArguments());
	// add new arguments and parameters to corresponding list
	VariableList params = lambdaExpr.getAddressedNode()->getParameterList();

	NodeMap ptrRplc;
	for(std::pair<ExpressionAddress, StatementPtr> a : varsToPropagate) // maybe use replacements?
		ptrRplc[a.first.getAddressedNode()] = a.second;

	for(ExpressionPtr& arg : args)
		arg = core::transform::replaceAllGen(mgr, arg, ptrRplc);
	for(const ExpressionPtr& newArg : newArgs)
		args.push_back(newArg);

	IRBuilder builder(mgr);

	// update function type
	FunctionTypePtr lambdaType = lambdaExpr->getType().as<FunctionTypePtr>();
	std::vector<TypePtr> funTyMembers = lambdaType->getParameterTypeList();

	for(StatementPtr newVar : newVars.as<CompoundStmtPtr>()->getStatements()) {
		params.push_back(newVar.as<VariablePtr>());
		funTyMembers.push_back(newVar.as<VariablePtr>()->getType());
	}
	FunctionTypePtr newFunType = builder.functionType(funTyMembers, lambdaType->getReturnType(), lambdaType->getKind());

	LambdaExprPtr newLambdaExpr = builder.lambdaExpr(newFunType, params, lambdaExpr.getAddressedNode()->getBody());

	CallExprPtr newCall = builder.callExpr(oldCall->getType(), newLambdaExpr, args);

	//migrate possible annotations
	core::transform::utils::migrateAnnotations(lambdaExpr.getAddressedNode(), newLambdaExpr);
	core::transform::utils::migrateAnnotations(oldCall.getAddressedNode(), newCall);

	return newCall;
}


} // datalayout
} // transform
} // insieme

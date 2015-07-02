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
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/address_mapper.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/transform/datalayout/datalayout_transform.h"
#include "insieme/transform/datalayout/datalayout_utils.h"

#include "insieme/annotations/data_annotations.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

namespace {
class StuffReplacer : public core::transform::AddressMapping {
	const ExprAddressMap& kernelVarReplacements;
	const std::map<core::NodeAddress, core::NodePtr>& replacements;

public:
	StuffReplacer(const ExprAddressMap& kernelVarReplacements, const std::map<core::NodeAddress, core::NodePtr>& replacements) :
		kernelVarReplacements(kernelVarReplacements), replacements(replacements) {}

	virtual const core::NodePtr mapAddress(const core::NodePtr& ptr, core::NodeAddress& prevAddr) {
		if(CallExprAddress oldCall = prevAddr.isa<CallExprAddress>()) {
			NodeManager& mgr = ptr->getNodeManager();

			auto replaceMe = replacements.find(prevAddr);
			if(replaceMe != replacements.end()) {

				if(CallExprPtr callFromReplacements = replaceMe->second.isa<CallExprPtr>()) {
					LambdaExprPtr lambdaExprFromReplacements = callFromReplacements->getFunctionExpr().isa<LambdaExprPtr>();
					LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().isa<LambdaExprAddress>();

					if(lambdaExpr && !mgr.getLangBasic().isBuiltIn(lambdaExpr)) {
						NodeAddress buf = lambdaExpr->getBody().as<NodeAddress>();

						CompoundStmtPtr body = lambdaExprFromReplacements->getBody().as<NodePtr>()->substitute(mgr, *this, buf).as<CompoundStmtPtr>();

						IRBuilder builder(mgr);
						LambdaExprPtr newLambdaExpr = builder.lambdaExpr(lambdaExprFromReplacements->getType().as<FunctionTypePtr>(),
								lambdaExprFromReplacements->getParameterList(), body);

						return builder.callExpr(callFromReplacements->getType(), newLambdaExpr, callFromReplacements->getArguments());
					}
				}

			}
		}

		if(ExpressionAddress expr = prevAddr.isa<ExpressionAddress>()) {
			auto replaceMe = kernelVarReplacements.find(getDeclaration(expr));
			if(replaceMe != kernelVarReplacements.end())
				if(replaceMe->second.isa<ExpressionPtr>()) // do not replace Statements with Expressions
					return replaceMe->second;
		}

		auto replaceMe = replacements.find(prevAddr);
		if(replaceMe != replacements.end()) {
			return replaceMe->second;//->substitute(ptr->getNodeManager(), *this, prevAddr);
		}

		return ptr->substitute(ptr->getNodeManager(), *this, prevAddr);
	}
};
}

const string RemoveMeAnnotation::NAME = "RemoveMeAnnotation";
const utils::StringKey<RemoveMeAnnotation> RemoveMeAnnotation::KEY("RemoveMe");

bool VariableComparator::operator()(const ExpressionAddress& a, const ExpressionAddress& b) const {
	core::LiteralAddress la = a.isa<core::LiteralAddress>();
	core::LiteralAddress lb = b.isa<core::LiteralAddress>();

	if(la && !lb)
		return true;

	if(!la && lb)
		return false;

	if(la && lb)
		return la->getStringValue() < lb->getStringValue();

	return a < b;
}

template<typename T>
bool contains(std::vector<core::Address<T>> vec, const core::Address<T>& val) {
	for(core::Address<T> elem : vec) {
		if(elem == val)
			return true;
//		if(val.getDepth() > elem.getDepth()) {
//			if(val.getParentAddress(val.getDepth() - elem.getDepth()) == elem) {
//				return true;
//			}
//		}
	}
	return false;
}

template<typename T>
ExpressionPtr expressionContainsMarshallingCandidate(const VariableMap<T>& candidates, const ExpressionAddress& expr,
		const NodeAddress& scope) {
		ExpressionAddress initVar = getRootVariable(scope, expr).as<ExpressionAddress>();
		if(initVar) for(std::pair<ExpressionAddress, T> candidate : candidates) {
			if(*initVar == *candidate.first)
			return candidate.first;
		}

	return ExpressionPtr();
}

ExprAddressArrayTypeMap findAllSuited(const NodeAddress& toTransform) {
	ExprAddressArrayTypeMap structs;

	pattern::TreePattern structType = pirp::refType(pattern::aT(var("structType", pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))));

	pattern::TreePattern structVar = var("structVar", pirp::variable(structType));
	pattern::TreePattern structVarDecl = pirp::declarationStmt(structVar, var("init", pattern::any));

	pattern::TreePattern tupleType = pattern::aT(var("tupleType", pirp::tupleType(*pattern::any)));

	pattern::TreePattern globalStruct = var("structVar", pirp::literal(structType, pattern::any));

	pirp::matchAllPairs(structVarDecl | globalStruct, toTransform, [&](const NodeAddress& match, pattern::AddressMatch nm) {
//std::cout << "Checking: " << *nm["structVar"].getValue() << std::endl;

		// check if marshalling is needed. It is not needed e.g. when the variable is initialized with an already marshalled one
		if(nm.isVarBound("init") && expressionContainsMarshallingCandidate(structs, nm["init"].getValue().as<ExpressionAddress>(), toTransform))
			return;

		ExpressionAddress structVar = (match >> nm["structVar"].getValue()).as<ExpressionAddress>();
		TypePtr varType = structVar->getType();
		ArrayTypePtr structType = nm["structType"].getValue().as<RefTypePtr>().getElementType().as<ArrayTypePtr>(); // TODO make that prettier
//std::cout << "\nadding " << structVar << " " << *structVar << std::endl;

		if(tupleType.match(varType)) {
			return; // tuples are not candidates since only one field needs to be altered.
					// They will only be changed if the field is an alias to some other candidate
		}

		if(isInsideJob(match)) {
			return; // variable in parallel regions are replaced with new variables of updated type. We don't keep the old version inside parallel regions.
		}

		// we are already at a declaration in any case
		structs[structVar] = structType;
//std::cout << "Adding: " << *structVar << " as a candidate" << std::endl;
	});
	return structs;
}

ExprAddressArrayTypeMap findPragma(const NodeAddress& toTransform) {
	ExprAddressArrayTypeMap structs;

	pattern::TreePattern structTypePattern = pirp::refType(pattern::aT(var("structType", pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))));
	core::visitDepthFirst(toTransform, [&](const core::DeclarationStmtAddress& decl) {
		if(decl->hasAnnotation(annotations::DataTransformAnnotation::KEY)) {
			annotations::DataTransformAnnotationPtr dta = decl->getAnnotation(annotations::DataTransformAnnotation::KEY);

			if(dta->isSoa()) {
				VariableAddress structVar = decl->getVariable();
				TypePtr varType = structVar->getType();

				pattern::MatchOpt match = structTypePattern.matchPointer(varType);
				assert_true(match) << "Pragma-marked variable does not have valid struct type";

				ArrayTypePtr structType = match.get()["structType"].getValue().as<RefTypePtr>()->getElementType().as<ArrayTypePtr>(); // TODO make me pretty

				// we are already at a declaration in any case
				structs[structVar] = structType;
			}
		}
	});

	return structs;
}

void DatalayoutTransformer::addToReplacements(std::map<core::NodeAddress, core::NodePtr>& replacements, const core::NodeAddress& toReplace,
		const core::NodePtr& replacement) {
	assert(*toTransform == *toReplace.getRootNode()); // make sure the correct base address is used
	replacements[toReplace] = replacement;
}

DatalayoutTransformer::DatalayoutTransformer(core::NodePtr& toTransform, CandidateFinder candidateFinder)
		: mgr(toTransform->getNodeManager()), toTransform(toTransform), candidateFinder(candidateFinder) {}

void DatalayoutTransformer::collectVariables(const std::pair<ExpressionAddress, ArrayTypePtr>& transformRoot, ExprAddressSet& toReplaceList,
		const NodeAddress& toTransform) {

	ArrayTypePtr structType = transformRoot.second;
	visitDepthFirst(toTransform, [&](const StatementAddress& stmt) {
		if(const CallExprAddress call = stmt.isa<CallExprAddress>()) {
			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
				ExpressionAddress lhs = removeMemLocationCreators(call[0]);
				ExpressionAddress rhs = removeMemLocationCreators(call[1]);
				ExpressionAddress lhsVar = getDeclaration(extractNonTupleVariable(lhs));
				if(!(lhsVar && isRefStruct(lhsVar, structType))) // lhs is not based on a variable
					return;
				ExpressionAddress rhsVar = getDeclaration(extractNonTupleVariable(rhs));
				if(!rhsVar || !isRefStruct(rhsVar, structType)) // rhs is not based on a variable
					return;

				if(toReplaceList.find(lhsVar) != toReplaceList.end()) { // lhsVar is already selected for replacement
					if(rhs->getType().isa<RefTypeAddress>()) {
//std::cout << "found lhs " << lhsVar << " " << *lhsVar << " adds " << *rhsVar << std::endl;
						toReplaceList.insert(rhsVar);
						return;
					}
				}
				if(toReplaceList.find(rhsVar) != toReplaceList.end()) { // rhsVar is already selected for replacement
					if(rhs->getType().isa<RefTypeAddress>()){
//std::cout << "found Rhs " << rhsVar << " " << *rhsVar << " adds " << *lhsVar << std::endl;
						toReplaceList.insert(lhsVar);
						return;
					}
				}
			}

			ExpressionAddress fun = call->getFunctionExpr();
			if(mgr.getLangBasic().isBuiltIn(fun))
				return;

			if(LambdaExprAddress lambda = fun.isa<LambdaExprAddress>()) {
				// go through argument list and check if one of the arguments is to be replaced
				for_range(make_paired_range(call->getArguments(), lambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
					ExpressionAddress argVar = getDeclaration(extractNonTupleVariable(pair.first));

					if(!argVar)
						return;

					VariableAddress param = pair.second;
					if(!isRefStruct(param, structType))
						return;

					if(toReplaceList.find(argVar) != toReplaceList.end()) {
						toReplaceList.insert(param);
					}
				});
			}
		}

		if(const DeclarationStmtAddress decl = stmt.isa<DeclarationStmtAddress>()) {
			ExpressionAddress init = removeMemLocationCreators(decl->getInitialization());
			VariableAddress var = decl->getVariable();

			ExpressionAddress initVar = getDeclaration(extractNonTupleVariable(init));
			if(!initVar)
				return;

			if(toReplaceList.find(initVar) != toReplaceList.end()) { // the initialization is already selected for replacement
					if(isRefStruct(init, structType)) {
						toReplaceList.insert(var);
						return;
					}
				}
		}
	});

//for(ExpressionAddress addr : toReplaceList)
//	std::cout << "from " << addr << "  " << *addr << std::endl;
}

NodeMap DatalayoutTransformer::generateTypeReplacements(TypePtr oldStructType, TypePtr newStructType) {
	NodeMap tyReplace;
	tyReplace[(oldStructType)] = newStructType;

	return tyReplace;
}

ExpressionPtr DatalayoutTransformer::updateAccess(const ExpressionPtr& oldAccess, const std::pair<ExpressionAddress, StatementPtr>& varInInit,
		const StringValuePtr& fieldName) {
	// replace accesses to old variable with accesses to field in new variable
	IRBuilder builder(mgr);
	ExpressionMap localReplacement;
	localReplacement[varInInit.first] = fieldName ?
			builder.accessMember(varInInit.second.as<ExpressionPtr>(), fieldName) : varInInit.second.as<ExpressionPtr>();

	return core::transform::fixTypesGen(mgr, oldAccess, localReplacement, true);
}

ExpressionPtr DatalayoutTransformer::updateInit(const ExprAddressMap& varReplacements, ExpressionAddress init, NodeMap& backupReplacements,
		StringValuePtr fieldName) {
	// check for marshalled variables in init expression
	std::pair<ExpressionAddress, StatementPtr> varInInit;
	visitBreadthFirstInterruptible(init, [&](const ExpressionAddress& currVar) {
		auto check = varReplacements.find(getDeclaration(currVar));
		if(check != varReplacements.end()) {
			varInInit = *check;
			return true;
		}
		return false;
	});

	if(varInInit.first) { // variable to be replaced found

		return updateAccess(init.getAddressedNode(), varInInit, fieldName);
	}

	// backup, works for e.g. memory allocation
	return core::transform::replaceAll(mgr, init, backupReplacements, core::transform::globalReplacement).as<ExpressionPtr>();
}

std::vector<std::pair<ExprAddressSet, ArrayTypePtr>> DatalayoutTransformer::createCandidateLists(const core::NodeAddress& toTransform) {
	ExprAddressArrayTypeMap structs = findCandidates(toTransform);
	std::vector<std::pair<ExprAddressSet, ArrayTypePtr>> toReplaceLists;

	for(std::pair<ExpressionAddress, ArrayTypePtr> candidate : structs) {
		ExprAddressSet toReplaceList;
		toReplaceList.insert(candidate.first);
//std::cout << "inserted " << candidate.first << " " << *candidate.first << std::endl;
		// fixpoint iteration to capture all variables that new a new versions with new type
		size_t curNumOfVars = 0;
		while(curNumOfVars != toReplaceList.size()) {
//std::cout << "curNumOfVars: " << curNumOfVars << " != " << toReplaceList.size() << std::endl;
			curNumOfVars = toReplaceList.size();
			collectVariables(candidate, toReplaceList, toTransform);
		}
		toReplaceLists.push_back(std::make_pair(toReplaceList, candidate.second));

	}

	toReplaceLists = mergeLists(toReplaceLists);

//	for(std::pair<ExprAddressSet, RefTypePtr> toReplaceList : toReplaceLists) {
//		std::cout << "\nList: \n";
//		for(ExpressionAddress tr : toReplaceList.first)
//			std::cout << tr << " " << *tr << std::endl;
//	}

	return toReplaceLists;
}

ExprAddressArrayTypeMap DatalayoutTransformer::findCandidates(const NodeAddress& toTransform) {
	return candidateFinder(toTransform);
}


std::vector<std::pair<ExprAddressSet, ArrayTypePtr>> DatalayoutTransformer::mergeLists(std::vector<std::pair<ExprAddressSet,
		ArrayTypePtr>>& toReplaceLists) {
	std::vector<std::pair<ExprAddressSet, ArrayTypePtr>> newLists;
	for(std::pair<ExprAddressSet, ArrayTypePtr>& toReplaceList : toReplaceLists) {
		bool addToOld = false;
		// merge lists of same type
		for(std::pair<ExprAddressSet, ArrayTypePtr>& newList : newLists) {
			if(toReplaceList.second == newList.second) {
				for(ExpressionAddress toInsert : toReplaceList.first)
					newList.first.insert(toInsert);
				addToOld = true;
			}
		}
		if(!addToOld) {
			newLists.push_back(toReplaceList);
		}
	}

	return newLists;
}

void DatalayoutTransformer::addNewDecls(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, std::map<StatementPtr, ExpressionPtr>& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	visitDepthFirst(toTransform, [&](const DeclarationStmtAddress& decl) {
		const VariableAddress& oldVar = decl->getVariable();

		auto newVarIter = varReplacements.find(oldVar);

		if(newVarIter != varReplacements.end()) {
			StatementPtr newVar = newVarIter->second;

			// check if the declaration does an allocation and try to extract the number of elements in that case
			pattern::MatchOpt match = allocPattern.matchPointer(decl->getInitialization());
			ExpressionPtr nElem;
			if(match) {
				nElem = match.get()["nElems"].getValue().as<ExpressionPtr>();
				nElems[newVar] = nElem;
			}

			IRBuilder builder(mgr);
			StatementList allDecls = generateNewDecl(varReplacements, decl, newVar, newStructType, oldStructType, nElem);
			CompoundStmtPtr cmpDecls = builder.compoundStmt(allDecls);
			cmpDecls.addAnnotation<RemoveMeAnnotation>();

			addToReplacements(replacements, decl, cmpDecls);
		}
	});
}

void DatalayoutTransformer::replaceAssignments(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, std::map<StatementPtr, ExpressionPtr>& nElems,
		std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const StatementPtr& newVar = vr.second;

		visitDepthFirst(toTransform, [&](const CallExprAddress& call) {

			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
				if(oldVar == getDeclaration(call[0])) {
					// check if the assignment does an allocation and try to extract the number of elements in that case
					pattern::MatchOpt match = allocPattern.matchPointer(call[1]);
					ExpressionPtr nElem;
					if(match && match.get().isVarBound("nElems")) {
						nElem = match.get()["nElems"].getValue().as<ExpressionPtr>();
						nElems[newVar] = nElem;
					}

					StatementList allAssigns = generateNewAssigns(varReplacements, call, newVar, newStructType, oldStructType, nElem);

					CompoundStmtPtr cmpAssigns = builder.compoundStmt(allAssigns);
					cmpAssigns.addAnnotation<RemoveMeAnnotation>();
					addToReplacements(replacements, call, cmpAssigns);
	//				newStuff = core::transform::insertAfter(mgr, call, allAssigns);
				}
			}
		});
	}
}

ExpressionPtr DatalayoutTransformer::determineNumberOfElements(const StatementPtr& newVar,const std::map<StatementPtr, ExpressionPtr>&  nElems) {
	ExpressionPtr numElements;

	auto checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable = nElems.find(newVar.isa<ExpressionPtr>());

	if(checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable != nElems.end())
		numElements = checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable->second;

	// backup, simply take the first which is there and hope for the best
	if(!numElements && !nElems.empty()) numElements = nElems.begin()->second;

	assert_true(numElements) << "Cannot determine number of elements for (un)marshalling";

	return numElements;
}

std::vector<StatementAddress> DatalayoutTransformer::addMarshalling(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, std::map<StatementPtr, ExpressionPtr>& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> marshalled;

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second.as<ExpressionPtr>();

		pattern::TreePattern aosVariable = pattern::var("oldVar", pattern::atom(oldVar));//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern assignToStruct = pirp::assignment(pattern::var("struct", pirp::callExpr(pirp::refType(pirp::structType(*pattern::any)),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)), pattern::any);
		pattern::TreePattern assignToStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << assignToStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				var("oldVarCandidate", pirp::exprOfType(pattern::aT(/*pirp::refType*/(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))))))
//				pattern::node(*pattern::any << pattern::single(pattern::atom(oldVar)))
//				pattern::aT(pattern::atom(oldVar))
				<< *pattern::any);
		pattern::TreePattern oldVarPattern = pattern::aT(pattern::var("oldVar", pattern::atom(oldVar)));

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> oldToNew : newMemberAccesses) {
	//		generateMarshalling(oldToNew.first.as<VariablePtr>(), oldToNew.second.first, builder.intLit(0), builder.intLit(100), oldToNew.second.second);
	//	}
		pirp::matchAllPairs(assignToStructInLoop | externalAosCall, NodeAddress(toTransform), [&](const NodeAddress& match, pattern::AddressMatch am) {
			ExpressionPtr start, end;
			if(am.isVarBound("start")) { // assignToStructInLoop
				ExpressionAddress matchedOldVar = match >> am["oldVar"].getValue().as<ExpressionAddress>();
				if(!compareVariables(oldVar, matchedOldVar))
					return;

				start = am["start"].getValue().as<ExpressionPtr>();
				end = am["end"].getValue().as<ExpressionPtr>();
			} else { // externalAosCall
				CallExprAddress call = match.as<CallExprAddress>();
				// filter out builtins
				if(call->getNodeManager().getLangBasic().isBuiltIn(call->getFunctionExpr()))
					return;

				// check if oldVar is accessed
				pattern::AddressMatchOpt oldVarInLoop = oldVarPattern.matchAddress(am["oldVarCandidate"].getValue());
				if(!oldVarInLoop)
					return;
				ExpressionAddress matchedOldVar = match >> oldVarInLoop.get()["oldVar"].getValue().as<ExpressionAddress>();
				if(!compareVariables(oldVar, matchedOldVar))
					return;


//for(std::pair<ExpressionPtr, ExpressionPtr> e : nElems) {
//	std::cout << "from: " << e.first << " to " << e.second << std::endl;
//}
//std::cout << "looking for: " << oldVar << std::endl;

				ExpressionPtr numElements = determineNumberOfElements(newVar, nElems);

				start = builder.literal(numElements->getType(), "0");
				end = numElements;
			}
//std::cout << "oldVAr " << oldVar << std::endl;

			StatementPtr marshalling = generateMarshalling(oldVar, newVar, start, end, newStructType);


			StatementAddress toBeReplaced = getStatementReplacableParent(match);

			StatementList initAndMarshall;
			initAndMarshall.push_back(toBeReplaced);
			initAndMarshall.push_back(marshalling);

			CompoundStmtPtr cmpInitAndMarshall = builder.compoundStmt(initAndMarshall);
			cmpInitAndMarshall.addAnnotation<RemoveMeAnnotation>();
			addToReplacements(replacements, toBeReplaced, cmpInitAndMarshall);

			// save the address where the marshalling was inserted
			marshalled.push_back(match.as<StatementAddress>());
		});
	}

	return marshalled;
}

std::vector<StatementAddress> DatalayoutTransformer::addUnmarshalling(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const std::vector<StatementAddress>& beginV, std::map<StatementPtr, ExpressionPtr>& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> unmarshallingPoints;
	StatementAddress begin = beginV.empty() ? StatementAddress() : beginV.front();

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second.as<ExpressionPtr>();

		pattern::TreePattern aosVariable = pattern::var("oldVar", pattern::atom(oldVar));//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern readFromStruct = pirp::assignment(pattern::any, pattern::var("struct", pirp::callExpr(pirp::structType(*pattern::any),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)));
		pattern::TreePattern readFromStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << readFromStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				pattern::aT(pattern::var("oldVar", pattern::atom(oldVar))) << *pattern::any);

		pirp::matchAllPairs(readFromStructInLoop | externalAosCall, NodeAddress(toTransform), [&](const NodeAddress& node, pattern::AddressMatch am) {
			ExpressionPtr start, end;
//std::cout << begin << " < " << node << " -> " << (begin < node) << std::endl;
			if(begin && !(begin < node)) {
				// do not unmarshall before marshalling
				return;
			}

			ExpressionAddress matchedOldVar = node >> am["oldVar"].getValue().as<ExpressionAddress>();
			if(!compareVariables(oldVar, matchedOldVar))
				return;

			if(am.isVarBound("start")) { // assignToStructInLoop

				start = am["start"].getValue().as<ExpressionPtr>();
				end = am["end"].getValue().as<ExpressionPtr>();
			} else { // externalAosCall
				CallExprAddress call = node.as<CallExprAddress>();

				// filter out builtins
				if(call->getNodeManager().getLangBasic().isBuiltIn(call->getFunctionExpr()))
					return;

//				for(std::pair<ExpressionPtr, ExpressionPtr> e : nElems) {
//					std::cout << "from: " << e.first << " to " << e.second << std::endl;
//				}
//				std::cout << "looking for: " << oldVar << std::endl;
//
				ExpressionPtr numElements = determineNumberOfElements(newVar, nElems);

				start = builder.literal(numElements->getType(), "0");
				end = numElements;
			}

			StatementList unmarshallAndExternalCall;
			unmarshallAndExternalCall.push_back(generateUnmarshalling(oldVar, newVar, start, end, newStructType));

			unmarshallingPoints.push_back(node.as<StatementAddress>());

			StatementAddress toBeReplaced = getStatementReplacableParent(node);

			auto check = replacements.find(toBeReplaced);
			CompoundStmtPtr cmpUnmarshallAndExternalCall;
			// check if there is already a replacement for the corresponding node
			if(check != replacements.end()) {
				// if so, add to this replacement, assume the original call is already in the existing replacement
				CompoundStmtPtr oldReplacement = (*check).second.as<CompoundStmtPtr>();
				StatementList combinedReplacement(oldReplacement.getElements());
				combinedReplacement.insert(combinedReplacement.begin(), unmarshallAndExternalCall.begin(), unmarshallAndExternalCall.end());

				cmpUnmarshallAndExternalCall = builder.compoundStmt(combinedReplacement);
			} else {
				// add origninal call
				unmarshallAndExternalCall.push_back(toBeReplaced);
				cmpUnmarshallAndExternalCall = builder.compoundStmt(unmarshallAndExternalCall);
			}
			cmpUnmarshallAndExternalCall.addAnnotation<RemoveMeAnnotation>();
			addToReplacements(replacements, toBeReplaced, cmpUnmarshallAndExternalCall);
		});
	}

	return unmarshallingPoints;
}

void DatalayoutTransformer::replaceAccesses(const ExprAddressMap& varReplacements, const StructTypePtr& newStructType, const NodeAddress& toTransform,
		const std::vector<StatementAddress>& begin, const std::vector<StatementAddress>& end, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const StatementPtr& newVar = vr.second;

//std::cout << "Replacing accesses to " << oldVar << "  " << *oldVar << std::endl;

		pattern::TreePattern structAccessWithOptionalDeref = pattern::var("structAccess", optionalDeref(pattern::aT(
				pattern::var("oldVar", pattern::atom(oldVar)))));
		pattern::TreePattern structArrayElementAccess = pirp::arrayRefElem1D(structAccessWithOptionalDeref, var("index", pattern::any));
		pattern::TreePattern structMemberAccess =  pattern::var("call", pirp::compositeRefElem(structArrayElementAccess,
				pattern::var("member", pattern::any)));
//pattern::TreePattern structMemberAccess1 =  pattern::var("call", pirp::compositeRefElem(pirp::arrayRefElem1D(pirp::refDeref(
//		pattern::var("structAccess", pattern::aT(pattern::atom(oldVar)))), var("index", pattern::any)), pattern::var("member", pattern::any)));

		pattern::TreePattern assignArrayAccess = declOrAssignment(pattern::var("target", pattern::any), pattern::var("arrayAccess", structArrayElementAccess));
		pattern::TreePattern structAccess = pattern::var("call", pirp::refDeref(pirp::arrayRefElem1D(optionalDeref(pattern::var("structAccess",
				pattern::aT(pattern::var("oldVar", pattern::atom(oldVar))))), var("index", pattern::any))));
		pattern::TreePattern assignStructAccess = declOrAssignment(pattern::var("target",
				pirp::variable(pattern::aT(pirp::refType(pirp::structType(*pattern::any))))), structAccess);

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> c : newMemberAccesses) {
	//		ExpressionPtr old = builder.arrayRefElem()
	//	}

		visitDepthFirstPrunable(toTransform, [&](const StatementAddress& node)->bool {

			// do not touch marshalling and unmrashalling points
			if(contains(begin, node))
				return true;
			if(contains(end, node))
				return true;

			CallExprAddress call = node.isa<CallExprAddress>();

			if(call) {
				pattern::AddressMatchOpt match = structMemberAccess.matchAddress(node);
				if(match) {
					ExpressionAddress matchedOldVar = match.get()["oldVar"].getValue().as<ExpressionAddress>();
					if(!compareVariables(oldVar, matchedOldVar))
						return false;
//std::cout << "\nCall: ";
//dumpPretty(node);
					StringValuePtr member = builder.stringValue(match.get()["member"].getValue().as<LiteralPtr>()->getStringValue());
					ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
					ExpressionPtr structAccess = match.get()["structAccess"].getValue().as<ExpressionPtr>();
					ExpressionPtr replacement = generateNewAccesses(oldVar, newVar, member, index, structAccess);

					addToReplacements(replacements, node, replacement);
					return true;
				}
			}
			pattern::AddressMatchOpt match = (assignStructAccess | assignArrayAccess).matchAddress(node);

			if(match) {
				ExpressionAddress matchedOldVar = match.get()["oldVar"].getValue().as<ExpressionAddress>();
				if(!compareVariables(oldVar, matchedOldVar)) {
					return false;
				}
//std::cout << "\nMatching \n";
//dumpPretty(node);
				ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
				ExpressionPtr oldStructAccess = match.get()["structAccess"].getValue().as<ExpressionPtr>();
				ExpressionPtr inplaceUnmarshalled = generateByValueAccesses(oldVar, newVar, newStructType, index, oldStructAccess);

				if(match.get().isVarBound("decl"))
					addToReplacements(replacements, match.get().getRoot(), builder.declarationStmt(match.get()["target"].getValue().as<VariablePtr>(),
							builder.refVar(inplaceUnmarshalled)));
				if(match.get().isVarBound("assignment"))
					addToReplacements(replacements, match.get().getRoot(),
							builder.assign(match.get()["target"].getValue().as<ExpressionPtr>(), inplaceUnmarshalled));
			}

//			pattern::AddressMatchOpt match = (structAccess | structArrayElementAccess).matchAddress(node);
//
//			if(match) {
//				ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
//				ExpressionPtr oldStructAccess = match.get()["structAccess"].getValue().as<ExpressionPtr>();
//				ExpressionPtr inplaceUnmarshalled = generateByValueAccesses(oldVar, newVar, newStructType, index, oldStructAccess);
//
//				if(match.get().isVarBound("decl"))
//					addToReplacements(replacements, match.get().getRoot(), builder.declarationStmt(match.get()["target"].getValue().as<VariablePtr>(),
//							builder.refVar(inplaceUnmarshalled)));
//				if(match.get().isVarBound("assignment"))
//					addToReplacements(replacements, match.get().getRoot(),
//							builder.assign(match.get()["target"].getValue().as<ExpressionPtr>(), inplaceUnmarshalled));
//			}

			return false;
		});

	}

}

void DatalayoutTransformer::updateScalarStructAccesses(NodePtr& toTransform) {
	IRBuilder builder(mgr);

}

void DatalayoutTransformer::addNewDel(const ExprAddressMap& varReplacements, const NodeAddress& toTransform, const StructTypePtr& newStructType,
		std::map<NodeAddress, NodePtr>& replacements) {

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
		const ExpressionAddress& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second.as<ExpressionPtr>();
		IRBuilder builder(mgr);

		visitDepthFirstInterruptible(toTransform, [&](const ExpressionAddress& call)->bool {
			StatementList deletes = generateDel(call, oldVar, newVar, newStructType);
			if(!deletes.empty()) {
			CompoundStmtPtr cmpDeletes = builder.compoundStmt(deletes);
				cmpDeletes.addAnnotation<RemoveMeAnnotation>();

				StatementPtr multiDelCompound = cmpDeletes;
				addToReplacements(replacements, call, multiDelCompound);

				return true;
			}

			return false;
		});
	}
}

TypePtr DatalayoutTransformer::generateNewTupleType(const TypePtr& oldTupleVarType, const StructTypePtr& newStructType, const TypePtr& oldStructType) {
	NodeMap tyReplace = generateTypeReplacements(oldStructType, newStructType);
	return core::transform::replaceAllGen(mgr, oldTupleVarType, tyReplace);
}

void DatalayoutTransformer::updateTuples(ExprAddressMap& varReplacements, const core::StructTypePtr& newStructType, const core::TypePtr& oldStructType,
		const NodeAddress& toTransform,	std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	ExprAddressMap tupleVars;

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {

		const ExpressionAddress& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second.as<ExpressionPtr>();

		pattern::TreePattern structAccess =  pattern::var("structAccess", pattern::aT(pattern::var("oldVar", pattern::atom(oldVar))));
		pattern::TreePattern tupleAccess = pattern::var("access", pirp::tupleRefElem(pattern::aT(pattern::var("tupleVar", pirp::variable())),
				pattern::var("idx"), pattern::var("type")));
		pattern::TreePattern tupleAssign = pirp::assignment(tupleAccess, structAccess);

		pirp::matchAllPairs(tupleAssign, toTransform, [&](const NodeAddress& node, pattern::AddressMatch match) {
			ExpressionAddress matchedOldVar = node >> match["oldVar"].getValue().as<ExpressionAddress>();

			if(!compareVariables(oldVar, matchedOldVar))
				return;

			ExpressionAddress oldTupleAccess = node >> match["access"].getValue().as<ExpressionAddress>();
			ExpressionAddress oldTupleVar = getDeclaration(node >> match["tupleVar"].getValue().as<ExpressionAddress>());

			TypePtr oldTupleVarType = oldTupleVar->getType();
			TypeAddress oldComponentType = node >> match["type"].getValue().as<LiteralAddress>()->getType();
			ExpressionAddress idx = node >> match["idx"].getValue().as<ExpressionAddress>();

			TypePtr newTupleType = generateNewTupleType(oldTupleVarType, newStructType, oldStructType);

			ExpressionPtr newTupleVar;

			if(tupleVars.find(oldTupleVar) == tupleVars.end()) {
				// check if local or global variable
				LiteralPtr globalTuple = oldTupleVar.isa<LiteralPtr>();
				newTupleVar = globalTuple ?
						builder.literal(globalTuple->getStringValue() + "_soa", newTupleType).as<ExpressionPtr>() :
						builder.variable(newTupleType).as<ExpressionPtr>();

				tupleVars[oldTupleVar] = newTupleVar;
			} else {
				newTupleVar = tupleVars[oldTupleVar].as<ExpressionPtr>();
			}

			ExpressionAddress oldRootVar = getRootVariable(oldTupleVar, node).as<ExpressionAddress>();

			if(tupleVars.find(oldRootVar) == tupleVars.end()) {
				TypePtr newRootType = generateNewTupleType(oldRootVar->getType().getAddressedNode(), newStructType, oldStructType);

				LiteralPtr globalRoot = oldRootVar.isa<LiteralPtr>();
				ExpressionPtr newRootVar = globalRoot ?
						builder.literal(globalRoot->getStringValue() + "_soa", newRootType).as<ExpressionPtr>() :
						builder.variable(newRootType).as<ExpressionPtr>();

				tupleVars[oldRootVar] = newRootVar;
			}

			ExpressionPtr newTupleAccess = core::transform::fixTypesGen(mgr, oldTupleAccess.as<ExpressionPtr>(), oldTupleVar.as<ExpressionPtr>(),
					newTupleVar, true);

			ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, match["structAccess"].getValue().getAddressedNode(),
					oldVar, newVar, true).as<ExpressionPtr>();
//std::cout << "\nntv: \n";
//dumpPretty(oldStructType);
//dumpPretty(newStructType);

			newStructAccess = core::transform::replaceAllGen(mgr, newStructAccess, oldTupleVarType, newTupleType, core::transform::globalReplacement);

//			std::cout << "\nreplacing\n";
//			dumpPretty(newTupleType);
//			std::cout << "\nwith\n---------------------------------------------------\n";
//			dumpPretty(builder.assign(newTupleAccess, newStructAccess));
			addToReplacements(replacements, node, builder.assign(newTupleAccess, newStructAccess));
		});

	}

	//replace all occurrences of the selected tuples
	for(std::pair<ExpressionAddress, StatementPtr> vr : tupleVars) {

		const ExpressionAddress& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second.as<ExpressionPtr>();


		if(DeclarationStmtAddress decl = oldVar.getParentAddress().isa<DeclarationStmtAddress>()) { //update init expression
			NodeMap tyReplace = generateTypeReplacements(oldStructType, newStructType);

			ExpressionPtr newInit = updateInit(varReplacements, decl->getInitialization(), tyReplace, StringValuePtr());

			addToReplacements(replacements, decl, builder.declarationStmt(newVar.as<VariablePtr>(), newInit));
		}

		visitDepthFirst(toTransform, [&](const ExpressionAddress& expr) {


			if(oldVar.getAddressedNode() != expr.getAddressedNode())
				return;

			const ExpressionAddress decl = getDeclaration(expr);
			if(!decl)
				return;

			if(decl == oldVar)
				replacements[expr] = newVar;
		});

		varReplacements[oldVar] = newVar;
	}
}

// unused
void DatalayoutTransformer::updateCopyDeclarations(ExprAddressMap& varReplacements, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType,
		const NodeAddress& toTransform,	std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {

		const ExpressionAddress& oldVar = vr.first;
//		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern influencedDecl = pirp::declarationStmt(var("influencedVar", pirp::variable()),
				pattern::aT(pattern::var("oldVar", pattern::atom(oldVar))));

		pirp::matchAllPairs(influencedDecl, toTransform, [&](const NodeAddress& node, pattern::AddressMatch match) {
			ExpressionAddress matchedOldVar = node >> match["oldVar"].getValue().as<ExpressionAddress>();
			if(!compareVariables(oldVar, matchedOldVar))
				return;

			DeclarationStmtAddress decl = node.as<DeclarationStmtAddress>();

			if(replacements.find(decl) != replacements.end()) {
				return; // already handled
			}

			VariablePtr var = match["influencedVar"].getValue().as<VariablePtr>();

			TypePtr oldType = var->getType();
			TypePtr newType = core::transform::replaceAllGen(mgr, oldType, oldStructType, newStructType, core::transform::globalReplacement);

			if(oldType == newType) // check if the type depends on the transformation
				return;
			VariablePtr updatedVar = builder.variable(newType);

			DeclarationStmtPtr updatedDecl = builder.declarationStmt(updatedVar, decl->getInitialization());

			addToReplacements(replacements, decl, updatedDecl);
		});
	}
}

void DatalayoutTransformer::doReplacements(const ExprAddressMap& kernelVarReplacements, const std::map<NodeAddress, NodePtr>& replacements,
		const core::transform::TypeHandler& typeOfMemAllocHandler) {
	IRBuilder builder(mgr);

	// check if all replacements have the current root
	if(!replacements.empty()) {
		for(std::pair<NodeAddress, NodePtr> replacement : replacements) {
//			std::cout << "\nFrom-------------------------------------------\n";
//			dumpPretty(replacement.first);
//			std::cout << "\nTo++++++++++++++++++++++++++++++++++++++++++++++\n";
//			dumpPretty(replacement.second);
//			std::cout << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
//			NodeAddress withNewRoot = replacement.first.switchRoot(toTransform);
//			dumpPretty(core::transform::replaceNode(mgr, withNewRoot, replacement.second));

			if(*replacement.first.getRootAddress() != *toTransform) {
//				dumpPretty(replacement.first);
				assert_fail() << "Replacement target has a wrong root";
			}
		}

//		toTransform = core::transform::replaceAll(mgr, replacements);
	}

//	toTransform = core::transform::fixInterfaces(mgr, toTransform);
//visitDepthFirst(toTransform, [](const NodePtr& node) {
//	std::cout << "\nNODE: " << node << "---------------\n";
//	dumpPretty(node);
//	std::cout << "++++++++++++++++++++++++++++++\n";
//});

	// perform all the replacements. Also replace all variables inside kernels
	StuffReplacer sr(kernelVarReplacements, replacements);

//	auto mapper = core::transform::makeLambdaAddressMapping([&](const NodePtr& builtPtr, const NodeAddress& prevAddress) -> NodePtr {
//		auto replaceMe = replacements.find(prevAddress);
//		if(replaceMe != replacements.end())
//			return replaceMe->second;
//
//		if(ExpressionAddress expr = prevAddress.isa<ExpressionAddress>()) {
//			auto replaceMe = kernelVarReplacements.find(getDeclaration(expr));
//			if(replaceMe != kernelVarReplacements.end())
//				if(replaceMe->second.isa<ExpressionPtr>()) // do not replace Expressions with Statements
//					return replaceMe->second;
//		}
//
//		return builtPtr;
//	});

	toTransform = sr.mapFromRoot(toTransform);

	ExpressionMap structures;
//	if(!structures.empty())
//		toTransform = core::transform::replaceVarsRecursive(mgr, toTransform, structures, false);
	do {
		toTransform = core::transform::fixTypes(mgr, toTransform, structures, false, typeOfMemAllocHandler);

		structures.clear();
		visitDepthFirst(toTransform, [&](const StatementPtr& stmt) {
			if(DeclarationStmtPtr decl = stmt.isa<DeclarationStmtPtr>()) {
				ExpressionPtr var = decl->getVariable();
				ExpressionPtr init = decl->getInitialization();

				if(structures.find(var) != structures.end()) // already there
					return;

				if(!types::isSubTypeOf(init->getType(), var->getType())) {
					structures[var] = builder.variable(init->getType());

				}
			}
		});
	}while(!structures.empty());

}


std::map<int, ExpressionPtr> VariableAdder::searchInArgumentList(const std::vector<ExpressionAddress>& args) {
	ExpressionAddress oldVar;
	ExpressionPtr newVar;
	std::map<int, ExpressionPtr> indicesToNewArgs;
	int idx = 0;
//std::cout << "\nFUN\n";
	for(ExpressionAddress arg : args) {
//std::cout << "ARG: " << arg << std::endl;

		pattern::AddressMatchOpt match = varWithOptionalDeref.matchAddress(arg);
		if(match) {
			auto varCheck = varsToReplace.find(match.get()["variable"].getValue().as<ExpressionAddress>());
			if(varCheck != varsToReplace.end()) {
				oldVar = varCheck->first;
				newVar = varCheck->second.as<ExpressionPtr>();

				ExpressionPtr newArg = core::transform::fixTypesGen(mgr, arg.getAddressedNode(), oldVar, newVar, false);
				indicesToNewArgs[idx] = newArg;
			}
		}
		++idx;
	}

	return indicesToNewArgs;
}

ExpressionPtr VariableAdder::updateArgument(const ExpressionAddress& oldArg) {
	ExpressionPtr newArg;
	pattern::AddressMatchOpt match = (varWithOptionalDeref | pirp::arrayView(varWithOptionalDeref)).matchAddress(oldArg);
//std::cout << *getDeclaration(oldArg) << std::endl;
	if(match) {
		ExpressionAddress oldVarDecl = getDeclaration(match.get()["variable"].getValue().as<ExpressionAddress>());
//{ExpressionAddress oldVarDecl = getDeclaration(oldArg);

		auto varCheck = varsToReplace.find(oldVarDecl);
		if(varCheck != varsToReplace.end()) {
			ExpressionAddress oldVar = varCheck->first;
			ExpressionPtr newVar = varCheck->second.as<ExpressionPtr>();

			newArg = core::transform::fixTypesGen(mgr, oldArg.getAddressedNode(), oldVar, newVar, false);
		}
	}

	return newArg;
}

VariableAdder::VariableAdder(core::NodeManager& mgr, ExprAddressMap& varReplacements)
	: mgr(mgr), varsToReplace(varReplacements),
	  typePattern(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))),
	  variablePattern(pirp::variable(typePattern) // local variable
			| pirp::literal(pirp::refType(typePattern), pattern::any)),// global variable
	  namedVariablePattern(var("variable", variablePattern)),
	  varWithOptionalDeref(optionalDeref(namedVariablePattern)) {

//	for(std::pair<ExpressionPtr, ExpressionPtr> rep : varReplacements) {
//		std::cout<< *rep.first << " -> " << *rep.second << std::endl;
//	}
}

NodePtr VariableAdder::generateNewCall(const CallExprAddress& oldCall, const StatementPtr& newVar, const int argIdx) {
	const LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().as<LambdaExprAddress>();

	ExpressionAddress oldArg = oldCall->getArgument(argIdx);

	// update to new variable which will be added to argument list;
	ExpressionPtr newArg = updateArgument(oldArg);

	assert(newArg && "Parameter to be replaced doesn't have replaceable argument.");

	std::vector<ExpressionPtr> args(oldCall.getAddressedNode()->getArguments());
	// add new arguments and parameters to corresponding list
	VariableList params = lambdaExpr.getAddressedNode()->getParameterList();
	args.push_back(newArg);
	params.push_back(newVar.as<VariablePtr>());

	IRBuilder builder(mgr);

	// update function type
	FunctionTypePtr lambdaType = lambdaExpr->getType().as<FunctionTypePtr>();
	std::vector<TypePtr> funTyMembers = lambdaType->getParameterTypeList();
	funTyMembers.push_back(newVar.as<VariablePtr>()->getType());
	FunctionTypePtr newFunType = builder.functionType(funTyMembers, lambdaType->getReturnType(), lambdaType->getKind());

	LambdaExprPtr newLambdaExpr = builder.lambdaExpr(newFunType, params, lambdaExpr.getAddressedNode()->getBody());

	CallExprPtr newCall = builder.callExpr(oldCall->getType(), newLambdaExpr, args);

	//migrate possible annotations
	core::transform::utils::migrateAnnotations(lambdaExpr.getAddressedNode(), newLambdaExpr);
	core::transform::utils::migrateAnnotations(oldCall.getAddressedNode(), newCall);

	return newCall;
}

NodeAddress VariableAdder::addVariablesToLambdas(NodePtr& src) {
	std::map<NodeAddress, NodePtr> replacements;
//for(std::pair<ExpressionAddress, ExpressionPtr> vr : varsToReplace) {
//	std::cout << "to replace: " << vr.first << " " << *vr.first << std::endl;
//}
	NodeAddress srcAddr = NodeAddress(src);
	ExprAddressMap tmp;
	for(std::pair<ExpressionAddress, StatementPtr> vr : varsToReplace) {
		tmp[vr.first] = vr.second;
	}

	for(std::pair<ExpressionAddress, StatementPtr> vr : tmp) {
		// update current address
		ExpressionAddress oldVar = vr.first.switchRoot(srcAddr);
		StatementPtr newVar = vr.second;
		if(oldVar.getDepth() > 5) {
			// if the variable's parent is a lambda it means it is part of a call to a subfunction -> add the new variable to the call
			if(LambdaAddress lambda = oldVar.getParentAddress(2).isa<LambdaAddress>()) {
				CallExprAddress oldCall = lambda.getParentAddress(4).as<CallExprAddress>();
				const LambdaExprAddress lambdaExpr = oldCall->getFunctionExpr().as<LambdaExprAddress>();
				std::vector<VariableAddress> paramAddrs = lambdaExpr->getParameterList();

				// determine index of the argument/parameter
				size_t i = 0u;
				for(; i < paramAddrs.size(); ++i) {
					if(paramAddrs[i] == oldVar) {
						break;
					}
				}

				replacements[oldCall] = generateNewCall(oldCall, newVar, i);
			}
		}

//for(std::pair<NodeAddress, NodePtr> replacement : replacements) {
//	std::cout << "\ndo it\n";
//	dumpPretty(replacement.second);
//}
		if(!replacements.empty()) {
			src = core::transform::replaceAll(mgr, replacements);
			replacements.clear();
			srcAddr = NodeAddress(src);

			// update replacement with new root
			varsToReplace.clear();
			for(std::pair<ExpressionAddress, StatementPtr> vr : tmp) {
				varsToReplace[vr.first.switchRoot(srcAddr)] = vr.second;
			}
		}

	}

	return srcAddr;
}


const NodePtr NewCompoundsRemover::resolveElement(const core::NodePtr& element) {
	// stop recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;
	}

	if(CompoundStmtPtr compound = element.isa<CompoundStmtPtr>()) {
		IRBuilder builder(mgr);
		StatementList newCompoundStmts;
		for(StatementPtr stmt : compound->getStatements()) {
			stmt = stmt->substitute(mgr, *this);

			if(stmt.hasAnnotation(RemoveMeAnnotation::KEY)) {
				CompoundStmtPtr innerCompound = stmt.as<CompoundStmtPtr>();
				StatementList innerStmts = innerCompound->getStatements();
				newCompoundStmts.insert(newCompoundStmts.end(), innerStmts.begin(), innerStmts.end());
			} else {
				newCompoundStmts.push_back(stmt);
			}
		}
		return builder.compoundStmt(newCompoundStmts);
	}

	return element->substitute(mgr, *this);
}

} // datalayout
} // transform
} // insieme

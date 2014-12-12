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

#include "insieme/transform/datalayout/aos_to_soa.h"
#include "insieme/transform/datalayout/datalayout_utils.h"

#include "insieme/utils/annotation.h"

#include "insieme/analysis/defuse_collect.h"
#include "insieme/analysis/cba/analysis.h"
#include "insieme/analysis/scopes_map.h"

#include "insieme/annotations/data_annotations.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;
namespace ia = insieme::analysis;

namespace {

class RemoveMeAnnotation : public NodeAnnotation {
public:
	static const string NAME;
    static const utils::StringKey<RemoveMeAnnotation> KEY;

    const utils::AnnotationKeyPtr getKey() const { return &KEY; }
    const std::string& getAnnotationName() const { return NAME; }

    RemoveMeAnnotation() {}


    virtual bool migrate(const core::NodeAnnotationPtr& ptr, const core::NodePtr& before, const core::NodePtr& after) const {
		// always copy the annotation
		assert(&*ptr == this && "Annotation pointer should reference this annotation!");
		after->addAnnotation(ptr);
		return true;
	}
};

const string RemoveMeAnnotation::NAME = "RemoveMeAnnotation";
const utils::StringKey<RemoveMeAnnotation> RemoveMeAnnotation::KEY("RemoveMe");

StatementPtr aosToSoaAllocTypeUpdate(const StatementPtr& stmt) {
	pattern::TreePattern oldStructTypePattern = pattern::aT(pattern::var("structType", pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
	pattern::TreePattern newStructTypePattern = pattern::aT(pattern::var("structType", pirp::structType(*pattern::any)));

	return allocTypeUpdate(stmt, oldStructTypePattern, newStructTypePattern);
}

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
ExpressionPtr expressionContainsMarshallingCandidate(const utils::map::PointerMap<ExpressionPtr, T>& candidates, const ExpressionAddress& expr,
		const NodeAddress& scope) {
		ExpressionAddress initVar = getRootVariable(scope, expr).as<ExpressionAddress>();
		if(initVar) for(std::pair<ExpressionPtr, T> candidate : candidates) {
			if(*initVar == *candidate.first)
			return candidate.first;
		}

	return ExpressionPtr();
}

utils::map::PointerMap<ExpressionPtr, RefTypePtr> findAllSuited(NodeAddress toTransform) {
	utils::map::PointerMap<ExpressionPtr, RefTypePtr> structs;

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

		ExpressionPtr structVar = nm["structVar"].getValue().as<ExpressionPtr>();
		TypePtr varType = structVar->getType();
		RefTypePtr structType = nm["structType"].getValue().as<RefTypePtr>();

		if(match.getParentAddress(1)->getNodeType() == NT_DeclarationStmts)
			return; // do not consider variables which are part of declaration statements

		if(tupleType.match(varType)) {
			return; // tuples are not candidates since only one field needs to be altered.
					// They will only be changed if the field is an alias to some other candidate
		}

		if(isInsideJob(match)) {
			return; // variable in parallel regions are replaced with new variables of updated type. We don't keep the old version inside parallel regions.
		}

		structs[structVar] = structType;
//std::cout << "Adding: " << *structVar << " as a candidate" << std::endl;
	});
	return structs;
}

utils::map::PointerMap<ExpressionPtr, RefTypePtr> findPragma(NodeAddress toTransform) {
	utils::map::PointerMap<ExpressionPtr, RefTypePtr> structs;

	pattern::TreePattern structTypePattern = pirp::refType(pattern::aT(var("structType", pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))));
	core::visitDepthFirst(toTransform, [&](const core::DeclarationStmtAddress& decl) {
		if(decl->hasAnnotation(annotations::DataTransformAnnotation::KEY)) {
			annotations::DataTransformAnnotationPtr dta = decl->getAnnotation(annotations::DataTransformAnnotation::KEY);

			if(dta->isSoa()) {
				VariablePtr structVar = decl->getVariable();
				TypePtr varType = structVar->getType();

				pattern::MatchOpt match = structTypePattern.matchPointer(varType);
				assert(match && "Pragma-marked variable does not have valid struct type");

				RefTypePtr structType = match.get()["structType"].getValue().as<RefTypePtr>();

				structs[structVar] = structType;
			}
		}
	});

	return structs;
}


AosToSoa::AosToSoa(core::NodePtr& toTransform, CandidateFinder candidateFinder)
		: mgr(toTransform->getNodeManager()), toTransform(toTransform), candidateFinder(candidateFinder) {}

void AosToSoa::transform() {
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
			TypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), toReplaceList.second, newStructType).as<TypePtr>();
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

//for(std::pair<ExpressionPtr, ExpressionPtr> rep : varReplacements)
//	std::cout << "from " << rep.first << "  to  " << rep.second << std::endl;

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
		updateTuples(varReplacements, newStructType, toReplaceList.second, tta, replacements, structures);

		doReplacements(replacements, structures, aosToSoaAllocTypeUpdate);

//		replacements.clear();
//		tta = NodeAddress(toTransform);
	}


	// remove all inserted compound expressions
	NewCompoundsRemover remComp(mgr);
	toTransform = remComp.mapElement(0, toTransform);
//dumpPretty(toTransform);
}

std::vector<std::pair<ExpressionSet, RefTypePtr>> AosToSoa::createCandidateLists() {
	NodeAddress tta(toTransform);
	utils::map::PointerMap<ExpressionPtr, RefTypePtr> structs = findCandidates(tta);
	std::vector<std::pair<ExpressionSet, RefTypePtr>> toReplaceLists;

	ia::VariableScopeMap scopes = ia::mapVariablesToScopes(tta);

	for(std::pair<ExpressionPtr, RefTypePtr> candidate : structs) {
		ExpressionSet toReplaceList;
		toReplaceList.insert(candidate.first);

		// fixpoint iteration to capture all variables that new a new versions with new type
		size_t curNumOfVars = 0;
//std::cout << "curNumOfVars: " << curNumOfVars << " != " << toReplaceList.size() << std::endl;
		while(curNumOfVars != toReplaceList.size()) {
			curNumOfVars = toReplaceList.size();
			collectVariables(candidate, toReplaceList, tta, scopes);
		}
		toReplaceLists.push_back(std::make_pair(toReplaceList, candidate.second));

	}

	toReplaceLists = mergeLists(toReplaceLists);

//	for(std::pair<ExpressionSet, RefTypePtr> toReplaceList : toReplaceLists) {
//		std::cout << "\nList: \n";
//		for(ExpressionPtr tr : toReplaceList.first)
//			std::cout << *tr->getType() << " " << *tr << std::endl;
//	}

	return toReplaceLists;
}

utils::map::PointerMap<ExpressionPtr, RefTypePtr> AosToSoa::findCandidates(NodeAddress toTransform) {

	return candidateFinder(toTransform);
}

bool isRefStruct(ExpressionPtr expr, RefTypePtr structType) {
	TypePtr type = expr->getType();

	if(!type.isa<RefTypePtr>())
		return false;

	pattern::TreePattern containsStructType = pattern::aT(pattern::atom(structType));

	if(containsStructType.match(type))
		return true;

	return false;
}

void AosToSoa::collectVariables(const std::pair<ExpressionPtr, RefTypePtr>& transformRoot, ExpressionSet& toReplaceList,
		const NodeAddress& toTransform, ia::VariableScopeMap& scopes) {

#if 1
	RefTypePtr structType = transformRoot.second;
	visitDepthFirst(toTransform, [&](const StatementAddress& stmt) {
		if(const CallExprAddress call = stmt.isa<CallExprAddress>()) {
			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
				ExpressionAddress lhs = removeMemLocationCreators(call[0]);
				ExpressionAddress rhs = removeMemLocationCreators(call[1]);
				ExpressionAddress lhsVar = extractNonTupleVariable(lhs);
				if(!(lhsVar && isRefStruct(lhsVar, structType))) // lhs is not based on a variable
					return;
				ExpressionAddress rhsVar = extractNonTupleVariable(rhs);
				if(!(rhsVar && isRefStruct(rhsVar, structType))) // rhs is not based on a variable
					return;

				if(toReplaceList.find(lhsVar) != toReplaceList.end()) { // lhsVar is already selected for replacement
					if(rhs->getType().isa<RefTypeAddress>()) {
						toReplaceList.insert(rhsVar);
						return;
					}
				}
				if(toReplaceList.find(rhsVar) != toReplaceList.end()) { // rhsVar is already selected for replacement
					if(rhs->getType().isa<RefTypeAddress>()){
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
					ExpressionAddress argVar = extractNonTupleVariable(pair.first);
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

			ExpressionAddress initVar = extractNonTupleVariable(init);
			if(!initVar)
				return;

			if(toReplaceList.find(initVar) != toReplaceList.end()) { // the initialization is already selected for replacement
					if(isRefStruct(init, structType)) {
						toReplaceList.insert(var);
						return;
					}
				}
		}
#else
		visitDepthFirst(toTransform, [&](const ExpressionAddress& expr) {
			if(expr->getType()->getNodeType() != NT_RefType)
				return;
		// check if it is an access to the transformRoot's memory
		if(*transformRoot.first == *extractVariable(expr)) {
//std::cout << "\ttesting: " << *expr << std::endl;
			// if so, add all variable which alias it to the toReplaceList
			visitDepthFirst(toTransform, [&](const ExpressionAddress& potentialAlias) {
				if(potentialAlias->getType()->getNodeType() != NT_RefType)
					return;

				if(*potentialAlias->getType() == *transformRoot.second)
				{
//dumpPretty(potentialAlias);
					if(ExpressionPtr varToAdd = extractVariable(potentialAlias)) {
						// no need to add the same variable again
						if(*varToAdd == *transformRoot.first)
							return;

						if(expr->getNodeType() == NT_Variable && potentialAlias->getNodeType() == NT_Variable)
							if(scopes[expr.as<VariableAddress>()] != scopes[potentialAlias.as<VariableAddress>()])
								return;


						if(varToAdd->getNodeType() == NT_Variable ||
								(varToAdd->getNodeType() == NT_Literal && varToAdd->getType()->getNodeType() == NT_RefType)) {
//dumpPretty(expr );
//dumpPretty(potentialAlias );
//std::cout << expr << " ------------------------------- \n";
							if(ia::cba::mayAlias(expr, potentialAlias)) {
								/*auto newly = */toReplaceList.insert(varToAdd);
//if(newly.second) {
//std::cout << "Expr:  " << *expr;
//std::cout << " alias: " /* << *potentialAlias */<< std::endl;
//dumpPretty(potentialAlias);
//}
							}
						}
					}
				}
			});
		}
#endif
	});

}

std::vector<std::pair<ExpressionSet, RefTypePtr>> AosToSoa::mergeLists(std::vector<std::pair<ExpressionSet, RefTypePtr>>& toReplaceLists) {
	std::vector<std::pair<ExpressionSet, RefTypePtr>> newLists;
	for(std::pair<ExpressionSet, RefTypePtr>& toReplaceList : toReplaceLists) {
		bool addToOld = false;
	// merge lists which contain the same variables
//		for(ExpressionPtr toReplace : toReplaceList.first) {
//			for(std::pair<ExpressionSet, RefTypePtr>& newList : newLists) {
//				if(newList.first.find(toReplace) != newList.first.end()) {
//					assert(newList.second == toReplaceList.second && "Correlated variables would need to be changed to different types");
//					for(ExpressionPtr toInsert : toReplaceList.first)
//						newList.first.insert(toInsert);
//					addToOld = true;
//				}
//			}
//		}
		// merge lists of same type
		for(std::pair<ExpressionSet, RefTypePtr>& newList : newLists) {
			if(toReplaceList.second == newList.second) {
				for(ExpressionPtr toInsert : toReplaceList.first)
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

StructTypePtr AosToSoa::createNewType(core::StructTypePtr oldType) {
	IRBuilder builder(mgr);

	NodeRange<NamedTypePtr> member = oldType->getElements();
	std::vector<NamedTypePtr> newMember;
	for(NamedTypePtr memberType : member) {
//			std::cout << "member: " << memberType << std::endl;
		newMember.push_back(builder.namedType(memberType->getName(), builder.refType(builder.arrayType(memberType->getType()))));

	}
	return builder.structType(newMember);
}

ExpressionPtr AosToSoa::updateInit(const ExpressionMap& varReplacements, ExpressionPtr init, NodeMap& backupReplacements, StringValuePtr fieldName) {
	// check for marshalled variables in init expression
	std::pair<ExpressionPtr, ExpressionPtr> varInInit;
	visitBreadthFirstInterruptible(init, [&](const ExpressionPtr& currVar) {
		auto check = varReplacements.find(currVar);
		if(check != varReplacements.end()) {
			varInInit = *check;
			return true;
		}
		return false;
	});

	if(varInInit.first) { // variable to be replaced found
		// replace accesses to old variable with accesses to field in new variable
		ExpressionMap localReplacement;
		IRBuilder builder(mgr);
		localReplacement[varInInit.first] = fieldName ? builder.accessMember(varInInit.second, fieldName) : varInInit.second;

		ExpressionPtr newInit = core::transform::fixTypesGen(mgr, init, localReplacement, true);

		return newInit;
	}

	// backup, works for e.g. memory allocation
	return core::transform::replaceAll(mgr, init, backupReplacements, false).as<ExpressionPtr>();
}


StatementList AosToSoa::generateNewDecl(const ExpressionMap& varReplacements, const DeclarationStmtAddress& decl, const VariablePtr& newVar,
		const StructTypePtr& newStructType, const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);

	// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and it's initialization
	StatementList allDecls;

//			if(memAlloc.match(decl->getInitialization())) { // if memory is allocated, allocate memory of new newVar
		allDecls.push_back(decl);
		allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), builder.undefinedVar(newVar->getType())));

		// split up initialization expressions
		for(NamedTypePtr memberType : newStructType->getElements()) {
//std::cout << "\nnewVar: " << *newVar << "\nnewVarType: " << *newVar->getType() << std::endl;
//std::cout << "\ninitType: " << *decl->getInitialization()->getType() << std::endl;
//builder.refMember(newVar, memberType->getName());
//removeRefVar(decl->getInitialization());

			NodeMap inInitReplacementsInCaseOfNovarInInit;
			inInitReplacementsInCaseOfNovarInInit[oldStructType] = getBaseType(memberType->getType(), memberType->getName());

			allDecls.push_back(builder.assign(builder.accessMember(newVar, memberType->getName()),
					updateInit(varReplacements, removeRefVar(decl->getInitialization()), inInitReplacementsInCaseOfNovarInInit, memberType->getName())));
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


void AosToSoa::addNewDecls(const ExpressionMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, ExpressionMap& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	visitDepthFirst(toTransform, [&](const DeclarationStmtAddress& decl) {
		const VariablePtr& oldVar = decl->getVariable();

		auto newVarIter = varReplacements.find(oldVar);
		if(newVarIter != varReplacements.end()) {
			const VariablePtr& newVar = newVarIter->second.as<VariablePtr>();

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

			replacements[decl] = cmpDecls;
		}
	});
}

StatementList AosToSoa::generateNewAssigns(const ExpressionMap& varReplacements, const CallExprAddress& call,
		const ExpressionPtr& newVar, const StructTypePtr& newStructType, const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(mgr);
	StatementList allAssigns;

	allAssigns.push_back(call);

	for(NamedTypePtr memberType : newStructType->getElements()) {
		NodeMap inInitReplacementsInCaseOfNovarInInit;
		inInitReplacementsInCaseOfNovarInInit[oldStructType] = removeRefArray(memberType->getType());

		allAssigns.push_back(builder.assign(builder.refMember(newVar, memberType->getName()),
				updateInit(varReplacements, call[1], inInitReplacementsInCaseOfNovarInInit, memberType->getName())));
	}

	return allAssigns;
}

void AosToSoa::replaceAssignments(const ExpressionMap& varReplacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, ExpressionMap& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;
		visitDepthFirst(toTransform, [&](const CallExprAddress& call) {

			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
				if(*oldVar == *call[0]) {
					// check if the assignment does an allocation and try to extract the number of elements in that case
					pattern::MatchOpt match = allocPattern.matchPointer(call[1]);
					ExpressionPtr nElem;
					if(match) {
						nElem = match.get()["nElems"].getValue().as<ExpressionPtr>();
						nElems[newVar] = nElem;
					}

					StatementList allAssigns = generateNewAssigns(varReplacements, call, newVar, newStructType, oldStructType, nElem);

					CompoundStmtPtr cmpAssigns = builder.compoundStmt(allAssigns);
					cmpAssigns.addAnnotation<RemoveMeAnnotation>();
					replacements[call] = cmpAssigns;
	//				newStuff = core::transform::insertAfter(mgr, call, allAssigns);
				}
			}
		});
	}

}

ExpressionPtr AosToSoa::determineNumberOfElements(const ExpressionPtr& newVar,const ExpressionMap&  nElems) {
	ExpressionPtr numElements;

	auto checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable = nElems.find(newVar);

	if(checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable != nElems.end())
		numElements = checkIfThereIsTheNumberOfElementsGatheredFromMemoryAllocationForThisVariable->second;

	// backup, simply take the first which is there and hope for the best
	if(!numElements && !nElems.empty()) numElements = nElems.begin()->second;

	assert(numElements && "Cannot determine number of elements for (un)marshalling");

	return numElements;
}

StatementPtr AosToSoa::generateMarshalling(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for(NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = builder.deref(refAccess(oldVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName()));
		ExpressionPtr soaAccess = refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		loopBody.push_back(builder.assign(soaAccess, aosAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.literal(start->getType(), "1"), builder.compoundStmt(loopBody));
}

std::vector<StatementAddress> AosToSoa::addMarshalling(const ExpressionMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, ExpressionMap& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> marshalled;

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern aosVariable = pattern::atom(oldVar);//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern assignToStruct = pirp::assignment(pattern::var("struct", pirp::callExpr(pirp::refType(pirp::structType(*pattern::any)),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)), pattern::any);
		pattern::TreePattern assignToStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << assignToStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				var("oldVarCandidate", pirp::exprOfType(pattern::aT(/*pirp::refType*/(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))))))
//				pattern::node(*pattern::any << pattern::single(pattern::atom(oldVar)))
//				pattern::aT(pattern::atom(oldVar))
				<< *pattern::any);
		pattern::TreePattern oldVarPattern = pattern::aT(pattern::atom(oldVar));

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> oldToNew : newMemberAccesses) {
	//		generateMarshalling(oldToNew.first.as<VariablePtr>(), oldToNew.second.first, builder.intLit(0), builder.intLit(100), oldToNew.second.second);
	//	}
		pirp::matchAllPairs(assignToStructInLoop | externalAosCall, NodeAddress(toTransform), [&](const NodeAddress& match, pattern::AddressMatch am) {
			ExpressionPtr start, end;
			if(am.isVarBound("start")) { // assignToStructInLoop
				start = am["start"].getValue().as<ExpressionPtr>();
				end = am["end"].getValue().as<ExpressionPtr>();
			} else { // externalAosCall
				CallExprAddress call = match.as<CallExprAddress>();
				// filter out builtins
				if(call->getNodeManager().getLangBasic().isBuiltIn(call->getFunctionExpr()))
					return;

				// check if oldVar is accessed
				if(!oldVarPattern.match(am["oldVarCandidate"].getValue()))
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
			replacements[toBeReplaced] = cmpInitAndMarshall;

			// save the address where the marshalling was inserted
			marshalled.push_back(match.as<StatementAddress>());
		});
	}

	return marshalled;
}

StatementPtr AosToSoa::generateUnmarshalling(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for(NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = refAccess(oldVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		ExpressionPtr soaAccess = builder.deref(refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName()));
		loopBody.push_back(builder.assign(aosAccess, soaAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.literal(start->getType(), "1"), builder.compoundStmt(loopBody));

}

std::vector<StatementAddress> AosToSoa::addUnmarshalling(const ExpressionMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const std::vector<StatementAddress>& beginV, ExpressionMap& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> unmarshallingPoints;
	StatementAddress begin = beginV.empty() ? StatementAddress() : beginV.front();

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern aosVariable = pattern::atom(oldVar);//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern readFromStruct = pirp::assignment(pattern::any, pattern::var("struct", pirp::callExpr(pirp::structType(*pattern::any),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)));
		pattern::TreePattern readFromStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << readFromStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				pattern::aT(pattern::atom(oldVar)) << *pattern::any);

		pirp::matchAllPairs(readFromStructInLoop | externalAosCall, NodeAddress(toTransform), [&](const NodeAddress& node, pattern::AddressMatch am) {
			ExpressionPtr start, end;

//std::cout << begin << " < " << node << " -> " << (begin < node) << std::endl;
			if(begin && !(begin < node)) {
				// do not unmarshall before marshalling
				return;
			}

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
			replacements[toBeReplaced] = cmpUnmarshallAndExternalCall;
		});
	}

	return unmarshallingPoints;
}

// obtain return type
//TypeList returnTypes;
//visitDepthFirstPrunable(newBody, [&](const NodePtr& cur) {
//	if (cur->getNodeType() == NT_ReturnStmt) {
//		returnTypes.push_back(cur.as<ReturnStmtPtr>()->getReturnExpr()->getType());
//	}
//	return cur->getNodeType() == NT_LambdaExpr || cur->getNodeCategory() != NC_Statement;
//});

ExpressionPtr AosToSoa::generateNewAccesses(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const StringValuePtr& member, const ExpressionPtr& index,
		const ExpressionPtr& structAccess) {
	IRBuilder builder(mgr);
	ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, structAccess,
			oldVar, newVar, false).as<ExpressionPtr>();
	ExpressionPtr replacement = refAccess(newStructAccess, index, member);

	return replacement;
}

ExpressionMap AosToSoa::replaceAccesses(const ExpressionMap& varReplacements, const StructTypePtr& newStructType, const NodeAddress& toTransform,
		const std::vector<StatementAddress>& begin, const std::vector<StatementAddress>& end, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	ExpressionMap structures;

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern structMemberAccess =  pattern::var("call", pirp::compositeRefElem(pirp::arrayRefElem1D(pirp::refDeref(
				pattern::var("structAccess", pattern::aT(pattern::atom(oldVar)))), var("index", pattern::any)), pattern::var("member", pattern::any)));

		pattern::TreePattern structAccess = pattern::var("call", pirp::refDeref(pirp::arrayRefElem1D(pirp::refDeref(pattern::var("structAccess",
				pattern::aT(pattern::atom(oldVar)))), var("index", pattern::any))));
		pattern::TreePattern assignStructAccess = declOrAssignment(pattern::var("target",
				pirp::variable(pattern::aT(pirp::refType(pirp::structType(*pattern::any))))), structAccess);

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> c : newMemberAccesses) {
	//		ExpressionPtr old = builder.arrayRefElem()
	//	}
//		bool doSomething = begin.empty();

		visitDepthFirstPrunable(toTransform, [&](const StatementAddress& node)->bool {

//			if(!doSomething) {
//				if(!begin.empty() && node == begin.front()) {
//					doSomething = true;
//					return true;
//				}
//				return false;
//			}

//			if(!end.empty() && node == end.back()) {
//				doSomething = false;
//				return true;
//			}

			// do not touch marshalling and unmrashalling points
			if(contains(begin, node))
				return true;
			if(contains(end, node))
				return true;

			CallExprAddress call = node.isa<CallExprAddress>();

			if(call) {
				pattern::AddressMatchOpt match = structMemberAccess.matchAddress(node);
//std::cout << "\nCall: ";
//dumpPretty(node);
				if(match) {
//	assert(false);
					StringValuePtr member = builder.stringValue(match.get()["member"].getValue().as<LiteralPtr>()->getStringValue());
					ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
					ExpressionPtr structAccess = match.get()["structAccess"].getValue().as<ExpressionPtr>();
					ExpressionPtr replacement = generateNewAccesses(oldVar, newVar, member, index, structAccess);

					replacements[node] = replacement;
					return true;
				}
			}
			pattern::AddressMatchOpt match = assignStructAccess.matchAddress(node);

			if(match) {
				ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
				ExpressionPtr oldStructAccess = match.get()["structAccess"].getValue().as<ExpressionPtr>();

				ExpressionPtr inplaceUnmarshalled = generateByValueAccesses(oldVar, newVar, newStructType, index, oldStructAccess);
				if(match.get().isVarBound("decl"))
					replacements[match.get().getRoot()] = builder.declarationStmt(match.get()["target"].getValue().as<VariablePtr>(),
							builder.refVar(inplaceUnmarshalled));
				if(match.get().isVarBound("assignment"))
					replacements[match.get().getRoot()] = builder.assign(match.get()["target"].getValue().as<ExpressionPtr>(), inplaceUnmarshalled);
			}

			return false;
		});
	}

	return structures;
}

ExpressionPtr AosToSoa::generateByValueAccesses(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const StructTypePtr& newStructType,
		const ExpressionPtr& index, const ExpressionPtr& oldStructAccess) {
	IRBuilder builder(mgr);

	ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, oldStructAccess,
			oldVar, newVar, false).as<ExpressionPtr>();

	vector<std::pair<StringValuePtr, ExpressionPtr>> values;
	for(NamedTypePtr memberType : newStructType->getElements()) {
		StringValuePtr memberName = memberType->getName();
		ExpressionPtr arrayAccess = builder.deref(refAccess(newStructAccess, index, memberName));
		values.push_back(std::make_pair(memberName, arrayAccess));
	}

	return builder.structExpr(values);
}

void AosToSoa::updateScalarStructAccesses(NodePtr& toTransform) {
	IRBuilder builder(mgr);

}

/*
void AosToSoa::replaceStructPointers(const pattern::AddressMatchOpt& match, const VariablePtr& newVar, std::map<NodeAddress, NodePtr>& replacements,
		ExpressionMap& structures) {
	IRBuilder builder(mgr);

//std::cout << " addr " << match.get().getRoot() << std::endl;
	ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
	TypePtr newStructType = newVar.getType().as<RefTypePtr>()->getElementType();
	TupleTypePtr tty = builder.tupleType(toVector(newStructType, index->getType()));

	TupleExprPtr tupleThing = builder.tupleExpr(builder.deref(newVar), index);
	VariablePtr newTargetVar = builder.variable(builder.refType(tty));
	if(match.get().isVarBound("decl"))
		replacements[match.get().getRoot()] = builder.declarationStmt(newTargetVar, builder.refVar(tupleThing));
	if(match.get().isVarBound("assignment"))
		replacements[match.get().getRoot()] = builder.assign(newTargetVar, tupleThing);

	structures[match.get()["target"].getValue().as<ExpressionPtr>()] = newTargetVar;
}

void AosToSoa::updateStructPointerAccesses(NodePtr& toTransform) {
	IRBuilder builder(mgr);

	pattern::TreePattern scalarStructAccess = pirp::callExpr(pattern::any,
			pirp::refDeref(pattern::var("tuple", pirp::variable(
			pirp::refType(pirp::tupleType(pirp::structType(*pattern::any) << pattern::any))))) << pattern::var("member", pattern::any) << pattern::any);
	NodeMap replacements;

	pirp::matchAllPairs(scalarStructAccess, toTransform, [&](const NodePtr& match, pattern::NodeMatch nm) {
		std::cout << "\tFOUND: " << std::endl;
		LiteralPtr member = nm["member"].getValue().as<LiteralPtr>();
		VariablePtr tupleVar = nm["tuple"].getValue().as<VariablePtr>();

		ExpressionPtr tupleAccess = builder.accessComponent(builder.deref(tupleVar), 0);
		ExpressionPtr structAcces = builder.accessMember(tupleAccess, member.getValue());
		ExpressionPtr arrayAccess = builder.arrayAccess(structAcces, builder.accessComponent(builder.deref(tupleVar), 1));

std::cout << match.as<ExpressionPtr>()->getType() << " tt " << arrayAccess->getType() << std::endl;
//dumpPretty(arrayAccess);
		replacements[match] = builder.castExpr(match.as<ExpressionPtr>()->getType(), arrayAccess);
//assert(false);
	});

	toTransform = core::transform::replaceAll(mgr, toTransform, replacements);
}
*/

StatementList AosToSoa::generateDel(const StatementAddress& stmt, const ExpressionPtr& oldVar, const ExpressionPtr& newVar,
		const StructTypePtr& newStructType) {
	StatementList deletes;

	pattern::TreePattern delVarPattern = pirp::refDelete(aT(pattern::atom(oldVar)));
	pattern::AddressMatchOpt match = delVarPattern.matchAddress(stmt);

	if(match) {
		IRBuilder builder(mgr);
		for(NamedTypePtr memberType : newStructType->getElements()) {
			deletes.push_back(builder.refDelete(refAccess(newVar, ExpressionPtr(), memberType->getName())));
		}
		deletes.push_back(stmt);

	}

	return deletes;
}

void AosToSoa::addNewDel(const ExpressionMap& varReplacements, const NodeAddress& toTransform, const StructTypePtr& newStructType,
		std::map<NodeAddress, NodePtr>& replacements) {

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;
		IRBuilder builder(mgr);

		visitDepthFirstInterruptible(toTransform, [&](const ExpressionAddress& call)->bool {
			StatementList deletes = generateDel(call, oldVar, newVar, newStructType);
			if(!deletes.empty()) {
			CompoundStmtPtr cmpDeletes = builder.compoundStmt(deletes);
				cmpDeletes.addAnnotation<RemoveMeAnnotation>();

				StatementPtr multiDelCompound = cmpDeletes;
				replacements[call] = multiDelCompound;

				return true;
			}

			return false;
		});
	}
}

void AosToSoa::updateTuples(ExpressionMap& varReplacements, const core::StructTypePtr& newStructType, const core::TypePtr& oldStructType,
		const NodeAddress& toTransform,	std::map<NodeAddress, NodePtr>& replacements, ExpressionMap& structures) {
	IRBuilder builder(mgr);
	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {

		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern structAccess =  pattern::var("structAccess", pattern::aT(pattern::atom(oldVar)));
		pattern::TreePattern tupleAccess = pattern::var("access", pirp::tupleRefElem(pattern::aT(pattern::var("tupleVar", pirp::variable())),
				pattern::var("idx"), pattern::var("type")));
		pattern::TreePattern tupleAssign = pirp::assignment(tupleAccess, structAccess);

		pirp::matchAllPairs(tupleAssign, toTransform, [&](const NodeAddress& node, pattern::AddressMatch match) {
			ExpressionAddress oldTupleAccess = match["access"].getValue().as<ExpressionAddress>();
			ExpressionAddress oldTupleVar = match["tupleVar"].getValue().as<ExpressionAddress>();
			TypePtr oldTupleVarType = oldTupleVar->getType();
			TypeAddress oldComponentType = match["type"].getValue().as<LiteralAddress>()->getType();
			ExpressionAddress idx = match["idx"].getValue().as<ExpressionAddress>();

			TypePtr newTupleType = core::transform::replaceAllGen(mgr, oldTupleVarType, oldStructType, newStructType, true);

			ExpressionPtr newTupleVar;

			if(structures.find(oldTupleVar) == structures.end()) {
				// check if local or global variable
				LiteralPtr globalTuple = oldTupleVar.isa<LiteralPtr>();
				newTupleVar = globalTuple ?
						builder.literal(globalTuple->getStringValue() + "_soa", newTupleType).as<ExpressionPtr>() :
						builder.variable(newTupleType).as<ExpressionPtr>();

				structures[oldTupleVar] = newTupleVar;
			} else {
				newTupleVar = structures[oldTupleVar];
			}

			ExpressionPtr oldRootVar = getRootVariable(node >> oldTupleVar, node).as<ExpressionPtr>();

			if(structures.find(oldRootVar) == structures.end()) {
				TypePtr newRootType = core::transform::replaceAllGen(mgr, oldRootVar->getType(), oldStructType, newStructType, true);
				LiteralPtr globalRoot = oldRootVar.isa<LiteralPtr>();
				ExpressionPtr newRootVar = globalRoot ?
						builder.literal(globalRoot->getStringValue() + "_soa", newRootType).as<ExpressionPtr>() :
						builder.variable(newRootType).as<ExpressionPtr>();

				structures[oldRootVar] = newRootVar;
			}

			ExpressionPtr newTupleAccess = core::transform::fixTypesGen(mgr, oldTupleAccess.as<ExpressionPtr>(), oldTupleVar.as<ExpressionPtr>(),
					newTupleVar, true);

			ExpressionPtr newStructAccess = core::transform::fixTypes(mgr, match["structAccess"].getValue().getAddressedNode(),
					oldVar, newVar, true).as<ExpressionPtr>();
//std::cout << "\nntv: \n";
//dumpPretty(oldStructType);
//dumpPretty(newStructType);

			newStructAccess = core::transform::replaceAllGen(mgr, newStructAccess, oldTupleVarType, newTupleType, false);

			replacements[node] = builder.assign(newTupleAccess, newStructAccess);
		});

	}
}

// unused
void AosToSoa::updateCopyDeclarations(ExpressionMap& varReplacements, const core::StructTypePtr& newStructType, const core::StructTypePtr& oldStructType,
		const NodeAddress& toTransform,	std::map<NodeAddress, NodePtr>& replacements, ExpressionMap& structures) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {

		const ExpressionPtr& oldVar = vr.first;
//		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern influencedDecl = pirp::declarationStmt(var("influencedVar", pirp::variable()), pattern::aT(pattern::atom(oldVar)));

		pirp::matchAllPairs(influencedDecl, toTransform, [&](const NodeAddress& node, pattern::AddressMatch match) {
			DeclarationStmtAddress decl = node.as<DeclarationStmtAddress>();

			if(replacements.find(decl) != replacements.end()) {
				return; // already handled
			}

			VariablePtr var = match["influencedVar"].getValue().as<VariablePtr>();

			TypePtr oldType = var->getType();
			TypePtr newType = core::transform::replaceAllGen(mgr, oldType, oldStructType, newStructType, false);

			if(oldType == newType) // check if the type depends on the tranformation
				return;
			VariablePtr updatedVar = builder.variable(newType);

			DeclarationStmtPtr updatedDecl = builder.declarationStmt(updatedVar, decl->getInitialization());

			replacements[decl] = updatedDecl;
		});
	}
}

void AosToSoa::doReplacements(const std::map<NodeAddress, NodePtr>& replacements, ExpressionMap& structures,
		const core::transform::TypeHandler& typeOfMemAllocHandler) {
	IRBuilder builder(mgr);

	if(!replacements.empty())
		toTransform = core::transform::replaceAll(mgr, replacements);

//	if(!structures.empty())
//		toTransform = core::transform::replaceVarsRecursive(mgr, toTransform, structures, false);
	while(!structures.empty()) {
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
	}
}


VariableAdder::VariableAdder(NodeManager& mgr, ExpressionMap& varReplacements)
		: mgr(mgr), varsToReplace(varReplacements),
		  typePattern(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))),
		  variablePattern(pirp::variable(typePattern) // local variable
				| pirp::literal(pirp::refType(typePattern), pattern::any)),// global variable
		  namedVariablePattern(var("variable", variablePattern)),
		  varWithOptionalDeref(namedVariablePattern | pirp::refDeref(namedVariablePattern) | pirp::scalarToArray(namedVariablePattern)){

//	for(std::pair<ExpressionPtr, ExpressionPtr> rep : varReplacements) {
//		std::cout<< *rep.first << " -> " << *rep.second << std::endl;
//	}
}

std::map<int, ExpressionPtr> VariableAdder::searchInArgumentList(const std::vector<ExpressionPtr>& args) {
	ExpressionPtr oldVarArg;
	ExpressionPtr oldVar, newVar;
	std::map<int, ExpressionPtr> indicesToNewArgs;
	int idx = 0;
//std::cout << "\nFUN\n";
	for(ExpressionPtr arg : args) {
//std::cout << "ARG: " << arg << std::endl;

		pattern::MatchOpt match = varWithOptionalDeref.matchPointer(arg);
		if(match) {
			auto varCheck = varsToReplace.find(match.get()["variable"].getValue().as<ExpressionPtr>());
			if(varCheck != varsToReplace.end()) {
				oldVarArg = arg;
				oldVar = varCheck->first;
				newVar = varCheck->second;

				ExpressionPtr newArg = core::transform::fixTypesGen(mgr, oldVarArg, oldVar, newVar, false);
				indicesToNewArgs[idx] = newArg;
			}
		}
		++idx;
	}

	return indicesToNewArgs;
}

const NodePtr VariableAdder::resolveElement(const core::NodePtr& element) {
	// stop recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;
	}

//	pattern::TreePattern typePattern = pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))));
//	pattern::TreePattern variablePattern = pirp::variable(typePattern) // local variable
//										| pirp::literal(pirp::refType(typePattern), pattern::any); // global variable
//	pattern::TreePattern namedVariablePattern = var("variable", variablePattern);
//	pattern::TreePattern isOldVarArg = namedVariablePattern | pirp::refDeref(namedVariablePattern);

	if(element.isa<CompoundStmtPtr>())
		return element->substitute(mgr, *this);

//	if(ForStmtPtr f = element.isa<ForStmtPtr>()) {
//std::cout << "found for\n";
//dumpPretty(f->getBody());
//		return element->substitute(mgr, *this);
//	}

	if(CallExprPtr call = element.isa<CallExprPtr>()) {

		LambdaExprPtr lambdaExpr = call->getFunctionExpr().isa<LambdaExprPtr>();

		if(!lambdaExpr)
			return element;

//dumpPretty(call);
//std::cout << "is builtin " << mgr.getLangBasic().isBuiltIn(lambdaExpr) << std::endl << "----------------------------------------------------------------\n\n";
		// do the substitution to take care of body and function calls inside the argument list
		CallExprPtr newCall = call->substitute(mgr, *this);

		// functions with no implementation are ignored
		if(mgr.getLangBasic().isBuiltIn(lambdaExpr))
			return newCall;

//std::cout << "checking a lambda with " << *varReplacements.begin() << std::endl;
		std::vector<ExpressionPtr> args(call->getArguments());

		std::map<int, ExpressionPtr> indicesToNewArgs = searchInArgumentList(args);

		// oldVar is not an argument, nothing will be done
		if(indicesToNewArgs.empty())
			return newCall;

		lambdaExpr = newCall->getFunctionExpr().as<LambdaExprPtr>();

		IRBuilder builder(mgr);
		FunctionTypePtr lambdaType = lambdaExpr->getType().as<FunctionTypePtr>();
		std::vector<TypePtr> funTyMembers = lambdaType->getParameterTypeList();
		std::vector<VariablePtr> params = lambdaExpr->getParameterList();

		for(std::pair<int, ExpressionPtr> itna : indicesToNewArgs) {
			int idx = itna.first;
			ExpressionPtr newArg  = itna.second;
			// get new variable from the previously created map
			VariablePtr newParam = varsToReplace[params[idx]].as<VariablePtr>();
//std::cout << params[idx] << std::endl;
			assert(newParam && "no replacement for parameter found");

			// if oldVar was an argument, newVar will be added too and search is continued in the called function
			args.push_back(newArg);
			//args.push_back(core::transform::replaceAll(mgr, oldVarArg, oldVar, newVar).as<ExpressionPtr>());
			// add it also to the new type of the lambda
			funTyMembers.push_back(newParam->getType());
			// add a new variable to the parameter list
			params.push_back(newParam);
		}

		FunctionTypePtr newFunType = builder.functionType(funTyMembers, lambdaType->getReturnType(), lambdaType->getKind());

		StatementPtr newBody = lambdaExpr->getBody()->substitute(mgr, *this);

		LambdaExprPtr newLambdaExpr = builder.lambdaExpr(newFunType, params, newBody);
		newCall = builder.callExpr(call->getType(), newLambdaExpr, args);

		//migrate possible annotations
		core::transform::utils::migrateAnnotations(lambdaExpr->getBody(), newBody);
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		core::transform::utils::migrateAnnotations(call, newCall);

		return newCall;
	}

	return element->substitute(mgr, *this);
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

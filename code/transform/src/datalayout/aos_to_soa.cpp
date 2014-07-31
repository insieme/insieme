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

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;

ExpressionPtr removeRefVar(ExpressionPtr refVar) {
//	std::cout << "remvoing var from " << refVar << std::endl;
	if(core::analysis::isCallOf(refVar, refVar->getNodeManager().getLangBasic().getRefVar())) {
//		dumpPretty(refVar.as<CallExprPtr>()[0]);
		return refVar.as<CallExprPtr>()[0];
	}

	return refVar;
}

TypePtr removeRef(TypePtr refTy) {
	if(RefTypePtr r = refTy.isa<RefTypePtr>())
		return r->getElementType();
	return refTy;
}

TypePtr removeRefArray(TypePtr refTy) {
	if(RefTypePtr r = refTy.isa<RefTypePtr>())
		if(ArrayTypePtr a = r->getElementType().isa<ArrayTypePtr>())
			return a->getElementType();
	return refTy;
}

TypePtr getBaseType(TypePtr type, StringValuePtr field) {
	if(RefTypePtr ref = type.isa<RefTypePtr>()) {
		return getBaseType(ref->getElementType(), field);
	}

	if(ArrayTypePtr arr = type.isa<ArrayTypePtr>()) {
		return getBaseType(arr->getElementType(), field);
	}

	if(StructTypePtr str = type.isa<StructTypePtr>()) {
		return getBaseType(str->getTypeOfMember(field), field);
	}

	return type;
}

ExpressionPtr valueAccess(ExpressionPtr thing, ExpressionPtr index, StringValuePtr field) {
	NodeManager& mgr = thing->getNodeManager();
	IRBuilder builder(mgr);

	if(RefTypePtr ref = thing->getType().isa<RefTypePtr>()) {
		if(ref->getElementType().isa<ArrayTypePtr>())
			return valueAccess(builder.deref(builder.arrayRefElem(thing, index)), index, field);

		return valueAccess(builder.deref(thing), index, field);
	}

	if(thing->getType().isa<ArrayTypePtr>()) {
		if(!index)
			return thing;

		return valueAccess(builder.arraySubscript(thing, index), index, field);
	}

	if(thing->getType().isa<StructTypePtr>()) {
		if(!field)
			return thing;

		return valueAccess(builder.accessMember(thing, field), index, field);
	}

	return thing;
}

ExpressionPtr refAccess(ExpressionPtr thing, ExpressionPtr index, StringValuePtr field) {
	NodeManager& mgr = thing->getNodeManager();
	IRBuilder builder(mgr);

	if(RefTypePtr ref = thing->getType().isa<RefTypePtr>()) {

		if(builder.getLangBasic().isPrimitive(ref->getElementType()))
			return thing;

		if(ref->getElementType().isa<ArrayTypePtr>()) {
			if(!index)
				return thing;

			return refAccess(builder.arrayRefElem(thing, index), index, field);
		}

		if(ref->getElementType().isa<StructTypePtr>()) {
			if(!field)
				return thing;

			return refAccess(builder.refMember(thing, field), index, field);
		}

		return refAccess(builder.deref(thing), index, field);
	}

	return thing;

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

pattern::TreePattern declOrAssignment(pattern::TreePattern lhs, pattern::TreePattern rhs) {
	pattern::TreePattern assign = pattern::var("assignment", pirp::assignment(lhs, rhs));
	pattern::TreePattern decl = pattern::var("decl", pirp::declarationStmt(lhs, rhs));

	return assign | decl;
}

AosToSoa::AosToSoa(core::NodePtr& toTransform) : mgr(toTransform->getNodeManager()){
	IRBuilder builder(mgr);

	std::map<VariablePtr, RefTypePtr> structs = findCandidates(toTransform);

	std::map<NodeAddress, NodePtr> replacements;
	NodeAddress tta(toTransform);

	for(std::pair<VariablePtr, RefTypePtr> candidate : structs) {
		VariableAddress oldVar;
		VariablePtr newVar;
		StructTypePtr newStructType;
		ExpressionPtr nElems;

		std::map<VariablePtr, VariablePtr> varMapping;

		pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(mgr.getLangBasic().getArrayCreate1D(),
				pattern::any << var("nElems", pattern::any))));

		visitDepthFirst(tta, [&](const DeclarationStmtAddress& decl) {
			VariableAddress check = decl->getVariable();

			if(*check != *candidate.first)
				return;

			oldVar = check;

			StructTypePtr oldType = candidate.second->getElementType().as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();

			newStructType = createNewType(oldType);

			// check if the declaration does an allocation and try to extract the number of elements in that case
			pattern::MatchOpt match = allocPattern.matchPointer(decl->getInitialization());
			if(match) {
				nElems = match.get()["nElems"].getValue().as<ExpressionPtr>();
			}

			RefTypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), candidate.second, newStructType).as<RefTypePtr>();
			newVar = builder.variable(newType);

			// replace declaration with compound statement containing the declaration itself, the
			StatementList allDecls;
			allDecls.push_back(decl);
			allDecls.push_back(builder.declarationStmt(newVar, builder.undefinedVar(newType)));

			// split up initialization expressions

			for(NamedTypePtr memberType : newStructType->getElements()) {
				allDecls.push_back(builder.assign(builder.refMember(newVar, memberType->getName()),
						updateInit(removeRefVar(decl->getInitialization()), oldType, getBaseType(memberType->getType(), memberType->getName()))));
			}
			replacements[decl] = builder.compoundStmt(allDecls);

			varMapping[oldVar] = newVar;

			toTransform = core::transform::replaceAll(mgr, replacements);

			VariableMap varReplacements;
			VariableAdder varAdd(oldVar, newVar, varReplacements);
			toTransform = varAdd.mapElement(0, toTransform);

			tta = NodeAddress(toTransform);
			replacements.clear();

			// assignments to the entire struct should be ported to the new sturct members
			replaceAssignments(varReplacements, newStructType, tta, allocPattern, nElems, replacements);

			//introducing marshalling
			std::vector<StatementAddress> begin = addMarshalling(varReplacements, newStructType, tta, nElems, replacements);

			//introducing unmarshalling
			std::vector<StatementAddress> end = addUnmarshalling(varReplacements, newStructType, tta, begin.front(), nElems, replacements);

			//free memory of the new variable
			addNewDel(varReplacements, tta, newStructType, replacements);

			//replace array accesses
			ExpressionMap structures = replaceAccesses(varReplacements, tta, begin, end, replacements);

			toTransform = core::transform::replaceAll(mgr, replacements);

			if(!structures.empty())
				toTransform = core::transform::replaceVarsRecursive(mgr, toTransform, structures, false);
		});
	}

	// fix all accesses to now marshalled scalar structs
	updateScalarStructAccesses(toTransform);

//dumpPretty(toTransform);
}

std::map<VariablePtr, RefTypePtr> AosToSoa::findCandidates(NodePtr toTransform) {
	std::map<VariablePtr, RefTypePtr> structs;

	pattern::TreePattern structVar = pirp::variable(pattern::aT(var("structType", pirp::refType(pirp::arrayType(
			pirp::structType(*pattern::any))))));

	pirp::matchAllPairs(structVar, toTransform, [&](const NodePtr& match, pattern::NodeMatch nm) {
		structs[match.as<VariablePtr>()] = nm["structType"].getValue().as<RefTypePtr>();
	});

	return structs;
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

ExpressionPtr AosToSoa::updateInit(ExpressionPtr init, TypePtr oldType, TypePtr newType) {
	return core::transform::replaceAll(mgr, init, oldType, newType).as<ExpressionPtr>();
}

void AosToSoa::replaceAssignments(const VariableMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<VariablePtr, VariablePtr> vr : varReplacements) {
		const VariablePtr& oldVar = vr.first;
		const VariablePtr& newVar = vr.second;
		visitDepthFirst(toTransform, [&](const CallExprAddress& call) {

			if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
				if(*oldVar == *call[0]) {
					// check if the declaration does an allocation and try to extract the number of elements in that case
					pattern::MatchOpt match = allocPattern.matchPointer(call[1]);
					if(match) {
						nElems = match.get()["nElems"].getValue().as<ExpressionPtr>();
					}

					StatementList allAssigns;

					allAssigns.push_back(call);

					for(NamedTypePtr memberType : newStructType->getElements()) {
						allAssigns.push_back(builder.assign(builder.refMember(newVar, memberType->getName()),
								updateInit(call[1], removeRefArray(call[1].getType()), removeRefArray(memberType->getType()))));
					}

					replacements[call] = builder.compoundStmt(allAssigns);
	//				newStuff = core::transform::insertAfter(mgr, call, allAssigns);
				}
			}
		});
	}

}

StatementPtr AosToSoa::generateMarshalling(const VariablePtr& oldVar, const VariablePtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for(NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = valueAccess(oldVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		ExpressionPtr soaAccess = refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		loopBody.push_back(builder.assign(soaAccess, aosAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.literal(start->getType(), "1"), builder.compoundStmt(loopBody));
}

std::vector<StatementAddress> AosToSoa::addMarshalling(const VariableMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> marshalled;

	for(std::pair<VariablePtr, VariablePtr> vr : varReplacements) {
		const VariablePtr& oldVar = vr.first;
		const VariablePtr& newVar = vr.second;

		pattern::TreePattern aosVariable = pattern::atom(oldVar);//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern assignToStruct = pirp::assignment(pattern::var("struct", pirp::callExpr(pirp::refType(pirp::structType(*pattern::any)),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)), pattern::any);
		pattern::TreePattern assignToStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << assignToStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				pattern::atom(oldVar) << *pattern::any);

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

				start = builder.literal(nElems->getType(), "0");
				end = nElems;
			}

			StatementPtr marshalling = generateMarshalling(oldVar, newVar, start, end, newStructType);

			StatementList initAndMarshall;
			initAndMarshall.push_back(match.getAddressedNode().as<StatementPtr>());
			initAndMarshall.push_back(marshalling);

			replacements[match] = builder.compoundStmt(initAndMarshall);

			// save the address where the marshalling was inserted
			marshalled.push_back(match.as<StatementAddress>());
		});
	}

	return marshalled;
}

StatementPtr AosToSoa::generateUnmarshalling(const VariablePtr& oldVar, const VariablePtr& newVar, const ExpressionPtr& start,
		const ExpressionPtr& end, const StructTypePtr& structType) {
	IRBuilder builder(mgr);

	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for(NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = refAccess(oldVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		ExpressionPtr soaAccess = valueAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		loopBody.push_back(builder.assign(aosAccess, soaAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.literal(start->getType(), "1"), builder.compoundStmt(loopBody));

}

std::vector<StatementAddress> AosToSoa::addUnmarshalling(const VariableMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const StatementAddress& begin, const ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	std::vector<StatementAddress> unmarshallingPoints;

	for(std::pair<VariablePtr, VariablePtr> vr : varReplacements) {
		const VariablePtr& oldVar = vr.first;
		const VariablePtr& newVar = vr.second;

		pattern::TreePattern aosVariable = pattern::atom(oldVar);//pirp::variable(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
		pattern::TreePattern readFromStruct = pirp::assignment(pattern::any, pattern::var("struct", pirp::callExpr(pirp::structType(*pattern::any),
				pattern::any, pattern::aT(pattern::var("variable", aosVariable)) << *pattern::any)));
		pattern::TreePattern readFromStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
				pattern::any, *pattern::any << readFromStruct << *pattern::any);

		pattern::TreePattern externalAosCall = pirp::callExpr(pirp::literal(pattern::any, pattern::any), *pattern::any <<
				pattern::aT(pattern::atom(oldVar)) << *pattern::any);

		pirp::matchAllPairs(readFromStructInLoop | externalAosCall, NodeAddress(toTransform), [&](const NodeAddress& node, pattern::AddressMatch am) {
			ExpressionPtr start, end;

			if(!(begin < node)) {
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

				start = builder.literal(nElems->getType(), "0");
				end = nElems;
			}

			StatementList unmarshallAndExternalCall;
			unmarshallAndExternalCall.push_back(generateUnmarshalling(oldVar, newVar, start, end, newStructType));

			unmarshallingPoints.push_back(node.as<StatementAddress>());

			auto check = replacements.find(node);
			// check if there is already a replacement for the corresponding node
			if(check != replacements.end()) {
				// if so, add to this replacement, assume the original call is already in the existing replacement
				CompoundStmtPtr oldReplacement = (*check).second.as<CompoundStmtPtr>();
				StatementList combinedReplacement(oldReplacement.getElements());
				combinedReplacement.insert(combinedReplacement.begin(), unmarshallAndExternalCall.begin(), unmarshallAndExternalCall.end());
				replacements[node] = builder.compoundStmt(combinedReplacement);
			} else {
				// add origninal call
				unmarshallAndExternalCall.push_back(node.as<StatementPtr>());
				replacements[node] = builder.compoundStmt(unmarshallAndExternalCall);
			}
		});
	}

	return unmarshallingPoints;
}


ExpressionMap AosToSoa::replaceAccesses(const VariableMap& varReplacements, const NodeAddress& toTransform,
		const std::vector<StatementAddress>& begin, const std::vector<StatementAddress>& end, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	ExpressionMap structures;

	for(std::pair<VariablePtr, VariablePtr> vr : varReplacements) {
		const VariablePtr& oldVar = vr.first;
		const VariablePtr& newVar = vr.second;

		pattern::TreePattern structMemberAccess =  pattern::var("call", pirp::compositeRefElem(pirp::arrayRefElem1D(pirp::refDeref(pattern::atom(oldVar)),
				var("index", pattern::any)), pattern::var("member", pattern::any)));

		pattern::TreePattern structAccess = pattern::var("call", pirp::arrayRefElem1D(pirp::refDeref(pattern::atom(oldVar)), var("index", pattern::any)));
		pattern::TreePattern assignStructAccess = declOrAssignment(pattern::var("lhs", pattern::var("target",
				pirp::variable(pirp::refType(pirp::structType(*pattern::any))))), structAccess);

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> c : newMemberAccesses) {
	//		ExpressionPtr old = builder.arrayRefElem()
	//	}
		bool doSomething = false;

		visitDepthFirstPrunable(toTransform, [&](const StatementAddress& node)->bool {

			if(!doSomething) {
				if(node == begin.front()) {
					doSomething = true;
					return true;
				}
				return false;
			}

			if(node == end.back()) {
				doSomething = false;
				return true;
			}

			// do not touch marshalling and unmrashalling points
			if(contains(begin, node))
				return true;
			if(contains(end, node))
				return true;

			CallExprAddress call = node.isa<CallExprAddress>();

			pattern::AddressMatchOpt match = structMemberAccess.matchAddress(node);

			if(call && match) {
				StringValuePtr member = builder.stringValue(match.get()["member"].getValue().as<LiteralPtr>()->getStringValue());
				ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
				ExpressionPtr replacement = builder.arrayRefElem(builder.deref(builder.refMember(newVar, member)), index);
				replacements[node] = replacement;
				return true;
			}

			match = assignStructAccess.matchAddress(node);

			if(match) {
				replaceScalarStructs(match, newVar, replacements, structures);
			}

			return false;
		});
	}

	return structures;
}

void AosToSoa::replaceScalarStructs(const pattern::AddressMatchOpt& match, const VariablePtr& newVar, std::map<NodeAddress, NodePtr>& replacements,
		ExpressionMap& structures) {
	IRBuilder builder(mgr);

//std::cout << " addr " << match.get().getRoot() << std::endl;
	ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
	TypePtr newStructType = newVar.getType().as<RefTypePtr>()->getElementType();
	TupleTypePtr tty = builder.tupleType(toVector(newStructType, index->getType()));

	TupleExprPtr tupleThing = builder.tupleExpr(builder.deref(newVar), index);
	VariablePtr newTargetVar = builder.variable(tty);
	if(match.get().isVarBound("decl"))
		replacements[match.get().getRoot()] = builder.declarationStmt(newTargetVar, builder.refVar(tupleThing));
	if(match.get().isVarBound("assignment"))
		replacements[match.get().getRoot()] = builder.assign(newTargetVar, tupleThing);

//dumpPretty(newTargetVar->getType());
	structures[match.get()["target"].getValue().as<ExpressionPtr>()] = newTargetVar;
}

void AosToSoa::updateScalarStructAccesses(NodePtr& toTransform) {
	IRBuilder builder(mgr);

	pattern::TreePattern scalarStructAccess = pirp::callExpr(pattern::any,
			pattern::var("tuple", pirp::variable(
			pirp::tupleType(pirp::structType(*pattern::any) << pattern::any))) << pattern::var("member", pattern::any) << pattern::any);
	NodeMap replacements;

	pirp::matchAllPairs(scalarStructAccess, toTransform, [&](const NodePtr& match, pattern::NodeMatch nm) {
		std::cout << "\tFOUND: " << std::endl;
dumpPretty(match);
		LiteralPtr member = nm["member"].getValue().as<LiteralPtr>();
		VariablePtr tupleVar = nm["tuple"].getValue().as<VariablePtr>();


		ExpressionPtr tupleAccess = builder.accessComponent(tupleVar, 0);
		ExpressionPtr structAcces = builder.accessMember(tupleAccess, member.getValue());
		ExpressionPtr arrayAccess = builder.arrayAccess(structAcces, builder.accessComponent(tupleVar, 1));

		replacements[match] = arrayAccess;
	});

	toTransform = core::transform::replaceAll(mgr, toTransform, replacements);
}

CompoundStmtPtr AosToSoa::generateDel(const StatementAddress& stmt, const VariablePtr& oldVar, const VariablePtr& newVar,
		const StructTypePtr& newStructType) {
	CompoundStmtPtr replacement;

	pattern::TreePattern delVarPattern = pirp::refDelete(aT(pattern::atom(oldVar)));

	pattern::AddressMatchOpt match = delVarPattern.matchAddress(stmt);

	if(match) {
		IRBuilder builder(mgr);
		StatementList deletes;
		for(NamedTypePtr memberType : newStructType->getElements()) {
			deletes.push_back(builder.refDelete(refAccess(newVar, ExpressionPtr(), memberType->getName())));
		}
		deletes.push_back(stmt);
		replacement = builder.compoundStmt(deletes);
	}

	return replacement;
}

void AosToSoa::addNewDel(const VariableMap& varReplacements, const NodeAddress& toTransform, const StructTypePtr& newStructType,
		std::map<NodeAddress, NodePtr>& replacements) {

	for(std::pair<VariablePtr, VariablePtr> vr : varReplacements) {
		const VariablePtr& oldVar = vr.first;
		const VariablePtr& newVar = vr.second;

		visitDepthFirstInterruptible(toTransform, [&](const ExpressionAddress& call)->bool {
			StatementPtr multiDelCompound = generateDel(call, oldVar, newVar, newStructType);
			if(multiDelCompound) {
				replacements[call] = multiDelCompound;

				return true;
			}

			return false;
		});
	}
}

const NodePtr VariableAdder::resolveElement(const core::NodePtr& element) {
	// stop recursion at type level
	if (element->getNodeCategory() == NodeCategory::NC_Type) {
		return element;
	}

	if(element.isa<CompoundStmtPtr>())
		return element->substitute(mgr, *this);

	if(CallExprPtr call = element.isa<CallExprPtr>()) {
		LambdaExprPtr lambdaExpr = call->getFunctionExpr().isa<LambdaExprPtr>();

		if(!lambdaExpr)
			return element;

		// functions with no implementation are ignored
		if(mgr.getLangBasic().isBuiltIn(lambdaExpr))
			return element;

		std::vector<ExpressionPtr> args(call->getArguments());

		pattern::TreePattern isOldVarArg = pattern::atom(oldVar) | pirp::refDeref(pattern::atom(oldVar));
		ExpressionPtr oldVarArg;

		int idx = 0;
		for(ExpressionPtr arg : args) {
			if(isOldVarArg.match(arg)) {
				oldVarArg = arg;
				break;
			}
			++idx;
		}

		// oldVar is not an argument, nothing will be done
		if(!oldVarArg)
			return element;

		IRBuilder builder(mgr);
		FunctionTypePtr lambdaType = lambdaExpr->getType().as<FunctionTypePtr>();
		std::vector<TypePtr> funTyMembers = lambdaType->getParameterTypeList();
		std::vector<VariablePtr> params = lambdaExpr->getParameterList();

		// if oldVar was an argument, newVar will be added too and search is continued in the called function
		args.push_back(core::transform::replaceAll(mgr, oldVarArg, oldVar, newVar).as<ExpressionPtr>());
		// add it also to the new type of the lambda
		funTyMembers.push_back(args.back()->getType());
		// add a new variable to the parameter list
		params.push_back(builder.variable(funTyMembers.back()));

		FunctionTypePtr newFunType = builder.functionType(funTyMembers, lambdaType->getReturnType(), lambdaType->getKind());

		VariableAdder vaForInnserScope(params[idx], params.back(), varReplacements);
		StatementPtr newBody = vaForInnserScope.mapElement(0, lambdaExpr->getBody()).as<StatementPtr>();

		LambdaExprPtr newLambdaExpr = builder.lambdaExpr(newFunType, params, newBody);
		CallExprPtr newCall = builder.callExpr(call->getType(), newLambdaExpr, args);

		//migrate possible annotations
		core::transform::utils::migrateAnnotations(lambdaExpr->getBody(), newBody);
		core::transform::utils::migrateAnnotations(lambdaExpr, newLambdaExpr);
		core::transform::utils::migrateAnnotations(call, newCall);

		return newCall;
	}

	return element;
}

} // datalayout
} // transform
} // insieme

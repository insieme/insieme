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
		return valueAccess(builder.arraySubscript(thing, index), index, field);
	}

	if(thing->getType().isa<StructTypePtr>()) {
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

		if(ref->getElementType().isa<ArrayTypePtr>())
			return refAccess(builder.arrayRefElem(thing, index), index, field);

		if(ref->getElementType().isa<StructTypePtr>())
			return refAccess(builder.refMember(thing, field), index, field);

		return refAccess(builder.deref(thing), index, field);
	}

	return thing;

}
AosToSoa::AosToSoa(core::NodePtr& toTransform) : mgr(toTransform->getNodeManager()){
	IRBuilder builder(mgr);

	std::map<VariablePtr, RefTypePtr> structs;

	pattern::TreePattern structVar = pirp::variable(pattern::aT(var("structType", pirp::refType(pirp::arrayType(
			pirp::structType(*pattern::any))))));

	pirp::matchAllPairs(structVar, toTransform, [&](const NodePtr& match, pattern::NodeMatch nm) {
		structs[match.as<VariablePtr>()] = nm["structType"].getValue().as<RefTypePtr>();
	});
/*
	std::map<VariablePtr, TypePtr> newStructTypes;
	for(std::pair<VariablePtr, RefTypePtr> struct_ : structs) {

//		std::cout << *struct_.first->getType() << std::endl;

		StructTypePtr oldType = struct_.second->getElementType().as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();
		NodeRange<NamedTypePtr> member = oldType->getElements();
		std::vector<NamedTypePtr> newMember;
		for(NamedTypePtr memberType : member) {
//			std::cout << "member: " << memberType << std::endl;
			newMember.push_back(builder.namedType(memberType->getName(), builder.refType(builder.arrayType(memberType->getType()))));

		}
		newStructTypes[struct_.first] = core::transform::replaceAll(mgr, struct_.first->getType(), struct_.second, builder.structType(newMember)).as<TypePtr>();

//std::cout << struct_.first->getType() << std::endl << newStructTypes[struct_.first] << std::endl;
	}
*/
	NodeMap replacements;
	NodeAddress tta(toTransform);
	std::map<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> newMemberAccesses;
	visitDepthFirst(tta, [&](const DeclarationStmtAddress& decl) {
		VariablePtr oldVar = decl->getVariable();
		if(structs.find(oldVar) != structs.end()) {
			StructTypePtr oldType = structs[oldVar]->getElementType().as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();
			NodeRange<NamedTypePtr> member = oldType->getElements();
			std::vector<NamedTypePtr> newMember;
			for(NamedTypePtr memberType : member) {
	//			std::cout << "member: " << memberType << std::endl;
				newMember.push_back(builder.namedType(memberType->getName(), builder.refType(builder.arrayType(memberType->getType()))));

			}
//			newStructTypes[struct_.first] = core::transform::replaceAll(mgr, struct_.first->getType(), struct_.second, builder.structType(newMember)).as<TypePtr>();


			RefTypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), structs[oldVar], builder.structType(newMember)).as<RefTypePtr>();
			VariablePtr newStruct = builder.variable(newType);

			// replace declaration with compound statement containing the declaration itself, the
			StatementList allDecls;
			allDecls.push_back(decl);
			allDecls.push_back(builder.declarationStmt(newStruct, builder.undefinedVar(newType)));

			// split up initialization expressions
			StructTypePtr newStructType = newType->getElementType().as<StructTypePtr>();

			// store information for future use
			newMemberAccesses[oldVar] = std::make_pair(newStruct, newStructType);

			for(NamedTypePtr memberType : newStructType->getElements()) {
				allDecls.push_back(builder.assign(builder.refMember(newStruct, memberType->getName()),
						updateInit(removeRefVar(decl->getInitialization()), oldType, getBaseType(memberType->getType(), memberType->getName()))));
//dumpPretty(removeRef(memberType->getType()));
			}
			replacements[decl] = builder.compoundStmt(allDecls);
//			newStuff = core::transform::insertAfter(mgr, decl, allDecls);
		}
	});


	// assignments to the entire struct should be ported to the new sturct members
	visitDepthFirst(tta, [&](const CallExprAddress& call) {
		if(core::analysis::isCallOf(call.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
			if(newMemberAccesses.find(call[0]) != newMemberAccesses.end()) {
				VariablePtr newStruct = newMemberAccesses[call[0]].first;
				StructTypePtr newStructType = newMemberAccesses[call[0]].second;

				StatementList allAssigns;

				allAssigns.push_back(call);

				for(NamedTypePtr memberType : newMemberAccesses[call[0]].second->getElements()) {

					allAssigns.push_back(builder.assign(builder.refMember(newStruct, memberType->getName()),
							updateInit(call[1], removeRefArray(call[1].getType()), removeRefArray(memberType->getType()))));
				}

				replacements[call] = builder.compoundStmt(allAssigns);
//				newStuff = core::transform::insertAfter(mgr, call, allAssigns);
			}
		}
	});



	//searching places to introduce marshalling
	std::map<StatementAddress, StatementAddress> doMarshall;
	std::map<ExpressionPtr, bool> checkMarshalling;

	pattern::TreePattern assignToStruct = pirp::assignment(pattern::var("struct", pirp::callExpr(pirp::refType(pirp::structType(*pattern::any)),
			pattern::any, pattern::aT(pattern::var("variable", pirp::variable(pattern::any))) << *pattern::any)), pattern::any);
	pattern::TreePattern assignToStructInLoop = pirp::forStmt(pattern::any, pattern::var("start", pattern::any), pattern::var("end", pattern::any),
			pattern::any, *pattern::any << assignToStruct << *pattern::any);


//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> oldToNew : newMemberAccesses) {
//		generateMarshalling(oldToNew.first.as<VariablePtr>(), oldToNew.second.first, builder.intLit(0), builder.intLit(100), oldToNew.second.second);
//	}

	pirp::matchAllPairs(assignToStructInLoop, NodeAddress(toTransform), [&](const NodeAddress& match, pattern::AddressMatch am) {
		StatementAddress insertAfter = match.as<StatementAddress>();

		if(newMemberAccesses.find(am["variable"].getValue().as<VariablePtr>()) != newMemberAccesses.end()) {
			VariablePtr oldVar = am["variable"].getValue().as<VariablePtr>();
			VariablePtr newVar = newMemberAccesses[oldVar].first;
			StructTypePtr newStructType = newMemberAccesses[oldVar].second;

			StatementPtr marshalling = generateMarshalling(oldVar, newVar, am["start"].getValue().as<ExpressionPtr>(),
					am["end"].getValue().as<ExpressionPtr>(), newStructType);

			StatementList initAndMarshall;
			initAndMarshall.push_back(match.getAddressedNode().as<StatementPtr>());
			initAndMarshall.push_back(marshalling);

			replacements[match] = builder.compoundStmt(initAndMarshall);

			//remember that this variable was marshaled
			checkMarshalling[oldVar] = true;
		}
	});

	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> c : newMemberAccesses) {
		assert(checkMarshalling.find(c.first) != checkMarshalling.end() && "didn't marshal all variables");
	}

	toTransform = core::transform::replaceAll(mgr, toTransform, replacements);
	dumpPretty(toTransform);
}

ExpressionPtr AosToSoa::updateInit(ExpressionPtr init, TypePtr oldType, TypePtr newType) {
	return core::transform::replaceAll(mgr, init, oldType, newType).as<ExpressionPtr>();
}

StatementPtr AosToSoa::generateMarshalling(VariablePtr oldVar, VariablePtr newVar, ExpressionPtr start, ExpressionPtr end, StructTypePtr structType) {
	IRBuilder builder(mgr);

//	pattern::TreePattern structTypePattern = pattern::aT(pirp::structType(*pattern::any));
//	pattern::MatchOpt stMatch = structTypePattern.matchPointer(oldVar->getType());
//	StructTypePtr structType = stMatch.get().getRoot().as<StructTypePtr>();


	std::vector<StatementPtr> loopBody;
	VariablePtr iterator = builder.variable(start->getType());

	for(NamedTypePtr memberType : structType->getElements()) {

		ExpressionPtr aosAccess = valueAccess(oldVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		ExpressionPtr soaAccess = refAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		loopBody.push_back(builder.assign(soaAccess, aosAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.intLit(1), builder.compoundStmt(loopBody));
}

} // datalayout
} // transform
} // insieme

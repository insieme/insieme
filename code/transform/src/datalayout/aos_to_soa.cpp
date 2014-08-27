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

#include "insieme/utils/annotation.h"

namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;

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

}


ExpressionAddress extractVariable(ExpressionAddress expr) {
	const lang::BasicGenerator& gen = expr->getNodeManagerPtr()->getLangBasic();

	if(expr->getNodeType() == NT_Variable) // return variable
		return expr;

	if(expr->getNodeType() == NT_Literal) // return literal, e.g. global variable
		return expr;

	if(CallExprAddress call = expr.isa<CallExprAddress>()) {
		if(gen.isSubscriptOperator(call->getFunctionExpr()))
			return expr;

		if(gen.isCompositeRefElem(call->getFunctionExpr())) {
			return expr;
		}
	}

	if(CastExprAddress cast = expr.isa<CastExprAddress>())
		return extractVariable(cast->getSubExpression());

	if(CallExprAddress call = expr.isa<CallExprAddress>()){
		return extractVariable(call->getArgument(0)); // crossing my fingers that that will work ;)

	}

	return expr;
}

/*
 * Returns either the expression itself or the expression inside a nest of ref.deref calls
 */
ExpressionAddress tryRemoveDeref(const ExpressionAddress& expr) {
	NodeManager& mgr = expr->getNodeManager();
	if(const CallExprAddress& call = expr.isa<CallExprAddress>()) {
		if(mgr.getLangBasic().isRefDeref(call->getFunctionExpr()))
			return tryRemoveDeref(call->getArgument(0));
	}
	return expr;
}

NodeAddress getRootVariable(const NodeAddress scope, NodeAddress var) {

//std::cout << "\nlooking for var " << *var << std::endl;
	// if the variable is a literal, its a global variable and should therefore be the root
	if(LiteralAddress lit = var.isa<LiteralAddress>()) {
		if(lit.getType().isa<RefTypeAddress>()) { // check if literal was a global variable in the input code
//std::cout << "found literal " << *var << std::endl;
		return var;
		}
	}

	// search in declaration in siblings
	NodeManager& mgr = var.getNodeManager();

	pattern::TreePattern localOrGlobalVar = pirp::variable() | pirp::literal(pirp::refType(pattern::any), pattern::any);
	pattern::TreePattern valueCopy = pattern::var("val", pirp::variable()) |
			pirp::callExpr(mgr.getLangBasic().getRefDeref(), pattern::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefNew(), pattern::var("val", pirp::variable())) |
			pirp::callExpr(mgr.getLangBasic().getRefVar(), pattern::var("val", pirp::variable()));

	vector<NodeAddress> childAddresses = scope.getChildAddresses();
	for(auto I = childAddresses.rbegin(); I != childAddresses.rend(); ++I) {
		NodeAddress child = *I;

		if(child.getDepth() > 4) {
			if(LambdaAddress lambda = child.isa<LambdaAddress>()) {
//	std::cout << "Lambda: " << lambda << "\n var " << var << std::endl;
				// if var is a parameter, continue search for declaration of corresponding argument in outer scope

//	for(int i = 1; i <= 4; ++i)
//		std::cout << "\nlp: " << utils::whatIs(lambda.getParentNode(i)) << std::endl;

				CallExprAddress call = lambda.getParentAddress(4).as<CallExprAddress>();
				NodeAddress nextScope, nextVar;

				for_range(make_paired_range(call->getArguments(), lambda->getParameters()->getElements()),
						[&](const std::pair<const ExpressionAddress, const VariableAddress>& pair) {
					if(*var == *pair.second) {
						nextScope = call.getParentAddress(1);
						nextVar = tryRemoveDeref(pair.first);
						return;
					}
				});
				return getRootVariable(nextScope, nextVar);
			}
		}

		if(DeclarationStmtAddress decl = child.isa<DeclarationStmtAddress>()) {
			if(*(decl->getVariable()) == *var) {
				// check if init expression is another variable
				if(pattern::AddressMatchOpt valueInit = valueCopy.matchAddress(decl->getInitialization())) {
					// if so, continue walk with other variable
					return getRootVariable(scope, valueInit->getVarBinding("val").getValue());
				}
//std::cout << "found decl of " << *var << std::endl;
				// if init is no other varable, the root is found
				return decl->getVariable();
			}
		}

		if(CallExprAddress call = var.isa<CallExprAddress>()) {
//std::cout << "\ncalling " << *call << std::endl;//"\narg0: " << *call->getArgument(0) << "\nvar: " << *extractVariable(call->getArgument(0)) << std::endl;
			if(call->getNodeManager().getLangBasic().isBuiltIn(call->getFunctionExpr())) {
//std::cout << "\tthing: " << *call->getFunctionExpr() << std::endl;
				return getRootVariable(scope, extractVariable(call->getArgument(0))); // crossing my fingers that that will work ;)
			}
		}

	}

	//check if we already reached the top
	if(scope.getDepth() <= 1)
		return NodeAddress();

	//compound expressions may not open a new scope, therefore declaration can be in the parent
	return getRootVariable(scope.getParentAddress(), var);
}

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
	pattern::TreePattern decl = pattern::var("decl", pirp::declarationStmt(lhs, pirp::refVar(rhs)));

	return assign | decl;
}

template<typename T>
ExpressionPtr expressionContainsMarshallingCandidate(const utils::map::PointerMap<ExpressionPtr, T>& candidates, const ExpressionAddress& expr,
		const NodeAddress& scope) {
		ExpressionAddress initVar = getRootVariable(scope, expr).as<ExpressionAddress>();
		if(initVar) for(std::pair<ExpressionPtr, T> candidate : candidates) {
			if(initVar == candidate.first)
			return candidate.first;
		}

	return ExpressionPtr();
}

AosToSoa::AosToSoa(core::NodePtr& toTransform) : mgr(toTransform->getNodeManager()){
	IRBuilder builder(mgr);

	utils::map::PointerMap<ExpressionPtr, RefTypePtr> structs = findCandidates(toTransform);

	std::map<NodeAddress, NodePtr> replacements;
	NodeAddress tta(toTransform);

	for(std::pair<ExpressionPtr, RefTypePtr> candidate : structs) {
		ExpressionPtr oldVar = candidate.first;
		ExpressionPtr newVar;
		StructTypePtr newStructType;
		ExpressionPtr nElems;

//std::cout << "Handling : " << candidate.first << std::endl;
//if(candidate.first.as<VariablePtr>()->getId() > 400) continue;
//		ExpressionMap varMapping;

		StructTypePtr oldType = candidate.second->getElementType().as<ArrayTypePtr>()->getElementType().as<StructTypePtr>();

		newStructType = createNewType(oldType);

		TypePtr newType = core::transform::replaceAll(mgr, oldVar->getType(), candidate.second, newStructType).as<TypePtr>();

		pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(mgr.getLangBasic().getArrayCreate1D(),
				pattern::any << var("nElems", pattern::any))));

		visitDepthFirst(tta, [&](const DeclarationStmtAddress& decl) {
			VariableAddress check = decl->getVariable();

			if(*check != *oldVar)
				return;

			// check if the declaration does an allocation and try to extract the number of elements in that case
			pattern::MatchOpt match = allocPattern.matchPointer(decl->getInitialization());
			if(match) {
				nElems = match.get()["nElems"].getValue().as<ExpressionPtr>();
			}

			newVar = builder.variable(newType);

			// replace declaration with compound statement containing the declaration itself, the declaration of the new variable and it's initialization
			StatementList allDecls;
			allDecls.push_back(decl);
			allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), builder.undefinedVar(newType)));

			// split up initialization expressions
			for(NamedTypePtr memberType : newStructType->getElements()) {
//std::cout << "\noldVar: " << *oldVar << "\nnewVarType: " << *newVar->getType() << std::endl;
//std::cout << "\ninitType: " << *decl->getInitialization()->getType() << std::endl;
//builder.refMember(newVar, memberType->getName());
//removeRefVar(decl->getInitialization());
				allDecls.push_back(builder.assign(builder.refMember(newVar, memberType->getName()),
						updateInit(removeRefVar(decl->getInitialization()), oldType, getBaseType(memberType->getType(), memberType->getName()))));
//			allDecls.push_back(removeRefVar(decl->getInitialization()));
			}

			CompoundStmtPtr cmpDecls = builder.compoundStmt(allDecls);
			cmpDecls.addAnnotation<RemoveMeAnnotation>();
			replacements[decl] = cmpDecls;

//			varMapping[oldVar] = newVar;

			toTransform = core::transform::replaceAll(mgr, replacements);
		});

		if(!newVar) // newVar has not been set, meaning the declaration of oldVar could not be found -> assuming oldVar was a global variable
			newVar = builder.literal(newType, oldVar.as<LiteralPtr>()->getStringValue() + "_soa");

		ExpressionMap varReplacements;

		varReplacements[oldVar] = newVar;
		VariableAdder varAdd(mgr, varReplacements);
		toTransform = varAdd.mapElement(0, toTransform);

		tta = NodeAddress(toTransform);
		replacements.clear();

		// assignments to the entire struct should be ported to the new sturct members
		replaceAssignments(varReplacements, newStructType, tta, allocPattern, nElems, replacements);

		//introducing marshalling
		std::vector<StatementAddress> begin = addMarshalling(varReplacements, newStructType, tta, nElems, replacements);
			//introducing unmarshalling
		std::vector<StatementAddress> end = addUnmarshalling(varReplacements, newStructType, tta, begin, nElems, replacements);

		//free memory of the new variable
		addNewDel(varReplacements, tta, newStructType, replacements);

		//replace array accesses
		ExpressionMap structures = replaceAccesses(varReplacements, tta, begin, end, replacements);

//for(std::pair<NodeAddress, NodePtr> r : replacements ) {
//	dumpPretty(r.first);
//	std::cout << ". . . . . . . . . . . . . . . . . . . . . . . \n";
//	dumpPretty(r.second);
//	std::cout << "---------------------------------------------------\n";
//
//	std::map<NodeAddress, NodePtr> tmp;
//	tmp[r.first] = r.second;
//	toTransform = core::transform::replaceAll(mgr, tmp);
//}


		if(!replacements.empty())
			toTransform = core::transform::replaceAll(mgr, replacements);

		if(!structures.empty())
			toTransform = core::transform::replaceVarsRecursive(mgr, toTransform, structures, false);
	}

	// fix all accesses to now marshalled scalar structs
	updateScalarStructAccesses(toTransform);

	// remove all inserted compound expressions
	NewCompoundsRemover remComp(mgr);
	toTransform = remComp.mapElement(0, toTransform);


//dumpPretty(toTransform);
}

utils::map::PointerMap<ExpressionPtr, RefTypePtr> AosToSoa::findCandidates(NodePtr toTransform) {
	utils::map::PointerMap<ExpressionPtr, RefTypePtr> structs;

	pattern::TreePattern structType = pirp::refType(pattern::aT(var("structType", pirp::refType(pirp::arrayType(pirp::structType(*pattern::any))))));

	pattern::TreePattern structVar = var("structVar", pirp::variable(structType));
	pattern::TreePattern structVarDecl = pirp::declarationStmt(structVar, var("init", pattern::any));

	pattern::TreePattern globalStruct = var("structVar", pirp::literal(structType, pattern::any));

	NodeAddress tta(toTransform);
	pirp::matchAllPairs(structVarDecl | globalStruct, tta, [&](const NodeAddress& match, pattern::AddressMatch nm) {
//std::cout << "Checking: " << *nm["structVar"].getValue() << std::endl;

		// check if marshalling is needed. It is not needed e.g. when the variable is initialized with an already marshalled one
		if(nm.isVarBound("init") && expressionContainsMarshallingCandidate(structs, nm["init"].getValue().as<ExpressionAddress>(), tta))
			return;

		structs[nm["structVar"].getValue().as<ExpressionPtr>()] = nm["structType"].getValue().as<RefTypePtr>();
//std::cout << "Adding: " << *nm["structVar"].getValue() << " as a candidate" << std::endl;
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

void AosToSoa::replaceAssignments(const ExpressionMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const pattern::TreePattern& allocPattern, ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;
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

					CompoundStmtPtr cmpAssigns = builder.compoundStmt(allAssigns);
					cmpAssigns.addAnnotation<RemoveMeAnnotation>();
					replacements[call] = cmpAssigns;
	//				newStuff = core::transform::insertAfter(mgr, call, allAssigns);
				}
			}
		});
	}

}

StatementPtr AosToSoa::generateMarshalling(const ExpressionPtr& oldVar, const ExpressionPtr& newVar, const ExpressionPtr& start,
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

StatementAddress getStatementReplacableParent(NodeAddress toBeReplacedByAStatement) {
	while(!toBeReplacedByAStatement.getParentAddress().isa<CompoundStmtAddress>())
		toBeReplacedByAStatement = toBeReplacedByAStatement.getParentAddress();

	return toBeReplacedByAStatement.as<StatementAddress>();
}

std::vector<StatementAddress> AosToSoa::addMarshalling(const ExpressionMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
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
				var("oldVarCandidate", pirp::exprOfType(pattern::aT(pirp::refType(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))))))
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

				start = builder.literal(nElems->getType(), "0");
				end = nElems;
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
		ExpressionPtr soaAccess = valueAccess(newVar, builder.castExpr(builder.getLangBasic().getUInt8(), iterator), memberType->getName());
		loopBody.push_back(builder.assign(aosAccess, soaAccess));
	}

	return builder.forStmt(builder.declarationStmt(iterator, start), end, builder.literal(start->getType(), "1"), builder.compoundStmt(loopBody));

}

std::vector<StatementAddress> AosToSoa::addUnmarshalling(const ExpressionMap& varReplacements, const StructTypePtr& newStructType,
		const NodeAddress& toTransform, const std::vector<StatementAddress>& beginV, const ExpressionPtr& nElems, std::map<NodeAddress, NodePtr>& replacements) {
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

				start = builder.literal(nElems->getType(), "0");
				end = nElems;
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


ExpressionMap AosToSoa::replaceAccesses(const ExpressionMap& varReplacements, const NodeAddress& toTransform,
		const std::vector<StatementAddress>& begin, const std::vector<StatementAddress>& end, std::map<NodeAddress, NodePtr>& replacements) {
	IRBuilder builder(mgr);
	ExpressionMap structures;

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

		pattern::TreePattern structMemberAccess =  pattern::var("call", pirp::compositeRefElem(pirp::arrayRefElem1D(pirp::refDeref(pattern::atom(oldVar)),
				var("index", pattern::any)), pattern::var("member", pattern::any)));

		pattern::TreePattern structAccess = pattern::var("call", pirp::refDeref(pirp::arrayRefElem1D(pirp::refDeref(pattern::atom(oldVar)),
				var("index", pattern::any))));
		pattern::TreePattern assignStructAccess = declOrAssignment(pattern::var("target",
				pirp::variable(pirp::refType(pirp::structType(*pattern::any)))), structAccess);

	//	for(std::pair<ExpressionPtr, std::pair<VariablePtr, StructTypePtr>> c : newMemberAccesses) {
	//		ExpressionPtr old = builder.arrayRefElem()
	//	}
		bool doSomething = begin.empty();

		visitDepthFirstPrunable(toTransform, [&](const StatementAddress& node)->bool {

			if(!doSomething) {
				if(!begin.empty() && node == begin.front()) {
					doSomething = true;
					return true;
				}
				return false;
			}

			if(!end.empty() && node == end.back()) {
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

void AosToSoa::replaceScalarStructs(const pattern::AddressMatchOpt& match, const ExpressionPtr& newVar, std::map<NodeAddress, NodePtr>& replacements,
		ExpressionMap& structures) {
	IRBuilder builder(mgr);

//std::cout << " addr " << match.get().getRoot() << std::endl;
//assert(false);

	ExpressionPtr index = match.get()["index"].getValue().as<ExpressionPtr>();
	StructTypePtr newStructType = newVar.getType().as<RefTypePtr>()->getElementType().as<StructTypePtr>();
	vector<std::pair<StringValuePtr, ExpressionPtr>> values;
	for(NamedTypePtr memberType : newStructType->getElements()) {
		StringValuePtr memberName = memberType->getName();
		ExpressionPtr oldStructAccess = builder.deref(builder.refMember(newVar, memberName));
		ExpressionPtr oldArrayAccess = builder.deref(builder.arrayAccess(oldStructAccess, index));
		values.push_back(std::make_pair(memberName, oldArrayAccess));
	}

	StructExprPtr inplaceUnmarshalled = builder.structExpr(values);
	if(match.get().isVarBound("decl"))
		replacements[match.get().getRoot()] = builder.declarationStmt(match.get()["target"].getValue().as<VariablePtr>(), builder.refVar(inplaceUnmarshalled));
	if(match.get().isVarBound("assignment"))
		replacements[match.get().getRoot()] = builder.assign(match.get()["target"].getValue().as<ExpressionPtr>(), inplaceUnmarshalled);
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

CompoundStmtPtr AosToSoa::generateDel(const StatementAddress& stmt, const ExpressionPtr& oldVar, const ExpressionPtr& newVar,
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

		CompoundStmtPtr cmpDeletes = builder.compoundStmt(deletes);
		cmpDeletes.addAnnotation<RemoveMeAnnotation>();

		replacement = cmpDeletes;
	}

	return replacement;
}

void AosToSoa::addNewDel(const ExpressionMap& varReplacements, const NodeAddress& toTransform, const StructTypePtr& newStructType,
		std::map<NodeAddress, NodePtr>& replacements) {

	for(std::pair<ExpressionPtr, ExpressionPtr> vr : varReplacements) {
		const ExpressionPtr& oldVar = vr.first;
		const ExpressionPtr& newVar = vr.second;

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

	pattern::TreePattern variablePattern = pirp::exprOfType(pattern::aT(pirp::refType(pirp::arrayType(pirp::structType(*pattern::any)))));
	if(element.isa<CompoundStmtPtr>())
		return element->substitute(mgr, *this);

	if(CallExprPtr call = element.isa<CallExprPtr>()) {
		LambdaExprPtr lambdaExpr = call->getFunctionExpr().isa<LambdaExprPtr>();

		if(!lambdaExpr)
			return element;

		// functions with no implementation are ignored
		if(mgr.getLangBasic().isBuiltIn(lambdaExpr))
			return element;

//std::cout << "checking a lambda with " << *varReplacements.begin() << std::endl;
		std::vector<ExpressionPtr> args(call->getArguments());

		pattern::TreePattern namedVariablePattern = var("variable", variablePattern);
		pattern::TreePattern isOldVarArg = namedVariablePattern | pirp::refDeref(namedVariablePattern);
		ExpressionPtr oldVarArg;
		ExpressionPtr oldVar, newVar;

		int idx = 0;
		for(ExpressionPtr arg : args) {
			pattern::MatchOpt match = isOldVarArg.matchPointer(arg);
			if(match) {
				auto varCheck = varReplacements.find(match.get()["variable"].getValue().as<ExpressionPtr>());
				if(varCheck != varReplacements.end()) {
					oldVarArg = arg;
					oldVar = varCheck->first;
					newVar = varCheck->second;
					break;
				}
			}
			++idx;
		}

		// do the subsitution to take care of body and function calls inside the argument list
		CallExprPtr newCall = call->substitute(mgr, *this);

		// oldVar is not an argument, nothing will be done
		if(!oldVarArg)
			return newCall;

		lambdaExpr = newCall->getFunctionExpr().isa<LambdaExprPtr>();

		IRBuilder builder(mgr);
		FunctionTypePtr lambdaType = lambdaExpr->getType().as<FunctionTypePtr>();
		std::vector<TypePtr> funTyMembers = lambdaType->getParameterTypeList();
		std::vector<VariablePtr> params = lambdaExpr->getParameterList();

		// if oldVar was an argument, newVar will be added too and search is continued in the called function
		ExpressionMap oldToNew;
		oldToNew[oldVar] = newVar;
		args.push_back(core::transform::fixTypesGen(mgr, oldVarArg, oldToNew, false));
		//args.push_back(core::transform::replaceAll(mgr, oldVarArg, oldVar, newVar).as<ExpressionPtr>());
		// add it also to the new type of the lambda
		funTyMembers.push_back(args.back()->getType());
		// add a new variable to the parameter list
		params.push_back(builder.variable(funTyMembers.back()));
		// add the old and new variable to the replacement list
		varReplacements[params[idx]] = params.back();

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

	if(DeclarationStmtPtr decl = element.isa<DeclarationStmtPtr>()) {
		StructTypePtr newType;

		// check type, not needed any more for correctness but should be a little bit faster on most programs
		if(RefTypePtr refTy = decl->getVariable()->getType().isa<RefTypePtr>())
			if(RefTypePtr refRefTy = refTy->getElementType().isa<RefTypePtr>())
				if(ArrayTypePtr refRefArrayTy = refRefTy->getElementType().isa<ArrayTypePtr>())
					if(StructTypePtr refRefArrayStructTy = refRefArrayTy->getElementType().isa<StructTypePtr>())
						newType = refRefArrayStructTy;
					else return element->substitute(mgr, *this);
				else return element->substitute(mgr, *this);
			else return element->substitute(mgr, *this);
		else return element->substitute(mgr, *this);

		pattern::TreePattern declaredVar = var("declaredVar", variablePattern);
		pattern::TreePattern usedVar = var("usedVar", variablePattern);
		pattern::TreePattern initValue = var("initValue", pattern::aT(usedVar));
		pattern::TreePattern declOfAdditionalVar = pirp::declarationStmt(declaredVar, pirp::refVar(initValue));
		pattern::MatchOpt match = declOfAdditionalVar.matchPointer(decl);

		if(match) {
			IRBuilder builder(mgr);
			VariablePtr oldVar = match.get()["usedVar"].getValue().as<VariablePtr>();

			auto check = varReplacements.find(oldVar);
			if(check == varReplacements.end())
				return element->substitute(mgr, *this);

			ExpressionPtr newVar = check->second;
			VariablePtr oldDeclaredVar = decl->getVariable();
			VariablePtr newDeclaredVar = builder.variable(newVar->getType());
			varReplacements[oldDeclaredVar] = newDeclaredVar;

			// getting the address in each array of the corresponding field to initialize the new variable
			vector<std::pair<StringValuePtr, ExpressionPtr>> values;
			for(NamedTypePtr value : newType->getEntries()) {
				ExpressionPtr fieldInit = core::transform::replaceAll(mgr, match.get()["initValue"].getValue(), oldVar,
						builder.accessMember(newVar, value->getName())).as<ExpressionPtr>();
				fieldInit = core::transform::fixTypesGen(mgr, fieldInit, ExpressionMap(), true );
				values.push_back(std::make_pair(value->getName(), fieldInit));
			}


			StructExprPtr fieldAccesses = builder.structExpr(values);

			return builder.declarationStmt(newDeclaredVar, builder.refVar(fieldAccesses));
					//core::transform::replaceAll(mgr, oldInit, oldLocalVar, varReplacements[oldVar]).as<ExpressionPtr>());
		}
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

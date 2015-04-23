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

#include "insieme/transform/datalayout/parallelSecTransform.h"
#include "insieme/transform/datalayout/datalayout_utils.h"


namespace insieme {
namespace transform {
namespace datalayout {

using namespace core;
namespace pirp = pattern::irp;

template<class Baseclass>
ExprAddressRefTypeMap ParSecTransform<Baseclass>::findCandidates(const NodeAddress& toTransform) {
	ExprAddressRefTypeMap structs;

	NodeManager& m = Baseclass::mgr;
	IRBuilder builder(m);

//for(std::pair<ExpressionAddress, StatementPtr> dudu : varsToPropagate) {
//	std::cout << "      things " << dudu.first << " " << *dudu.first << std::endl;
//}
	core::visitBreadthFirst(toTransform, [&](const ExpressionAddress& expr) {
		// adding arguments which use a tuple member expression as argument which's tuple member has been replaced already to replace list
		if(CallExprAddress call = expr.isa<CallExprAddress>()) {
			if(!core::analysis::isCallOf(call.getAddressedNode(), m.getLangBasic().getTupleMemberAccess()))
				return;

			// check if tuple argument has a member which will be updated
			ExpressionAddress oldRootVar = getRootVariable(call, call->getArgument(0)).as<ExpressionAddress>();
			auto newRootVarIter = varsToPropagate.find(oldRootVar);
//std::cout << "\nat the tuple member access " << oldRootVar << "\n";

			if(newRootVarIter != varsToPropagate.end()) { // tuple has been updated, check if it was the current field
				ExpressionPtr newRootVar = newRootVarIter->second.as<ExpressionPtr>();

				RefTypePtr newType = getBaseType(newRootVar->getType()).as<TupleTypePtr>()->getElement(
						call->getArgument(1).as<LiteralPtr>()->getValueAs<unsigned>()).as<RefTypePtr>();
				TypePtr oldType = call->getArgument(2)->getType().as<GenericTypePtr>()->getTypeParameter(0);
//std::cout << "Creating var of new type: " << newType << std::endl;

				// check if and update is needed
				if(newType == oldType) {
//std::cout << "no update needed\n";
					return;
				}

				ExpressionAddress argument = call.getParentAddress(2).isa<CallExprAddress>();
				CallExprAddress parent = argument.getParentAddress(1).isa<CallExprAddress>();
				if(!parent){
					return;
				}

				LambdaExprAddress lambda = parent->getFunctionExpr().isa<LambdaExprAddress>();

				if(!lambda){
					return;
				}

				globalLambdas.push_back(lambda);

				for_range(make_paired_range(parent->getArguments(), lambda->getLambda()->getParameters()->getElements()),
						[&](const std::pair<const core::ExpressionAddress, const core::VariableAddress>& pair) {
					if(pair.first == argument) {// found the argument which will be updated
						// create replacement for corresponding parameter
//						RefTypePtr newParamType = newType->getElementType().as<ArrayTypePtr>()->getElementType().as<RefTypePtr>();
//						VariablePtr newParam = builder.variable(newParamType);

						// add corresponding parameter to update list
						structs[pair.second] = pair.first->getType().as<RefTypePtr>();
//						varsToPropagate[pair.second] = newParam;
//std::cout << ": \nAdding: " << pair.second << " " << *pair.second << " - " << structs.size() << std::endl;
//std::cout << ": from: " << getDeclaration(call->getArgument(0)) << " " << *oldRootVar << " - " << structs.size() << std::endl;
						NodeManager& m = call->getNodeManager();
						ExpressionAddress localTuple = getDeclaration(call->getArgument(0));
						varReplacements[localTuple] = builder.variable(
							core::transform::replaceAllGen(m, localTuple->getType().getAddressedNode(),
							builder.refType(builder.arrayType(oldStructType)), newStructType));
//replacements[localTuple] = varsToPropagate[localTuple];
//						structs[getDeclaration(call->getArgument(0))] = pair.first->getType().as<RefTypePtr>();
					}
				});
			}
		}

		// propagating variables to be replaced through job expressions
		if(JobExprAddress job = expr.isa<JobExprAddress>()) {

            assert_true(job->getDefaultExpr().isa<BindExprAddress>()) << "expected bind expression inside of job";

			CallExprAddress parallelCall = job->getDefaultExpr().isa<BindExprAddress>()->getCall();

			if(!parallelCall)
				return;

			LambdaExprAddress parallelLambda = parallelCall->getFunctionExpr().isa<LambdaExprAddress>();
			if(!parallelLambda)
				return;

			for_range(make_paired_range(parallelCall->getArguments(), parallelLambda->getParameterList()->getElements()),
					[&](const std::pair<const ExpressionAddress, const VariableAddress>& pair) {
				ExpressionAddress newArg = getDeclaration(pair.first);
//std::cout << "Looking for " << newArg << " " << *newArg << std::endl;
//				auto newArgIter = varsToPropagate.find(newArg);
//				if(newArgIter != varsToPropagate.end()) {
//					structs[pair.second] = newArgIter->second->getType().as<RefTypePtr>();
//					varsToPropagate[pair.second] = newArgIter->second;
//std::cout << "Found in VARreplacements: " << newArg << " -> " << structs.size() << std::endl;
//				}

				auto newArgIter1 = structs.find(newArg);
				if(newArgIter1 != structs.end()) {
					structs[pair.second] = newArgIter1->second.as<RefTypePtr>();
//std::cout << "adding in jobREPLACEMENTS: " << pair.second << " -> " << *pair.second << std::endl;
				}
			});
		}
	});

	return structs;
}

template<class Baseclass>
StatementList ParSecTransform<Baseclass>::generateNewDecl(const ExprAddressMap& varReplacements, const DeclarationStmtAddress& decl, const StatementPtr& newVar,
		const StructTypePtr& newStructType,	const StructTypePtr& oldStructType, const ExpressionPtr& nElems) {
	IRBuilder builder(Baseclass::mgr);

	// replace declaration with compound statement containing only the declaration of the new variable and its initialization
	StatementList allDecls;

	NodeMap inInitReplacementsInCaseOfNovarInInit;
	inInitReplacementsInCaseOfNovarInInit[oldStructType] = newStructType;
	// divide initialization size by tilesize
	if(nElems) inInitReplacementsInCaseOfNovarInInit[nElems] = builder.div(nElems, builder.uintLit(84537493));

	allDecls.push_back(builder.declarationStmt(newVar.as<VariablePtr>(), Baseclass::updateInit(varReplacements, decl->getInitialization(),
		inInitReplacementsInCaseOfNovarInInit)));

	return allDecls;
}

template<class Baseclass>
ParSecTransform<Baseclass>::ParSecTransform(core::NodePtr& toTransform, ExprAddressMap& varsToPropagate, std::map<NodeAddress, NodePtr>& replacements,
		const StructTypePtr& newStructType, const StructTypePtr& oldStructType)
		: Baseclass(toTransform), varsToPropagate(varsToPropagate), replacements(replacements), newStructType(newStructType), oldStructType(oldStructType) {}

// template initializations
template ParSecTransform<DatalayoutTransformer>::ParSecTransform(NodePtr& toTransform, ExprAddressMap& varsToPropagate, std::map<NodeAddress,
		NodePtr>& replacements, const StructTypePtr& newStructType, const StructTypePtr& oldStructType);
template ParSecTransform<AosToTaos>::ParSecTransform(NodePtr& toTransform, ExprAddressMap& varsToPropagate, std::map<NodeAddress, NodePtr>& replacements,
		const StructTypePtr& newStructType, const StructTypePtr& oldStructType);
template ParSecTransform<AosToSoa>::ParSecTransform(NodePtr& toTransform, ExprAddressMap& varsToPropagate, std::map<NodeAddress, NodePtr>& replacements,
		const StructTypePtr& newStructType, const StructTypePtr& oldStructType);

template<class Baseclass>
void ParSecTransform<Baseclass>::transform() {
	NodeManager& m = Baseclass::mgr;
	IRBuilder builder(m);
	const NodeAddress tta(Baseclass::toTransform);
	std::vector<std::pair<ExprAddressSet, RefTypePtr>> toReplaceLists = Baseclass::createCandidateLists(tta);

	pattern::TreePattern allocPattern = pattern::aT(pirp::refNew(pirp::callExpr(m.getLangBasic().getArrayCreate1D(),
			pattern::any << var("nElems", pattern::any))));


	for(std::pair<ExprAddressSet, RefTypePtr> toReplaceList : toReplaceLists) {
		ExpressionMap nElems;

		for(ExpressionAddress oldVar : toReplaceList.first) {
			TypePtr newType = core::transform::replaceAll(m, oldVar->getType(), oldStructType,
					newStructType).as<TypePtr>();
//std::cout << "NT: " << newStructType << " var " << oldVar << " " << *oldVar << std::endl;

			// check if local or global variable
			LiteralPtr globalVar = oldVar.isa<LiteralPtr>();

			// create new variables, local or global
			varReplacements[oldVar] = globalVar ?
					builder.literal(globalVar->getStringValue() + "_soa", newType).as<ExpressionPtr>() :
					builder.variable(newType).as<ExpressionPtr>();
		}

		// replacing the declarations of the old variables with new ones
		Baseclass::addNewDecls(varReplacements, newStructType, oldStructType, tta, allocPattern, nElems, replacements);

		const std::vector<core::StatementAddress> begin, end;
		//replace array accesses
		Baseclass::replaceAccesses(varReplacements, newStructType, tta, begin, end, replacements);

		// assignments to the entire struct should be ported to the new struct members
//TODO	replaceAssignments(varReplacements, newStructType, oldStructType, tta, pattern::TreePattern(), nElems, replacements);

//		for(std::pair<core::NodeAddress, core::NodePtr> r : replacements) {
//			std::cout << "\nfrom ";
//			dumpPretty(r.first);
//			std::cout << "to ";
//			dumpPretty(r.second);
//		}

		//replace arguments
		for(std::pair<ExpressionAddress, StatementPtr> vr : varReplacements) {
//			std::cout << "from " << *vr.first << " to " << *vr.second << std::endl;

			if(vr.first.isa<VariableAddress>())
				visitDepthFirst(vr.first.getParentAddress(2), [&](const VariableAddress& var) {
					if(compareVariables(vr.first, var)) {
						replacements[var] = vr.second;
					}
				});
		}

	}
}


} // datalayout
} // transform
} // insieme

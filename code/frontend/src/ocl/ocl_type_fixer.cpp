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

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/ocl/ocl_type_fixer.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

bool TypeFixer::isClType(TypePtr type) {
	RefTypePtr refTy = type.isa<RefTypePtr>();
	if(refTy) return isClType(refTy->getElementType());

	if(type.isa<StructTypePtr>())
		return false;
	return type->toString().find("array<_cl_") != string::npos;
}

void TypeFixer::cleanStructures(const StructTypePtr& st, NodeMap& ptrReplacements) {
	vector<NamedTypePtr> newMembers;
	bool updated = false;
	for(NamedTypePtr m : st->getElements()) {
		if(isClType(m->getType())) {
			updated = true;
		} else
			newMembers.push_back(m);
	}

	if(updated) {
		NodeManager& mgr = st->getNodeManager();
		IRBuilder builder(mgr);
		ptrReplacements[st] = builder.structType(newMembers);
	}
}

void TypeFixer::removeClVars() {
	NodeMapping* h;
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = builder.getLangBasic();

	// removes cl_* variables from argument lists of lambdas
	auto cleaner = makeLambdaMapper([&](unsigned index, const NodePtr& element)->NodePtr{
		if (element->getNodeCategory() == NodeCategory::NC_Type) {
			// stop recursion at type level
			return element;
		}

		// remove declarations of cl_* variables
		if(const DeclarationStmtPtr& decl = element.isa<DeclarationStmtPtr>()) {
			TypePtr varType = decl->getVariable()->getType();
			TypePtr initType = decl->getInitialization()->getType();
			if(isClType(varType) || isClType(initType)) {
				return builder.getNoOp();
			}
			return element->substitute(builder.getNodeManager(), *h);
		}

		if(const CallExprPtr& call = element.isa<CallExprPtr>()) {

			// update lambdas which have cl_* arguments
			if(const LambdaExprPtr& lambda = call->getFunctionExpr().isa<LambdaExprPtr>()) {
				ExpressionList newArgs;
				core::VariableList newParams;
				const core::VariableList& oldParams = lambda->getParameterList()->getElements();
				TypeList paramTypes;
				bool update = false;
				int cnt = 0;

				for_each(call->getArguments(), [&](const ExpressionPtr& arg){
					// do nothing if the argument type is not a cl_* type
					if(!isClType(arg->getType())) {
						newArgs.push_back(arg);
						newParams.push_back(oldParams.at(cnt));
						paramTypes.push_back(oldParams.at(cnt)->getType());
					} else {
						// do not port cl_* types to the new type
						update = true;
					}
					++cnt;
				});
				if(update) {
					const LambdaExprPtr newLambda = builder.lambdaExpr(builder.functionType(paramTypes, call->getType()), newParams, lambda->getBody());
					return builder.callExpr(call->getType(), newLambda, newArgs)->substitute(builder.getNodeManager(), *h);
				}
			}

			// drop ocl functions
//if(call->getFunctionExpr()->toString().find("cl") == 0u) {
//std::cout << dumpPretty(call) << "dropping " << utils::whatIs(call->getFunctionExpr()) << std::endl;
//assert_fail();
//	}
			if(gen.isRefAssign(call->getFunctionExpr())) {
				TypePtr lhsTy = call->getArgument(0)->getType();
				TypePtr rhsTy = call->getArgument(1)->getType();
				if(isClType(lhsTy) || isClType(rhsTy)) {
					return builder.getNoOp();
				}
			}
		}

		return element->substitute(builder.getNodeManager(), *h);
	});

	h = &cleaner;
	prog = h->map(0, prog);

}

void TypeFixer::fixDecls(NodeAddress pA, TypePtr type) {
	NodeManager& mgr = pA->getNodeManager();
	IRBuilder builder(mgr);

	TreePattern decls = irp::declarationStmt(var("variable", pattern::any),
			irp::callExpr(aT(pattern::atom(type)), pattern::atom(BASIC.getRefVar()), pattern::single(var("init", pattern::any))));

	irp::matchAllPairs(decls, pA, [&](const NodeAddress& matchAddress, const AddressMatch& decl) {

		TypePtr varType = decl["variable"].getValue().as<ExpressionPtr>()->getType();
		TypePtr memType = varType.as<RefTypePtr>()->getElementType();

		replacements[matchAddress >> decl["init"].getValue()] = builder.callExpr(memType, BASIC.getUndefined(), builder.getTypeLiteral(memType));
	});

}


void TypeFixer::updateTemps(TypePtr type, ExpressionMap& varReplacements) {
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);

	TreePattern decls = irp::declarationStmt(var("variable", irp::variable(aT(pattern::atom(type)))), (var("init", pattern::any)));
	TreePattern assigns = irp::assignment(var("variable", irp::variable(aT(pattern::atom(type)))), (var("rhs", pattern::any)));
	TreePattern oneFitsAll = decls | assigns;

	irp::matchAllPairs(oneFitsAll, prog, [&](const NodePtr& matchPtr, const NodeMatch& decl) {

		VariablePtr var = decl["variable"].getValue().as<VariablePtr>();
		TypePtr varType = var->getType();
		TypePtr initType = decl.isVarBound("init") ? decl["init"].getValue().as<ExpressionPtr>()->getType() :
				builder.refType(decl["rhs"].getValue().as<ExpressionPtr>()->getType());

		varReplacements[var] = builder.variable(initType);
	});

}

TypeFixer::TypeFixer(NodePtr toTransform, std::vector<TypePtr> types) : prog(toTransform) {
	// replace some OpenCL type variables with int<4>.
	// Variables will be useless, but the semantics will be correct
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = builder.getLangBasic();
	TypePtr int4 = gen.getInt4();

	NodeMap ptrReplacements;

	// replace cl_program
//	TypePtr cl_program = builder.genericType("_cl_program");
//	ptrReplacements[cl_program] = int4;

	NodeAddress pA(prog);
	for(TypePtr typeTofix : types)
		fixDecls(pA, typeTofix);

	if(!this->replacements.empty())
		prog = transform::replaceAll(prog->getNodeManager(), this->replacements);

	ExpressionMap varReplacements;
	// replace temporal variables
	for(TypePtr typeTofix : types)
		updateTemps(typeTofix, varReplacements);

	prog = core::transform::fixTypesGen(prog->getNodeManager(), prog, varReplacements, false);

	removeClVars();

	// removes cl_* variables from structures
	visitDepthFirst(prog, [&](const StructTypePtr& st) {
		cleanStructures(st, ptrReplacements);
	}, true, true);

	prog = transform::replaceAll(mgr, prog, ptrReplacements, false);
}

}
}
}

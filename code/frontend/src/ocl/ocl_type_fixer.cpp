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

void TypeFixer::removeClVars() {
	NodeMapping* h;
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = builder.getLangBasic();

	// removes cl_* variables from argument lists of lambdas
	auto cleaner = makeLambdaMapper([&](unsigned index, const NodePtr& element)->NodePtr{
		// stop recursion at type level
		if (element->getNodeCategory() == NodeCategory::NC_Type) {
			return element;
		}

		// remove declarations of cl_* variables
		if(const DeclarationStmtPtr& decl = element.isa<DeclarationStmtPtr>()) {
			TypePtr varType = decl->getVariable()->getType();
			TypePtr initType = decl->getInitialization()->getType();
			if(varType->toString().find("array<_cl_") != string::npos || initType->toString().find("array<_cl_") != string::npos ) {
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
					if(arg->getType()->toString().find("array<_cl_") == string::npos) {
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
//assert(false);
//	}
			if(gen.isRefAssign(call->getFunctionExpr())) {
				TypePtr lhsTy = call->getArgument(0)->getType();
				TypePtr rhsTy = call->getArgument(1)->getType();
				if(lhsTy->toString().find("array<_cl_") != string::npos || rhsTy->toString().find("array<_cl_") != string::npos) {
					return builder.getNoOp();
				}
			}
		}

		return element->substitute(builder.getNodeManager(), *h);
	});

	h = &cleaner;
	prog = h->map(0, prog);

}

void TypeFixer::fixDecls(NodeAddress pA, std::string typeString) {
	NodeManager& mgr = pA->getNodeManager();
	IRBuilder builder(mgr);

	TreePatternPtr decls = irp::declarationStmt(var("variable", pattern::any), var("init",
			irp::callExpr(aT(irp::genericType(typeString)), pattern::any, *pattern::any)));

	irp::matchAllPairs(decls, pA, [&](const NodeAddress& matchAddress, const AddressMatch& decl) {

		TypePtr varType = decl["variable"].getValue().as<ExpressionPtr>()->getType();
		TypePtr initType = varType.as<RefTypePtr>()->getElementType();

		replacements[matchAddress >> decl["init"].getValue()] = builder.callExpr(varType, BASIC.getRefNew(),
				builder.callExpr(initType, BASIC.getUndefined(), builder.getTypeLiteral(initType)));
//		dumpPretty(replacements[decl["init"].getValue()]);
	});

}


TypeFixer::TypeFixer(NodePtr toTransform) : prog(toTransform) {
	// replace some OpenCL type variables with int<4>.
	// Variables will be useless, but the semantics will be correct
	NodeManager& mgr = prog->getNodeManager();
	IRBuilder builder(mgr);
	const lang::BasicGenerator& gen = builder.getLangBasic();
	TypePtr int4 = gen.getInt4();

	NodeMap replacements;

	// replace cl_program
	TypePtr cl_program = builder.genericType("_cl_program");
	replacements[cl_program] = int4;
	prog = transform::replaceAll(mgr, prog, replacements, false);

	NodeAddress pA(prog);
	fixDecls(pA, "_cl_kernel");
	fixDecls(pA, "_cl_mem");

	prog = transform::replaceAll(prog->getNodeManager(), this->replacements);

	VariableMap emptyMap;
	prog = core::transform::fixTypesGen(prog->getNodeManager(), prog, emptyMap, false);

	removeClVars();
}

}
}
}

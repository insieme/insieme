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

#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/omp/omp_utils.h"

#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/annotation.h"
#include "insieme/utils/timer.h"


namespace insieme {
namespace frontend {
namespace omp {
	
using namespace std;
using namespace core;
using namespace utils::log;

namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;

class OMPSemaMapper : public insieme::core::transform::CachedNodeMapping {
	NodeManager& nodeMan;
	IRBuilder build;
	const lang::BasicGenerator& basic;
	us::PointerSet<CompoundStmtPtr> toFlatten;
	
public:
	OMPSemaMapper(NodeManager& nodeMan) 
			: nodeMan(nodeMan), build(nodeMan), basic(nodeMan.getLangBasic()) {
	}

protected:
	virtual const NodePtr resolveElement(const NodePtr& node) {
		if(node->getNodeCategory() == NC_Type) return node;
		NodePtr newNode;
		if(BaseAnnotationPtr anno = node->getAnnotation(BaseAnnotation::KEY)) {
			if(auto mExp = dynamic_pointer_cast<const MarkerExpr>(node)) {
				newNode = mExp->getSubExpression()->substitute(nodeMan, *this);
			} else if(auto mStmt = dynamic_pointer_cast<const MarkerStmt>(node)) {
				newNode = mStmt->getSubStatement()->substitute(nodeMan, *this);
			} else { 
				assert(0 && "OMP annotation on non-marker node.");
			}
			//LOG(DEBUG) << "omp annotation(s) on: \n" << printer::PrettyPrinter(newNode);
			std::for_each(anno->getAnnotationListRBegin(), anno->getAnnotationListREnd(), [&](AnnotationPtr subAnn) {
				newNode = flattenCompounds(newNode);
				if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
					newNode = handleParallel(static_pointer_cast<const Statement>(newNode), parAnn);
				} else if(auto forAnn = std::dynamic_pointer_cast<For>(subAnn)) {
					newNode = handleFor(static_pointer_cast<const Statement>(newNode), forAnn);
				} else if(auto parForAnn = std::dynamic_pointer_cast<ParallelFor>(subAnn)) {
					newNode = handleParallelFor(static_pointer_cast<const Statement>(newNode), parForAnn);
				} else if(auto singleAnn = std::dynamic_pointer_cast<Single>(subAnn)) {
					newNode = handleSingle(static_pointer_cast<const Statement>(newNode), singleAnn);
				} else if(auto barrierAnn = std::dynamic_pointer_cast<Barrier>(subAnn)) {
					newNode = handleBarrier(static_pointer_cast<const Statement>(newNode), barrierAnn);
				} else if(auto criticalAnn = std::dynamic_pointer_cast<Critical>(subAnn)) {
					newNode = handleCritical(static_pointer_cast<const Statement>(newNode), criticalAnn);
				} else {
					LOG(ERROR) << "Unhandled OMP annotation: " << *subAnn;
					assert(0);
				}
			});
			//LOG(DEBUG) << "replaced with: \n" << printer::PrettyPrinter(newNode);
		} else {
			newNode = node->substitute(nodeMan, *this);
		}
		newNode = flattenCompounds(newNode);
		newNode = handleFunctions(newNode);
		// migrate annotations if applicable
		if(newNode != node) transform::utils::migrateAnnotations(node, newNode);
		return newNode;
	}

	NodePtr flattenCompounds(const NodePtr& newNode) {
		// flatten generated compounds if required
		if(toFlatten.empty()) {
			if(CompoundStmtPtr newCompound = dynamic_pointer_cast<const CompoundStmt>(newNode)) {
				//LOG(DEBUG) << "Starting flattening for: " << printer::PrettyPrinter(newCompound);
				//LOG(DEBUG) << ">- toFlatten: " << toFlatten;
				StatementList sl = newCompound->getStatements();
				StatementList newSl;
				for(auto i = sl.begin(); i != sl.end(); ++i) {
					CompoundStmtPtr innerCompound = dynamic_pointer_cast<const CompoundStmt>(*i);
					if(innerCompound && toFlatten.contains(innerCompound)) {
						//LOG(DEBUG) << "Flattening: " << printer::PrettyPrinter(innerCompound);
						toFlatten.erase(innerCompound);
						for_each(innerCompound->getStatements(), [&](const StatementPtr s) { newSl.push_back(s); });
					} else {
						newSl.push_back(*i);
					}
				}
				return build.compoundStmt(newSl);
			}
		}
		return newNode;
	}

	NodePtr handleFunctions(const NodePtr& newNode) {
		if(CallExprPtr callExp = dynamic_pointer_cast<const CallExpr>(newNode)) {
			if(LiteralPtr litFunExp = dynamic_pointer_cast<const Literal>(callExp->getFunctionExpr())) {
				const string& funName = litFunExp->getStringValue();
				if(funName == "omp_get_thread_num") {
					return build.getThreadId();
				}
			}
		}
		return newNode;
	}

	StatementPtr implementDataClauses(const StatementPtr& stmtNode, const ParallelPtr& par) {
		StatementList replacements;
		assert(!par->hasReduction() && "Reduction not yet supported");
		VarList allp;
		if(par->hasFirstPrivate()) allp.insert(allp.end(), par->getFirstPrivate().begin(), par->getFirstPrivate().end());
		if(par->hasPrivate()) allp.insert(allp.end(), par->getPrivate().begin(), par->getPrivate().end());
		if(par->hasReduction()) allp.insert(allp.end(), par->getReduction().getVars().begin(), par->getReduction().getVars().end());
		VariableMap publicToPrivateMap;
		for_each(allp, [&](const ExpressionPtr& varExp){
			VariablePtr var = dynamic_pointer_cast<const Variable>(varExp);
			assert(var && "Omp frontend expected Variable, got Expression.");
			VariablePtr pVar = build.variable(var->getType());
			publicToPrivateMap[var] = pVar;
			DeclarationStmtPtr decl = build.declarationStmt(pVar, basic.getUndefined());
			if(par->hasFirstPrivate() && contains(par->getFirstPrivate(), var)) {
				decl = build.declarationStmt(pVar, var);
			}
			replacements.push_back(decl);
		});
		StatementPtr stmtNodeNew = transform::replaceVarsGen(nodeMan, stmtNode, publicToPrivateMap);
		replacements.push_back(stmtNodeNew);
		return build.compoundStmt(replacements);
	}

	NodePtr handleParallel(const StatementPtr& stmtNode, const ParallelPtr& par) {
		auto newStmtNode = implementDataClauses(stmtNode, par);
		auto parLambda = transform::extractLambda(nodeMan, newStmtNode);
		auto jobExp = build.jobExpr(build.getThreadNumRange(1) , vector<core::DeclarationStmtPtr>(), vector<core::GuardedExprPtr>(), parLambda);
		auto parallelCall = build.callExpr(basic.getParallel(), jobExp);
		auto mergeCall = build.callExpr(basic.getMerge(), parallelCall);
		return mergeCall;
	}

	NodePtr handleFor(const StatementPtr& stmtNode, const ForPtr& forP) {
		ForStmtPtr forStmt = dynamic_pointer_cast<const ForStmt>(stmtNode);
		assert(forStmt && "OpenMP for attached to non-for statement");

		StatementList replacements;
		auto pfor = build.pfor(forStmt);
		replacements.push_back(pfor);
		if(!forP->hasNoWait()) {
			replacements.push_back(build.barrier());
		}
		return build.compoundStmt(replacements);
	}
	
	NodePtr handleParallelFor(const StatementPtr& stmtNode, const ParallelForPtr& pforP) {
		NodePtr newNode = stmtNode;
		newNode = handleFor(static_pointer_cast<const Statement>(newNode), pforP->toFor());
		newNode = handleParallel(static_pointer_cast<const Statement>(newNode), pforP->toParallel());
		return newNode;
	}

	NodePtr handleSingle(const StatementPtr& stmtNode, const SinglePtr& singleP) {
		StatementList replacements;
		// implement single as pfor with 1 item
		auto pforLambdaParams = toVector(build.variable(basic.getInt4()));
		auto body = transform::extractLambda(nodeMan, stmtNode, pforLambdaParams);
		auto pfor = build.pfor(body, build.intLit(0), build.intLit(1));
		replacements.push_back(pfor);
		if(!singleP->hasNoWait()) {
			replacements.push_back(build.barrier());
		}
		return build.compoundStmt(replacements);
	}

	NodePtr handleBarrier(const StatementPtr& stmtNode, const BarrierPtr& barrier) {
		CompoundStmtPtr replacement = build.compoundStmt(build.barrier(), stmtNode);
		toFlatten.insert(replacement);
		return replacement;
	}

	NodePtr handleCritical(const StatementPtr& stmtNode, const CriticalPtr& criticalP) {
		StatementList replacements;
		// push lock
		string prefix = "global_omp_critical_lock_";
		string name = "default";
		if(criticalP->hasName()) {
			name = criticalP->getName();
		}
		name = prefix + name;
		replacements.push_back(build.aquireLock(build.literal(nodeMan.getLangBasic().getLock(), name)));
		// push original code fragment
		replacements.push_back(stmtNode);
		// push unlock
		replacements.push_back(build.releaseLock(build.literal(nodeMan.getLangBasic().getLock(), name)));
		// build replacement compound
		CompoundStmtPtr replacement = build.compoundStmt(replacements);
		toFlatten.insert(replacement);
		return replacement;
	}
};


const core::ProgramPtr applySema(const core::ProgramPtr& prog, core::NodeManager& resultStorage) {
	ProgramPtr result = prog;

	// new sema
	{	
		OMPSemaMapper semaMapper(resultStorage);
		//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP PRE SEMA\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
		utils::Timer timer("Omp sema");
		result = semaMapper.map(result);
		timer.stop();
		LOG(INFO) << timer;
		//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP POST SEMA\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
	}

	// fix globals
	{	
		utils::Timer timer("Omp global handling");
		auto collectedGlobals = markGlobalUsers(result);
		auto globalDecl = transform::createGlobalStruct(resultStorage, result, collectedGlobals);
		GlobalMapper globalMapper(resultStorage, globalDecl->getVariable());
		//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP PRE GLOBAL\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
		result = globalMapper.map(result);
		timer.stop();
		LOG(INFO) << timer;
		//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP POST GLOBAL\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
	}
	return result;
}

} // namespace omp
} // namespace frontend
} // namespace insieme

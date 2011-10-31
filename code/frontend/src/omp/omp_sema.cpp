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
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/ast_mapper.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/annotation.h"

#include <stack>


namespace insieme {
namespace frontend {
namespace omp {
	
using namespace std;
using namespace core;
using namespace utils::log;

namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;

struct GlobalRequiredAnnotation : public NodeAnnotation { 
	const static string name;
	const static utils::StringKey<GlobalRequiredAnnotation> key; 
	virtual const utils::AnnotationKey* getKey() const {
		return &key;
	}
	virtual const std::string& getAnnotationName() const {
		return name;
	}
	virtual bool migrate(const NodeAnnotationPtr& ptr, const NodePtr& before, const NodePtr& after) const {
		after->addAnnotation(ptr);
		return true; 
	};
};
const string GlobalRequiredAnnotation::name = "GlobalRequiredAnnotation";
const utils::StringKey<GlobalRequiredAnnotation> GlobalRequiredAnnotation::key("GlobalRequiredAnnotation");

// Utilities for marking paths that require OMP globals and gathering the required globals
namespace {
	StructExpr::Members markGlobalUsers(const core::ProgramPtr& prog) {
		NodeManager& nodeMan = prog->getNodeManager();
		ASTBuilder builder(nodeMan);
		boost::unordered_set<std::string> handledGlobals;
		StructExpr::Members retval;
		auto anno = std::make_shared<GlobalRequiredAnnotation>();
		visitDepthFirst(ProgramAddress(prog), [&](const LiteralAddress& lit) {
			const string& gname = lit->getValue();
			if(gname.find("global_omp") == 0) {
				// add global to set if required
				if(handledGlobals.count(gname) == 0) {
					ExpressionPtr initializer;
					if(nodeMan.basic.isLock(lit->getType())) {
						initializer = builder.createLock();
					} else assert(false && "Unsupported OMP global type");
					retval.push_back(StructExpr::Member(builder.identifier(gname), initializer));
					handledGlobals.insert(gname);
				}
				// mark upward path from global
				auto pathMarker = makeLambdaVisitor([&](const NodeAddress& node) { node->addAnnotation(anno); });
				visitPathBottomUp(lit, pathMarker);
			}
		});
		return retval;
	}
}

// Utilities for passing global to functions that require it, and replacing literals with global accesses
namespace {
	class GlobalMapper : public NodeMapping {
		NodeManager& nodeMan;
		ASTBuilder build;
		std::stack<VariablePtr> curVar;

	public:
		GlobalMapper(NodeManager& nodeMan, const VariablePtr& global) : nodeMan(nodeMan), build(nodeMan) { 
			curVar.push(global);
		}

	protected:
		virtual const NodePtr mapElement(unsigned index, const NodePtr& ptr) {
			if(ptr->hasAnnotation(GlobalRequiredAnnotation::key)) {
				//LOG(INFO) << "?????? Mapping 1 " << ptr;
				// recursively forward the variable
				switch(ptr->getNodeType()) {
				case NT_CallExpr:
					return mapCall(static_pointer_cast<const CallExpr>(ptr));
					break;
				case NT_BindExpr:
					return mapBind(static_pointer_cast<const BindExpr>(ptr));
					break;
				default:
					// no changes at this node, but at child nodes
					return ptr->substitute(nodeMan, *this);
				}
			} else {
				//LOG(INFO) << "?????? Mapping 2 " << ptr;
				// no changes required
				return ptr;
			}
		}
		
		const NodePtr mapCall(const CallExprPtr& call) {
			LOG(INFO) << "?????? Mapping call " << call;
			auto func = call->getFunctionExpr();
			if(func && func->hasAnnotation(GlobalRequiredAnnotation::key)) {				
				LOG(INFO) << "!!!!!! Mapping call " << call;
				vector<ExpressionPtr> newCallArgs = call->getArguments();
				newCallArgs.push_back(curVar.top());
				// build function body
				curVar.push(build.variable(curVar.top()->getType()));
				LOG(INFO) << func->getNodeType();
				LOG(INFO) << func;
				curVar.pop();
				// complete call
				return build.callExpr(call->getType(), static_pointer_cast<const Expression>(func->substitute(nodeMan, *this)), newCallArgs);
			}
			return call->substitute(nodeMan, *this);
		}
		const NodePtr mapBind(const BindExprPtr& bind) {
			LOG(INFO) << "?????? Mapping bind " << bind;
			auto boundCall = bind->getCall();
			auto boundCallFunc = boundCall->getFunctionExpr();
			if(boundCallFunc && boundCallFunc->hasAnnotation(GlobalRequiredAnnotation::key)) {
				LOG(INFO) << "!!!!!! Mapping bind " << bind;
				LOG(INFO) << bind->getBoundExpressions();
				LOG(INFO) << bind->getCall();
				LOG(INFO) << bind->getParameters();
				vector<ExpressionPtr> newBoundCallArguments = boundCall->getArguments();
				newBoundCallArguments.push_back(curVar.top());
				auto newFunctionExpr = static_pointer_cast<const Expression>(boundCall->getFunctionExpr()->substitute(nodeMan, *this));
				LOG(INFO) << newFunctionExpr->getNodeType();
				auto newCall = build.callExpr(boundCall->getType(), newFunctionExpr, newBoundCallArguments);
				return build.bindExpr(bind->getParameters(), newCall);
			}
			return bind->substitute(nodeMan, *this);
		}


	};
}


const core::ProgramPtr applySema(const core::ProgramPtr& prog, core::NodeManager& resultStorage) {
	ProgramPtr result = prog;
	for(;;) {
		SemaVisitor v(resultStorage, prog);
		core::visitDepthFirstInterruptible(core::ProgramAddress(result), v);
		if(v.getReplacement()) result = v.getReplacement();
		else break;	
	}
	// fix globals
	auto collectedGlobals = markGlobalUsers(result);
	//LOG(INFO) << "[[[[[[[[[[[[[[[[[ PRE\n" << core::printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
	auto globalDecl = transform::createGlobalStruct(resultStorage, result, collectedGlobals);
	//LOG(INFO) << "[[[[[[[[[[[[[[[[[ POST\n" << core::printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
	GlobalMapper mapper(resultStorage, globalDecl->getVariable());
	return mapper.map(result);
}

bool SemaVisitor::visitNode(const NodeAddress& node) {
	return false; // default behaviour: continue visiting
}


bool SemaVisitor::visitCallExpr(const core::CallExprAddress& callExp) {
	if(auto litFunExp = dynamic_pointer_cast<const Literal>(callExp->getFunctionExpr())) {
		auto funName = litFunExp->getValueAs<string>();
		if(funName == "omp_get_thread_num") {
			replacement = dynamic_pointer_cast<const Program>(transform::replaceNode(nodeMan, callExp, build.getThreadId()));
			return true;
		}
	}
	return false;
}


bool SemaVisitor::visitMarkerStmt(const MarkerStmtAddress& mark) {
	const StatementAddress stmt = static_address_cast<const Statement>(mark.getAddressOfChild(0));
	//LOG(INFO) << "marker on: \n" << *stmt;
	if(BaseAnnotationPtr anno = mark->getAnnotation(BaseAnnotation::KEY)) {
		LOG(INFO) << "omp statement annotation(s) on: \n" << *stmt;
		std::for_each(anno->getAnnotationListBegin(), anno->getAnnotationListEnd(), [&](AnnotationPtr subAnn) {
			LOG(INFO) << "annotation: " << *subAnn;
			NodePtr newNode;
			if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
				newNode = handleParallel(stmt, parAnn);
			} else if(auto forAnn = std::dynamic_pointer_cast<For>(subAnn)) {
				newNode = handleFor(stmt, forAnn);
			} else if(auto singleAnn = std::dynamic_pointer_cast<Single>(subAnn)) {
				newNode = handleSingle(stmt, singleAnn);
			} 
			else assert(0 && "Unhandled OMP statement annotation.");
			//LOG(INFO) << "Pre replace: " << *mark.getRootNode();
			//LOG(INFO) << "Replace: " << *mark;
			//LOG(INFO) << "   with: " << *newNode;
			replacement = dynamic_pointer_cast<const Program>(transform::replaceNode(nodeMan, mark, newNode));
			//LOG(INFO) << "Post replace: " << replacement;
		});
		return true;
	}
	return false;
}

bool SemaVisitor::visitMarkerExpr(const MarkerExprAddress& mark) {
	const ExpressionAddress expr = static_address_cast<const Expression>(mark.getAddressOfChild(0));
	if(BaseAnnotationPtr anno = mark->getAnnotation(BaseAnnotation::KEY)) {
		LOG(INFO) << "omp expression annotation(s) on: \n" << *expr;
		std::for_each(anno->getAnnotationListBegin(), anno->getAnnotationListEnd(), [&](AnnotationPtr subAnn) {
			LOG(INFO) << "annotation: " << *subAnn;
			if(auto barrierAnn = std::dynamic_pointer_cast<Barrier>(subAnn)) {
				replacement = handleBarrier(mark, barrierAnn);
			} else if(auto criticalAnn = std::dynamic_pointer_cast<Critical>(subAnn)) {
				replacement = handleCritical(mark, criticalAnn);
			} else if(auto singleAnn = std::dynamic_pointer_cast<Single>(subAnn)) {
				replacement = dynamic_pointer_cast<const Program>(transform::replaceNode(nodeMan, mark, handleSingle(expr, singleAnn)));
			} 
			else assert(0 && "Unhandled OMP expression annotation.");
		});
		return true;
	}
	return false;
}


ProgramPtr SemaVisitor::handleCritical(const NodeAddress& node, const CriticalPtr& criticalP) {
	auto parent = node.getParentAddress();
	CompoundStmtAddress surroundingCompound = dynamic_address_cast<const CompoundStmt>(parent);
	assert(surroundingCompound && "OMP critical pragma not surrounded by compound statement");
	StatementList replacements;

	// push lock
	string prefix = "global_omp_critical_lock_";
	string name = "default";
	if(criticalP->hasName()) {
		name = criticalP->getName();
	}
	name = prefix + name;
	replacements.push_back(build.aquireLock(build.literal(nodeMan.basic.getLock(), name)));

	// push original code fragment
	if(auto expMarker = dynamic_address_cast<const MarkerExpr>(node))
		replacements.push_back(expMarker->getSubExpression());
	else if(auto stmtMarker = dynamic_address_cast<const MarkerStmt>(node)) {
		replacements.push_back(stmtMarker->getSubStatement());
	}

	// push unlock
	replacements.push_back(build.releaseLock(build.literal(nodeMan.basic.getLock(), name)));

	return dynamic_pointer_cast<const Program>(transform::replace(nodeMan, surroundingCompound, node.getIndex(), replacements));
}

ProgramPtr SemaVisitor::handleBarrier(const NodeAddress& node, const BarrierPtr& barrier) {
	auto parent = node.getParentAddress();
	CompoundStmtAddress surroundingCompound = dynamic_address_cast<const CompoundStmt>(parent);
	assert(surroundingCompound && "OMP statement pragma not surrounded by compound statement");
	StatementList replacements;
	replacements.push_back(build.barrier());
	if(auto expMarker = dynamic_address_cast<const MarkerExpr>(node))
		replacements.push_back(expMarker->getSubExpression());
	else if(auto stmtMarker = dynamic_address_cast<const MarkerStmt>(node)) {
		replacements.push_back(stmtMarker->getSubStatement());
	}
	return dynamic_pointer_cast<const Program>(transform::replace(nodeMan, surroundingCompound, node.getIndex(), replacements));
}

NodePtr SemaVisitor::handleParallel(const StatementAddress& stmt, const ParallelPtr& par) {
	auto stmtNode = stmt.getAddressedNode();

	auto parLambda = transform::extractLambda(nodeMan, stmtNode);

	auto& basic = nodeMan.basic;
	auto jobExp = build.jobExpr(build.getThreadNumRange(1) , parLambda, JobExpr::GuardedStmts(), JobExpr::LocalDecls());
	auto parallelCall = build.callExpr(basic.getParallel(), jobExp);
	auto mergeCall = build.callExpr(basic.getMerge(), parallelCall);
	//LOG(INFO) << "mergeCall:\n" << mergeCall;
	return mergeCall;
}

NodePtr SemaVisitor::handleFor(const core::StatementAddress& stmt, const ForPtr& forP) {
	auto stmtNode = stmt.getAddressedNode();
	ForStmtPtr forStmt = dynamic_pointer_cast<const ForStmt>(stmtNode);
	assert(forStmt && "OpenMP for attached to non-for statement");

	StatementList replacements;
	auto pfor = build.pfor(forStmt);
	replacements.push_back(pfor);

	if(!forP->hasNoWait()) {
		replacements.push_back(build.barrier());
	}
	//LOG(INFO) << "for stmtNode:\n" << stmtNode;
	return build.compoundStmt(replacements);
}

NodePtr SemaVisitor::handleSingle(const core::StatementAddress& stmt, const SinglePtr& singleP) {
	auto stmtNode = stmt.getAddressedNode();
	StatementList replacements;
	// implement single as pfor with 1 item
	auto pforLambdaParams = toVector(build.variable(nodeMan.basic.getInt4()));
	auto body = transform::extractLambda(nodeMan, stmtNode, pforLambdaParams);
	auto pfor = build.pfor(body, build.intLit(0), build.intLit(1));
	replacements.push_back(pfor);
	if(!singleP->hasNoWait()) {
		replacements.push_back(build.barrier());
	}
	return build.compoundStmt(replacements);
}

} // namespace omp
} // namespace frontend
} // namespace insieme

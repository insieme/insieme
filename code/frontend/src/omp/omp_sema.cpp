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
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/parser/type_parse.h"

#include "insieme/analysis/dep_graph.h"
#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/utils/set_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/annotation.h"
#include "insieme/utils/timer.h"

#define MAX_THREADPRIVATE 80

namespace insieme {
namespace frontend {
namespace omp {
	
using namespace std;
using namespace core;
using namespace utils::log;

namespace cl = lang;
namespace us = utils::set;
namespace um = utils::map;
namespace ad = insieme::analysis::dep;
namespace scop = insieme::analysis::polyhedral::scop;

// TODO: use transformation instead!
namespace {
int canCollapse(const ForStmtPtr& outer) {
	ForStmtPtr inner = dynamic_pointer_cast<const ForStmt>(outer->getBody()->getStatement(0));
	if(!inner) return 0;
	//LOG(INFO) << "+ Nested for in pfor.";
	if(outer->getBody()->getStatements().size() != 1) return 0;
	//LOG(INFO) << "++ Perfectly nested for in pfor.";
	scop::mark(inner);
	if(!inner->hasAnnotation(scop::ScopRegion::KEY)) return 0;
	//LOG(INFO) << "+++ Pfor is scop region.";
	scop::ScopRegion& scopR = *inner->getAnnotation(scop::ScopRegion::KEY);
	if(!scopR.isValid() || !scopR.isResolved()) return 0;
	//LOG(INFO) << "++++ Scop region is valid and resolved.";
	ad::DependenceGraph dg = ad::extractDependenceGraph(inner, ad::WRITE);
	ad::DependenceList dl = dg.getDependencies();
	if(dl.empty()) {
		//LOG(INFO) << "<<<<< Perfectly nested for in for, no dependencies:\n" << printer::PrettyPrinter(outer);
		return 1 + canCollapse(inner);
	} else {
		//LOG(INFO) << "pfor nested for deps:\n=====================================\n";
		dg.printTo(std::cout);
		//LOG(INFO) << "=====================================\npfor nested for deps end.";
		for(auto it = dl.begin(); it != dl.end(); ++it) {
			ad::DependenceInstance di = *it;
			ad::FormulaList distances = get<0>(get<3>(di));
			core::arithmetic::Formula d0 = distances[0];
			if(!d0.isZero()) {
				return 0;
			}
		}
		//LOG(INFO) << "<<<<< Perfectly nested for in for, no outer loop dependencies:\n" << printer::PrettyPrinter(outer);
		return 1 + canCollapse(inner);
	}
}

ForStmtPtr collapseFor(const ForStmtPtr& outer, int collapseLevels) {
	if(collapseLevels == 0) return outer;
	NodeManager& mgr = outer->getNodeManager();
	IRBuilder build(mgr);
	// determine existing loop characteristics
	ForStmtPtr inner = static_pointer_cast<const ForStmt>(outer->getBody()->getStatement(0));
	TypePtr iteratorType = outer->getIterator()->getType();
	ExpressionPtr os = outer->getStart(), oe = outer->getEnd(), ost = outer->getStep();
	ExpressionPtr is = inner->getStart(), ie = inner->getEnd(), ist = inner->getStep();
	ExpressionPtr oext = build.div(build.sub(oe,os), ost); // outer loop extent
	ExpressionPtr iext = build.div(build.sub(ie,is), ist); // inner loop extent
	// calculate new loop bounds and generate body
	ExpressionPtr newExt = build.mul(oext, iext); // generated loop extent
	VariablePtr newI = build.variable(iteratorType);
	ExpressionPtr oIterReplacement = build.add(build.mul(build.div(newI, iext), ost), os);
	ExpressionPtr iIterReplacement = build.add(build.mul(build.mod(newI, iext), ist), is);
	NodeMap replacements;
	replacements.insert(make_pair(outer.getIterator(), oIterReplacement));
	replacements.insert(make_pair(inner.getIterator(), iIterReplacement));
	StatementPtr newBody = transform::replaceAllGen(mgr, inner.getBody(), replacements);
	return collapseFor(build.forStmt(newI, build.intLit(0), newExt, build.intLit(1), newBody), collapseLevels-1);
}

ForStmtPtr collapseForNest(const ForStmtPtr& outer) {
	ForStmtPtr ret = outer;
	int collapseLevels = canCollapse(ret); 
	if(collapseLevels>0) ret = collapseFor(ret, collapseLevels);
	if(*ret != *outer) {
		LOG(INFO) << "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%"
		<< "\n%%%%%%%%%%% Replaced existing for loop:\n" << printer::PrettyPrinter(outer) 
		<< "\n%%%%%%%%%%% with collapsed loop:\n" << printer::PrettyPrinter(ret)
		<< "\n%%%%%%%%%%% collapased " << collapseLevels << " levels";
	}
	return ret;
}
} // anonymous namespace

class OMPSemaMapper : public insieme::core::transform::CachedNodeMapping {
	NodeManager& nodeMan;
	IRBuilder build;
	const lang::BasicGenerator& basic;
	us::PointerSet<CompoundStmtPtr> toFlatten; // set of compound statements to flatten one step further up
	
	// the following vars handle global struct type adjustment due to threadprivate
	bool fixStructType; // when set, implies that the struct was just modified and needs to be adjusted 
	StructTypePtr adjustStruct; // marks a struct that was modified and needs to be adjusted when encountered
	StructTypePtr adjustedStruct; // type that should replace the above
	// threadprivate optimization map
	ExprVarMap thisLambdaTPAccesses; 
	
public:
	OMPSemaMapper(NodeManager& nodeMan) 
			: nodeMan(nodeMan), build(nodeMan), basic(nodeMan.getLangBasic()), toFlatten(), 
			  fixStructType(false), adjustStruct(), adjustedStruct(), thisLambdaTPAccesses() {
	}

	StructTypePtr getAdjustStruct() { return adjustStruct; }
	StructTypePtr getAdjustedStruct() { return adjustedStruct; }

protected:
	// Identifies omp annotations and uses the correct methods to deal with them
	// except for threadprivate, all omp annotations are on statement or expression marker nodes
	virtual const NodePtr resolveElement(const NodePtr& node) {
		if(node->getNodeCategory() == NC_Type) return node;
		NodePtr newNode;
		if(BaseAnnotationPtr anno = node->getAnnotation(BaseAnnotation::KEY)) {
			if(auto mExp = dynamic_pointer_cast<const MarkerExpr>(node)) {
				newNode = mExp->getSubExpression()->substitute(nodeMan, *this);
			} else if(auto mStmt = dynamic_pointer_cast<const MarkerStmt>(node)) {
				newNode = mStmt->getSubStatement()->substitute(nodeMan, *this);
			} else { 
				// mayhap it is threadprivate, my eternal nemesis!
				if(std::dynamic_pointer_cast<ThreadPrivate>(anno->getAnnotationList().front())) {
					newNode = node;
				}
				else assert(0 && "OMP annotation on non-marker node.");
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
				} else if(auto masterAnn = std::dynamic_pointer_cast<Master>(subAnn)) {
					newNode = handleMaster(static_pointer_cast<const Statement>(newNode), masterAnn);
				} else if(auto flushAnn = std::dynamic_pointer_cast<Flush>(subAnn)) {
					newNode = handleFlush(static_pointer_cast<const Statement>(newNode), flushAnn);
				} else if(std::dynamic_pointer_cast<ThreadPrivate>(subAnn)) {
					newNode = handleThreadprivate(newNode);
				} else {
					LOG(ERROR) << "Unhandled OMP annotation: " << *subAnn;
					assert(0);
				}
			});
			//LOG(DEBUG) << "replaced with: \n" << printer::PrettyPrinter(newNode);
		} else {
			// no changes required at this level, recurse
			newNode = node->substitute(nodeMan, *this);
		}
		newNode = handleTPVars(newNode);
		newNode = fixStruct(newNode);
		newNode = flattenCompounds(newNode);
		newNode = handleFunctions(newNode);
		// migrate annotations if applicable
		if(newNode != node) transform::utils::migrateAnnotations(node, newNode);
		return newNode;
	}

	// fixes a struct type to correctly resemble its members
	// used to make the global struct in line with its new shape after modification by one/multiple threadprivate(s)
	NodePtr fixStruct(const NodePtr& newNode) {
		if(fixStructType) {
			if(StructExprPtr structExpr = dynamic_pointer_cast<const StructExpr>(newNode)) {
				// WHY doesn't StructExpr::getType() return a StructType?
				adjustStruct = static_pointer_cast<const StructType>(structExpr->getType());
				fixStructType = false;
				NamedValuesPtr members = structExpr->getMembers();
				// build new type from member initialization expressions' types
				vector<NamedTypePtr> memberTypes;
				::transform(members, std::back_inserter(memberTypes), [&](const NamedValuePtr& cur) {
					return build.namedType(cur->getName(), cur->getValue()->getType());
				});
				adjustedStruct = build.structType(memberTypes);
				return build.structExpr(adjustedStruct, members);
			}
		}
		return newNode;
	}

	// flattens generated compound statements if requested
	// used to preserve the correct scope for variable declarations
	NodePtr flattenCompounds(const NodePtr& newNode) {
		if(!toFlatten.empty()) {
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

	// implements OpenMP built-in functions by replacing the call expression
	NodePtr handleFunctions(const NodePtr& newNode) {
		if(CallExprPtr callExp = dynamic_pointer_cast<const CallExpr>(newNode)) {
			if(LiteralPtr litFunExp = dynamic_pointer_cast<const Literal>(callExp->getFunctionExpr())) {
				const string& funName = litFunExp->getStringValue();
				if(funName == "omp_get_thread_num") {
					return build.getThreadId();
				} else if(funName == "omp_get_num_threads") {
					return build.getThreadGroupSize();
				} else if(funName == "omp_") {
					LOG(ERROR) << "Function name: " << funName;
					assert(false && "Unknown OpenMP function");
				}
			}
		}
		return newNode;
	}

	// helper that checks for variable in subtree
	bool isInTree(VariablePtr var, NodePtr tree) {
		bool inside = false;
		visitDepthFirstOnceInterruptible(tree, [&](const VariablePtr& cVar) {
			if(*cVar == *var) inside = true;
			return inside;
		} );
		return inside;
	}
	
	// internal implementation of TP variable generation used by both
	// handleTPVars and implementDataClauses
	CompoundStmtPtr handleTPVarsInternal(const CompoundStmtPtr& body, bool generatedByOMP = false) {
		StatementList statements;
		StatementList oldStatements = body->getStatements();
		StatementList::const_iterator oi = oldStatements.cbegin();
		// insert existing decl statements before
		if(!generatedByOMP) while((*oi)->getNodeType() == NT_DeclarationStmt) {
			statements.push_back(*oi);
			oi++;
		}
		// insert new decls
		ExprVarMap newLambdaAcc;
		for_each(thisLambdaTPAccesses, [&](const ExprVarMap::value_type& entry) {
			VariablePtr var = entry.second;
			if(isInTree(var, body)) {
				ExpressionPtr expr = entry.first;
				statements.push_back(build.declarationStmt(var, expr));
				if(generatedByOMP) newLambdaAcc.insert(entry);
			} else {
				newLambdaAcc.insert(entry);
			}
		} );
		thisLambdaTPAccesses = newLambdaAcc;
		// insert rest of existing body
		while(oi != oldStatements.cend()) {
			statements.push_back(*oi);
			oi++;
		}
		return build.compoundStmt(statements);
	}

	// generates threadprivate access variables for the current lambda
	NodePtr handleTPVars(const NodePtr& node) {
		LambdaPtr lambda = dynamic_pointer_cast<const Lambda>(node);
		if(lambda && !thisLambdaTPAccesses.empty()) {
			return build.lambda(lambda->getType(), lambda->getParameters(), handleTPVarsInternal(lambda->getBody()));
		}
		return node;
	}

	// beware! the darkness hath returned to prey upon innocent globals yet again
	// will the frontend prevail?
	// new and improved crazyness! does not directly implement accesses, replaces with variable
	// masterCopy -> return access expression for master copy of tp value
	NodePtr handleThreadprivate(const NodePtr& node, bool masterCopy = false) {
		NamedValuePtr member = dynamic_pointer_cast<const NamedValue>(node);
		if(member) {
			//cout << "%%%%%%%%%%%%%%%%%%\nMEMBER THREADPRIVATE:\n" << *member << "\n";
			ExpressionPtr initExp = member->getValue();
			StringValuePtr name = member->getName();
			ExpressionPtr vInit = build.vectorInit(initExp, build.concreteIntTypeParam(MAX_THREADPRIVATE));
			fixStructType = true;
			return build.namedValue(name, vInit);
		}
		CallExprPtr call = dynamic_pointer_cast<const CallExpr>(node);
		if(call) {
			//cout << "%%%%%%%%%%%%%%%%%%\nCALL THREADPRIVATE:\n" << *call << "\n";
			assert(call->getFunctionExpr() == basic.getCompositeRefElem() && "Threadprivate not on composite ref elem access");
			ExpressionList args = call->getArguments();
			TypePtr elemType = core::analysis::getReferencedType(call->getType());
			elemType = build.vectorType(elemType, build.concreteIntTypeParam(MAX_THREADPRIVATE));
			CallExprPtr memAccess = 
				build.callExpr(build.refType(elemType), basic.getCompositeRefElem(), args[0], args[1], build.getTypeLiteral(elemType));
			ExpressionPtr indexExpr = build.castExpr(basic.getUInt8(), build.getThreadId());
			if(masterCopy) indexExpr = build.literal(basic.getUInt8(), "0");
			ExpressionPtr accessExpr = build.arrayRefElem(memAccess, indexExpr);
			if(masterCopy) return accessExpr;
			// if not a master copy, optimize access
			if(thisLambdaTPAccesses.find(accessExpr) != thisLambdaTPAccesses.end()) {
				// repeated access, just use existing variable
				return thisLambdaTPAccesses[accessExpr];
			} else {
				// new access, generate var and add to map
				VariablePtr varP = build.variable(accessExpr->getType());
				assert(varP->getType()->getNodeType() == NT_RefType && "Non-ref threadprivate!");
				thisLambdaTPAccesses.insert(std::make_pair(accessExpr, varP));
				return varP;
			}
			 
		}
		assert(false && "OMP threadprivate annotation on non-member / non-call");
	}

	// implements omp flush by generating INSPIRE flush() calls
	NodePtr handleFlush(const StatementPtr& stmt, const FlushPtr& flush) {
		StatementList replacements;
		if(flush->hasVarList()) {
			const VarList& vars = flush->getVarList();
			for_each(vars, [&](const ExpressionPtr& exp) {
				replacements.push_back(build.callExpr(basic.getUnit(), basic.getFlush(), exp));
			} );
		}
		// add unrelated next statement to replacement
		replacements.push_back(stmt);
		CompoundStmtPtr replacement = build.compoundStmt(replacements);
		toFlatten.insert(replacement);
		return replacement;
	}

	// implements reduction steps after parallel / for clause
	CompoundStmtPtr implementReductions(const DatasharingClause* clause, NodeMap& publicToPrivateMap) {
		static unsigned redId = 0;
		StatementList replacements;
		for_each(clause->getReduction().getVars(), [&](const ExpressionPtr& varExp) {
			StatementPtr operation;
			switch(clause->getReduction().getOperator()) {
			case Reduction::PLUS:
				operation = build.assign(varExp, build.add(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
				break;
			default:
				LOG(ERROR) << "OMP reduction operator: " << Reduction::opToStr(clause->getReduction().getOperator());
				assert(false && "Unsupported reduction operator");
			}
			replacements.push_back(operation);
		});
		return makeCritical(build.compoundStmt(replacements), string("reduce_") + toString(++redId));
	}

	// returns the correct initial reduction value for the given operator and type
	ExpressionPtr getReductionInitializer(Reduction::Operator op, const TypePtr& type) {
		ExpressionPtr ret;
		RefTypePtr rType = dynamic_pointer_cast<const RefType>(type);
		assert(rType && "OMP reduction on non-reference type");
		switch(op) {
		case Reduction::PLUS:
			ret = build.refVar(build.literal("0", rType->getElementType()));
			break;
		default:
			LOG(ERROR) << "OMP reduction operator: " << Reduction::opToStr(op);
			assert(false && "Unsupported reduction operator");
		}
		return ret;
	}

	StatementPtr implementDataClauses(const StatementPtr& stmtNode, const DatasharingClause* clause) {
		const For* forP = dynamic_cast<const For*>(clause);
		const Parallel* parallelP = dynamic_cast<const Parallel*>(clause);
		StatementList replacements;
		VarList allp;
		if(clause->hasFirstPrivate()) allp.insert(allp.end(), clause->getFirstPrivate().begin(), clause->getFirstPrivate().end());
		if(clause->hasPrivate()) allp.insert(allp.end(), clause->getPrivate().begin(), clause->getPrivate().end());
		if(clause->hasReduction()) allp.insert(allp.end(), clause->getReduction().getVars().begin(), clause->getReduction().getVars().end());
		NodeMap publicToPrivateMap;
		NodeMap privateToPublicMap;
		// implement private copies where required
		for_each(allp, [&](const ExpressionPtr& varExp) {
			VariablePtr pVar = build.variable(varExp->getType());
			publicToPrivateMap[varExp] = pVar;
			privateToPublicMap[pVar] = varExp;
			DeclarationStmtPtr decl = build.declarationStmt(pVar, build.undefinedVar(varExp->getType()));
			if(clause->hasFirstPrivate() && contains(clause->getFirstPrivate(), varExp)) {
				decl = build.declarationStmt(pVar, varExp);
			}
			if(clause->hasReduction() && contains(clause->getReduction().getVars(), varExp)) {
				decl = build.declarationStmt(pVar, getReductionInitializer(clause->getReduction().getOperator(), varExp->getType()));
			}
			replacements.push_back(decl);
		});
		// implement copyin for threadprivate vars
		if(parallelP && parallelP->hasCopyin()) {
			for_each(parallelP->getCopyin(), [&](const ExpressionPtr& varExp) {
				// assign master copy to private copy
				StatementPtr assignment = build.assign(
					static_pointer_cast<const Expression>(handleThreadprivate(varExp)), 
					build.deref(static_pointer_cast<const Expression>(handleThreadprivate(varExp, true))) );
				replacements.push_back(assignment);
			});
		}
		StatementPtr subStmt = transform::replaceAllGen(nodeMan, stmtNode, publicToPrivateMap);
		// specific handling if clause is a omp for
		if(forP) subStmt = build.pfor(static_pointer_cast<const ForStmt>(subStmt));
		replacements.push_back(subStmt);
		// implement reductions
		if(clause->hasReduction()) replacements.push_back(implementReductions(clause, publicToPrivateMap));
		// specific handling if clause is a omp for (insert barrier if not nowait)
		if(forP && !forP->hasNoWait()) replacements.push_back(build.barrier());
		// handle threadprivates before it is too late!
		auto res = handleTPVarsInternal(build.compoundStmt(replacements), true);

		return res;
	}

	NodePtr markUnordered(const NodePtr& node) {
		auto& attr = nodeMan.getLangExtension<core::analysis::AttributeExtension>();
		auto printfNodePtr = build.literal("printf", core::parse::parseType(nodeMan, "(ref<array<char,1> >, var_list) -> int<4>"));
		return transform::replaceAll(nodeMan, node, printfNodePtr, core::analysis::addAttribute(printfNodePtr, attr.getUnordered()));
	}

	NodePtr handleParallel(const StatementPtr& stmtNode, const ParallelPtr& par) {
		auto newStmtNode = implementDataClauses(stmtNode, &*par);
		auto parLambda = transform::extractLambda(nodeMan, newStmtNode);
		// mark printf as unordered
		parLambda = markUnordered(parLambda).as<BindExprPtr>();
		auto range = build.getThreadNumRange(1); // if no range is specified, assume 1 to infinity
		if(par->hasNumThreads()) range = build.getThreadNumRange(par->getNumThreads(), par->getNumThreads());
		auto jobExp = build.jobExpr(range, vector<core::DeclarationStmtPtr>(), vector<core::GuardedExprPtr>(), parLambda);
		auto parallelCall = build.callExpr(basic.getParallel(), jobExp);
		auto mergeCall = build.callExpr(basic.getMerge(), parallelCall);
		return mergeCall;
	}

	NodePtr handleFor(const StatementPtr& stmtNode, const ForPtr& forP) {
		assert(stmtNode.getNodeType() == NT_ForStmt && "OpenMP for attached to non-for statement");
		ForStmtPtr outer = dynamic_pointer_cast<const ForStmt>(stmtNode);
		//outer = collapseForNest(outer);
		return implementDataClauses(outer, &*forP);
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
		auto pforLambdaParams = toVector(build.variable(basic.getInt4()), build.variable(basic.getInt4()), build.variable(basic.getInt4()));
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

	CompoundStmtPtr makeCritical(const StatementPtr& statement, const string& nameSuffix) {
		string name = "global_omp_critical_lock_" + nameSuffix;
		StatementList replacements;
		// push lock
		replacements.push_back(build.acquireLock(build.literal(nodeMan.getLangBasic().getLock(), name)));
		// push original code fragment
		replacements.push_back(statement);
		// push unlock
		replacements.push_back(build.releaseLock(build.literal(nodeMan.getLangBasic().getLock(), name)));
		// build replacement compound
		return build.compoundStmt(replacements);
	}

	NodePtr handleCritical(const StatementPtr& stmtNode, const CriticalPtr& criticalP) {
		string name = "default";
		if(criticalP->hasName()) name = criticalP->getName();
		CompoundStmtPtr replacement = makeCritical(stmtNode, name);
		toFlatten.insert(replacement);
		return replacement;
	}

	NodePtr handleMaster(const StatementPtr& stmtNode, const MasterPtr& masterP) {
		return build.ifStmt(build.eq(build.getThreadId(), build.getZero(build.getThreadId().getType())), stmtNode);
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
		
		// fix global struct type if modified by threadprivate
		if(semaMapper.getAdjustStruct()) {
			result = static_pointer_cast<const Program>(
				transform::replaceAll(resultStorage, result, semaMapper.getAdjustStruct(), semaMapper.getAdjustedStruct(), false));
		}
	}

	// fix globals
	{	
		utils::Timer timer("Omp global handling");
		auto collectedGlobals = markGlobalUsers(result);
		if(collectedGlobals.size() > 0) {
			auto globalDecl = transform::createGlobalStruct(resultStorage, result, collectedGlobals);
			GlobalMapper globalMapper(resultStorage, globalDecl->getVariable());
			//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP PRE GLOBAL\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
			result = globalMapper.map(result);
			timer.stop();
			LOG(INFO) << timer;
			//LOG(DEBUG) << "[[[[[[[[[[[[[[[[[ OMP POST GLOBAL\n" << printer::PrettyPrinter(result, core::printer::PrettyPrinter::OPTIONS_DETAIL);
		}
	}

	// omp postprocessing optimization
	//{
	//	utils::Timer timer("Omp postprocessing optimization");

	//	p::TreePattern tpAccesses

	//	timer.stop();
	//	LOG(INFO) << timer;
	//}

	return result;
}

} // namespace omp
} // namespace frontend
} // namespace insieme

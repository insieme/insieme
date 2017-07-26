/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/frontend/omp/omp_sema.h"
#include "insieme/frontend/omp/omp_annotation.h"

#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/arithmetic/arithmetic.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/time.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/annotation.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/name_mangling.h"
#include "insieme/utils/set_utils.h"
#include "insieme/utils/timer.h"

#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/core/tu/ir_translation_unit.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

#include "insieme/annotations/omp/omp_annotations.h"
#include "insieme/annotations/meta_info/meta_infos.h"

#include <stack>

#define MAX_THREADPRIVATE 80

namespace insieme {
namespace frontend {
namespace omp {

	using namespace std;
	using namespace core;
	using namespace insieme::utils::log;

	namespace cl = lang;
	namespace us = insieme::utils::set;
	namespace um = insieme::utils::map;

	namespace {
		bool contains(const vector<ExpressionPtr>& list, const ExpressionPtr& element) {
			return ::contains(list, element, equal_target<ExpressionPtr>());
		}

		template<typename T>
		T renameLambdaReference(const T& nodeIn, const string& newName) {
			NodePtr node = nodeIn;
			core::IRBuilder builder(node->getNodeManager());
			ExpressionPtr ret = nullptr;
			node = builder.normalize(node);

			if(LambdaExprPtr lambdaExp = node.isa<core::LambdaExprPtr>()) {
				ret = builder.lambdaExpr(lambdaExp->getLambda(), newName);
			}
			if(BindExprPtr bindExp = node.isa<core::BindExprPtr>()) {
				auto call = bindExp->getCall();
				auto lambda = call->getFunctionExpr().isa<LambdaExprPtr>();
				assert_true(lambda) << "Trying to rename bind without lambda expr";
				auto newLambda = renameLambdaReference(lambda, newName);
				auto newCall = builder.callExpr(call->getType(), newLambda, call->getArgumentDeclarations());
				core::transform::utils::migrateAnnotations(call, newCall);
				ret = builder.bindExpr(bindExp->getParameters(), newCall);
			}

			assert_true(ret) << "Trying to rename unsupported node type";

			core::transform::utils::migrateAnnotations(node, ret);
			return ret.as<T>();
		}

	}

	class OMPSemaMapper : public insieme::core::transform::CachedNodeMapping {
		NodeManager& nodeMan;
		IRBuilder build;
		const lang::BasicGenerator& basic;
		const lang::ParallelExtension& parExt;
		const lang::ReferenceExtension& refExt;
		us::PointerSet<CompoundStmtPtr> toFlatten; // set of compound statements to flatten one step further up

		// this stack is used to keep track of which variables are shared in enclosing constructs, to correctly parallelize
		std::stack<VariableList> sharedVarStack;

		// literal markers for "ordered" implementation
		const LiteralPtr orderedCountLit, orderedItLit, orderedIncLit;

		int nameSuffix;

	  public:
		OMPSemaMapper(NodeManager& nodeMan)
			: nodeMan(nodeMan), build(nodeMan), basic(nodeMan.getLangBasic()), parExt(nodeMan.getLangExtension<lang::ParallelExtension>()),
			  refExt(nodeMan.getLangExtension<lang::ReferenceExtension>()), toFlatten(),
			  orderedCountLit(build.literal("ordered_counter", build.refType(basic.getInt8(), false, true))),
			  orderedItLit(build.literal("ordered_loop_it", basic.getInt8())), orderedIncLit(build.literal("ordered_loop_inc", basic.getInt8())),
			  nameSuffix(0) {}

	  protected:
		template<typename T>
		T makeUniqueName(const T& node, const string& prefix) {
			return renameLambdaReference(node, format("_ins_%s_%d", prefix, nameSuffix++));
		}

		// Identifies omp annotations and uses the correct methods to deal with them
		// except for threadprivate, all omp annotations are on statement or expression marker nodes
		virtual const NodePtr resolveElement(const NodePtr& node) {
			NodePtr newNode;
			sharedVarStackEnter(node);
			if(node->getNodeCategory() == NC_Type) { // go top-down for types!
				newNode = handleTypes(node);
				if(newNode == node) { newNode = node->substitute(nodeMan, *this); }
				return newNode;
			}
			if(BaseAnnotationPtr anno = node->getAnnotation(BaseAnnotation::KEY)) {
				if(auto mExp = dynamic_pointer_cast<const MarkerExpr>(node)) {
					newNode = mExp->getSubExpression()->substitute(nodeMan, *this);
				} else if(auto mStmt = dynamic_pointer_cast<const MarkerStmt>(node)) {
					newNode = mStmt->getSubStatement()->substitute(nodeMan, *this);
				} else {
					// mayhap it is threadprivate, my eternal nemesis!
					if(std::dynamic_pointer_cast<ThreadPrivate>(anno->getAnnotationList().front())) {
						newNode = node;
					} else {
						std::cout << "Annotated Node: " << *node << "\n";
						assert_fail() << "OMP annotation on non-marker node.";
					}
				}
				// LOG(DEBUG) << "omp annotation(s) on: \n" << printer::PrettyPrinter(newNode);
				std::for_each(anno->getAnnotationListRBegin(), anno->getAnnotationListREnd(), [&](AnnotationPtr subAnn) {
					newNode = flattenCompounds(newNode);
					if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
						newNode = handleParallel(static_pointer_cast<const Statement>(newNode), parAnn);
					} else if(auto taskAnn = std::dynamic_pointer_cast<Task>(subAnn)) {
						newNode = handleTask(static_pointer_cast<const Statement>(newNode), taskAnn);
					} else if(auto forAnn = std::dynamic_pointer_cast<For>(subAnn)) {
						newNode = handleFor(static_pointer_cast<const Statement>(newNode), forAnn);
					} else if(auto orderedAnn = std::dynamic_pointer_cast<Ordered>(subAnn)) {
						newNode = handleOrdered(static_pointer_cast<const Statement>(newNode), orderedAnn);
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
					} else if(auto taskwaitAnn = std::dynamic_pointer_cast<TaskWait>(subAnn)) {
						newNode = handleTaskWait(static_pointer_cast<const Statement>(newNode), taskwaitAnn);
					} else if(auto atomicAnn = std::dynamic_pointer_cast<Atomic>(subAnn)) {
						newNode = handleAtomic(static_pointer_cast<const Statement>(newNode), atomicAnn);
					} else if(std::dynamic_pointer_cast<ThreadPrivate>(subAnn)) {
						newNode = handleThreadprivate(newNode);
					} else {
						LOG(ERROR) << "Unhandled OMP annotation: " << *subAnn;
						assert_fail();
					}
				});
				// LOG(DEBUG) << "replaced with: \n" << printer::PrettyPrinter(newNode);
			} else {
				// check whether it is a struct-init expression of a lock
				if(node->getNodeType() == NT_InitExpr && isLockStructType(node.as<ExpressionPtr>()->getType())) {
					// replace with uninitialized lock
					newNode = build.getZero(parExt.getLock());
				} else {
					// no changes required at this level, recurse
					newNode = node->substitute(nodeMan, *this);
				}
			}
			newNode = flattenCompounds(newNode);
			newNode = handleFunctions(newNode);
			if(LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(newNode)) {
				newNode = transform::correctRecursiveLambdaVariableUsage(nodeMan, lambda);
			}
			// migrate annotations if applicable
			if(newNode != node) { transform::utils::migrateAnnotations(node, newNode); }
			sharedVarStackLeave(node);
			return newNode;
		}

		// called upon entering a new node, to update the shared var stack
		void sharedVarStackEnter(const NodePtr& node) {
			// push empty on entering new function
			if(node.isa<LambdaDefinitionPtr>()) { sharedVarStack.push(VariableList()); }
			// handle omp annotations
			if(BaseAnnotationPtr anno = node->getAnnotation(BaseAnnotation::KEY)) {
				std::for_each(anno->getAnnotationListRBegin(), anno->getAnnotationListREnd(), [&](AnnotationPtr subAnn) {
					// if parallel, push new var list with its explicitly and implicitly shared variables
					if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
						VariableList free = core::analysis::getFreeVariables(node);
						VariableList autoShared;
						std::copy_if(free.begin(), free.end(), back_inserter(autoShared), [&](const VariablePtr& var) {
							return !(parAnn->hasPrivate() && contains(parAnn->getPrivate(), var))
							       && !(parAnn->hasFirstPrivate() && contains(parAnn->getFirstPrivate(), var));
						});
						sharedVarStack.push(autoShared);
					}
					// if other (non-parallel) data sharing clause, remove explicitly privatized vars from stack
					else if(auto dataAnn = std::dynamic_pointer_cast<DatasharingClause>(subAnn)) {
						VariableList curShared = sharedVarStack.top();
						VariableList newShared;
						sharedVarStack.pop();
						std::copy_if(curShared.begin(), curShared.end(), back_inserter(newShared), [&](const VariablePtr& var) {
							return !(dataAnn->hasPrivate() && contains(dataAnn->getPrivate(), var))
							       && !(dataAnn->hasFirstPrivate() && contains(dataAnn->getFirstPrivate(), var));
						});
						sharedVarStack.push(newShared);
					}
				});
			}
		}

		// called upon leaving a node *with the original node*, to update the shared var stack
		void sharedVarStackLeave(const NodePtr& node) {
			// pop empty on leaving new function
			if(node.isa<LambdaDefinitionPtr>()) {
				assert(sharedVarStack.size() > 0 && sharedVarStack.top() == VariableList() && "leaving lambda def: shared var stack corrupted");
				sharedVarStack.pop();
			}
			// handle omp annotations
			if(BaseAnnotationPtr anno = node->getAnnotation(BaseAnnotation::KEY)) {
				std::for_each(anno->getAnnotationListRBegin(), anno->getAnnotationListREnd(), [&](AnnotationPtr subAnn) {
					// if parallel, pop a var list
					if(auto parAnn = std::dynamic_pointer_cast<Parallel>(subAnn)) {
						assert_gt(sharedVarStack.size(), 0) << "leaving omp parallel: shared var stack corrupted";
						sharedVarStack.pop();
					}
				});
			}
			// check stack integrity when leaving program
			if(node.isa<ProgramPtr>()) { assert_eq(sharedVarStack.size(), 0) << "ending omp translation: shared var stack corrupted"; }
		}

		// flattens generated compound statements if requested
		// used to preserve the correct scope for variable declarations
		NodePtr flattenCompounds(const NodePtr& newNode) {
			if(!toFlatten.empty()) {
				if(CompoundStmtPtr newCompound = dynamic_pointer_cast<const CompoundStmt>(newNode)) {
					// LOG(DEBUG) << "Starting flattening for: " << printer::PrettyPrinter(newCompound);
					// LOG(DEBUG) << ">- toFlatten: " << toFlatten;
					StatementList sl = newCompound->getStatements();
					StatementList newSl;
					for(auto i = sl.begin(); i != sl.end(); ++i) {
						CompoundStmtPtr innerCompound = dynamic_pointer_cast<const CompoundStmt>(*i);
						if(innerCompound && toFlatten.contains(innerCompound)) {
							// LOG(DEBUG) << "Flattening: " << printer::PrettyPrinter(innerCompound);
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

		ExpressionPtr buildLockArgument(const CallExprPtr& callExp) {
			ExpressionPtr arg = callExp->getArgument(0);
			// nothing to do in this case, eg. ref<lock,f,f>
			if(core::lang::isReferenceTo(arg, parExt.getLock()))
				return arg;

			auto& ptrExt = nodeMan.getLangExtension<core::lang::PointerExtension>();
			auto& refExt = nodeMan.getLangExtension<core::lang::ReferenceExtension>();
			// we get a pointer created from a ref, simply strip off conversion
			if(analysis::isCallOf(arg, ptrExt.getPtrFromRef()))
				return analysis::getArgument(arg, 0);
			// we get a pointer, convert to ref
			if(core::lang::isPointer(arg))
				return core::lang::buildPtrToRef(arg);
			// we get a deref of a ref, check if enclosed node is a pointer and convert it to a ref if required
			if(analysis::isCallOf(arg, refExt.getRefDeref())) {
				auto argOfDeref = arg.as<CallExprPtr>()->getArgument(0);
				if(core::lang::isPointer(argOfDeref)) {
					return core::lang::buildPtrToRef(argOfDeref);
				}
			}
			assert_fail() << "OMP FE: unexpected lock function argument type: " << dumpColor(arg->getType());
			return CallExprPtr();
		}

		// implements OpenMP built-in functions by replacing the call expression
		NodePtr handleFunctions(const NodePtr& newNode) {
			if(CallExprPtr callExp = dynamic_pointer_cast<const CallExpr>(newNode)) {
				auto fun = callExp->getFunctionExpr();
				auto& refExt = nodeMan.getLangExtension<core::lang::ReferenceExtension>();
				auto& timeExt = nodeMan.getLangExtension<core::lang::TimeExtension>();

				if(refExt.isRefScalarToRefArray(fun)) {
					ExpressionPtr arg = callExp->getArgument(0);
					if(analysis::isRefOf(arg, parExt.getLock())) { return arg; }
					return newNode;
				} else if(LiteralPtr litFunExp = dynamic_pointer_cast<const Literal>(fun)) {
					const string& funName = insieme::utils::demangle(litFunExp->getStringValue());
					if(funName == "omp_get_thread_num") {
						return build.numericCast(build.getThreadId(), basic.getInt4());
					} else if(funName == "omp_get_num_threads") {
						return build.numericCast(build.getThreadGroupSize(), basic.getInt4());
					} else if(funName == "omp_get_max_threads") {
						return build.numericCast(build.getDefaultThreads(), basic.getInt4());
					} else if(funName == "omp_get_wtime") {
						return build.callExpr(basic.getReal8(), timeExt.getGetTime());
					} else if(funName == "omp_set_num_threads") {
						auto newCall = build.callExpr(build.literal("irt_set_default_parallel_wi_count", fun->getType()), callExp->getArgument(0));
						return newCall;
					}
					// OMP Locks --------------------------------------------
					else if(funName == "omp_init_lock") {
						return build.initLock(buildLockArgument(callExp));
					} else if(funName == "omp_set_lock") {
						return build.acquireLock(buildLockArgument(callExp));
					} else if(funName == "omp_unset_lock") {
						return build.releaseLock(buildLockArgument(callExp));
					} else if(funName == "omp_test_lock") {
						// as omp_test_lock is delared as 'int omp_test_lock(omp_lock_t *)' we need to convert the return value
						return utils::buildBoolToInt(build.tryAcquireLock(buildLockArgument(callExp)));
					}
					// Unhandled OMP functions
					else if(funName.substr(0, 4) == "omp_") {
						assert_fail() << "Unknown OpenMP function: " << funName;
					}
				}
			}
			return newNode;
		}

		bool isLockStructType(TypePtr type) {
			if(type.isa<core::GenericTypePtr>()) {
				auto typeName = insieme::utils::demangle(type.as<core::GenericTypePtr>()->getName()->getValue());
				return typeName == "omp_lock_t" || typeName == "_omp_lock_t";
			}

			return false;
		}

		// implements OpenMP built-in types by replacing them with the correct IR constructs
		NodePtr handleTypes(const NodePtr& newNode) {
			if(TypePtr type = newNode.isa<core::TypePtr>()) {
				if(isLockStructType(type)) {
					return parExt.getLock();
				}
			}
			return newNode;
		}

		// helper that checks for variable in subtree
		bool isInTree(VariablePtr var, NodePtr tree) {
			bool inside = false;
			visitDepthFirstOnceInterruptible(tree, [&](const VariablePtr& cVar) {
				if(*cVar == *var) { inside = true; }
				return inside;
			});
			return inside;
		}

		// beware! the darkness hath returned to prey upon innocent globals yet again
		// will the frontend prevail?
		// new and improved crazyness! does not directly implement accesses, replaces with variable
		// masterCopy -> return access expression for master copy of tp value
		NodePtr handleThreadprivate(const NodePtr& node, bool masterCopy = false) {
			//NamedValuePtr member = dynamic_pointer_cast<const NamedValue>(node);
			//if(member) {
			//	// cout << "%%%%%%%%%%%%%%%%%%\nMEMBER THREADPRIVATE:\n" << *member << "\n";
			//	ExpressionPtr initExp = member->getValue();
			//	StringValuePtr name = member->getName();
			//	ExpressionPtr vInit = build.vectorInit(initExp, build.concreteIntTypeParam(MAX_THREADPRIVATE));
			//	fixStructType = true;
			//	return build.namedValue(name, vInit);
			//}

			//// prepare index expression
			//ExpressionPtr indexExpr = build.castExpr(basic.getUInt8(), build.getThreadId());
			//if(masterCopy) { indexExpr = build.literal(basic.getUInt8(), "0"); }

			//CallExprPtr call = node.isa<CallExprPtr>();
			//if(call) {
			//	// cout << "%%%%%%%%%%%%%%%%%%\nCALL THREADPRIVATE:\n" << *call << "\n";
			//	assert(call->getFunctionExpr() == basic.getCompositeRefElem() && "Threadprivate not on composite ref elem access");
			//	ExpressionList args = call->getArguments();
			//	TypePtr elemType = core::analysis::getReferencedType(call->getType());
			//	elemType = build.vectorType(elemType, build.concreteIntTypeParam(MAX_THREADPRIVATE));
			//	CallExprPtr memAccess = build.callExpr(build.refType(elemType), basic.getCompositeRefElem(), args[0], args[1], build.getTypeLiteral(elemType));
			//	ExpressionPtr accessExpr = build.arrayRefElem(memAccess, indexExpr);
			//	if(masterCopy) { return accessExpr; }
			//	// if not a master copy, optimize access
			//	if(thisLambdaTPAccesses.find(accessExpr) != thisLambdaTPAccesses.end()) {
			//		// repeated access, just use existing variable
			//		return thisLambdaTPAccesses[accessExpr];
			//	} else {
			//		// new access, generate var and add to map
			//		VariablePtr varP = build.variable(accessExpr->getType());
			//		assert_eq(varP->getType()->getNodeType(), NT_RefType) << "Non-ref threadprivate!";
			//		thisLambdaTPAccesses.insert(std::make_pair(accessExpr, varP));
			//		return varP;
			//	}
			//}
			//LiteralPtr literal = node.isa<LiteralPtr>();
			//if(literal) {
			//	// std::cout << "Encountered thread-private annotation at literal: " << *literal << " of type " << *literal->getType() << "\n";
			//	assert_eq(literal->getType()->getNodeType(), NT_RefType);
			//	// alter the type of the literal
			//	TypePtr newType =
			//	    build.refType(build.vectorType(core::analysis::getReferencedType(literal->getType()), build.concreteIntTypeParam(MAX_THREADPRIVATE)));
			//	// create the new literal
			//	LiteralPtr newLiteral = build.literal(literal->getValue(), newType);
			//	// create an expression accessing the literal
			//	ExpressionPtr accessExpr = build.arrayRefElem(newLiteral, indexExpr);
			//	return accessExpr;
			//}
			//assert_fail() << "OMP threadprivate annotation on non-member / non-call / non-literal";
			assert_not_implemented() << "OMP threadprivate not yet supported";
			return NodePtr();
		}

		// implements omp flush by generating INSPIRE flush() calls
		NodePtr handleFlush(const StatementPtr& stmt, const FlushPtr& flush) {
			StatementList replacements;
			if(flush->hasVarList()) {
				const VarList& vars = flush->getVarList();
				for_each(vars, [&](const ExpressionPtr& exp) { replacements.push_back(build.callExpr(basic.getUnit(), basic.getFlush(), exp)); });
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
					operation =
					    build.assign(varExp, build.add(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				case Reduction::MINUS:
					operation =
					    build.assign(varExp, build.add(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				case Reduction::MUL:
					operation =
					    build.assign(varExp, build.mul(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				case Reduction::AND:
					operation = build.assign(
					    varExp, build.bitwiseAnd(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				case Reduction::OR:
					operation = build.assign(
					    varExp, build.bitwiseOr(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				case Reduction::XOR:
					operation = build.assign(
					    varExp, build.bitwiseXor(build.deref(varExp), build.deref(static_pointer_cast<const Expression>(publicToPrivateMap[varExp]))));
					break;
				// TODO: re-enable when new conversion from int to bool is available
				//			case Reduction::LAND:
				//				operation = build.assign(varExp, build.logicAnd(build.deref(varExp), build.deref(static_pointer_cast<const
				//Expression>(publicToPrivateMap[varExp]))));
				//				break;
				//			case Reduction::LOR:
				//				operation = build.assign(varExp, build.logicOr(build.deref(varExp), build.deref(static_pointer_cast<const
				//Expression>(publicToPrivateMap[varExp]))));
				//				break;
				default:
					LOG(ERROR) << "OMP reduction operator: " << Reduction::opToStr(clause->getReduction().getOperator());
					assert_fail() << "Unsupported reduction operator";
				}
				replacements.push_back(operation);
			});
			return makeCritical(build.compoundStmt(replacements), string("reduce_") + toString(++redId));
		}

		// returns the correct initial reduction value for the given operator and type
		ExpressionPtr getReductionInitializer(Reduction::Operator op, const TypePtr& type) {
			ExpressionPtr ret;
			assert_true(core::analysis::isRefType(type)) << "OMP reduction on non-reference type";
			TypePtr elemType = core::analysis::getReferencedType(type);
			switch(op) {
			case Reduction::PLUS:
			case Reduction::MINUS:
			case Reduction::OR:
			case Reduction::XOR: ret = build.literal("0", elemType); break;
			case Reduction::MUL:
			case Reduction::AND: ret = build.literal("1", elemType); break;
			case Reduction::LAND: ret = build.boolLit(true); break;
			case Reduction::LOR: ret = build.boolLit(false); break;
			default: LOG(ERROR) << "OMP reduction operator: " << Reduction::opToStr(op); assert_fail() << "Unsupported reduction operator";
			}
			return ret;
		}

		StatementPtr handleThisCapture(const StatementPtr& stmtNode, StatementList& resultStmts) {
			// for C++, we need special handling for "this"
			auto freeVars = core::analysis::getFreeVariables(stmtNode);
			for(const auto& var : freeVars) {
				if(core::lang::isReference(var) && core::lang::isReference(core::analysis::getReferencedType(var))) {
					auto newThis = build.variable(var->getType());
					auto newThisDecl = build.declarationStmt(newThis, build.deref(var));
					resultStmts.push_back(newThisDecl);
					return core::transform::replaceAllGen(nodeMan, stmtNode, var, newThis);
				}
			}
			return stmtNode;
		}

		StatementPtr implementDataClauses(const StatementPtr& stmtNodeIn, const DatasharingClause* clause, StatementList& outsideDecls,
		                                  StatementList postFix = StatementList()) {

			auto stmtNode = handleThisCapture(stmtNodeIn, outsideDecls);

			const For* forP = dynamic_cast<const For*>(clause);
			const Parallel* parallelP = dynamic_cast<const Parallel*>(clause);
			const Task* taskP = dynamic_cast<const Task*>(clause);
			StatementList replacements;
			VarList allp;
			VarList firstPrivates;
			VarList lastPrivates;
			StatementList ifStmtBodyLast;
			// for OMP tasks, default free variable binding is threadprivate
			if(taskP) {
				auto&& freeVarsAndFuns = core::analysis::getFreeVariables(stmtNode);
				VariableList freeVars;
				// free function variables should not be captured
				std::copy_if(freeVarsAndFuns.begin(), freeVarsAndFuns.end(), back_inserter(freeVars), [&](const VariablePtr& v) {
					auto t = v.getType();
					// free function variables should not be captured
					bool ret = !t.isa<FunctionTypePtr>();
					// neither should pointers be depointerized
					ret = ret && !(core::analysis::isRefType(t) && (core::lang::isArray(core::analysis::getReferencedType(t))));
					// explicitly declared variables should not be auto-privatized
					if(taskP->hasShared()) { ret = ret && !contains(taskP->getShared(), v); }
					if(taskP->hasFirstPrivate()) { ret = ret && !contains(taskP->getFirstPrivate(), v); }
					if(taskP->hasPrivate()) { ret = ret && !contains(taskP->getPrivate(), v); }
					if(taskP->hasReduction()) { ret = ret && !contains(taskP->getReduction().getVars(), v); }
					// variables declared shared in all enclosing constructs, up to and including
					// the innermost parallel construct, should not be privatized
					ret = ret && !contains(sharedVarStack.top(), v);
					return ret;
				});
				firstPrivates.insert(firstPrivates.end(), freeVars.begin(), freeVars.end());
				allp.insert(allp.end(), freeVars.begin(), freeVars.end());
				LOG(DEBUG) << "==========================================\n" << freeVars << "\n======================\n";
			}
			if(clause->hasFirstPrivate()) {
				firstPrivates.insert(firstPrivates.end(), clause->getFirstPrivate().begin(), clause->getFirstPrivate().end());
				allp.insert(allp.end(), clause->getFirstPrivate().begin(), clause->getFirstPrivate().end());
			}
			if(forP && forP->hasLastPrivate()) {
				lastPrivates.insert(lastPrivates.end(), forP->getLastPrivate().begin(), forP->getLastPrivate().end());
				allp.insert(allp.end(), forP->getLastPrivate().begin(), forP->getLastPrivate().end());
			}
			if(clause->hasPrivate()) {
				allp.insert(allp.end(), clause->getPrivate().begin(), clause->getPrivate().end());
			}
			if(clause->hasReduction()) { allp.insert(allp.end(), clause->getReduction().getVars().begin(), clause->getReduction().getVars().end()); }
			NodeMap publicToPrivateMap;
			NodeMap privateToPublicMap;
			// implement private copies where required
			for(const auto& varExp : allp) {
				const auto& expType = varExp->getType();
				VariablePtr pVar = build.variable(expType);
				publicToPrivateMap[varExp] = pVar;
				privateToPublicMap[pVar] = varExp;
				DeclarationStmtPtr decl = build.declarationStmt(pVar);
				if(contains(firstPrivates, varExp)) {
					// make sure to actually get *copies* for firstprivate initialization, not copies of references
					if(core::analysis::isRefType(expType)) {
						VariablePtr fpPassVar = build.variable(core::analysis::getReferencedType(expType));
						DeclarationStmtPtr fpPassDecl = build.declarationStmt(fpPassVar, build.deref(varExp));
						outsideDecls.push_back(fpPassDecl);
						decl = build.declarationStmt(pVar, fpPassVar);
					} else {
						decl = build.declarationStmt(pVar, varExp);
					}
				}
				if(clause->hasReduction() && contains(clause->getReduction().getVars(), varExp)) {
					decl = build.declarationStmt(pVar, getReductionInitializer(clause->getReduction().getOperator(), expType));
				}
				if(contains(lastPrivates, varExp)) { ifStmtBodyLast.push_back(build.assign(varExp, build.deref(pVar))); }
				replacements.push_back(decl);
			}
			// pFor firstprivate semantics require additional barrier
			if(forP && clause->hasFirstPrivate()) { replacements.push_back(build.barrier()); }
			// implement copyin for threadprivate vars
			if(parallelP && parallelP->hasCopyin()) {
				for(const ExpressionPtr& varExp : parallelP->getCopyin()) {
					// assign master copy to private copy
					StatementPtr assignment = build.assign(static_pointer_cast<const Expression>(handleThreadprivate(varExp)),
					                                       build.deref(static_pointer_cast<const Expression>(handleThreadprivate(varExp, true))));
					replacements.push_back(assignment);
				}
			}
			// find var addresses to replace (only within same lambda in original program)
			// make local copies of global vars available in sub-lambdas introduced by the insieme compiler
			std::map<ExpressionAddress, VariablePtr> publicToPrivateAddressMap;
			visitDepthFirstPrunable(NodeAddress(stmtNode), [&](const ExpressionAddress& expA) -> bool {
				// prune if new named lambda
				auto lambda = expA.getAddressedNode().isa<LambdaExprPtr>();
				if(lambda && core::annotations::hasAttachedName(lambda)) { return true; }
				// check if privatized expression
				for(auto mapping : publicToPrivateMap) {
					if(*mapping.first == *expA.getAddressedNode()) {
						publicToPrivateAddressMap[expA] = mapping.second.as<VariablePtr>();
						return true;
					}
				}
				return false; // don't prune
			});
			StatementPtr subStmt = stmtNode;
			// the variable will be made available by pushInto if we are inside a lambda introduced by insieme
			if(!publicToPrivateAddressMap.empty()) { subStmt = transform::pushInto(nodeMan, publicToPrivateAddressMap).as<StatementPtr>(); }

			// specific handling if clause is a omp for
			if(forP) {
				if(forP->hasOrdered()) { subStmt = processOrderedFor(subStmt); }
				// Handling lastlocal
				if(lastPrivates.size()) {
					auto outer = static_pointer_cast<const ForStmt>(subStmt);
					StatementList newForBodyStmts;
					for_each(outer->getBody()->getStatements(), [&](core::StatementPtr elem) { newForBodyStmts.push_back(elem); });
					//auto condition = build.eq(build.forStmtFinalValue(outer), outer->getIterator());
					assert_not_implemented() << "forStmtFinalValue needs to be replaced";
					//auto ifStmt = build.ifStmt(condition, build.compoundStmt(ifStmtBodyLast));
					//newForBodyStmts.push_back(ifStmt);
					//auto newForStmt = build.forStmt(outer->getDeclaration(), outer->getEnd(), outer->getStep(), build.compoundStmt(newForBodyStmts));
					//subStmt = build.pfor(newForStmt);
				} else {
					subStmt = build.pfor(static_pointer_cast<const ForStmt>(subStmt));
				}
			}
			replacements.push_back(subStmt);
			// implement reductions
			if(clause->hasReduction()) { replacements.push_back(implementReductions(clause, publicToPrivateMap)); }
			// specific handling if clause is a omp for (insert barrier if not nowait)
			if(forP && !forP->hasNoWait()) { replacements.push_back(build.barrier()); }
			// append postfix
			copy(postFix.cbegin(), postFix.cend(), back_inserter(replacements));

			return build.compoundStmt(replacements);
		}

		NodePtr markUnordered(const NodePtr& node) {
			auto& attr = nodeMan.getLangExtension<core::analysis::AttributeExtension>();
			auto printfNodePtr = build.literal("printf", build.parseType("(ref<array<char,1> >, var_list) -> int<4>"));
			return transform::replaceAll(nodeMan, node, printfNodePtr, core::analysis::addAttribute(printfNodePtr, attr.getUnordered()));
		}

		NodePtr handleParallel(const StatementPtr& stmtNode, const ParallelPtr& par) {
			StatementList resultStmts;

			// handle implicit taskwait in postfix of task
			StatementList postFix;
			postFix.push_back(build.mergeAll());
			auto newStmtNode = implementDataClauses(stmtNode, &*par, resultStmts, postFix);
			auto parLambda = transform::extractLambda(nodeMan, newStmtNode);
			// mark printf as unordered
			parLambda = markUnordered(parLambda).as<BindExprPtr>();
			parLambda = makeUniqueName(parLambda, "omp_parallel");
			auto range = build.getThreadNumRange(1); // if no range is specified, assume 1 to infinity
			if(par->hasNumThreads()) { range = build.getThreadNumRange(par->getNumThreads(), par->getNumThreads()); }
			auto jobExp = build.jobExpr(range, parLambda.as<ExpressionPtr>());
			auto parallelCall = build.callExpr(parExt.getParallel(), jobExp);
			auto mergeCall = build.callExpr(parExt.getMerge(), parallelCall);

			resultStmts.push_back(mergeCall);
			// if clause handling
			assert_false(par->hasIf()) << "OMP parallel if clause not supported";

			StatementPtr ret = build.compoundStmt(resultStmts);
			// add code for "ordered" pfors in this parallel, if any
			ret = processOrderedParallel(ret);
			return ret;
		}

		NodePtr handleTask(const StatementPtr& stmtNode, const TaskPtr& par) {
			StatementList resultStmts;
			auto newStmtNode = implementDataClauses(stmtNode, &*par, resultStmts);
			auto parLambda = transform::extractLambda(nodeMan, newStmtNode);
			parLambda = makeUniqueName(parLambda, "omp_task");
			auto range = build.getThreadNumRange(1, 1); // range for tasks is always 1

			JobExprPtr jobExp = build.jobExpr(range, parLambda.as<ExpressionPtr>());
			auto parallelCall = build.callExpr(parExt.getParallel(), jobExp);
			insieme::annotations::migrateMetaInfos(stmtNode, parallelCall);
			resultStmts.push_back(parallelCall);

			return build.compoundStmt(resultStmts);
		}

		NodePtr handleTaskWait(const StatementPtr& stmtNode, const TaskWaitPtr& par) {
			CompoundStmtPtr replacement = build.compoundStmt(build.mergeAll(), stmtNode);
			toFlatten.insert(replacement);
			return replacement;
		}

		ExpressionPtr getVolatileOrderedCount() {
			return orderedCountLit;
		}

		// Process parallel regions containing omp fors with the "ordered" clause
		StatementPtr processOrderedParallel(const StatementPtr& origStmt) {
			VariablePtr orderedCountVar = build.variable(orderedCountLit->getType());
			std::map<ExpressionAddress, VariablePtr> pushMap;
			visitDepthFirst(StatementAddress(origStmt), [&](const LiteralAddress& lit) { // could prune this to improve performance
				if(lit.getAddressedNode() == orderedCountLit) { pushMap.insert(std::make_pair(lit, orderedCountVar)); }
			});
			// if we don't find any orderedCountLit literals, we don't have to do anything
			if(pushMap.empty()) { return origStmt; }
			// create variable outside the parallel
			auto countVarDecl = build.declarationStmt(orderedCountVar);
			// push ordered variable to where it is needed
			auto newStmt = transform::pushInto(nodeMan, pushMap).as<CompoundStmtPtr>();
			// build new compound and return it
			newStmt = build.compoundStmt(countVarDecl, newStmt);
			return newStmt;
		}

		// Process "for" statements which feature the "ordered" clause
		StatementPtr processOrderedFor(const StatementPtr& origStmt) {
			auto& b = build;
			ForStmtPtr forStmt = origStmt.as<ForStmtPtr>();
			VariablePtr iterator = forStmt->getIterator();
			ExpressionPtr start = forStmt->getStart();
			ExpressionPtr step = forStmt->getStep();
			CompoundStmtPtr body = forStmt->getBody();
			// create variable for pushing step
			auto stepVar = b.variable(step->getType());
			auto stepVarDecl = b.declarationStmt(stepVar, step);
			// set value in first iteration of loop
			auto initialSet = b.ifStmt(b.eq(iterator, start), b.assign(getVolatileOrderedCount(), start));
			// add safety net at end of body (for cases where ordered section not encountered)
			auto waitLoop = b.callExpr(nodeMan.getLangExtension<core::lang::ParallelExtension>().getBusyLoop(),
			                           b.wrapLazy(b.ne(b.deref(getVolatileOrderedCount()), orderedItLit)));
			auto atomic = b.atomicAssignment(b.assign(getVolatileOrderedCount(), b.add(b.deref(getVolatileOrderedCount()), orderedIncLit)));
			auto increment = b.compoundStmt(waitLoop, atomic);
			auto conditionalIncrease = b.ifStmt(b.ge(orderedIncLit, b.getZero(step->getType())), // deal with loops counting down as well as up
			                                    b.ifStmt(b.le(b.deref(getVolatileOrderedCount()), orderedItLit), increment),
			                                    b.ifStmt(b.ge(b.deref(getVolatileOrderedCount()), orderedItLit), increment));
			// build new body
			CompoundStmtPtr newBody = b.compoundStmt(stepVarDecl, initialSet, body, conditionalIncrease);
			// push loop step to required position
			std::map<ExpressionAddress, VariablePtr> pushMap;
			visitDepthFirst(CompoundStmtAddress(newBody), [&](const LiteralAddress& lit) { // could prune this to improve performance
				if(lit.getAddressedNode() == orderedIncLit) { pushMap.insert(std::make_pair(lit, stepVar)); }
				if(lit.getAddressedNode() == orderedItLit) { pushMap.insert(std::make_pair(lit, iterator)); }
			});
			newBody = transform::pushInto(nodeMan, pushMap).as<CompoundStmtPtr>();
			// replace original body
			forStmt = transform::replaceNode(nodeMan, ForStmtAddress(forStmt)->getBody(), newBody).as<ForStmtPtr>();
			return forStmt;
		}

		NodePtr handleOrdered(const StatementPtr& stmtNode, const OrderedPtr& orderedP) {
			auto& b = build;
			auto int8 = basic.getInt8();
			auto waitLoop = b.callExpr(nodeMan.getLangExtension<core::lang::ParallelExtension>().getBusyLoop(),
			                           b.wrapLazy(b.ne(b.deref(getVolatileOrderedCount()), orderedItLit)));
			auto increment = b.atomicAssignment(b.assign(getVolatileOrderedCount(), b.add(b.deref(getVolatileOrderedCount()), orderedIncLit)));
			return b.compoundStmt(waitLoop, stmtNode, increment);
		}

		NodePtr handleFor(const StatementPtr& stmtNode, const ForPtr& forP, bool isParallel = false) {
			assert_true(stmtNode.getNodeType() == NT_ForStmt) << "Trying to attach OpenMP for to non-for statement.\n" << dumpColor(stmtNode);
			ForStmtPtr outer = dynamic_pointer_cast<const ForStmt>(stmtNode);
			// outer = collapseForNest(outer);
			StatementList resultStmts;
			auto newStmtNode = implementDataClauses(outer, &*forP, resultStmts);
			resultStmts.push_back(newStmtNode);
			return build.compoundStmt(resultStmts);
		}

		NodePtr handleParallelFor(const StatementPtr& stmtNode, const ParallelForPtr& pforP) {
			NodePtr newNode = stmtNode;
			newNode = handleFor(static_pointer_cast<const Statement>(newNode), pforP->toFor(), true);
			newNode = handleParallel(static_pointer_cast<const Statement>(newNode), pforP->toParallel());
			return newNode;
		}

		NodePtr handleSingle(const StatementPtr& stmtNode, const SinglePtr& singleP) {
			StatementList replacements;
			// implement single as pfor with 1 item
			auto pforLambdaParams = toVector(build.variable(basic.getInt4()), build.variable(basic.getInt4()), build.variable(basic.getInt4()));
			auto body = transform::extractLambda(nodeMan, stmtNode, pforLambdaParams);
			body = makeUniqueName(body, "omp_single");
			auto pfor = build.pfor(body, build.intLit(0), build.intLit(1), build.intLit(1));
			replacements.push_back(pfor);
			if(!singleP->hasNoWait()) { replacements.push_back(build.barrier()); }
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
			replacements.push_back(build.acquireLock(build.literal(build.refType(parExt.getLock()), name)));
			// push original code fragment
			replacements.push_back(statement);
			// push unlock
			replacements.push_back(build.releaseLock(build.literal(build.refType(parExt.getLock()), name)));
			// build replacement compound
			return build.compoundStmt(replacements);
		}

		NodePtr handleCritical(const StatementPtr& stmtNode, const CriticalPtr& criticalP) {
			string name = "default";
			if(criticalP->hasName()) { name = criticalP->getName(); }
			CompoundStmtPtr replacement = makeCritical(stmtNode, name);
			toFlatten.insert(replacement);
			return replacement;
		}

		NodePtr handleMaster(const StatementPtr& stmtNode, const MasterPtr& masterP) {
			return build.ifStmt(build.eq(build.getThreadId(), build.getZero(build.getThreadId().getType())), stmtNode);
		}

		NodePtr handleAtomic(const StatementPtr& stmtNode, const AtomicPtr& atomicP) {
			CallExprPtr call = dynamic_pointer_cast<CallExprPtr>(stmtNode);
			if(!call) { cerr << printer::PrettyPrinter(stmtNode) << std::endl; }
			if(refExt.isCallOfRefDeref(call)) call = core::analysis::getArgument(call, 0).isa<core::CallExprPtr>();
			assert_true(call) << "Unhandled OMP atomic";
			auto at = build.atomicAssignment(call);
			// std::cout << "ATOMIC: \n" << printer::PrettyPrinter(at, printer::PrettyPrinter::NO_LET_BINDINGS);
			return at;
		}
	};

	namespace {
		void collectAndRegisterLocks(core::NodeManager& mgr, core::tu::IRTranslationUnit& unit, const core::ExpressionPtr& fragment) {
			// search locks
			visitDepthFirstOnce(fragment, [&](const LiteralPtr& lit) {
				const string& gname = lit->getStringValue();
				if(gname.find("global_omp") != 0) { return; }
				assert_true(analysis::isRefOf(lit, mgr.getLangExtension<lang::ParallelExtension>().getLock()));

				// add lock to global list
				unit.addGlobal(lit);

				// add lock initialization code
				unit.addInitializer(IRBuilder(mgr).initLock(lit));
			});
		}
	}


	core::tu::IRTranslationUnit applySema(const core::tu::IRTranslationUnit& unit, core::NodeManager& mgr) {
		// everything has to run through the OMP sema mapper
		OMPSemaMapper semaMapper(mgr);

		// resulting tu
		core::tu::IRTranslationUnit res(mgr);

		// process the contained types ...
		for(auto& cur : unit.getTypes()) {
			auto mapped = semaMapper.map(cur.second);
			res.addType(cur.first, mapped);
		}

		// ... the functions ...
		for(auto& cur : unit.getFunctions()) {
			// convert implementation
			auto newImpl = semaMapper.map(cur.second);
			collectAndRegisterLocks(mgr, res, newImpl);

			// save result
			res.addFunction(semaMapper.map(cur.first), newImpl);
		}

		// ... the globals ...
		auto& parExt = mgr.getLangExtension<core::lang::ParallelExtension>();
		for(auto& cur : unit.getGlobals()) {
			ExpressionPtr newGlobal = semaMapper.map(cur.first.as<ExpressionPtr>());

			// if it is an access to a thread-private value
			//if(CallExprPtr call = newGlobal.isa<CallExprPtr>()) {
			//	assert_true(core::analysis::isCallOf(call, mgr.getLangBasic().getVectorRefElem()));
			//	newGlobal = call[0]; // take first argument
			//}

			// global locks are initialized by calls, should not have initializers here
			if(parExt.isLock(core::analysis::getReferencedType(newGlobal))) {
				res.addGlobal(newGlobal.as<LiteralPtr>());
				continue;
			}
			res.addGlobal(newGlobal.as<LiteralPtr>(), (cur.second) ? semaMapper.map(cur.second) : cur.second);
		}

		// ... and the entry points
		for(auto& cur : unit.getEntryPoints()) {
			res.addEntryPoints(semaMapper.map(cur));
		}

		// done
		return res;
	}


} // namespace omp
} // namespace frontend
} // namespace insieme

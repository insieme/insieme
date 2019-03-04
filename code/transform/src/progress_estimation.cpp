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

#include "insieme/transform/progress_estimation.h"

#include <map>

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_annotation.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/node_replacer.h"


namespace insieme {
namespace transform {

	namespace {

		ProgressReportingType EFFORT_SIMPLE_OP = 1;
		ProgressReportingType EFFORT_BRANCH = 2;
		ProgressReportingType EFFORT_BUILTIN = 1;
		ProgressReportingType EFFORT_FUN_CALL = 5;
		ProgressReportingType EFFORT_FOR_LOOP_ITERATOR = 1;
		ProgressReportingType EFFORT_FOR_LOOP_CONDITION = 1;
		ProgressReportingType EFFORT_FOR_LOOP_ITERATOR_UPDATE = 1;

		struct ReportingParameters {
			ProgressReportingType progressReportingLimit;
			float maxIfElseReportingDifferenceFactor;
			ProgressReportingType minimumReportingThreshold;
		};

		struct UnreportedProgressAnnotation : public core::value_annotation::copy_on_migration {
			ProgressReportingType progress;
			UnreportedProgressAnnotation(const ProgressReportingType& progress) : progress(progress) {}
			bool operator==(const UnreportedProgressAnnotation& other) const {
				return progress == other.progress;
			}
		};

		ProgressReportingType getNodeEffort(const core::NodePtr& node) {
			// get cached value from annotation if present
			if(node.hasAttachedValue<UnreportedProgressAnnotation>()) {
				return node.getAttachedValue<UnreportedProgressAnnotation>().progress;
			}

			// otherwise the effort depends on the node type
			switch(node->getNodeType()) {
				case core::NT_BreakStmt:
				case core::NT_ContinueStmt:
				case core::NT_ReturnStmt:
				case core::NT_DeclarationStmt:
					return EFFORT_SIMPLE_OP;

				case core::NT_SwitchStmt:
					return EFFORT_BRANCH;

				case core::NT_CallExpr: {
					const auto& callee = node.as<core::CallExprPtr>()->getFunctionExpr();
					if(core::lang::isBuiltIn(callee) || core::lang::isDerived(callee)) {
						return EFFORT_BUILTIN;
					} else {
						return EFFORT_FUN_CALL;
					}
				}

				default:
					return 0;
			}
		}

		ProgressReportingType getUnreportedProgress(const core::NodePtr& parent) {
			ProgressReportingType progress = 0;

			core::visitDepthFirstPrunable(parent, [&](const core::NodePtr& node) {
				// don't process or descend into types
				if(node.getNodeCategory() == core::NC_Type) {
					return core::Prune;
				}

				// add the unreported progress for every child node
				progress += getNodeEffort(node);

				// but don't descend into lambda expressions. These already have their unreported progress attached and we already added that
				if(node.getNodeType() == core::NT_LambdaExpr) {
					return core::Prune;
				}
				return core::Descent;
			});

			return progress;
		}

		core::CompoundStmtPtr handleCompound(ProgressReportingType& progress, const core::CompoundStmtPtr& compound, const ReportingParameters& reportingParameters, bool isRoot) {
			core::NodeManager& mgr = compound.getNodeManager();
			core::IRBuilder builder(mgr);
			core::StatementList stmts(compound.getStatements());
			const auto& ext = mgr.getLangExtension<ProgressEstimationExtension>();

			// if the current progress is 0, we insert a new statement into the list and update the iterator
			auto insertProgressReportingCall = [&](core::StatementList::iterator& it) {
				if(progress != 0) {
					it = stmts.insert(it, buildProgressReportingThreadCall(mgr, progress));
					++it;
					progress = 0;
				}
			};

			const auto exitPoints = core::analysis::getExitPoints(compound);
			auto isExitPoint = [&exitPoints](const core::StatementPtr& stmt) {
				return ::any(exitPoints, [&stmt](const auto& exitPoint) { return exitPoint.getDepth() == 2 && exitPoint.getAddressedNode() == stmt; });
			};

			for(auto it = stmts.begin(); it < stmts.end(); ++it) {
				auto stmt = *it;

				// special handling for compounds
				if(const auto& innerCompound = stmt.isa<core::CompoundStmtPtr>()) {
					// compound statement children are handeled recursively, counting from the current progress
					*it = handleCompound(progress, innerCompound, reportingParameters, false);

					// special handling for for loops
				} else if(const auto& forLoop = stmt.isa<core::ForStmtPtr>()) {
					// we have to report the current progress before entering the for loop. We also add some overhead for the loop iterator variable
					progress += EFFORT_FOR_LOOP_ITERATOR;
					insertProgressReportingCall(it);
					// now we handle the body. Each iteration gets a start progress offset which accounts for evaluating the condition as well as updating the iterator
					ProgressReportingType bodyProgress = EFFORT_BRANCH + EFFORT_FOR_LOOP_CONDITION + EFFORT_FOR_LOOP_ITERATOR_UPDATE;
					const auto newBody = handleCompound(bodyProgress, forLoop->getBody(), reportingParameters, true); // enforce reporting of progress at exit points
					*it = core::transform::replaceNode(mgr, core::ForStmtAddress(forLoop)->getBody(), newBody).as<core::ForStmtPtr>();

					// special handling for while loops
				} else if(const auto& whileLoop = stmt.isa<core::WhileStmtPtr>()) {
					// we have to report the current progress before entering the while loop
					insertProgressReportingCall(it);
					// now we handle the body. Each iteration gets a start progress offset which accounts the unreported progress of the condition + some branching overhead
					ProgressReportingType bodyProgress = EFFORT_BRANCH + getUnreportedProgress(whileLoop->getCondition());
					const auto newBody = handleCompound(bodyProgress, whileLoop->getBody(), reportingParameters, true); // enforce reporting of progress at exit points
					*it = core::transform::replaceNode(mgr, core::WhileStmtAddress(whileLoop)->getBody(), newBody).as<core::WhileStmtPtr>();

					// special handling for if/else stmts
				} else if(const auto& ifStmt = stmt.isa<core::IfStmtPtr>()) {
					// acount for the branching overhead and the unreported progress of the condition
					progress += EFFORT_BRANCH + getUnreportedProgress(ifStmt->getCondition());
					// now handle both branches with a start offset of the progress we have at the moment
					ProgressReportingType thenBodyProgress = progress;
					ProgressReportingType elseBodyProgress = progress;
					auto newThenBody = handleCompound(thenBodyProgress, ifStmt->getThenBody(), reportingParameters, true); // enforce reporting of progress at exit points
					auto newElseBody = handleCompound(elseBodyProgress, ifStmt->getElseBody(), reportingParameters, true); // enforce reporting of progress at exit points
					const auto progressDifference = abs(((long long) thenBodyProgress) - ((long long) elseBodyProgress));

					assert_true(newThenBody->size() >= 1);
					assert_true(newElseBody->size() >= 1);
					const auto& lastThenStmt = *(newThenBody.end() - 1);
					const auto& lastElseStmt = *(newElseBody.end() - 1);

					core::NodeMap replacements;
					// if both last statements are reporting calls, none of them reports something above the reporting limit and their difference is sufficiently small
					if(ext.isCallOfAnyReportingLiteral(lastThenStmt) && ext.isCallOfAnyReportingLiteral(lastElseStmt)
							&& thenBodyProgress < reportingParameters.progressReportingLimit && elseBodyProgress < reportingParameters.progressReportingLimit
							&& progressDifference <= reportingParameters.progressReportingLimit * reportingParameters.maxIfElseReportingDifferenceFactor) {
						// we remove the reporting calls at the end and set the current progress to the average of both numbers
						newThenBody = builder.compoundStmt(core::StatementList(newThenBody.begin(), newThenBody.end() - 1));
						newElseBody = builder.compoundStmt(core::StatementList(newElseBody.begin(), newElseBody.end() - 1));
						progress = (thenBodyProgress + elseBodyProgress) / 2;

						// otherwise we keep the bodies as they are and set the current progress to 0
					} else {
						progress = 0;
					}
					replacements[core::IfStmtAddress(ifStmt)->getThenBody()] = newThenBody;
					replacements[core::IfStmtAddress(ifStmt)->getElseBody()] = newElseBody;
					*it = core::transform::replaceAllGen(mgr, ifStmt, replacements);

					// all other nodes are processed the same
				} else {
					const auto stmtProgress = getUnreportedProgress(stmt);

					// if the current node is an exit point of the surrounding stmt
					if(isExitPoint(stmt)) {
						// we add the progress for that exit point as well before reporting our current progress
						progress += stmtProgress;
						insertProgressReportingCall(it);

					} else {
						// if the progress until now and the stmtProgress are more than our limit
						if(progress + stmtProgress > reportingParameters.progressReportingLimit) {
							// we report the progress until here
							insertProgressReportingCall(it);
						}
						progress += stmtProgress;
					}
				}
			}

			// if we do have some unreported progress left and this is the root compound, we report it here
			if(isRoot && progress != 0) {
				stmts.push_back(buildProgressReportingThreadCall(mgr, progress));
			}

			return builder.compoundStmt(stmts);
		}

		core::CompoundStmtPtr inlineSmallLambdaBodies(ProgressReportingType& progress, const core::CompoundStmtPtr& compound, const ProgressReportingType progressReportingLimit) {
			auto& mgr = compound.getNodeManager();
			const auto& ext = mgr.getLangExtension<ProgressEstimationExtension>();
			// only proceed if we could at least have a reporting call and a return stmt
			if(compound.size() >= 2) {
				// if we only have one reporting call
				if(core::analysis::countInstances(compound, ext.getProgressReportingThreadLiteral(), true) == 1) {
					const auto& reportingCall = *(compound.end() - 2);
					// if that is the penultimate stmt and the last one is a return
					if(ext.isCallOfProgressReportingThreadLiteral(reportingCall) && (*(compound.end() - 1)).isa<core::ReturnStmtPtr>()) {
						const auto reportedProgress = getReportedProgress(reportingCall);
						// and the reported progress is below the reporting limit, we inline this progress (i.e. remove the reporting call and increase the unreported effort)
						if(reportedProgress < progressReportingLimit) {
							progress += reportedProgress;
							core::StatementList stmts(compound.begin(), compound.end());
							stmts.erase(stmts.end() - 2);
							return core::IRBuilder(mgr).compoundStmt(stmts);
						}
					}
				}
			}
			return compound;
		}

		core::NodePtr handleParallelAndSingleReportingCalls(const core::NodePtr& node) {
			auto& mgr = node.getNodeManager();
			core::IRBuilder builder(mgr);
			const auto& ext = mgr.getLangExtension<ProgressEstimationExtension>();
			const auto& parallelExt= mgr.getLangExtension<core::lang::ParallelExtension>();

			// collect replacements
			std::map<core::NodeAddress, core::NodePtr> replacements;
			core::visitDepthFirst(core::NodeAddress(node), [&](const core::LiteralAddress& addr){
				// only interested in progress reporting literals (calls)
				const auto& node = addr.getAddressedNode();
				if(node == ext.getProgressReportingThreadLiteral()) {
					// check the path up to root and check, whether we need to replace the reporting literal to a non-thread-local one
					bool isParallel = false;
					for(unsigned i = 0; i < addr.getDepth(); ++i) {
						const auto& ancestor = addr.getParentNode(i);
						// if the first OMP ancestor we find is a parallel construct, we are running this code in parallel
						if(parallelExt.isCallOfParallel(ancestor)) {
							isParallel = true;
							break;

							// if the first OMP ancestor we find is a single construct, we are not running this code in parallel
						} else if(parallelExt.isCallOfPFor(ancestor)
								&& parallelExt.isCallOfGetThreadGroup(core::analysis::getArgument(ancestor, 0))
								&& core::analysis::getArgument(ancestor, 1) == builder.intLit(0)
								&& core::analysis::getArgument(ancestor, 2) == builder.intLit(1)
								&& core::analysis::getArgument(ancestor, 3) == builder.intLit(1)) {
							break;

							// if the first OMP ancestor we find is a master construct, we are not running this code in parallel
						} else if(const auto& ifStmt = ancestor.isa<core::IfStmtPtr>()) {
							const auto& condition = ifStmt.getCondition().isa<core::CallExprPtr>();
							const auto getThreadId = builder.getThreadId();
							if(condition
									&& condition.getFunctionExpr() == builder.getLangBasic().getOperator(getThreadId.getType(), core::lang::BasicGenerator::Eq)
									&& core::analysis::getArgument(condition, 0) == getThreadId
									&& core::analysis::getArgument(condition, 1) == builder.getZero(getThreadId.getType())) {
								break;
							}
						}
					}

					// if we are inside a parallel region, we don't replace the node
					if(!isParallel) {
						replacements[addr] = ext.getProgressReportingLiteral();
					}
				}
			}, true, false);

			if(replacements.empty()) return node;
			// and perform the replacement
			return core::transform::replaceAll(node.getNodeManager(), replacements);
		}

		core::NodePtr removeSmallReportings(const core::NodePtr& node, const ProgressReportingType minimumReportingThreshold) {
			core::IRBuilder builder(node->getNodeManager());
			const auto& reportingExt = node->getNodeManager().getLangExtension<ProgressEstimationExtension>();

			// we handle every compound statement
			return core::transform::transformBottomUp(node, [&](const core::CompoundStmtPtr& compound){
				core::StatementList newBody;
				for(const core::StatementPtr& stmt : compound) {
					// if a statement within the compound is a reporting but it reports less than the requested minimum
					if(reportingExt.isCallOfAnyReportingLiteral(stmt)) {
						if(getReportedProgress(stmt) < minimumReportingThreshold) {
							// we do not keep this statement
							continue;
						}
					}
					newBody.push_back(stmt);
				}

				// create a new compound and migrate annotations if we should create a new one
				if(newBody.size() != compound.size()) {
					auto newCompound = builder.compoundStmt(newBody);
					core::transform::utils::migrateAnnotations(compound, newCompound);
					return newCompound;
				}
				return compound;
			}, core::transform::globalReplacement);
		}

		core::NodePtr applyProgressEstimationImpl(const core::NodePtr& node, const ReportingParameters& reportingParameters) {
			// first we transform all the lambdas bottom to top. We add reporting calls and annotate the unreported progress to them
			auto res = core::transform::transformBottomUp(node, [&](const core::LambdaExprPtr& lambda) {
				auto res = lambda;
				ProgressReportingType progress = 0;

				// we do not modify derived lambdas
				if(core::lang::isDerived(lambda)) {
					progress = EFFORT_BUILTIN;

					// every other lambda is processed though
				} else {
					// we create a new body with inserted reporting calls if necessary
					auto newBody = handleCompound(progress, lambda->getBody(), reportingParameters, false);
					newBody = inlineSmallLambdaBodies(progress, newBody, reportingParameters.progressReportingLimit);
					res = core::transform::replaceNode(res.getNodeManager(), core::LambdaExprAddress(res)->getBody(), newBody).as<core::LambdaExprPtr>();
				}

				// and we attach the unreported progress to the lambda itself
				res.attachValue(UnreportedProgressAnnotation{progress});
				return res;
			}, core::transform::globalReplacement);

			// this is to ensure that if we are handling a compound only (mostly for testing), we also report the progress at it's end
			if(res.getNodeType() == core::NT_CompoundStmt) {
				ProgressReportingType progress = 0;
				res = handleCompound(progress, res.as<core::CompoundStmtPtr>(), reportingParameters, true);
			}

			// Our transformation generated just per-thread reporting calls.
			// Now we transform the code so that all the reporting outside parallel regions is changed to non-thread-local reportings
			res = handleParallelAndSingleReportingCalls(res);

			// remove all reportings below a certain threshold if requested
			if(reportingParameters.minimumReportingThreshold > 0) {
				res = removeSmallReportings(res, reportingParameters.minimumReportingThreshold);
			}

			return res;
		}

		core::CallExprPtr buildReportingCall(core::NodeManager& manager, const core::LiteralPtr& literal, const ProgressReportingType progress) {
			core::IRBuilder builder(manager);
			return builder.callExpr(literal, builder.literal(builder.getLangBasic().getUInt16(), toString(progress)));
		}
	}


	core::CallExprPtr buildProgressReportingCall(core::NodeManager& manager, const ProgressReportingType progress) {
		return buildReportingCall(manager, manager.getLangExtension<ProgressEstimationExtension>().getProgressReportingLiteral(), progress);
	}

	core::CallExprPtr buildProgressReportingThreadCall(core::NodeManager& manager, const ProgressReportingType progress) {
		return buildReportingCall(manager, manager.getLangExtension<ProgressEstimationExtension>().getProgressReportingThreadLiteral(), progress);
	}

	ProgressReportingType getReportedProgress(const core::NodePtr& node) {
		const auto& ext = node.getNodeManager().getLangExtension<ProgressEstimationExtension>();
		if(!ext.isCallOfAnyReportingLiteral(node)) return 0;
		auto uintValue = core::analysis::getArgument(node, 0).as<core::LiteralPtr>();
		return std::stoull(uintValue->getValue()->getValue());
	}

	core::NodePtr applyProgressEstimation(const core::NodePtr& node,
	                                      const ProgressReportingType progressReportingLimit,
	                                      float maxIfElseReportingDifferenceFactor,
	                                      const ProgressReportingType minimumReportingThreshold) {
		ReportingParameters parameters{ progressReportingLimit, maxIfElseReportingDifferenceFactor, minimumReportingThreshold };
		return applyProgressEstimationImpl(node, parameters);
	}

} // end namespace transform
} // end namespace insieme


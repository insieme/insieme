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

#include "insieme/transform/tasks/granularity_tuning.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_mapper.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/sequentialize.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/transform/inline.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/rule.h"
#include "insieme/core/pattern/generator.h"
#include "insieme/core/pattern/ir_generator.h"

namespace insieme {
namespace transform {
namespace tasks {

	namespace {

		using namespace std;
		using namespace insieme::utils::log;
		using namespace insieme::core;
		namespace p = insieme::core::pattern;
		namespace irp = insieme::core::pattern::irp;
		namespace g = insieme::core::pattern::generator;
		namespace irg = insieme::core::pattern::generator::irg;

		class TaskMultiversioner {
			NodeManager& nodeMan;
			IRBuilder build;
			const lang::BasicGenerator& basic;
			const lang::ParallelExtension& parExt;

			vector<JobExprAddress> gatherJobAddresses(const NodePtr& root) {
				vector<JobExprAddress> jobAddresses;
				visitDepthFirstPrunable(ProgramAddress(root), [&](const JobExprAddress& job) -> bool {
					if(CallExprPtr rangeCall = dynamic_pointer_cast<CallExprPtr>(job.getAddressedNode()->getThreadNumRange())) {
						if(analysis::isCallOf(rangeCall, parExt.getCreateBoundRange()) || analysis::isCallOf(rangeCall, parExt.getCreateBoundRangeMod())) {
							try {
								arithmetic::Formula lowBound = arithmetic::toFormula(analysis::getArgument(rangeCall, 0));
								arithmetic::Formula highBound = arithmetic::toFormula(analysis::getArgument(rangeCall, 1));
								if(lowBound.isOne() && highBound.isOne()) {
									jobAddresses.push_back(job);
									return true;
								}
							} catch(arithmetic::NotAFormulaException) {}
						}
					}
					return false;
				});
				return jobAddresses;
			}

			set<LambdaDefinitionAddress> gatherLambdaDefAddresses(const vector<JobExprAddress>& jobs) {
				set<LambdaDefinitionAddress> lambdaDefAddresses;
				for(const JobExprAddress& job : jobs) {
					LambdaDefinitionAddress lamDef;
					auto recSearchVisitor = makeLambdaVisitor([&](const LambdaDefinitionAddress& lam) {
						lamDef = lam;
						return true;
					});
					visitPathBottomUpInterruptible(job, recSearchVisitor);
					lambdaDefAddresses.insert(lamDef);
				}
				return lambdaDefAddresses;
			}

			LambdaDefinitionPtr removeOuterParallels(LambdaDefinitionPtr lamDef) {
				// create a cached check for nested parallel calls
				auto containsParallel = makeCachedLambdaVisitor([&](const NodePtr& node, rec_call<bool>::type& rec) {
					return analysis::isCallOf(node, parExt.getParallel()) || any(node->getChildList(), rec);
				});

				// sequentialize all job-spawning steps running jobs with nested parallels
				return core::transform::makeCachedLambdaMapper([&](const NodePtr& ptr) -> NodePtr {

					       // only interested in parallel calls
					       if(!analysis::isCallOf(ptr, parExt.getParallel())) return ptr;

					       // skip parallel if there is another nested parallel
					       ExpressionPtr branch = ptr.as<CallExprPtr>()->getArgument(0).as<JobExprPtr>()->getBody();
					       if(!containsParallel(branch)) return ptr; // preserve the innermost one

					       // inline recursive call
					       CallExprPtr callExpr = build.callExpr(basic.getUnit(), branch);
					       // return callExpr;
					       CallExprPtr simplified = core::transform::simplify(nodeMan, callExpr);
					       return simplified;
					       // VLOG(1) << "AMAZE: \n" << printer::PrettyPrinter(simplified, printer::PrettyPrinter::NO_LET_BINDINGS) << "\n";
					       // CompoundStmtPtr inlined = core::transform::inlineMultiReturn(nodeMan, simplified);
					       // VLOG(1) << "INLINED: \n" << printer::PrettyPrinter(inlined, printer::PrettyPrinter::NO_LET_BINDINGS) << "\n";
					       // return inlined;
					   })
				    .map(lamDef);

				// pattern-based prototype
				// auto patt = p::aT(p::var("parallel", irp::callExpr(basic.getParallel(),
				//	irp::jobExpr(p::any, p::var("defaultExp", p::aT(irp::atom(basic.getParallel())))) )));
				////auto gen = g::substitute(g::root, g::var("parallel"), irg::callExpr(irg::atom(basic.getUnit()), g::var("defaultExp")));
				// LambdaDefinitionPtr prevLamDef;
				// int i = 0;
				// while(lamDef != prevLamDef) {
				//	prevLamDef = lamDef;
				//	//lamDef = p::apply(lamDef, patt, gen).as<LambdaDefinitionPtr>();
				//	i++;
				//	std::cout << "Matching " << i << " ...\n";
				//	auto match = patt->matchAddress(LambdaDefinitionAddress(lamDef));
				//	std::cout << "Updating " << i << "...\n";
				//	if(match) {
				//		NodeAddress parAddr = match->getVarBinding("parallel").getValue();
				//		NodeAddress defAddr = match->getVarBinding("defaultExp").getValue();
				//		CallExprPtr callExp = build.callExpr(basic.getUnit(), defAddr.getAddressedNode().as<ExpressionPtr>());
				//		StatementPtr inlined = core::transform::inlineMultiReturnPlainCall(nodeMan, core::transform::simplify(nodeMan, callExp));
				//		lamDef = core::transform::replaceNode(nodeMan, parAddr, inlined).as<LambdaDefinitionPtr>();
				//	}
				//}
				// return prevLamDef;
			}

			// does not correctly handle parallels generated in recursive function calls
			LambdaDefinitionPtr removeExtraneousMergeAlls(LambdaDefinitionPtr lamDef) {
				// create a cached check for nested parallel calls
				auto containsParallel = makeCachedLambdaVisitor([&](const NodePtr& node, rec_call<bool>::type& rec) {
					return analysis::isCallOf(node, parExt.getParallel()) || any(node->getChildList(), rec);
				});
				// create a cached check for nested mergeAll calls
				auto containsMergeAll = makeCachedLambdaVisitor([&](const NodePtr& node, rec_call<bool>::type& rec) {
					return analysis::isCallOf(node, parExt.getMergeAll()) || any(node->getChildList(), rec);
				});

				// remove all the superfluous mergeAlls from Compound Statements
				return core::transform::makeCachedLambdaMapper([&](const NodePtr& ptr) -> NodePtr {

					       // only interested in compound statements
					       CompoundStmtPtr comp = dynamic_pointer_cast<CompoundStmtPtr>(ptr);
					       if(!comp) return ptr;
					       bool changes = false;

					       // build compound statement list, omitting mergeAlls that are not required
					       StatementList replacementStmts;
					       for(int i = 0; i < comp.getStatements().end() - comp.getStatements().begin(); ++i) {
						       StatementPtr stat = comp.getStatement(i);
						       bool skip = false;
						       if(analysis::isCallOf(stat, parExt.getMergeAll())) {
							       for(int j = i - 1; j >= 0 && !skip; --j) {
								       StatementPtr backTrackStat = comp.getStatement(j);
								       if(containsMergeAll(backTrackStat)) {
									       skip = true;
								       } else if(containsParallel(backTrackStat)) {
									       break;
								       }
							       }
						       }
						       if(!skip) {
							       replacementStmts.push_back(stat);
						       } else {
							       changes = true;
						       }
					       }

					       if(changes)
						       return build.compoundStmt(replacementStmts);
					       else {
						       return ptr;
					       }
					   })
				    .map(lamDef);

				// pattern-based prototype
				// auto mergeAll = irp::callExpr(basic.getMergeAll());
				// auto para = irp::atom(basic.getParallel());
				// auto patt = p::aT(irp::compoundStmt( p::anyList << p::aT(mergeAll) << *(!p::aT(para))
				//									   << p::var("mergeAll", mergeAll) << p::anyList ) );
				// LambdaDefinitionPtr prevLamDef;
				// while(lamDef != prevLamDef) {
				//	prevLamDef = lamDef;
				//	std::cout << "Running 2 ...\n";
				//	auto match = patt->matchAddress(LambdaDefinitionAddress(lamDef));
				//	if(match) {
				//		NodeAddress addr = match->getVarBinding("mergeAll").getValue();
				//		lamDef = core::transform::replaceNode(nodeMan, addr, build.getNoOp()).as<LambdaDefinitionPtr>();
				//	}
				//}
				// return prevLamDef;
			}

			LambdaDefinitionPtr buildReplacement(const LambdaDefinitionPtr& lamDef) {
				// gather all required option variables
				vector<LambdaReferenceList> varOptionsList; // list of options for each original lambda definition
				NodeMap sequentialVarReplacements;   // replacement map for recursive calls in sequentialized version
				for(const LambdaBindingPtr& lb : lamDef->getDefinitions()) {
					LambdaReferencePtr rVar = lb->getReference();
					LambdaReferenceList varOptions;
					varOptions.push_back(rVar);                            // original version is the first option
					varOptions.push_back(build.lambdaReference(rVar->getType(),rVar->getNameAsString() + "_unroll_2")); // var for 2 unroll
					varOptions.push_back(build.lambdaReference(rVar->getType(), rVar->getNameAsString() + "_unroll_4")); // var for 4 unroll
					varOptions.push_back(build.lambdaReference(rVar->getType(), rVar->getNameAsString() + "_unroll_inf")); // var for sequential
					varOptionsList.push_back(varOptions);
					sequentialVarReplacements.insert(make_pair(rVar, varOptions.back()));
				}
				// gather all the required options for the lambda definition
				vector<LambdaDefinitionPtr> lambdaDefOptionsList;
				lambdaDefOptionsList.push_back(lamDef);
				lambdaDefOptionsList.push_back(removeExtraneousMergeAlls(removeOuterParallels(lamDef->unroll(nodeMan, 2))));
				lambdaDefOptionsList.push_back(removeExtraneousMergeAlls(removeOuterParallels(lamDef->unroll(nodeMan, 4))));
				lambdaDefOptionsList.push_back(core::transform::trySequentialize(nodeMan, lambdaDefOptionsList.back(), false));

				// replace recursive jobs in lambdas with pick from available options, generating new bindings
				LambdaBindingMap newBindings;
				// replace in originals
				int lbi = 0;
				// for every lambda
				for(const LambdaBindingPtr& lb : lamDef->getDefinitions()) {
					LambdaReferencePtr rVar = lb->getReference();
					// for every option
					for(size_t opti = 0; opti < varOptionsList[0].size(); ++opti) {
						LambdaPtr lam = lambdaDefOptionsList[opti]->getDefinitionOf(rVar);
						vector<JobExprAddress> jobsInLambda = gatherJobAddresses(lam);
						map<NodeAddress, NodePtr> replacements;
						// for every job in this option of this lambda
						for(const JobExprAddress& job : jobsInLambda) {
							VLOG(2) << "============================================= Start variant generation for " << *job << "\n";
							vector<ExpressionPtr> variants;
							// for every option again, for pickVariant generation
							for(size_t opti2 = 0; opti2 < varOptionsList[0].size(); ++opti2) {
								ExpressionPtr replacement = job.getAddressedNode()->getBody();
								for(size_t rvi = 0; rvi < varOptionsList.size(); ++rvi) { // for every recursion var
									// replace occurrences of recursion var with recursion var for this option
									VLOG(2) << "##################### Replacing " << *varOptionsList[rvi][0] << " with " << *varOptionsList[rvi][opti2]
									        << " in \n" << *replacement << "\n";
									replacement = core::transform::replaceAllGen(nodeMan, replacement, varOptionsList[rvi][0], varOptionsList[rvi][opti2],
									                                             core::transform::globalReplacement);
									VLOG(2) << "## Result: \n" << *replacement << "\n############\n";
								}
								VLOG(2) << ">>>> Fully replaced: " << *replacement << "\n<<<<<<<\n";
								variants.push_back(replacement);
							}
							// add this jobExpression to the replacement list, replace with pickVariant
							replacements.insert(make_pair(job->getBody(), build.pickVariant(variants)));
						}
						// replace all jobExpressions with the ones adjusted for this option
						if(replacements.size() > 0) { lam = core::transform::replaceAll(nodeMan, replacements).as<LambdaPtr>(); }
						// adjust sequential lambda to fully sequentialize
						if(opti == varOptionsList[0].size() - 1)
							lam = core::transform::replaceAll(nodeMan, lam, sequentialVarReplacements, core::transform::globalReplacement).as<LambdaPtr>();
						// add this option to the new bindings
						newBindings.insert( { varOptionsList[lbi][opti], lam } );
					}
					++lbi;
				}

				auto ret = core::transform::simplify(nodeMan, build.lambdaDefinition(newBindings));

				VLOG(1) << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n!!!!!!!!!!! orig:\n" << printer::PrettyPrinter(lamDef, printer::PrettyPrinter::NO_LET_BINDINGS)
				        << "\n!!!!!!!!!! new:\n" << printer::PrettyPrinter(ret, printer::PrettyPrinter::NO_LET_BINDINGS);

				return ret;
			}

		  public:
			TaskMultiversioner(NodeManager& nodeMan)
				: nodeMan(nodeMan), build(nodeMan), basic(nodeMan.getLangBasic()), parExt(nodeMan.getLangExtension<lang::ParallelExtension>()) {}

			ProgramPtr apply(const ProgramPtr& program) {
				vector<JobExprAddress> jobAddresses = gatherJobAddresses(program);
				LOG(DEBUG) << "TaskMultiversioner -- jobAddresses: " << jobAddresses;
				set<LambdaDefinitionAddress> lambdaDefAddresses = gatherLambdaDefAddresses(jobAddresses);
				LOG(DEBUG) << "TaskMultiversioner -- lambdaDefAddresses: " << lambdaDefAddresses;
				map<LambdaDefinitionPtr, LambdaDefinitionPtr> cache;
				map<NodeAddress, NodePtr> replacements;

				for(LambdaDefinitionAddress lamDefAddr : lambdaDefAddresses) {
					auto it = cache.find(lamDefAddr.getAddressedNode());
					LambdaDefinitionPtr replacement;
					if(it != cache.end()) {
						replacement = it->second;
					} else {
						replacement = buildReplacement(lamDefAddr.getAddressedNode());
						cache.insert(make_pair(lamDefAddr.getAddressedNode(), replacement));
					}
					replacements.insert(make_pair(lamDefAddr, replacement));
				}

				return core::transform::replaceAll(nodeMan, replacements).as<ProgramPtr>();
			}
		};

	} // end anonymous namespace

	// ----------------------------------------------------------------------------

	core::ProgramPtr applyTaskOptimization(const core::ProgramPtr& program) {
		TaskMultiversioner mult(program->getNodeManager());
		return mult.apply(program);
	}

} // end namespace tasks
} // end namespace playground
} // end namespace insieme

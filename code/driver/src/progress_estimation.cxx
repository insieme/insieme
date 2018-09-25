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

#include <string>
#include <map>

#include "insieme/analysis/features/effort_estimation.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/node_mapper_utils.h"

#include "insieme/driver/cmd/commandline_options.h"
#include "insieme/driver/utils/object_file_utils.h"

#include "insieme/utils/version.h"


using namespace insieme;


namespace {
	class ProgressMapper : public core::transform::CachedNodeMapping {

		using EffortType = analysis::features::EffortEstimationType;

		core::NodeManager& mgr;
		core::IRBuilder builder;
		const core::LiteralPtr reportingLiteral;

		const EffortType EFFORT_REPORTING_LIMIT = 30;

		const EffortType EFFORT_WHILE_LOOP_BRANCHING = 2;
		const EffortType EFFORT_FOR_LOOP_INITILIZATION = 1;
		const EffortType EFFORT_FOR_LOOP_BRANCHING = 2;
		const EffortType EFFORT_FOR_LOOP_CONDITION = 1;
		const EffortType EFFORT_FOR_LOOP_INCREMENT = 1;
		const EffortType EFFORT_IF_BRANCHING = 1;

	  public:
		ProgressMapper(core::NodeManager& manager) : mgr(manager), builder(manager),
				reportingLiteral(builder.literal("report_progress", builder.functionType(builder.getLangBasic().getUInt16(), builder.getLangBasic().getUnit()))) {
			core::lang::markAsBuiltIn(reportingLiteral);
		}

	  private:
		core::ExpressionPtr buildReportingCall(EffortType effort) const {
			return builder.callExpr(reportingLiteral, builder.integerLit(effort));
		}

		const core::NodePtr resolveElementInternal(const core::NodePtr& node, const EffortType startOffset) {
			const auto& nodeType = node.getNodeType();
			// prune calls to builtins
			if (nodeType == core::NT_CallExpr) {
				const auto& callee = node.as<core::CallExprPtr>()->getFunctionExpr();
				if (core::lang::isBuiltIn(callee) || core::lang::isDerived(callee)) {
					return node;
				}
			}

			// only process CompoundStmts here
			if(nodeType != core::NT_CompoundStmt) {
				return node->substitute(mgr, *this);
			}

			const core::CompoundStmtPtr& compound = node.as<core::CompoundStmtPtr>();
			core::StatementList stmts(compound.getStatements());

			EffortType effort = startOffset;

			auto insertEffortReportingCall = [&](core::StatementList::iterator& it) {
				if(effort != 0) {
					it = stmts.insert(it, buildReportingCall(effort));
					++it;
					effort = 0;
				}
			};

			const auto exitPoints = core::analysis::getExitPoints(compound);
			auto isExitPoint = [&](const core::StatementPtr& stmt) {
				return ::any(exitPoints, [&](const auto& exitPoint) { return exitPoint.getDepth() == 2 && exitPoint.getAddressedNode() == stmt; });
			};

			for(auto it = stmts.begin(); it < stmts.end(); ++it) {
				auto stmt = *it;

				// certain nodes get a special handling
				if(const auto& whileStmt = stmt.isa<core::WhileStmtPtr>()) {
					// report the effort until here before the loop starts
					insertEffortReportingCall(it);
					// get the loop overhead
					const auto loopOverhead = analysis::features::estimateEffort(whileStmt->getCondition()) + EFFORT_WHILE_LOOP_BRANCHING;
					// now build a new loop and replace the old one
					const auto newCondition = whileStmt->getCondition().substitute(mgr, *this);
					const auto newBody = resolveElementInternal(whileStmt->getBody(), loopOverhead).as<core::StatementPtr>();
					*it = builder.whileStmt(newCondition, newBody);
					continue;

				} else if(const auto& forStmt = stmt.isa<core::ForStmtPtr>()) {
					// report the effort until here before the loop starts + the initialization
					effort += EFFORT_FOR_LOOP_INITILIZATION;
					insertEffortReportingCall(it);
					// get the loop overhead
					const auto loopOverhead = EFFORT_FOR_LOOP_BRANCHING + EFFORT_FOR_LOOP_CONDITION + EFFORT_FOR_LOOP_INCREMENT;
					// now build a new loop and replace the old one
					const auto newBody = resolveElementInternal(forStmt->getBody(), loopOverhead).as<core::StatementPtr>();
					*it = builder.forStmt(forStmt->getDeclaration(), forStmt->getEnd(), forStmt->getStep(), newBody);
					continue;

				} else if(const auto& ifStmt = stmt.isa<core::IfStmtPtr>()) {
					// add the condition overhead to both branches
					effort += EFFORT_IF_BRANCHING + analysis::features::estimateEffort(ifStmt->getCondition());
					// now build a new if stmt and replace the old one
					const auto newCondition = ifStmt->getCondition().substitute(mgr, *this);
					const auto newThenBody = resolveElementInternal(ifStmt->getThenBody(), effort).as<core::StatementPtr>();
					const auto newElseBody = resolveElementInternal(ifStmt->getElseBody(), effort).as<core::StatementPtr>();
					*it = builder.ifStmt(newCondition, newThenBody, newElseBody);
					effort = 0;
					continue;
				}

				const auto stmtEffort = analysis::features::estimateEffort(stmt);

				// if the effort for the sub statement is high enough, we recursively handle this sub statement but report the current effort beforehand
				if(stmtEffort > EFFORT_REPORTING_LIMIT) {
					insertEffortReportingCall(it);
					*it = resolveElement(stmt).as<core::StatementPtr>();
					continue;
				}

				// insert reporting call before each exit point or if the effort until now and the stmtEffort are more than our limit
				if(isExitPoint(stmt) || effort + stmtEffort > EFFORT_REPORTING_LIMIT) {
					insertEffortReportingCall(it);
				}
				effort += stmtEffort;
			}

			// insert a reporting call in the end if there isn't already one and we don't have an exit point there
			if(!stmts.empty()) {
				const auto& lastStmt = stmts.back();
				if(!lastStmt.isa<core::CallExprPtr>() || lastStmt.as<core::CallExprPtr>()->getFunctionExpr() != reportingLiteral) {
					if(!isExitPoint(lastStmt) && effort != 0) {
						stmts.push_back(buildReportingCall(effort));
					}
				}

				// if we are processing an empty compound which has an effort (i.e. because it got some start effort from it's parent), report it
			} else if(effort != 0) {
				stmts.push_back(buildReportingCall(effort));
			}

			return builder.compoundStmt(stmts);
		}

		virtual const core::NodePtr resolveElement(const core::NodePtr& node) override {
			return resolveElementInternal(node, 0);
		}
	};
}


int main(int argc, char** argv) {
	std::cout << "Insieme Progress Estimation - Version: " << utils::getVersion() << "\n";

	// Step 1: parse input parameters
	auto options = driver::cmd::Options::parse(argc, argv);

	// if options are invalid, exit non-zero
	if(!options.valid) { return 1; }
	// if e.g. help was specified, exit with zero
	if(options.gracefulExit) { return 0; }

	// Step 2: filter input files
	core::NodeManager mgr;
	if(!driver::utils::filterInputFiles(mgr, options.job)) {
		return 1;
	}

	// Step 3: load input code

	// convert src file to IR
	auto program = options.job.execute(mgr);

	// ----- progress estimation ------

	//in order to have thousand's separators for printed numbers
	std::cout.imbue(std::locale(""));

	dumpReadable(program);

	auto res = ProgressMapper(program.getNodeManager()).map(program);

	std::cout << "\n\n###########\n\n\n" << std::endl;
	dumpReadable(res);

	return 0;
}

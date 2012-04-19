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

#include "insieme/backend/runtime/runtime_preprocessor.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/analysis/features/code_features.h"
#include "insieme/analysis/features/code_feature_catalog.h"
#include "insieme/analysis/polyhedral/scop.h"
#include "insieme/analysis/polyhedral/polyhedral.h"

#include "insieme/utils/cmd_line_utils.h"
#include "insieme/transform/pattern/ir_pattern.h"

namespace insieme {
namespace backend {
namespace runtime {
using namespace insieme::transform::pattern;

	namespace {


		core::StatementPtr registerEntryPoint(core::NodeManager& manager, const core::ExpressionPtr& workItemImpl) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();
			auto& extensions = manager.getLangExtension<Extensions>();

			// create register call
			return builder.callExpr(basic.getUnit(), extensions.registerWorkItemImpl, workItemImpl);
		}

		WorkItemImpl wrapEntryPoint(core::NodeManager& manager, const core::ExpressionPtr& entry) {
			core::IRBuilder builder(manager);
			const core::lang::BasicGenerator& basic = manager.getLangBasic();
			const Extensions& extensions = manager.getLangExtension<Extensions>();

			// create new lambda expression wrapping the entry point
			assert(entry->getType()->getNodeType() == core::NT_FunctionType && "Only functions can be entry points!");
			core::FunctionTypePtr entryType = static_pointer_cast<const core::FunctionType>(entry->getType());
			assert(entryType->isPlain() && "Only plain functions can be entry points!");


			// define parameter of resulting lambda
			core::VariablePtr workItem = builder.variable(builder.refType(extensions.workItemType));
			core::TypePtr tupleType = DataItem::toLWDataItemType(builder.tupleType(entryType->getParameterTypes()->getElements()));
			core::ExpressionPtr paramTypes = core::encoder::toIR(manager, tupleType);

			vector<core::ExpressionPtr> argList;
			unsigned counter = 0;
			::transform(entryType->getParameterTypes()->getElements(), std::back_inserter(argList), [&](const core::TypePtr& type) {
				return builder.callExpr(type, extensions.getWorkItemArgument,
						toVector<core::ExpressionPtr>(workItem, core::encoder::toIR(manager, counter++), paramTypes, builder.getTypeLiteral(type)));
			});

			// produce replacement
			core::TypePtr unit = basic.getUnit();
			core::ExpressionPtr call = builder.callExpr(entryType->getReturnType(), entry, argList);
			core::ExpressionPtr exit = builder.callExpr(unit, extensions.exitWorkItem, workItem);
			WorkItemVariant variant(builder.lambdaExpr(unit, builder.compoundStmt(call, exit), toVector(workItem)));
			return WorkItemImpl(toVector(variant));
		}


		core::ProgramPtr replaceMain(core::NodeManager& manager, const core::ProgramPtr& program) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();
			auto& extensions = manager.getLangExtension<Extensions>();

			core::TypePtr unit = basic.getUnit();
			core::TypePtr intType = basic.getUInt4();

			// build up list of statements for the body
			vector<core::StatementPtr> stmts;

			// -------------------- assemble parameters ------------------------------

			core::VariablePtr argc = builder.variable(basic.getInt4());
			core::VariablePtr argv = builder.variable(builder.refType(builder.arrayType(builder.refType(builder.arrayType(basic.getChar())))));
			vector<core::VariablePtr> params = toVector(argc,argv);

			// ------------------- Add list of entry points --------------------------

			vector<core::ExpressionPtr> workItemImpls;
			for_each(program->getEntryPoints(), [&](const core::ExpressionPtr& entry) {
				core::ExpressionPtr impl = WorkItemImpl::encode(manager, wrapEntryPoint(manager, entry));
				workItemImpls.push_back(impl);
				stmts.push_back(registerEntryPoint(manager, impl));
			});

			// ------------------- Start standalone runtime  -------------------------

			// construct light-weight data item tuple
			core::ExpressionPtr expr = builder.tupleExpr(toVector<core::ExpressionPtr>(argc, argv));
			core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(expr->getType());
			expr = builder.callExpr(DataItem::toLWDataItemType(tupleType), extensions.wrapLWData, toVector(expr));

			// create call to standalone runtime
			if (!workItemImpls.empty()) {
				stmts.push_back(builder.callExpr(unit, extensions.runStandalone, workItemImpls[0], expr));
			}

			// ------------------- Add return   -------------------------

			stmts.push_back(builder.returnStmt(builder.intLit(0)));

			// ------------------- Creation of new main function -------------------------


			core::FunctionTypePtr mainType = builder.functionType(toVector(argc->getType(), argv->getType()), basic.getInt4());

			// create new main function
			core::StatementPtr body = builder.compoundStmt(stmts);
			core::ExpressionPtr main = builder.lambdaExpr(mainType, params, body);

			// return resulting program
			return core::Program::get(manager, toVector(main));
		}

	}


	core::NodePtr StandaloneWrapper::process(core::NodeManager& manager, const core::NodePtr& node) {

		// simply convert entry points to work items and add new main
		auto nodeType = node->getNodeType();

		// handle programs specially
		if (nodeType == core::NT_Program) {
			return replaceMain(manager, static_pointer_cast<const core::Program>(node));
		}

		// if it is a expression, wrap it within a program and resolve equally
		if (core::ExpressionPtr expr = dynamic_pointer_cast<const core::Expression>(node)) {
			return replaceMain(manager, core::Program::get(manager, toVector(expr)));
		}

		// nothing to do otherwise
		return node;
	}


	// -----------------------------------------------------------------------------------------
	// 	          Replace parallel and job expressions with work-item equivalents
	// -----------------------------------------------------------------------------------------

	namespace {

		namespace coder = core::encoder;

		/**
		 * A small helper-visitor collecting all variables which should be automatically
		 * captured by jobs for their branches.
		 */
		class VariableCollector : public core::IRVisitor<> {

			/**
			 * A set of variables to be excluded.
			 */
			const utils::set::PointerSet<core::VariablePtr>& excluded;

			/**
			 * A reference to the resulting list of variables.
			 */
			vector<core::VariablePtr>& list;


		public:

			/**
			 * Creates a new instance of this visitor based on the given list of variables.
			 * @param list the list to be filled by this collector.
			 */
			VariableCollector(const utils::set::PointerSet<core::VariablePtr>& excluded, vector<core::VariablePtr>& list) : core::IRVisitor<>(false), excluded(excluded), list(list) {}

		protected:

			/**
			 * Visits a variable and adds the variable to the resulting list (without duplicates).
			 * It also terminates the recursive decent.
			 */
			void visitVariable(const core::VariablePtr& var) {
				// collect this variable
				if (excluded.find(var) == excluded.end() && !contains(list, var)) {
					list.push_back(var);
				}
			}

			/**
			 * Visiting a lambda expression terminates the recursive decent since a new scope
			 * is started.
			 */
			void visitLambdaExpr(const core::LambdaExprPtr& lambda) {
				// break recursive decent when new scope is reached
			}

			/**
			 * Do not collect parameters of bind expressions.
			 */
			void visitBindExpr(const core::BindExprPtr& bind) {
				// only visit bound expressions
				auto boundExpressions = bind->getBoundExpressions();
				for_each(boundExpressions, [this](const core::ExpressionPtr& cur) {
					this->visit(cur);
				});
			}

			/**
			 * Types are generally ignored by this visitor for performance reasons (no variables will
			 * occur within types).
			 */
			void visitType(const core::TypePtr& type) {
				// just ignore types
			}

			/**
			 * The default behavior for all other node types is to recursively decent by iterating
			 * through the child-node list.
			 */
			void visitNode(const core::NodePtr& node) {
				assert(node->getNodeType() != core::NT_LambdaExpr);
				// visit all children recursively
				for_each(node->getChildList(), [this](const core::NodePtr& cur){
					this->visit(cur);
				});
			}

		};


		/**
		 * Collects a list of variables to be captures by a job for proper initialization
		 * of the various job branches.
		 */
		vector<core::VariablePtr> getVariablesToBeCaptured(const core::ExpressionPtr& code, const utils::set::PointerSet<core::VariablePtr>& excluded = utils::set::PointerSet<core::VariablePtr>()) {

			vector<core::VariablePtr> res;

			// collect all variables potentially captured by this job
			VariableCollector collector(excluded, res);
			collector.visit(code);

			return res;
		}


		std::pair<WorkItemImpl, core::ExpressionPtr> wrapJob(core::NodeManager& manager, const core::JobExprPtr& job) {
			core::IRBuilder builder(manager);
			const core::lang::BasicGenerator& basic = manager.getLangBasic();
			const Extensions& extensions = manager.getLangExtension<Extensions>();

			// define parameter of resulting lambda
			core::VariablePtr workItem = builder.variable(builder.refType(extensions.workItemType));

			// collect parameters to be captured by the job
			vector<core::VariablePtr> capturedVars = getVariablesToBeCaptured(job);

			// add local declarations
			core::TypeList list;
			core::ExpressionList capturedValues;
			utils::map::PointerMap<core::VariablePtr, unsigned> varIndex;
			for_each(job->getLocalDecls()->getElements(), [&](const core::DeclarationStmtPtr& cur) {
				varIndex.insert(std::make_pair(cur->getVariable(), list.size()));
				list.push_back(cur->getVariable()->getType());
				capturedValues.push_back(cur->getInitialization());
			});
			for_each(capturedVars, [&](const core::VariablePtr& cur) {
				varIndex.insert(std::make_pair(cur, list.size()));
				list.push_back(cur->getType());
				capturedValues.push_back(cur);
			});

			core::TypePtr unit = basic.getUnit();

			// create variable replacement map
			core::TupleTypePtr tupleType = builder.tupleType(list);
			core::TypePtr dataItemType = DataItem::toLWDataItemType(tupleType);
			core::ExpressionPtr paramTypeToken = coder::toIR<core::TypePtr>(manager, dataItemType);
			utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> varReplacements;
			for_each(varIndex, [&](const std::pair<core::VariablePtr, unsigned>& cur) {
				core::TypePtr varType = cur.first->getType();
				core::ExpressionPtr index = coder::toIR(manager, cur.second);
				core::ExpressionPtr access = builder.callExpr(varType, extensions.getWorkItemArgument,
						toVector<core::ExpressionPtr>(workItem, index, paramTypeToken, coder::toIR(manager, varType)));
				varReplacements.insert(std::make_pair(cur.first, access));
			});

			auto fixVariables = [&](const core::ExpressionPtr& cur)->core::ExpressionPtr {
				return core::transform::replaceVarsGen(manager, cur, varReplacements);
			};

			auto fixBranch = [&](const core::ExpressionPtr& branch)->core::ExpressionPtr {
				core::CallExprPtr call = builder.callExpr(unit, branch, core::ExpressionList());
				core::ExpressionPtr res = core::transform::tryInlineToExpr(manager, call);
				return fixVariables(res);
			};

			// create function processing the job (forming the entry point)
			core::StatementList body;
			core::StatementPtr returnStmt = builder.returnStmt(basic.getUnitConstant());
			for(auto it = job->getGuardedExprs().begin(); it != job->getGuardedExprs().end(); ++it) {
				const core::GuardedExprPtr& cur = *it;
				core::ExpressionPtr condition = fixVariables(cur->getGuard());
				core::ExpressionPtr branch = fixBranch(cur->getExpression());
				body.push_back(builder.ifStmt(condition, builder.compoundStmt(branch, returnStmt)));
			}

			// add default branch
			body.push_back(fixBranch(job->getDefaultExpr()));

			// add exit work-item call
			body.push_back(builder.callExpr(unit, extensions.exitWorkItem, workItem));

			// produce work item implementation
			WorkItemVariant variant(builder.lambdaExpr(unit, builder.compoundStmt(body), toVector(workItem)));
			WorkItemImpl impl(toVector(variant));


			// ------------------- initialize work item parameters -------------------------

			// construct light-weight data item tuple
			core::ExpressionPtr tuple = builder.tupleExpr(capturedValues);
			core::ExpressionPtr parameters = builder.callExpr(dataItemType, extensions.wrapLWData, toVector(tuple));

			// return implementation + parameters
			return std::make_pair(impl, parameters);
		}


		class WorkItemIntroducer : public core::transform::CachedNodeMapping {

			core::NodeManager& manager;
			const core::lang::BasicGenerator& basic;
			const Extensions& ext;
			core::IRBuilder builder;

		public:

			WorkItemIntroducer(core::NodeManager& manager)
				: manager(manager), basic(manager.getLangBasic()),
				  ext(manager.getLangExtension<Extensions>()), builder(manager) {}

			virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {

				// skip types
				if (ptr->getNodeCategory() == core::NC_Type) {
					return ptr; // don't touch types
				}

				// start by processing the child nodes (bottom up)
				core::NodePtr res = ptr->substitute(manager, *this);

				// test whether it is something of interest
				if (res->getNodeType() == core::NT_JobExpr) {
					return convertJob(static_pointer_cast<const core::JobExpr>(res));
				}

				// handle call expressions
				if (res->getNodeType() == core::NT_CallExpr) {
					const auto& call = res.as<core::CallExprPtr>();
					const auto& fun = core::analysis::stripAttributes(call->getFunctionExpr());

					// handle parallel call
					if (basic.isParallel(fun)) {
						const core::ExpressionPtr& job = core::analysis::getArgument(res, 0);
						assert(*job->getType() == *ext.jobType && "Argument hasn't been converted!");
						return builder.callExpr(builder.refType(ext.workItemType), ext.parallel, job);
					}

					// handle merge call
					if (basic.isMerge(fun)) {
						return builder.callExpr(basic.getUnit(), ext.merge, core::analysis::getArgument(res, 0));
					}

					// handle pfor calls
					if (basic.isPFor(fun)) {
						return convertPfor(call);
					}
				}

				// handle calls to pick-variant calls
				if (res->getNodeType() == core::NT_CallExpr) {
					core::CallExprPtr call =  static_pointer_cast<core::CallExprPtr>(res);
					if (core::analysis::isCallOf(call->getFunctionExpr(), basic.getPick())) {
						return convertVariant(call);
					}
				}

				// nothing interesting ...
				return res;
			}

		private:

			core::ExpressionPtr convertJob(const core::JobExprPtr& job) {

				// extract range
				WorkItemRange range = coder::toValue<WorkItemRange>(job->getThreadNumRange());

				// create job parameters
				core::ExpressionPtr min = range.min;
				core::ExpressionPtr max = range.max;
				core::ExpressionPtr mod = range.mod;

				auto info = wrapJob(manager, job);
				core::ExpressionPtr wi = coder::toIR(manager, info.first);
				core::ExpressionPtr data = info.second;

				// create call to job constructor
				return builder.callExpr(ext.jobType, ext.createJob, toVector(min,max,mod, wi, data));

			}

			core::ExpressionPtr convertPfor(const core::CallExprPtr& call) {
				// check that it is indeed a pfor call
				assert(basic.isPFor(core::analysis::stripAttributes(call->getFunctionExpr())));

				// obtain pfor-attributes
				int regionId = -1; int numThreads = -1;
				auto attributes = core::analysis::getAttributes(call->getFunctionExpr());

				for_each(attributes, [&](const core::ExpressionPtr& cur) {
					if (core::analysis::isCallOf(cur, ext.regionAttribute)) {
						auto id = core::arithmetic::toFormula(cur.as<core::CallExprPtr>()->getArgument(0));
						assert(id.isInteger() && "Only supporting constant region IDs!");
						regionId = id.getConstantValue();
					}
				});


				// construct call to pfor ...
				const core::ExpressionList& args = call->getArguments();

				// convert pfor body
				auto info = pforBodyToWorkItem(args[4]);

				// update pfor-attributes
				for_each(info.first.getVariants(), [&](WorkItemVariant& cur) {
					cur.getFeatures().implicitRegionId = regionId;
					cur.getFeatures().suggestedThreadNum = numThreads;
				});

				// encode into IR
				core::ExpressionPtr bodyImpl = coder::toIR(manager, info.first);
				core::ExpressionPtr data = info.second;
				core::TypePtr resType = builder.refType(ext.workItemType);

				return builder.callExpr(resType, ext.pfor,
						toVector(args[0], args[1], args[2], args[3], bodyImpl, data));

//				irt_work_item* irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id, irt_lw_data_item* args);
			}

			bool isOpencl(const core::StatementPtr& stmt) {
				TreePatternPtr kernelCall = aT(irp::callExpr( irp::literal("call_kernel"), *any));
				MatchOpt&& match = kernelCall->matchPointer(stmt);
				if(match) return true;
				return false;
			}

			uint64_t estimateEffort(const core::StatementPtr& stmt) {
				// static references to the features used for the extraction
				static const analysis::features::FeaturePtr numOpsFtr
						= analysis::features::getFullCodeFeatureCatalog().getFeature("SCF_NUM_any_all_OPs_real");
				static const analysis::features::FeaturePtr numMemAccessFtr
						= analysis::features::getFullCodeFeatureCatalog().getFeature("SCF_IO_NUM_any_read/write_OPs_real");

				assert(numOpsFtr && "Missing required feature support!");
				assert(numMemAccessFtr && "Missing required feature support!");

				// extract values
				uint64_t numOps = (uint64_t)analysis::features::getValue<double>(numOpsFtr->extractFrom(stmt));
				uint64_t numMemAccess = (uint64_t)analysis::features::getValue<double>(numMemAccessFtr->extractFrom(stmt));

				// combine values
				return numOps + 3*numMemAccess;
			}

			core::LambdaExprPtr getLoopEffortEstimationFunction(const core::ExpressionPtr& loopFun) {

				core::LambdaExprPtr effort;

				// create artificial boundaries
				core::TypePtr iterType = basic.getInt4();
				core::VariablePtr lowerBound = builder.variable(iterType);
				core::VariablePtr upperBound = builder.variable(iterType);
				core::ExpressionPtr one = builder.literal("1", iterType);

				// create loop to base estimation up-on
				core::CallExprPtr estimatorForLoop = builder.callExpr(basic.getUnit(), loopFun, lowerBound, upperBound, one);

				// check whether it is a SCoP
				auto scop = analysis::polyhedral::scop::ScopRegion::toScop(estimatorForLoop);

				// check whether current node is the root of a SCoP
				std::cout << "~~~~~~~~~~~~~~\nEstimating effort for:\n" << core::printer::PrettyPrinter(estimatorForLoop);
				if (!scop) {
					// => not a scop, no way of estimating effort ... yet
					std::cout << "~~~~~~~~~~~~~~ NOT a scop\n";
					return effort;
				}
				std::cout << "~~~~~~~~~~~~~~ IS a scop\n";


				// compute total effort function
				core::arithmetic::Piecewise total;
				for_each(*scop, [&](const analysis::polyhedral::StmtPtr& cur) {

					// obtain cardinality of the current statement
					core::arithmetic::Piecewise cardinality = analysis::polyhedral::cardinality(manager, cur->getDomain());

					// fix parameters (except the boundary parameters)
					core::arithmetic::ValueReplacementMap replacements;
					for_each(cardinality.extractValues(), [&](const core::arithmetic::Value& cur) {
						if (cur != lowerBound && cur != upperBound) {
							replacements[cur] = 100;
						}
					});

					// fix parameters ...
					cardinality = cardinality.replace(replacements);

					// scale cardinality by weight of current stmt
					cardinality *= core::arithmetic::Piecewise(
							core::arithmetic::Formula(
									estimateEffort(cur->getAddr().getAddressedNode())
							)
					);

					// sum up cardinality
					total += cardinality;
				});

				// convert into IR
				core::ExpressionPtr formula = core::arithmetic::toIR(manager, total);

				// wrap into lambda
				return builder.lambdaExpr(builder.getLangBasic().getUInt8(),
						builder.returnStmt(formula),
						toVector(lowerBound, upperBound)
				);
			}

			WorkItemVariantFeatures getFeatures(const core::StatementPtr& body) {
				WorkItemVariantFeatures features;
				features.effort = estimateEffort(body);
				features.opencl = isOpencl(body);
				return features;
			}


			std::pair<WorkItemImpl, core::ExpressionPtr> pforBodyToWorkItem(const core::ExpressionPtr& body) {
				// ------------- build captured data -------------

				// collect variables to be captured
				vector<core::VariablePtr> captured = getVariablesToBeCaptured(body);

				// create tuple of captured data
				core::TypeList typeList;
				core::ExpressionList capturedValues;
				for_each(captured, [&](const core::VariablePtr& cur) {
					typeList.push_back(cur->getType());
					capturedValues.push_back(cur);
				});

				// construct light-weight data tuple to be passed to work item
				core::TupleTypePtr tupleType = builder.tupleType(typeList);
				core::TypePtr dataItemType = DataItem::toLWDataItemType(tupleType);
				core::ExpressionPtr tuple = builder.tupleExpr(capturedValues);
				core::ExpressionPtr data = builder.callExpr(dataItemType, ext.wrapLWData, toVector(tuple));


				// ------------- build up function computing for-loop body -------------

				core::StatementList resBody;

				// define parameter of resulting lambda
				core::VariablePtr workItem = builder.variable(builder.refType(ext.workItemType));

				// create variables containing loop boundaries
				core::TypePtr unit = basic.getUnit();
				core::TypePtr int4 = basic.getInt4();

				core::VariablePtr range = builder.variable(ext.workItemRange);
				core::VariablePtr begin = builder.variable(int4);
				core::VariablePtr end = builder.variable(int4);
				core::VariablePtr step = builder.variable(int4);

				resBody.push_back(builder.declarationStmt(range, builder.callExpr(ext.workItemRange, ext.getWorkItemRange, workItem)));
				resBody.push_back(builder.declarationStmt(begin, builder.accessMember(range, "begin")));
				resBody.push_back(builder.declarationStmt(end, 	 builder.accessMember(range, "end")));
				resBody.push_back(builder.declarationStmt(step,  builder.accessMember(range, "step")));

				// create loop calling body of p-for
				core::CallExprPtr loopBodyCall = builder.callExpr(unit, body, begin, end, step);
				core::StatementPtr loopBody = core::transform::tryInlineToStmt(manager, loopBodyCall);

				// replace variables within loop body to fit new context
				core::ExpressionPtr paramTypeToken = coder::toIR<core::TypePtr>(manager, dataItemType);
				utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> varReplacements;
				unsigned count = 0;
				for_each(captured, [&](const core::VariablePtr& cur) {
					core::TypePtr varType = cur->getType();
					core::ExpressionPtr index = coder::toIR(manager, count++);
					core::ExpressionPtr access = builder.callExpr(varType, ext.getWorkItemArgument,
							toVector<core::ExpressionPtr>(workItem, index, paramTypeToken, coder::toIR(manager, varType)));
					varReplacements.insert(std::make_pair(cur, access));
				});

				core::StatementPtr inlinedLoop = core::transform::replaceVarsGen(manager, loopBody, varReplacements);

				// build for loop
				resBody.push_back(inlinedLoop);

				// add exit work-item call
				resBody.push_back(builder.callExpr(unit, ext.exitWorkItem, workItem));

				core::LambdaExprPtr entryPoint = builder.lambdaExpr(unit, builder.compoundStmt(resBody), toVector(workItem));


				// ------------- try build up function estimating loop range effort -------------

				core::LambdaExprPtr effort;
				if(CommandLineOptions::EstimateEffort) effort = getLoopEffortEstimationFunction(body);
				WorkItemVariantFeatures features = getFeatures(builder.callExpr(basic.getUnit(), body, builder.intLit(1), builder.intLit(2), builder.intLit(1)));

				// ------------- finish process -------------

				// create implementation
				WorkItemImpl impl(toVector(WorkItemVariant(entryPoint, effort, features)));

				// combine results into a pair
				return std::make_pair(impl, data);
			}

			core::StatementPtr convertVariantToSwitch(const core::CallExprPtr& call) {
				// obtain arguments
				const auto& arguments = call->getArguments();

				// extract code variants
				auto variantCodes = coder::toValue<vector<core::ExpressionPtr>>(
						call->getFunctionExpr().as<core::CallExprPtr>()->getArgument(0));

				int i = 0;
				vector<core::SwitchCasePtr> cases;
				vector<uint16_t> options;
				for_each(variantCodes, [&](const core::ExpressionPtr& cur) {

					// variant needs to be a lambda expression!
					assert(cur->getNodeType() == core::NT_LambdaExpr);

					// create literal
					core::LiteralPtr lit = builder.uintLit(i);
					options.push_back(i);
					i++;

					// create case-body
					core::StatementPtr body = core::transform::tryInlineToStmt(manager,
						builder.callExpr(cur, arguments)
					);

					cases.push_back(builder.switchCase(lit, body));

				});

				// create resulting switch
				core::ExpressionPtr optionList = core::encoder::toIR(manager, options);
				core::ExpressionPtr switchExpr = builder.callExpr(
						basic.getUInt4(), basic.getPick(), optionList );
				return builder.switchStmt(switchExpr, cases, builder.getNoOp());
			}

			core::StatementPtr convertVariantToCall(const core::CallExprPtr& call) {

				// --- build work item parameters (arguments to variant) ---

				// collect values to be passed
				const vector<core::ExpressionPtr>& arguments = call->getArguments();

				// create tuple of captured data
				core::TypeList typeList;
				for_each(arguments, [&](const core::ExpressionPtr& cur) {
					typeList.push_back(cur->getType());
				});

				// construct light-weight data tuple to be passed to work item
				core::TupleTypePtr tupleType = builder.tupleType(typeList);
				core::TypePtr dataItemType = DataItem::toLWDataItemType(tupleType);
				core::ExpressionPtr tuple = builder.tupleExpr(arguments);
				core::ExpressionPtr data = builder.callExpr(dataItemType, ext.wrapLWData, toVector(tuple));
				core::ExpressionPtr paramTypeToken = coder::toIR<core::TypePtr>(manager, dataItemType);

				// --- Build Work Item Variations ---

				// extract variants
				core::CallExprPtr variantCall = static_pointer_cast<core::CallExprPtr>(call->getFunctionExpr());

				// extract variants
				auto variantCodes = coder::toValue<vector<core::ExpressionPtr>>(variantCall->getArgument(0));

				// create the code for each executable variant
				auto unit = basic.getUnit();
				vector<WorkItemVariant> variants;
				for_each(variantCodes, [&](const core::ExpressionPtr& variantImpl) {

					// Each variant has to be wrapped into a function being called by as an
					// entry point for the work item it is implementing. This wrapper function
					// should extract the parameters being passed via the work item struct and
					// forward those to the actual function implementing the variant.
					//
					//   Steps:
					//		- extract parameters
					//		- call function representing code variation
					//		- exit work item
					//

					// define parameter of resulting lambda
					core::VariablePtr workItem = builder.variable(builder.refType(ext.workItemType));

					// create function triggering the computation of this variant (forming the entry point)
					core::StatementList body;

					// create argument list
					core::ExpressionList newArgs;
					for(std::size_t i=0; i<arguments.size(); i++) {
						core::TypePtr argType = arguments[i]->getType();
						core::ExpressionPtr index = builder.uintLit(i);
						newArgs.push_back(builder.callExpr(argType, ext.getWorkItemArgument,
								toVector<core::ExpressionPtr>(workItem, index, paramTypeToken, coder::toIR(manager, argType))));
					}


					// add call to variant implementation
					body.push_back(builder.callExpr(basic.getUnit(), variantImpl, newArgs));

					// add exit work-item call
					body.push_back(builder.callExpr(unit, ext.exitWorkItem, workItem));

					// create the resulting lambda expression / work item variant
					variants.push_back(WorkItemVariant(builder.lambdaExpr(unit, builder.compoundStmt(body), toVector(workItem))));
				});

				// produce work item implementation
				WorkItemImpl impl(variants);
				core::ExpressionPtr wi = coder::toIR(manager, impl);


				// --- Encode variant call as work item call ---


				// create job parameters
				core::IRBuilder builder(call->getNodeManager());
				core::ExpressionPtr one = builder.uintLit(1);

				// create call to job constructor and merge
				return builder.callExpr(unit, ext.merge,
						builder.callExpr(builder.refType(ext.workItemType), ext.parallel,
							builder.callExpr(ext.jobType, ext.createJob, toVector(one,one,one, wi, data))
						)
					);
			}

			core::StatementPtr convertVariant(const core::CallExprPtr& call) {

				auto pickCall = call->getFunctionExpr();

				// check whether this is indeed a call to pick variants
				assert(core::analysis::isCallOf(pickCall, basic.getPick()) && "Invalid Variant call!");

				// check if picking between implementations
				if(pickCall->getType()->getNodeType() != core::NT_FunctionType) return call;

				if(pickCall->hasAttachedValue<PickImplementationHint>()) {
					auto implHint = pickCall->getAttachedValue<PickImplementationHint>();
					switch(implHint) {
					case PickImplementationHint::CALL: return convertVariantToSwitch(call);
					case PickImplementationHint::SWITCH: return convertVariantToCall(call);
					default: assert(false && "Invalid variant implementation hint"); break;
					}
				}
				// default to switch
				return convertVariantToCall(call);
			}
		};

	}


	core::NodePtr WorkItemizer::process(core::NodeManager& manager, const core::NodePtr& node) {
		return WorkItemIntroducer(manager).resolveElement(node);
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <insieme/backend/runtime/runtime_extension.h>
#include "insieme/backend/runtime/runtime_preprocessor.h"

#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/encoder/encoder.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/annotations/meta_info/meta_infos.h"
#include "insieme/annotations/omp/omp_annotations.h"

namespace insieme {
namespace backend {
namespace runtime {

	using namespace insieme::core::pattern;
	using namespace insieme::core;

	namespace {

		bool hasGroupOperations(const JobExprPtr& job) {
			if(!job) { return false; }

			auto& parExt = job->getNodeManager().getLangExtension<lang::ParallelExtension>();

			bool hasGroupOps = false;
			core::visitDepthFirstPrunable(job, [&](const core::NodePtr& cur) -> bool {
				if(cur->getNodeType() == core::NT_CallExpr) {
					const auto& c = cur.as<core::CallExprPtr>();
					const auto& f = core::analysis::stripAttributes(c->getFunctionExpr());

					if(parExt.isGetThreadGroup(f) || parExt.isGetGroupSize(f) || parExt.isGetThreadId(f) || parExt.isPFor(f) || parExt.isRedistribute(f)) {
						hasGroupOps = true;
					}

					// Prune on Parallel
					if(parExt.isParallel(f) && !c->hasAnnotation(insieme::annotations::omp::RegionAnnotation::KEY)) {
						return true;
					} else {
						return false;
					}
				}

				return false;
			});

			return hasGroupOps;
		}

		bool isTask(const JobExprPtr& job) {
			// make a null-check
			if(!job) { return false; }

			core::IRBuilder builder(job->getNodeManager());

			// range(1-inf)
			if(job->getThreadNumRange().as<core::CallExprPtr>()->getArgumentDeclarations().size() == 1) { return false; }

			// skipping casts
			try {
				// try to 'parse' the upper boundary as a integer constant
				core::arithmetic::Formula max =
					core::arithmetic::toFormula(core::transform::extractInitExprFromDecl(job->getThreadNumRange().as<core::CallExprPtr>()[1]));

				// check that the constant is 1
				if (!max.isInteger() || max.getIntegerValue() != 1) return false;

			} catch(const core::arithmetic::NotAFormulaException& nafe) {
				// ... nothing to do
			}

			// check for absence of group operators
			if (hasGroupOperations(job)) return false;

			// all fine
			return true;
		}

		/**
		* Tests whether the given job expression is a task (only processed by a single thread) or not.
		*/
		inline bool isTask(const insieme::core::NodePtr& job) {
			return isTask(job.isa<insieme::core::JobExprPtr>());
		}

		core::StatementPtr registerEntryPoint(core::NodeManager& manager, const core::ExpressionPtr& workItemImpl) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();
			auto& extension = manager.getLangExtension<RuntimeExtension>();

			// create register call
			return builder.callExpr(basic.getUnit(), extension.getRegisterWorkItemImpl(), workItemImpl);
		}

		WorkItemImpl wrapEntryPoint(core::NodeManager& manager, const core::ExpressionPtr& entry, bool instrumentBody) {
			core::IRBuilder builder(manager);
			const core::lang::BasicGenerator& basic = manager.getLangBasic();
			const RuntimeExtension& extensions = manager.getLangExtension<RuntimeExtension>();

			// create new lambda expression wrapping the entry point
			assert_eq(entry->getType()->getNodeType(), core::NT_FunctionType) << "Only functions can be entry points!";
			core::FunctionTypePtr entryType = entry->getType().as<FunctionTypePtr>();
			assert_true(entryType->isPlain()) << "Only plain functions can be entry points!";


			// define parameter of resulting lambda
			core::VariablePtr workItem = builder.variable(builder.refType(builder.refType(extensions.getWorkItemType())));

			// build up all the data that has to be passed to the resulting work item
			core::TypeList dataItemEntries = entryType->getParameterTypeList();

			// add value for storing return value of function if necessary
			TypePtr resultType = entryType->getReturnType();
			if (!basic.isUnit(resultType)) {
				dataItemEntries.push_back(builder.refType(resultType));
			}

			// build light-weight data item for main work-item call
			core::TypePtr tupleType = DataItem::toLWDataItemType(builder.tupleType(dataItemEntries));
			core::ExpressionPtr paramTypes = builder.getTypeLiteral(tupleType);

			vector<core::ExpressionPtr> argList;
			unsigned counter = 0;
			::transform(entryType->getParameterTypes()->getElements(), std::back_inserter(argList), [&](const core::TypePtr& type) {
				return builder.callExpr(type, extensions.getGetWorkItemArgument(), toVector<core::ExpressionPtr>(builder.deref(workItem), core::encoder::toIR(manager, counter++),
				                                                                                            paramTypes, builder.getTypeLiteral(type)));
			});

			// produce replacement
			core::TypePtr unit = basic.getUnit();
			core::ExpressionPtr call = builder.callExpr(entryType->getReturnType(), entry, argList);

			// save result if necessary
			if (!basic.isUnit(resultType)) {
				auto target = builder.callExpr(extensions.getGetWorkItemArgument(), toVector<core::ExpressionPtr>(
						builder.deref(workItem), core::encoder::toIR(manager, counter++), paramTypes, builder.getTypeLiteral(builder.refType(resultType))
					));
				call = builder.assign(target, call);
			}

			const auto& instExt = manager.getLangExtension<core::lang::InstrumentationExtension>();

			if(instrumentBody) {
				core::StatementList stmts;
				stmts.push_back(builder.callExpr(instExt.getInstrumentationRegionStart(), builder.uintLit(0)));
				stmts.push_back(call);
				stmts.push_back(builder.callExpr(instExt.getInstrumentationRegionEnd(), builder.uintLit(0)));
				auto instrumentedBody = builder.compoundStmt(stmts);
				WorkItemVariant variant(builder.lambdaExpr(unit, toVector(workItem), instrumentedBody));
				return WorkItemImpl(toVector(variant));
			} else {
				WorkItemVariant variant(builder.lambdaExpr(unit, toVector(workItem), call));
				return WorkItemImpl(toVector(variant));
			}
		}


		core::ProgramPtr replaceMain(core::NodeManager& manager, const core::ProgramPtr& program, const backend::Converter& converter) {
			core::IRBuilder builder(manager);
			auto& basic = manager.getLangBasic();
			auto& extensions = manager.getLangExtension<RuntimeExtension>();

			core::TypePtr unit = basic.getUnit();
			core::TypePtr intType = basic.getUInt4();

			// build up list of statements for the body
			vector<core::StatementPtr> stmts;

			// -------------------- assemble parameters ------------------------------

			core::VariablePtr argc = builder.variable(builder.parseType("ref<int<4>>"));
			core::VariablePtr argv = builder.variable(builder.parseType("ref<ref<ref<array<char,inf>>>>"));
			vector<core::VariablePtr> params = toVector(argc, argv);

			// ------------------- Add list of entry points --------------------------

			vector<core::ExpressionPtr> workItemImpls;
			for_each(program->getEntryPoints(), [&](const core::ExpressionPtr& entry) {
				core::ExpressionPtr impl =
				    WorkItemImpl::encode(manager, wrapEntryPoint(manager, entry, stmts.empty() && converter.getBackendConfig().instrumentMainFunction));
				workItemImpls.push_back(impl);
				stmts.push_back(registerEntryPoint(manager, impl));
			});

			// ----------------- Create Variable for Return Value  -------------------

			core::VariablePtr var;

			// ------------------- Start standalone runtime  -------------------------

			// create call to standalone runtime
			if(!workItemImpls.empty()) {

				// get result type
				auto resultType = program->getEntryPoints()[0]->getType().as<FunctionTypePtr>()->getReturnType();

				// construct parameter expression
				core::ExpressionPtr expr;
				if (!basic.isUnit(resultType)) {
					var = builder.variable(builder.refType(resultType));
					stmts.push_back(builder.declarationStmt(var, builder.getZero(resultType)));
					expr = builder.tupleExpr(toVector<core::ExpressionPtr>(builder.deref(argc), builder.deref(argv), var));
				} else {
					expr = builder.tupleExpr(toVector<core::ExpressionPtr>(builder.deref(argc), builder.deref(argv)));
				}

				// construct light-weight data item tuple
				core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(expr->getType());
				expr = builder.callExpr(DataItem::toLWDataItemType(tupleType), extensions.getWrapLWData(), toVector(expr));

				stmts.push_back(builder.callExpr(unit, extensions.getRunStandalone(), workItemImpls[0], expr));
			}

			// ------------------- Add return   -------------------------

			stmts.push_back(builder.returnStmt((var)
					? builder.deref(var).as<ExpressionPtr>()
					: builder.intLit(0).as<ExpressionPtr>()
				));

			// ------------------- Creation of new main function -------------------------


			// create new main function
			core::StatementPtr body = builder.compoundStmt(stmts);
			core::ExpressionPtr main = builder.lambdaExpr(basic.getInt4(), params, body);

			// return resulting program
			return core::Program::get(manager, toVector(main));
		}
	}


	core::NodePtr StandaloneWrapper::process(const backend::Converter& converter, const core::NodePtr& node) {
		core::NodeManager& manager = converter.getNodeManager();

		// simply convert entry points to work items and add new main
		auto nodeType = node->getNodeType();

		// handle programs specially
		if(nodeType == core::NT_Program) { return replaceMain(manager, static_pointer_cast<const core::Program>(node), converter); }

		// if it is a expression, wrap it within a program and resolve equally
		if(core::ExpressionPtr expr = dynamic_pointer_cast<const core::Expression>(node)) {
			return replaceMain(manager, core::Program::get(manager, toVector(expr)), converter);
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
		VariableCollector(const utils::set::PointerSet<core::VariablePtr>& excluded, vector<core::VariablePtr>& list)
		    : core::IRVisitor<>(false), excluded(excluded), list(list) {}

	  protected:
		/**
		 * Visits a variable and adds the variable to the resulting list (without duplicates).
		 * It also terminates the recursive decent.
		 */
		void visitVariable(const core::VariablePtr& var) {
			// collect this variable
			if(excluded.find(var) == excluded.end() && !contains(list, var)) { list.push_back(var); }
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
			for_each(boundExpressions, [this](const core::ExpressionPtr& cur) { this->visit(cur); });
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
			assert_ne(node->getNodeType(), core::NT_LambdaExpr);
			// visit all children recursively
			for_each(node->getChildList(), [this](const core::NodePtr& cur) { this->visit(cur); });
		}
	};


	/**
	 * Collects a list of variables to be captures by a job for proper initialization
	 * of the various job branches.
	 */
	vector<core::VariablePtr>
	getVariablesToBeCaptured(const core::ExpressionPtr& code,
	                         const utils::set::PointerSet<core::VariablePtr>& excluded = utils::set::PointerSet<core::VariablePtr>()) {
		vector<core::VariablePtr> res;

		// collect all variables potentially captured by this job
		VariableCollector collector(excluded, res);
		collector.visit(code);

		return res;
	}

	core::StatementPtr wrapWithInstrumentationRegion(core::NodeManager& manager, const core::ExpressionPtr& expr, const core::StatementPtr& stmt) {
		if(expr->hasAttachedValue<annotations::ompp_objective_info>()) {
			core::IRBuilder builder(manager);
			const auto& instExt = manager.getLangExtension<core::lang::InstrumentationExtension>();
			core::StatementList stmtList;
			unsigned region_id = expr->getAttachedValue<annotations::ompp_objective_info>().region_id;
			stmtList.push_back(builder.callExpr(instExt.getInstrumentationRegionStart(), builder.uintLit(region_id)));
			stmtList.push_back(stmt);
			stmtList.push_back(builder.callExpr(instExt.getInstrumentationRegionEnd(), builder.uintLit(region_id)));
			return builder.compoundStmt(stmtList);
		} else {
			return stmt;
		}
	}

	// migrate meta-infos from anywhere in subtree to job
	void collectSubMetaInfos(core::NodeManager& manager, core::NodePtr job) {
		const core::lang::ParallelExtension& parExt = manager.getLangExtension<lang::ParallelExtension>();
		auto rootMetas = annotations::getMetaInfos(job);
		core::visitDepthFirstPrunable(job, [&](const core::NodePtr& npr) -> bool {
			// skip root node of sub-tree
			if(npr == job) return false;
			// prune for subtrees which generate their own WI description
			if(core::analysis::isCallOf(npr, parExt.getParallel()) || core::analysis::isCallOf(npr, parExt.getPFor()) || npr.isa<core::JobExprPtr>()) {
				return true;
			}
			// prune for subtrees which already generated their WI
			if(npr.isa<core::ExpressionPtr>() && (core::encoder::is_encoding_of<WorkItemVariant>()(npr.as<core::ExpressionPtr>())
			                                      || core::encoder::is_encoding_of<WorkItemImpl>()(npr.as<core::ExpressionPtr>()))) {
				return true;
			}
			// get metainfo
			auto metas = annotations::getMetaInfos(npr);
			if(metas.empty()) return false;
			// check if duplicate metainfo
			assert(::all(rootMetas, [&](const annotations::AnnotationMap::value_type& v) { return metas.count(v.first) == 0; })
			       && "Duplicate Meta Information");
			// move metainfo
			annotations::moveMetaInfos(npr, job);
			return false;
		});
	}

	std::pair<WorkItemImpl, core::ExpressionPtr> wrapJob(core::NodeManager& manager, const core::JobExprPtr& job) {
		core::IRBuilder builder(manager);
		const core::lang::BasicGenerator& basic = manager.getLangBasic();
		const RuntimeExtension& extensions = manager.getLangExtension<RuntimeExtension>();

		// define parameter of resulting lambda
		core::VariablePtr workItem = builder.variable(builder.refType(builder.refType(extensions.getWorkItemType())));

		// collect parameters to be captured by the job

		// add captured variables
		core::TypeList list;
		core::ExpressionList capturedValues;
		utils::map::PointerMap<core::VariablePtr, unsigned> varIndex;

		vector<core::VariablePtr> capturedVars = getVariablesToBeCaptured(job);
		for_each(capturedVars, [&](const core::VariablePtr& cur) {
			varIndex.insert(std::make_pair(cur, list.size()));
			list.push_back(cur->getType());
			capturedValues.push_back(cur);
		});

		// create variable replacement map
		core::TupleTypePtr tupleType = builder.tupleType(list);
		core::TypePtr dataItemType = DataItem::toLWDataItemType(tupleType);
		core::ExpressionPtr paramTypeToken = builder.getTypeLiteral(dataItemType);
		utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> varReplacements;
		for_each(varIndex, [&](const std::pair<core::VariablePtr, unsigned>& cur) {
			core::TypePtr varType = cur.first->getType();
			core::ExpressionPtr index = coder::toIR(manager, cur.second);
			core::ExpressionPtr access = builder.callExpr(varType, extensions.getGetWorkItemArgument(),
			                                              toVector<core::ExpressionPtr>(builder.deref(workItem), index, paramTypeToken, builder.getTypeLiteral(varType)));
			varReplacements.insert(std::make_pair(cur.first, access));
		});

		core::TypePtr unit = basic.getUnit();

		auto fixVariables = [&](const core::ExpressionPtr& cur) -> core::ExpressionPtr {
			return core::transform::replaceVarsGen(manager, cur, varReplacements);
		};

		auto fixBranch = [&](const core::ExpressionPtr& branch) -> core::ExpressionPtr {
			core::CallExprPtr call = builder.callExpr(unit, branch, core::ExpressionList());
			core::ExpressionPtr res = core::transform::tryInlineToExpr(manager, call);
			return fixVariables(res);
		};

		// compute work-item implementation variants
		vector<WorkItemVariant> variants;

		// migrate meta-infos from anywhere in subtree to job
		collectSubMetaInfos(manager, job);

		// support for multiple body implementations
		auto params = toVector(workItem);
		if(core::analysis::isCallOf(job->getBody(), basic.getPick())) {
			// ---- default only but various variants ------
			auto alternatives = job->getBody().as<core::CallExprPtr>()->getArgument(0);
			auto impls = core::encoder::toValue<vector<core::ExpressionPtr>, core::encoder::DirectExprListConverter>(alternatives);

			assert_false(impls.empty()) << "There must be at least one implementation!";

			for(const auto& cur : impls) {
				core::StatementPtr instrumentedBody = wrapWithInstrumentationRegion(manager, job, fixBranch(cur));
				auto impl = builder.lambdaExpr(unit, params, instrumentedBody);
				annotations::migrateMetaInfos(job, impl);
				variants.push_back(WorkItemVariant(impl));
			}

		} else {
			// general case when body is no Pick

			// create function processing the job (forming the entry point)

			core::StatementPtr instrumentedBody = wrapWithInstrumentationRegion(manager, job, fixBranch(job->getBody()));

			// build implementation
			auto impl = builder.lambdaExpr(unit, params, instrumentedBody);

			// move meta-infos
			annotations::migrateMetaInfos(job, impl);

			// add to variant list
			variants.push_back(WorkItemVariant(impl));
		}

		// produce work item implementation
		WorkItemImpl impl(variants);

		// ------------------- initialize work item parameters -------------------------

		// construct light-weight data item tuple
		core::ExpressionPtr tuple = builder.tupleExpr(capturedValues);
		core::ExpressionPtr parameters = builder.callExpr(dataItemType, extensions.getWrapLWData(), toVector(tuple));

		// return implementation + parameters
		return std::make_pair(impl, parameters);
	}

	class WorkItemIntroducer : public core::transform::CachedNodeMapping {
		core::NodeManager& manager;
		const core::lang::BasicGenerator& basic;
		const core::lang::ParallelExtension& parExt;
		const RuntimeExtension& ext;
		core::IRBuilder builder;

	  public:
		WorkItemIntroducer(core::NodeManager& manager)
		    : manager(manager), basic(manager.getLangBasic()), parExt(manager.getLangExtension<lang::ParallelExtension>()),
			  ext(manager.getLangExtension<RuntimeExtension>()), builder(manager) {}

		virtual const core::NodePtr resolveElement(const core::NodePtr& ptr) {

			// skip types
			if(ptr->getNodeCategory() == core::NC_Type) {
				return ptr; // don't touch types
			}

			// start by processing the child nodes (bottom up)
			core::NodePtr res = ptr->substitute(manager, *this);

			// test whether it is something of interest
			if(res->getNodeType() == core::NT_JobExpr) { return convertJob(static_pointer_cast<const core::JobExpr>(res)); }

			// handle call expressions
			if(res->getNodeType() == core::NT_CallExpr) {
				const auto& call = res.as<core::CallExprPtr>();
				const auto& fun = core::analysis::stripAttributes(call->getFunctionExpr());

				// handle parallel/task call
				if(parExt.isParallel(fun)) {
					const core::ExpressionPtr& job = core::analysis::getArgument(res, 0);
					assert(ext.isJobType(job->getType()) && "Argument hasn't been converted!");

					// unpack job to attach meta information
					WorkItemImpl wi = core::encoder::toValue<WorkItemImpl>(core::transform::extractInitExprFromDecl(job.as<core::CallExprPtr>()[3]));
					for(const auto& var : wi.getVariants()) {
						annotations::migrateMetaInfos(call, var.getImplementation());
					}

					// build runtime call

					// handle region (must be handled before task or it will be mixed up)
					if(call->hasAnnotation(annotations::omp::RegionAnnotation::KEY)) {
						return builder.callExpr(builder.refType(ext.getWorkItemType()), ext.getRegion(), job);
					}

					// handle task
					if(isTask(ptr.as<core::CallExprPtr>()->getArgument(0))) { return builder.callExpr(builder.refType(ext.getWorkItemType()), ext.getTask(), job); }

					return builder.callExpr(builder.refType(ext.getWorkItemType()), ext.getParallel(), job);
				}

				// handle merge call
				if(parExt.isMerge(fun)) {
					const auto& arg = core::analysis::getArgument(res, 0);
					return builder.callExpr(basic.getUnit(), ext.getMerge(), arg);
				}

				// handle pfor calls
				if(parExt.isPFor(fun)) {
					return convertPfor(call).as<core::CallExprPtr>();
				}
			}

			// handle calls to pick-variant calls
			if(res->getNodeType() == core::NT_CallExpr) {
				core::CallExprPtr call = static_pointer_cast<core::CallExprPtr>(res);
				if(core::analysis::isCallOf(call->getFunctionExpr(), basic.getPick())) { return convertVariant(call); }
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

			// creates a list of work-item implementations and the work item data record to be passed along
			auto info = wrapJob(manager, job);
			core::ExpressionPtr wi = coder::toIR(manager, info.first); // the implementation list
			core::ExpressionPtr data = info.second;                    // the work item data record to be passed

			// create call to job constructor
			return builder.callExpr(ext.getJobType(), ext.getCreateJob(), toVector(min, max, mod, wi, data));
		}

		core::ExpressionPtr convertPfor(const core::CallExprPtr& call) {
			// check that it is indeed a pfor call
			assert_true(parExt.isPFor(core::analysis::stripAttributes(call->getFunctionExpr())));

			// construct call to pfor ...
			const core::ExpressionList& args = core::transform::extractArgExprsFromCall(call);

			// convert pfor body
			auto info = pforBodyToWorkItem(args[4]);

			// migrate meta-infos from anywhere in subtree to job
			collectSubMetaInfos(manager, call);

			// migrate meta infos
			for(const auto& cur : info.first.getVariants()) {
				annotations::migrateMetaInfos(call, cur.getImplementation());
				annotations::migrateMetaInfos(args[4], cur.getImplementation());
			}

			// encode into IR
			core::ExpressionPtr bodyImpl = coder::toIR(manager, info.first);
			core::ExpressionPtr data = info.second;
			core::TypePtr resType = builder.refType(ext.getWorkItemType());

			return builder.callExpr(resType, ext.getPfor(), toVector(args[0], args[1], args[2], args[3], bodyImpl, data));

			//				irt_work_item* irt_pfor(irt_work_item* self, irt_work_group* group, irt_work_item_range range, irt_wi_implementation_id impl_id,
			//irt_lw_data_item* args);
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
			core::ExpressionPtr data = builder.callExpr(dataItemType, ext.getWrapLWData(), toVector(tuple));


			// ------------- build up function computing for-loop body -------------

			core::StatementList resBody;

			// define parameter of resulting lambda
			core::VariablePtr workItem = builder.variable(builder.refType(builder.refType(ext.getWorkItemType())));

			// create variables containing loop boundaries
			core::TypePtr unit = basic.getUnit();
			core::TypePtr int4 = basic.getInt4();

			core::VariablePtr range = builder.variable(ext.getWorkItemRange());
			core::VariablePtr begin = builder.variable(int4);
			core::VariablePtr end = builder.variable(int4);
			core::VariablePtr step = builder.variable(int4);

			resBody.push_back(builder.declarationStmt(range, builder.callExpr(ext.getWorkItemRange(), ext.getGetWorkItemRange(), builder.deref(workItem))));
			resBody.push_back(builder.declarationStmt(begin, builder.accessMember(range, "begin")));
			resBody.push_back(builder.declarationStmt(end, builder.accessMember(range, "end")));
			resBody.push_back(builder.declarationStmt(step, builder.accessMember(range, "step")));

			// create loop calling body of p-for
			core::CallExprPtr loopBodyCall = builder.callExpr(unit, body, begin, end, step);
			core::StatementPtr loopBody = core::transform::tryInlineToStmt(manager, loopBodyCall);

			// replace variables within loop body to fit new context
			core::ExpressionPtr paramTypeToken = builder.getTypeLiteral(dataItemType);
			utils::map::PointerMap<core::VariablePtr, core::ExpressionPtr> varReplacements;
			unsigned count = 0;
			for_each(captured, [&](const core::VariablePtr& cur) {
				core::TypePtr varType = cur->getType();
				core::ExpressionPtr index = coder::toIR(manager, count++);
				core::ExpressionPtr access = builder.callExpr(varType, ext.getGetWorkItemArgument(),
				                                              toVector<core::ExpressionPtr>(builder.deref(workItem), index, paramTypeToken, builder.getTypeLiteral(varType)));
				varReplacements.insert(std::make_pair(cur, access));
			});

			core::StatementPtr inlinedLoop = core::transform::replaceVarsGen(manager, loopBody, varReplacements);

			// build for loop
			resBody.push_back(inlinedLoop);
			core::LambdaExprPtr entryPoint = builder.lambdaExpr(unit, toVector(workItem), builder.compoundStmt(resBody));

			// ------------- finish process -------------

			// create implementation
			WorkItemImpl impl(toVector(WorkItemVariant(entryPoint)));

			// combine results into a pair
			return std::make_pair(impl, data);
		}

		core::StatementPtr convertVariantToSwitch(const core::CallExprPtr& call) {
			// obtain arguments
			const auto& arguments = core::transform::extractArgExprsFromCall(call);

			// extract code variants
			auto variantCodes = coder::toValue<vector<core::ExpressionPtr>, core::encoder::DirectExprListConverter>(
			    call->getFunctionExpr().as<core::CallExprPtr>()->getArgument(0));

			int i = 0;
			vector<core::SwitchCasePtr> cases;
			vector<uint16_t> options;
			for_each(variantCodes, [&](const core::ExpressionPtr& cur) {

				// variant needs to be a lambda expression!
				assert_eq(cur->getNodeType(), core::NT_LambdaExpr);

				// create literal
				core::LiteralPtr lit = builder.uintLit(i);
				options.push_back(i);
				i++;

				// create case-body
				core::StatementPtr body = core::transform::tryInlineToStmt(manager, builder.callExpr(cur, arguments));

				cases.push_back(builder.switchCase(lit, body));

			});

			// create resulting switch
			core::ExpressionPtr optionList = core::encoder::toIR(manager, options);
			core::ExpressionPtr switchExpr = builder.callExpr(basic.getUInt4(), basic.getPick(), optionList);
			return builder.switchStmt(switchExpr, cases, builder.getNoOp());
		}

		core::StatementPtr convertVariantToCall(const core::CallExprPtr& call) {
			// --- build work item parameters (arguments to variant) ---

			// collect values to be passed
			const vector<core::ExpressionPtr>& arguments = core::transform::extractArgExprsFromCall(call);

			// create tuple of captured data
			core::TypeList typeList;
			for_each(arguments, [&](const core::ExpressionPtr& cur) { typeList.push_back(cur->getType()); });

			// construct light-weight data tuple to be passed to work item
			core::TupleTypePtr tupleType = builder.tupleType(typeList);
			core::TypePtr dataItemType = DataItem::toLWDataItemType(tupleType);
			core::ExpressionPtr tuple = builder.tupleExpr(arguments);
			core::ExpressionPtr data = builder.callExpr(dataItemType, ext.getWrapLWData(), toVector(tuple));
			core::ExpressionPtr paramTypeToken = builder.getTypeLiteral(dataItemType);

			// --- Build Work Item Variations ---

			// extract variants
			core::CallExprPtr variantCall = static_pointer_cast<core::CallExprPtr>(call->getFunctionExpr());

			// extract variants
			auto variantCodes = coder::toValue<vector<core::ExpressionPtr>, core::encoder::DirectExprListConverter>(variantCall->getArgument(0));

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
				core::VariablePtr workItem = builder.variable(builder.refType(builder.refType(ext.getWorkItemType())));

				// create function triggering the computation of this variant (forming the entry point)
				core::StatementList body;

				// create argument list
				core::ExpressionList newArgs;
				for(std::size_t i = 0; i < arguments.size(); i++) {
					core::TypePtr argType = arguments[i]->getType();
					core::ExpressionPtr index = builder.uintLit(i);
					newArgs.push_back(builder.callExpr(argType, ext.getGetWorkItemArgument(),
					                                   toVector<core::ExpressionPtr>(builder.deref(workItem), index, paramTypeToken, builder.getTypeLiteral(argType))));
				}

				// add call to variant implementation
				body.push_back(builder.callExpr(basic.getUnit(), variantImpl, newArgs));

				// put together the wrapper & migrate meta information from variantImpl to wrapperExpr
				LambdaExprPtr wrapperExpr = builder.lambdaExpr(unit, toVector(workItem), builder.compoundStmt(body));
				annotations::migrateMetaInfos(variantImpl, wrapperExpr);

				// create the resulting lambda expression / work item variant
				variants.push_back(WorkItemVariant(wrapperExpr));
			});

			// produce work item implementation
			WorkItemImpl impl(variants);
			core::ExpressionPtr wi = coder::toIR(manager, impl);

			// --- Encode variant call as work item call ---

			// create job parameters
			core::IRBuilder builder(call->getNodeManager());
			core::ExpressionPtr one = builder.uintLit(1);

			// create call to job constructor and merge
			return builder.callExpr(unit, ext.getMerge(), builder.callExpr(builder.refType(ext.getWorkItemType()), ext.getParallel(),
			                                                          builder.callExpr(ext.getJobType(), ext.getCreateJob(), toVector(one, one, one, wi, data))));
		}

		core::StatementPtr convertVariant(const core::CallExprPtr& call) {
			auto pickCall = call->getFunctionExpr();

			// check whether this is indeed a call to pick variants
			assert_true(core::analysis::isCallOf(pickCall, basic.getPick())) << "Invalid Variant call!";

			// check if picking between implementations
			if(pickCall->getType()->getNodeType() != core::NT_FunctionType) { return call; }

			if(pickCall->hasAttachedValue<PickImplementationHint>()) {
				auto implHint = pickCall->getAttachedValue<PickImplementationHint>();
				switch(implHint) {
				case PickImplementationHint::CALL: return convertVariantToCall(call);
				case PickImplementationHint::SWITCH: return convertVariantToSwitch(call);
				default: assert_fail() << "Invalid variant implementation hint"; break;
				}
			}
			// default to switch
			auto ret = convertVariantToCall(call);
			return ret;
		}
	};
	}


	core::NodePtr WorkItemizer::process(const backend::Converter& converter, const core::NodePtr& node) {
		WorkItemIntroducer wiIntroducer(converter.getNodeManager());
		return wiIntroducer.resolveElement(node);
	}


	core::NodePtr InstrumentationSupport::process(const backend::Converter& converter, const core::NodePtr& node) {
		// get language extension
		auto& mgr = converter.getNodeManager();
		auto& rtExt = mgr.getLangExtension<insieme::backend::runtime::RuntimeExtension>();
		const auto& instExt = mgr.getLangExtension<core::lang::InstrumentationExtension>();

		// get max region id
		unsigned max = 0;
		core::visitDepthFirstOnce(node, [&](const core::CallExprPtr& call) {

			// check whether this call is specifying some region ID
			if(!(core::analysis::isCallOf(call, instExt.getInstrumentationRegionStart()) || core::analysis::isCallOf(call, rtExt.getRegionAttribute()))) { return; }

			// take first argument
			auto arg0 = core::transform::extractInitExprFromDecl(call[0]);
			assert_eq(arg0->getNodeType(), core::NT_Literal) << "Region ID is expected to be a literal!";
			unsigned regionId = arg0.as<core::LiteralPtr>()->getValueAs<unsigned>();
			if(max < regionId) { max = regionId; }
		});

		// OMP+ instrumentationRegionStart calls will be added in a later preprocessing phase
		// so we have to check annotations on JobExpr
		core::visitDepthFirstOnce(node, [&](const core::ExpressionPtr& cur) {
			if(cur->hasAttachedValue<annotations::ompp_objective_info>()) {
				unsigned regionId = cur->getAttachedValue<annotations::ompp_objective_info>().region_id;
				if(max < regionId) { max = regionId; }
			}
		});

		// add information to application
		core::NodeAddress root(node);
		assert_eq(node->getNodeType(), core::NT_Program);

		core::ProgramAddress program = root.as<core::ProgramAddress>();
		assert_eq(program->size(), 1u);

		// get body of lambda expression
		core::LambdaExprAddress main = program[0].as<core::LambdaExprAddress>();
		core::CompoundStmtAddress body = main->getBody();

		// build init call
		core::IRBuilder builder(mgr);
		auto initStmt = builder.callExpr(mgr.getLangBasic().getUnit(), instExt.getInstrumentationInitRegions(), builder.uintLit(max + 1));

		// insert into body
		return core::transform::insert(mgr, body, initStmt, 0);
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

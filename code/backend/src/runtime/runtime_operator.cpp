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
 *
 */
#include "insieme/backend/runtime/runtime_operator.h"

#include "insieme/backend/backend_config.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/runtime/runtime_code_fragments.h"
#include "insieme/backend/runtime/runtime_extension.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"

#include "insieme/core/lang/instrumentation_extension.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/time.h"
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		c_ast::ExpressionPtr getWorkItemPointer(ConversionContext& context, const c_ast::ExpressionPtr& index) {
			auto implTableFragment = ImplementationTable::get(context.getConverter());
			context.addDependency(implTableFragment->getDeclaration());
			return c_ast::ref(c_ast::subscript(c_ast::deref(implTableFragment->getTable()), index));
		}
	}


	OperatorConverterTable& addRuntimeSpecificOps(core::NodeManager& manager, OperatorConverterTable& table, const BackendConfig& config) {
		const RuntimeExtension& ext = manager.getLangExtension<RuntimeExtension>();
		const core::lang::ParallelExtension& parExt = manager.getLangExtension<core::lang::ParallelExtension>();
		const core::lang::TimeExtension& timeExt = manager.getLangExtension<core::lang::TimeExtension>();
		const core::lang::InstrumentationExtension& instExt = manager.getLangExtension<core::lang::InstrumentationExtension>();
		const core::lang::BasicGenerator& basic = manager.getLangBasic();

		#include "insieme/backend/operator_converter_begin.inc"

		table[ext.getRunStandalone()] = OP_CONVERTER {

			// add dependencies
			ADD_HEADER_FOR("irt_runtime_standalone");
			ADD_HEADER_FOR("irt_get_default_worker_count");

			ContextHandlingFragmentPtr fragment = ContextHandlingFragment::get(context.getConverter());
			context.addDependency(fragment);

			// add call to irt_runtime_standalone
			c_ast::NodePtr fun = C_NODE_MANAGER->create("irt_runtime_standalone");

			// create arguments
			c_ast::ExpressionPtr numThreads = c_ast::call(C_NODE_MANAGER->create("irt_get_default_worker_count"));
			c_ast::ExpressionPtr initContext = c_ast::ref(fragment->getInitFunctionName());
			c_ast::ExpressionPtr cleanupContext = c_ast::ref(fragment->getCleanupFunctionName());
			c_ast::ExpressionPtr impl = getWorkItemPointer(context, CONVERT_ARG(0));
			c_ast::ExpressionPtr args = CONVERT_ARG(1);

			// produce call
			return c_ast::call(fun, numThreads, initContext, cleanupContext, impl, args);
		};

		table[ext.getRegisterWorkItemImpl()] = OP_CONVERTER {

			// just register new work item
			ImplementationTablePtr implTable = ImplementationTable::get(context.getConverter());

			// convert argument into list of variants
			implTable->registerWorkItemImpl(context, ARG(0));

			context.addDependency(implTable);

			// no code substitute, only dependencies
			return c_ast::ExpressionPtr();
		};

		table[ext.getWorkItemImplCtr()] = OP_CONVERTER {

			// register work item
			ImplementationTablePtr implTable = ImplementationTable::get(context.getConverter());
			unsigned id = implTable->registerWorkItemImpl(context, call);

			// produce work item id as a result
			const RuntimeExtension& ext = NODE_MANAGER.getLangExtension<RuntimeExtension>();
			return c_ast::lit(CONVERT_TYPE(ext.getWorkItemImplType()), utils::numeric_cast<string>(id));
		};

		table[ext.getWrapLWData()] = OP_CONVERTER {
			// check arguments
			assert_eq(ARG(0)->getNodeType(), core::NT_TupleExpr) << "Only supported for tuple expressions!";

			// add type dependency
			const TypeInfo& info = GET_TYPE_INFO(call->getType());
			context.addDependency(info.definition);

			// register type within type table
			TypeTablePtr typeTable = TypeTable::get(context.getConverter());

			core::TupleTypePtr tupleType = DataItem::getUnfoldedLWDataItemType(static_pointer_cast<const core::TupleType>(ARG(0)->getType()));
			unsigned id = typeTable->registerType(context, tupleType);

			// convert wrapped data item struct
			core::TupleExprPtr pure = static_pointer_cast<const core::TupleExpr>(ARG(0));
			core::TupleExprPtr dataItem = DataItem::getLWDataItemValue(id, pure);

			// just forward the inner expression
			c_ast::TypePtr resType = c_ast::ptr(C_NODE_MANAGER->create<c_ast::NamedType>(C_NODE_MANAGER->create("irt_lw_data_item")));
			return c_ast::cast(resType, c_ast::ref(CONVERT_EXPR(dataItem)));
		};

		table[ext.getGetWorkItemArgument()] = OP_CONVERTER {
			// access work item member and cast to proper value
			c_ast::TypePtr paramPtr = c_ast::ptr(CONVERT_TYPE(core::analysis::getRepresentedType(ARG(2))));
			c_ast::ExpressionPtr inner = c_ast::cast(paramPtr, c_ast::access(c_ast::deref(CONVERT_ARG(0)), "parameters"));
			return c_ast::access(c_ast::deref(inner), format("c%d", core::encoder::toValue<unsigned>(ARG(1)) + 1));
		};

		table[ext.getGetWorkItemRange()] = OP_CONVERTER {
			// access work item range directly
			return c_ast::access(c_ast::deref(CONVERT_ARG(0)), "range");
		};

		table[ext.getCreateJob()] = OP_CONVERTER {
			const RuntimeExtension& ext = NODE_MANAGER.getLangExtension<RuntimeExtension>();

			// uint4, uint4, uint4, implType, data
			c_ast::ExpressionPtr min = CONVERT_ARG(0);
			c_ast::ExpressionPtr max = CONVERT_ARG(1);
			c_ast::ExpressionPtr mod = CONVERT_ARG(2);

			c_ast::ExpressionPtr wi = getWorkItemPointer(context, CONVERT_ARG(3));
			c_ast::ExpressionPtr data = CONVERT_ARG(4);

			return c_ast::init(CONVERT_TYPE(ext.getJobType()), min, max, mod, wi, data);
		};

		table[ext.getParallel()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_parallel");
			return c_ast::call(C_NODE_MANAGER->create("irt_parallel"), c_ast::ref(CONVERT_ARG(0)));
		};

		table[ext.getTask()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_task");
			return c_ast::call(C_NODE_MANAGER->create("irt_task"), c_ast::ref(CONVERT_ARG(0)));
		};

		table[ext.getRegion()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_region");
			return c_ast::call(C_NODE_MANAGER->create("irt_region"), c_ast::ref(CONVERT_ARG(0)));
		};

		table[ext.getMerge()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_merge");
			return c_ast::call(C_NODE_MANAGER->create("irt_merge"), CONVERT_ARG(0));
		};

		table[ext.getPfor()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_pfor");
			ADD_HEADER_FOR("irt_wi_get_current");

			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			c_ast::ExpressionPtr group = CONVERT_ARG(0);
			c_ast::ExpressionPtr min = CONVERT_ARG(1);
			c_ast::ExpressionPtr max = CONVERT_ARG(2);
			c_ast::ExpressionPtr step = CONVERT_ARG(3);
			c_ast::ExpressionPtr range = c_ast::init(C_NODE_MANAGER->create<c_ast::NamedType>(C_NODE_MANAGER->create("irt_work_item_range")), min, max, step);
			c_ast::ExpressionPtr body = getWorkItemPointer(context, CONVERT_ARG(4));
			c_ast::ExpressionPtr data = CONVERT_ARG(5);
			return c_ast::call(C_NODE_MANAGER->create("irt_pfor"), item, group, range, body, data);
		};

		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// TIME

		table[timeExt.getGetTime()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_get_wtime");
			return c_ast::call(C_NODE_MANAGER->create("irt_get_wtime"));
		};

		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// PARALLEL

		table[parExt.getGetThreadGroup()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg"), item, CONVERT_ARG(0));
		};

		table[parExt.getGetThreadId()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg_num");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg_num"), item, CONVERT_ARG(0));
		};

		table[parExt.getGetGroupSize()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg_size");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg_size"), item, CONVERT_ARG(0));
		};

		table[parExt.getBarrier()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_wg_barrier");
			return c_ast::call(C_NODE_MANAGER->create("irt_wg_barrier"), CONVERT_ARG(0));
		};

		table[parExt.getMergeAll()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_join_all");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_join_all"), item);
		};

		table[basic.getFlush()] = OP_CONVERTER { return c_ast::call(C_NODE_MANAGER->create("IRT_FLUSH"), CONVERT_ARG(0)); };

		table[parExt.getBusyLoop()] = OP_CONVERTER {
			auto whileLoop = core::transform::evalLazy(NODE_MANAGER, ARG(0), true);
			return c_ast::call(C_NODE_MANAGER->create("IRT_BUSYWHILE"), CONVERT_EXPR((whileLoop)));
		};

		// locks

		table[parExt.getLockInit()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_lock_init");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_init"), CONVERT_ARG(0));
		};
		table[parExt.getLockAcquire()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_lock_acquire");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_acquire"), CONVERT_ARG(0));
		};
		table[parExt.getLockTryAcquire()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_lock_tryacquire");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_tryacquire"), CONVERT_ARG(0));
		};
		table[parExt.getLockRelease()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_lock_release");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_release"), CONVERT_ARG(0));
		};

		// atomics

		#define BIN_ATOMIC_CONVERTER(__IRNAME, __IRTNAME)                                                                                                      \
			table[parExt.get##__IRNAME()] = OP_CONVERTER {                                                                                                      \
		        ADD_HEADER_FOR(#__IRTNAME);                                                                                                                    \
		        core::IRBuilder builder(NODE_MANAGER);                                                                                                         \
		        return c_ast::call(C_NODE_MANAGER->create(#__IRTNAME), CONVERT_ARG(0), CONVERT_ARG(1), CONVERT_TYPE(builder.deref(ARG(0))->getType()));        \
			};

		BIN_ATOMIC_CONVERTER(AtomicFetchAndAdd, irt_atomic_fetch_and_add)
		BIN_ATOMIC_CONVERTER(AtomicAddAndFetch, irt_atomic_add_and_fetch)
		BIN_ATOMIC_CONVERTER(AtomicFetchAndSub, irt_atomic_fetch_and_sub)
		BIN_ATOMIC_CONVERTER(AtomicSubAndFetch, irt_atomic_sub_and_fetch)
		BIN_ATOMIC_CONVERTER(AtomicFetchAndAnd, irt_atomic_fetch_and_and)
		BIN_ATOMIC_CONVERTER(AtomicAndAndFetch, irt_atomic_and_and_fetch)
		BIN_ATOMIC_CONVERTER(AtomicFetchAndOr, irt_atomic_fetch_and_or)
		BIN_ATOMIC_CONVERTER(AtomicOrAndFetch, irt_atomic_or_and_fetch)
		BIN_ATOMIC_CONVERTER(AtomicFetchAndXor, irt_atomic_fetch_and_xor)
		BIN_ATOMIC_CONVERTER(AtomicXorAndFetch, irt_atomic_xor_and_fetch)
		#undef BIN_ATOMIC_CONVERTER

		table[parExt.getAtomicValCompareAndSwap()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_atomic_val_compare_and_swap");
			core::IRBuilder builder(NODE_MANAGER);
			return c_ast::call(C_NODE_MANAGER->create("irt_atomic_val_compare_and_swap"), CONVERT_ARG(0), CONVERT_ARG(1), CONVERT_ARG(2),
			                   CONVERT_TYPE(builder.deref(ARG(0))->getType()));
		};

		table[parExt.getAtomicBoolCompareAndSwap()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_atomic_bool_compare_and_swap");
			core::IRBuilder builder(NODE_MANAGER);
			return c_ast::call(C_NODE_MANAGER->create("irt_atomic_bool_compare_and_swap"), CONVERT_ARG(0), CONVERT_ARG(1), CONVERT_ARG(2),
			                   CONVERT_TYPE(builder.deref(ARG(0))->getType()));
		};

		/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// SPECIAL

		table[basic.getPick()] = OP_CONVERTER {
			// uint16 irt_variant_pick(uint16 knop_id, uint16 num_variants);
			ADD_HEADER_FOR("irt_variant_pick");

			/* this implementation is still incomplete => just supporting simple switch stmts */
			assert(core::encoder::isEncodingOf<vector<uint16_t>>(ARG(0)) && "Only selection from unsigned integer lists are supported.");
			vector<uint16_t> options = core::encoder::toValue<vector<uint16_t>>(ARG(0));

			c_ast::TypePtr uint16 = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt16);
			return c_ast::call(C_NODE_MANAGER->create("irt_variant_pick"), c_ast::lit(uint16, "0"), c_ast::lit(uint16, toString(options.size())));
		};

		table[basic.getExit()] = OP_CONVERTER {
			ADD_HEADER_FOR("irt_exit");
			return c_ast::call(C_NODE_MANAGER->create("irt_exit"), CONVERT_ARG(0));
		};

		////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////INSTRUMENTATION

		table[instExt.getInstrumentationInitRegions()] = OP_CONVERTER {
			// just add info to context init
			ContextHandlingFragment::get(context.getConverter())
			    ->addInitExpression(format("    context->num_regions = %s;\n", core::transform::extractInitExprFromDecl(call[0]).as<core::LiteralPtr>()->getStringValue()));
			return NULL; // this is not producing an expression
		};

		table[instExt.getInstrumentationRegionStart()] = OP_CONVERTER { return c_ast::call(C_NODE_MANAGER->create("ir_inst_region_start"), CONVERT_ARG(0)); };

		table[instExt.getInstrumentationRegionEnd()] = OP_CONVERTER { return c_ast::call(C_NODE_MANAGER->create("ir_inst_region_end"), CONVERT_ARG(0)); };

		// param clause

		table[basic.getPickInRange()] = OP_CONVERTER {
			return c_ast::call(C_NODE_MANAGER->create("irt_optimizer_pick_in_range"), CONVERT_ARG(0), CONVERT_ARG(1), CONVERT_ARG(2), CONVERT_ARG(3),
			                   CONVERT_ARG(4));
		};

		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

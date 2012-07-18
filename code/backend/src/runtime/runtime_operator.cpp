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

#include "insieme/backend/runtime/runtime_operator.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/function_manager.h"

#include "insieme/backend/statement_converter.h"
#include "insieme/backend/type_manager.h"

#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/runtime/runtime_code_fragments.h"

#include "insieme/backend/c_ast/c_code.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

namespace insieme {
namespace backend {
namespace runtime {


	OperatorConverterTable& addRuntimeSpecificOps(core::NodeManager& manager, OperatorConverterTable& table) {

		const Extensions& ext = manager.getLangExtension<Extensions>();
		const core::lang::BasicGenerator& basic = manager.getLangBasic();

		#include "insieme/backend/operator_converter_begin.inc"

		table[ext.runStandalone] = OP_CONVERTER({

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
			c_ast::ExpressionPtr impl = CONVERT_ARG(0);
			c_ast::ExpressionPtr args = CONVERT_ARG(1);

			// produce call
			return c_ast::call(fun, numThreads, initContext, cleanupContext, impl, args);
		});

		table[ext.registerWorkItemImpl] = OP_CONVERTER({

			// just register new work item
			ImplementationTablePtr implTable = ImplementationTable::get(context.getConverter());

			// convert argument into list of variants
			implTable->registerWorkItemImpl(ARG(0));

			context.addDependency(implTable);

			// no code substitute, only dependencies
			return c_ast::ExpressionPtr();
		});

		table[ext.workItemImplCtr] = OP_CONVERTER({

			// register work item
			ImplementationTablePtr implTable = ImplementationTable::get(context.getConverter());
			unsigned id = implTable->registerWorkItemImpl(call);

			// produce work item id as a result
			const Extensions& ext = NODE_MANAGER.getLangExtension<Extensions>();
			return c_ast::lit(CONVERT_TYPE(ext.workItemImplType), utils::numeric_cast<string>(id));
		});

		table[ext.wrapLWData] = OP_CONVERTER({
			// check arguments
			assert(ARG(0)->getNodeType() == core::NT_TupleExpr && "Only supported for tuple expressions!");

			// add type dependency
			const TypeInfo& info = GET_TYPE_INFO(call->getType());
			context.addDependency(info.definition);

			// register type within type table
			TypeTablePtr typeTable = TypeTable::get(context.getConverter());

			core::TupleTypePtr tupleType = DataItem::getUnfoldedLWDataItemType(static_pointer_cast<const core::TupleType>(ARG(0)->getType()));
			unsigned id = typeTable->registerType(tupleType);

			// convert wrapped data item struct
			core::TupleExprPtr pure = static_pointer_cast<const core::TupleExpr>(ARG(0));
			core::TupleExprPtr dataItem = DataItem::getLWDataItemValue(id, pure);

			// just forward the inner expression
			c_ast::TypePtr resType = c_ast::ptr(C_NODE_MANAGER->create<c_ast::NamedType>(C_NODE_MANAGER->create("irt_lw_data_item")));
			return c_ast::cast(resType,c_ast::ref(CONVERT_EXPR(dataItem)));
		});

		table[ext.getWorkItemArgument] = OP_CONVERTER({
			// access work item member and cast to proper value
			c_ast::TypePtr paramPtr = c_ast::ptr(CONVERT_TYPE(core::encoder::toValue<core::TypePtr>(ARG(2))));
			c_ast::ExpressionPtr inner = c_ast::cast(paramPtr, c_ast::access(c_ast::deref(CONVERT_ARG(0)), "parameters"));
			return c_ast::access(c_ast::deref(inner), format("c%d", core::encoder::toValue<unsigned>(ARG(1))+1));
		});

		table[ext.getWorkItemRange] = OP_CONVERTER({
			// access work item range directly
			return c_ast::access(c_ast::deref(CONVERT_ARG(0)), "range");
		});

		table[ext.createJob] = OP_CONVERTER({
			const Extensions& ext = NODE_MANAGER.getLangExtension<Extensions>();

			// uint4, uint4, uint4, implType, data
			c_ast::ExpressionPtr min = CONVERT_ARG(0);
			c_ast::ExpressionPtr max = CONVERT_ARG(1);
			c_ast::ExpressionPtr mod = CONVERT_ARG(2);

			c_ast::ExpressionPtr wi = CONVERT_ARG(3);
			c_ast::ExpressionPtr data = CONVERT_ARG(4);

			return c_ast::init(CONVERT_TYPE(ext.jobType), min, max, mod, wi, data);
		});

		table[ext.parallel] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_parallel");
			return c_ast::call(C_NODE_MANAGER->create("irt_parallel"), c_ast::ref(CONVERT_ARG(0)));
		});

		table[ext.merge] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_merge");
			return c_ast::call(C_NODE_MANAGER->create("irt_merge"), CONVERT_ARG(0));
		});

		table[ext.pfor] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_pfor");
			ADD_HEADER_FOR("irt_wi_get_current");

			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			c_ast::ExpressionPtr group = CONVERT_ARG(0);
			c_ast::ExpressionPtr min = CONVERT_ARG(1);
			c_ast::ExpressionPtr max = CONVERT_ARG(2);
			c_ast::ExpressionPtr step = CONVERT_ARG(3);
			c_ast::ExpressionPtr range = c_ast::init(C_NODE_MANAGER->create<c_ast::NamedType>(C_NODE_MANAGER->create("irt_work_item_range")), min, max, step);
			c_ast::ExpressionPtr body = CONVERT_ARG(4);
			c_ast::ExpressionPtr data = CONVERT_ARG(5);
			return c_ast::call(C_NODE_MANAGER->create("irt_pfor"), item, group, range, body, data);
		});
		
		table[basic.getGetThreadGroup()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg"), item, CONVERT_ARG(0));
		});
		
		table[basic.getGetThreadId()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg_num");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg_num"), item, CONVERT_ARG(0));
		});

		table[basic.getGetGroupSize()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_get_wg_size");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_get_wg_size"), item, CONVERT_ARG(0));
		});

		table[basic.getBarrier()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_wg_barrier");
			return c_ast::call(C_NODE_MANAGER->create("irt_wg_barrier"), CONVERT_ARG(0));
		});
		
		table[basic.getMergeAll()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_wi_get_current");
			ADD_HEADER_FOR("irt_wi_join_all");
			c_ast::ExpressionPtr item = c_ast::call(C_NODE_MANAGER->create("irt_wi_get_current"));
			return c_ast::call(C_NODE_MANAGER->create("irt_wi_join_all"), item);
		});

		// locks

		table[basic.getLockCreate()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_lock_create");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_create"));
		});

		table[basic.getLockAcquire()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_lock_acquire");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_acquire"), CONVERT_ARG(0));
		});

		table[basic.getLockRelease()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_lock_release");
			return c_ast::call(C_NODE_MANAGER->create("irt_lock_release"), CONVERT_ARG(0));
		});

		table[basic.getPick()] = OP_CONVERTER({
			//uint16 irt_variant_pick(uint16 knop_id, uint16 num_variants);
			ADD_HEADER_FOR("irt_variant_pick");

			/* this implementation is still incomplete => just supporting simple switch stmts */
			assert(core::encoder::isEncodingOf<vector<uint16_t>>(ARG(0)) && "Only selection from unsigned integer lists are supported.");
			vector<uint16_t> options = core::encoder::toValue<vector<uint16_t>>(ARG(0));

			c_ast::TypePtr uint16 = C_NODE_MANAGER->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::UInt16);
			return c_ast::call(C_NODE_MANAGER->create("irt_variant_pick"), c_ast::lit(uint16,"0"), c_ast::lit(uint16,toString(options.size())));
		});

		table[basic.getExit()] = OP_CONVERTER({
			ADD_HEADER_FOR("irt_exit");
			return c_ast::call( C_NODE_MANAGER->create("irt_exit"), CONVERT_ARG(0));
		});

		#include "insieme/backend/operator_converter_end.inc"

		return table;
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

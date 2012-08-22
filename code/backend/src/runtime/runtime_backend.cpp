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

#include "insieme/backend/runtime/runtime_backend.h"

#include <sstream>

#include <cstdlib>
#include <iostream>

#include "insieme/core/ir_node.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/postprocessor.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/parallel_manager.h"

#include "insieme/backend/runtime/runtime_operator.h"
#include "insieme/backend/runtime/runtime_type_handler.h"
#include "insieme/backend/runtime/runtime_stmt_handler.h"
#include "insieme/backend/runtime/runtime_preprocessor.h"

#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		OperatorConverterTable getOperatorTable(core::NodeManager& manager);

		TypeHandlerList getTypeHandlerList();

		StmtHandlerList getStmtHandlerList();

	}

	RuntimeBackendPtr RuntimeBackend::getDefault() {
		return std::make_shared<RuntimeBackend>();
	}

	TargetCodePtr RuntimeBackend::convert(const core::NodePtr& code) const {

		// create and set up the converter
		Converter converter("RuntimeBackend");

		// set up the node manager (for temporals)
		core::NodeManager& nodeManager = code->getNodeManager();
		converter.setNodeManager(&nodeManager);

		// set up the shared code fragment manager (for the result)
		c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
		converter.setFragmentManager(fragmentManager);

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
				getBasicPreProcessorSequence(),
				makePreProcessor<runtime::WorkItemizer>(),
				makePreProcessor<runtime::StandaloneWrapper>()
		);
		converter.setPreProcessor(preprocessor);

		// set up post-processing
		PostProcessorPtr postprocessor = makePostProcessor<NoPostProcessing>();
		converter.setPostProcessor(postprocessor);

		// Prepare managers
		SimpleNameManager nameManager;
		converter.setNameManager(&nameManager);

		TypeIncludeTable typeIncludeTable = getBasicTypeIncludeTable();
		runtime::addRuntimeTypeIncludes(typeIncludeTable);
		TypeManager typeManager(converter, typeIncludeTable, getTypeHandlerList());
		converter.setTypeManager(&typeManager);

		StmtConverter stmtConverter(converter);
		converter.setStmtConverter(&stmtConverter);

		FunctionIncludeTable functionIncludeTable = getBasicFunctionIncludeTable();
		runtime::addRuntimeFunctionIncludes(functionIncludeTable);

		OperatorConverterTable opTable = getOperatorTable(nodeManager);
		getOperatorTableExtender()(nodeManager, opTable);

		FunctionManager functionManager(converter, opTable, functionIncludeTable);
		converter.setFunctionManager(&functionManager);

		// conduct conversion
		return converter.convert(code);
	}


	FunctionIncludeTable& addRuntimeFunctionIncludes(FunctionIncludeTable& table) {

		// add runtime-specific includes
		table["irt_get_default_worker_count"] 	= "standalone.h";
		table["irt_runtime_standalone"] 		= "standalone.h";
		table["irt_exit"] 						= "standalone.h";

		table["irt_parallel"] 					= "ir_interface.h";
		table["irt_merge"] 						= "ir_interface.h";
		table["irt_pfor"]						= "ir_interface.h";

		table["irt_wi_end"]						= "irt_all_impls.h";
		table["irt_wi_get_current"]				= "irt_all_impls.h";
		table["irt_wi_get_wg"]					= "irt_all_impls.h";
		table["irt_wi_get_wg_num"]				= "irt_all_impls.h";
		table["irt_wi_get_wg_size"]				= "irt_all_impls.h";
		table["irt_wi_join_all"] 				= "irt_all_impls.h";
		
		table["irt_wg_join"]					= "irt_all_impls.h";
		table["irt_wg_barrier"]					= "irt_all_impls.h";
		table["irt_wg_joining_barrier"]			= "irt_all_impls.h";
		
		table["irt_inst_region_start"]			= "irt_all_impls.h";
		table["irt_inst_region_end"]			= "irt_all_impls.h";
		
		table["irt_lock_create"] 	= "irt_all_impls.h";
		table["irt_lock_acquire"] 	= "irt_all_impls.h";
		table["irt_lock_release"] 	= "irt_all_impls.h";

		table["irt_atomic_fetch_and_add"]			= "irt_atomic.h";
		table["irt_atomic_fetch_and_sub"]			= "irt_atomic.h";
		table["irt_atomic_add_and_fetch"]			= "irt_atomic.h";
		table["irt_atomic_sub_and_fetch"]			= "irt_atomic.h";
		table["irt_atomic_or_and_fetch"]			= "irt_atomic.h";
		table["irt_atomic_and_and_fetch"]			= "irt_atomic.h";
		table["irt_atomic_xor_and_fetch"]			= "irt_atomic.h";
		table["irt_atomic_val_compare_and_swap"]	= "irt_atomic.h";
		table["irt_atomic_bool_compare_and_swap"]	= "irt_atomic.h";

		table["irt_variant_pick"]	= "irt_all_impls.h";

		return table;
	}

	TypeIncludeTable& addRuntimeTypeIncludes(TypeIncludeTable& table) {

		// some runtime types ...
		table["irt_parallel_job"]				= "ir_interface.h";
		table["irt_work_item_range"]			= "irt_all_impls.h";
		table["irt_wi_implementation_id"]		= "irt_all_impls.h";

		return table;
	}

	namespace {

		OperatorConverterTable getOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res = getBasicOperatorTable(manager);
			return addRuntimeSpecificOps(manager, res);
		}

		TypeHandlerList getTypeHandlerList() {
			TypeHandlerList res;
			res.push_back(RuntimeTypeHandler);
			return res;
		}

		StmtHandlerList getStmtHandlerList() {
			StmtHandlerList res;
			res.push_back(RuntimeStmtHandler);
			return res;
		}

	}

} // end namespace sequential
} // end namespace backend
} // end namespace insieme




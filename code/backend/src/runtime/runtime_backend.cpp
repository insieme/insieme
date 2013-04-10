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

#include "insieme/backend/addons/cpp_references.h"


namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		void addRuntimeFunctionIncludes(FunctionIncludeTable& table);

		void addRuntimeTypeIncludes(TypeIncludeTable& table);

	}


	RuntimeBackendPtr RuntimeBackend::getDefault(bool includeEffortEstimation) {
		auto res = std::make_shared<RuntimeBackend>(includeEffortEstimation);
		res->addAddOn<addons::CppReferences>();
		return res;
	}

	Converter RuntimeBackend::buildConverter(core::NodeManager& manager) const {

		// create and set up the converter
		Converter converter(manager, "RuntimeBackend");

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
				getBasicPreProcessorSequence(),
				makePreProcessor<runtime::InstrumentationSupport>(),				// needs to be before the conversion to work-items
				makePreProcessor<runtime::WorkItemizer>(includeEffortEstimation),
				makePreProcessor<runtime::StandaloneWrapper>()
		);
		converter.setPreProcessor(preprocessor);

		// Prepare managers

		TypeManager& typeManager = converter.getTypeManager();
		addRuntimeTypeIncludes(typeManager.getTypeIncludeTable());
		typeManager.addTypeHandler(RuntimeTypeHandler);

		StmtConverter& stmtConverter = converter.getStmtConverter();
		stmtConverter.addStmtHandler(RuntimeStmtHandler);

		FunctionManager& functionManager = converter.getFunctionManager();
		addRuntimeFunctionIncludes(functionManager.getFunctionIncludeTable());
		addRuntimeSpecificOps(manager, functionManager.getOperatorConverterTable());

		// done
		return converter;
	}

	namespace {
		
		void addRuntimeFunctionIncludes(FunctionIncludeTable& table) {

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

			table["irt_lock_init"] 		= "irt_all_impls.h";
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
		}

		void addRuntimeTypeIncludes(TypeIncludeTable& table) {

			// some runtime types ...
			table["irt_parallel_job"]				= "ir_interface.h";
			table["irt_work_item_range"]			= "irt_all_impls.h";
			table["irt_wi_implementation_id"]		= "irt_all_impls.h";
		}
		
	}

} // end namespace sequential
} // end namespace backend
} // end namespace insieme




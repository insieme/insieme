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

#include "insieme/backend/backend_config.h"

namespace insieme {
namespace backend {
namespace runtime {

	namespace {

		void addRuntimeFunctionIncludes(FunctionIncludeTable& table);

		void addRuntimeTypeIncludes(TypeIncludeTable& table);
	}


	RuntimeBackendPtr RuntimeBackend::getDefault() {
		BackendConfigPtr config = std::make_shared<BackendConfig>();

		auto res = std::make_shared<RuntimeBackend>(config);
		res->addDefaultAddons();
		return res;
	}

	Converter RuntimeBackend::buildConverter(core::NodeManager& manager) const {
		// create and set up the converter
		Converter converter(manager, "RuntimeBackend", getConfiguration());

		// set up pre-processing
		PreProcessorPtr preprocessor =
			makePreProcessor<PreProcessingSequence>(getBasicPreProcessorSequence(),
			                                        makePreProcessor<runtime::InstrumentationSupport>(), // needs to be before the conversion to work-items
			                                        makePreProcessor<runtime::WorkItemizer>(),
			                                        makePreProcessor<runtime::StandaloneWrapper>());
		converter.setPreProcessor(preprocessor);

		// Prepare managers

		TypeManager& typeManager = converter.getTypeManager();
		addRuntimeTypeIncludes(typeManager.getTypeIncludeTable());
		typeManager.addTypeHandler(RuntimeTypeHandler);

		StmtConverter& stmtConverter = converter.getStmtConverter();
		stmtConverter.addStmtHandler(RuntimeStmtHandler);

		FunctionManager& functionManager = converter.getFunctionManager();
		addRuntimeFunctionIncludes(functionManager.getFunctionIncludeTable());
		addRuntimeSpecificOps(manager, functionManager.getOperatorConverterTable(), getConfiguration());

		NameManager& nameMan = converter.getNameManager();
		// this should be an exhaustive listing at some point, for now it only includes functions which caused issues
		nameMan.reserveName("read");  // unistd.h
		nameMan.reserveName("write"); // unistd.h

		// done
		return converter;
	}

	namespace {

		void addRuntimeFunctionIncludes(FunctionIncludeTable& table) {
			// add runtime-specific includes
			table["irt_get_default_worker_count"] = "standalone.h";
			table["irt_runtime_standalone"] = "standalone.h";
			table["irt_exit"] = "standalone.h";

			table["irt_parallel"] = "ir_interface.h";
			table["irt_task"] = "ir_interface.h";
			table["irt_region"] = "ir_interface.h";
			table["irt_merge"] = "ir_interface.h";
			table["irt_pfor"] = "ir_interface.h";
			table["irt_get_wtime"] = "ir_interface.h";

			table["irt_wi_end"] = "irt_all_impls.h";
			table["irt_wi_get_current"] = "irt_all_impls.h";
			table["irt_wi_get_wg"] = "irt_all_impls.h";
			table["irt_wi_get_wg_num"] = "irt_all_impls.h";
			table["irt_wi_get_wg_size"] = "irt_all_impls.h";
			table["irt_wi_join_all"] = "irt_all_impls.h";

			table["irt_wg_join"] = "irt_all_impls.h";
			table["irt_wg_barrier"] = "irt_all_impls.h";
			table["irt_wg_joining_barrier"] = "irt_all_impls.h";

			table["irt_inst_region_start"] = "irt_all_impls.h";
			table["irt_inst_region_end"] = "irt_all_impls.h";

			table["irt_lock_init"] = "irt_all_impls.h";
			table["irt_lock_acquire"] = "irt_all_impls.h";
			table["irt_lock_tryacquire"] = "irt_all_impls.h";
			table["irt_lock_release"] = "irt_all_impls.h";

			table["irt_atomic_fetch_and_add"] = "irt_all_impls.h";
			table["irt_atomic_fetch_and_sub"] = "irt_all_impls.h";
			table["irt_atomic_add_and_fetch"] = "irt_all_impls.h";
			table["irt_atomic_sub_and_fetch"] = "irt_all_impls.h";
			table["irt_atomic_or_and_fetch"] = "irt_all_impls.h";
			table["irt_atomic_and_and_fetch"] = "irt_all_impls.h";
			table["irt_atomic_xor_and_fetch"] = "irt_all_impls.h";
			table["irt_atomic_val_compare_and_swap"] = "irt_all_impls.h";
			table["irt_atomic_bool_compare_and_swap"] = "irt_all_impls.h";

			table["irt_atomic_fetch_and_add_fp"] = "irt_all_impls.h";
			table["irt_atomic_fetch_and_sub_fp"] = "irt_all_impls.h";
			table["irt_atomic_add_and_fetch_fp"] = "irt_all_impls.h";
			table["irt_atomic_sub_and_fetch_fp"] = "irt_all_impls.h";

			table["irt_variant_pick"] = "irt_all_impls.h";
		}

		void addRuntimeTypeIncludes(TypeIncludeTable& table) {
			// some runtime types ...
			table["irt_parallel_job"] = "ir_interface.h";
			table["irt_work_item_range"] = "irt_all_impls.h";
			table["irt_wi_implementation_id"] = "irt_all_impls.h";
		}
	}

} // end namespace sequential
} // end namespace backend
} // end namespace insieme

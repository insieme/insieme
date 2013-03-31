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

#include <sstream>
#include <fstream>
#include <cstdlib>
#include <iostream>

#include "insieme/core/ir_node.h"
#include "insieme/core/dump/binary_dump.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/postprocessor.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/parallel_manager.h"


#include "insieme/backend/ocl_kernel/kernel_operator.h"
#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"
#include "insieme/backend/ocl_kernel/kernel_type_handler.h"
#include "insieme/backend/ocl_kernel/kernel_stmt_handler.h"
#include "insieme/backend/ocl_kernel/kernel_backend.h"

#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {
namespace ocl_kernel {

	namespace {

		void extendFunctionIncludeTable(FunctionIncludeTable& table);

	}

	OCLKernelBackendPtr OCLKernelBackend::getDefault() {
		return std::make_shared<OCLKernelBackend>();
	}

	OCLKernelBackendPtr OCLKernelBackend::getDefault(const std::string& kernelDumpPath) {
		return std::make_shared<OCLKernelBackend>(kernelDumpPath);
	}

	Converter OCLKernelBackend::buildConverter(core::NodeManager& manager) const {

		// create and set up the converter
		Converter converter(manager, "OpenCL Kernel Backend");

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
				getBasicPreProcessorSequence(SKIP_POINTWISE_EXPANSION),
				makePreProcessor<KernelPreprocessor>(kernelDumpPath)
		);
		converter.setPreProcessor(preprocessor);

		// update type manager configuration
		TypeManager& typeManager = converter.getTypeManager();
		typeManager.addTypeHandler(OclKernelTypeHandler);

		// update stmt converter
		StmtConverter& stmtConverter = converter.getStmtConverter();
		stmtConverter.addStmtHandler(OclKernelStmtHandler);

		// update function manager
		FunctionManager& functionManager = converter.getFunctionManager();
		addOpenCLKernelSpecificOps(manager, functionManager.getOperatorConverterTable());
		extendFunctionIncludeTable(functionManager.getFunctionIncludeTable());

		// done
		return converter;
	}

	namespace {

		void extendFunctionIncludeTable(FunctionIncludeTable& table) {
			
			// add OpenCL-specific includes
			table["get_local_id"]                     = "";
			table["get_global_id"]                    = "";
			table["get_local_size"]                   = "";
			table["get_global_size"]                  = "";
			table["get_num_groups"]                   = "";
			table["barrier"]                          = "";
			table["exp"]                              = "";
			table["fabs"]                             = "";
			table["sqrt"]                             = "";
			table["log"]                              = "";
			table["hypot"]                            = "";
			table["cos"]                              = "";
			table["sin"]                              = "";
			table["min"]                              = "";
			table["max"]                              = "";
			table["normalize"]                        = "";
			table["floor"]                            = "";
			table["mix"]                              = "";
			table["pow"]                              = "";
			table["ceil"]                             = "";

		}

	}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme




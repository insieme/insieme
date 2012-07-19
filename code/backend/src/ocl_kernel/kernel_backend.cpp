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

		OperatorConverterTable getOperatorTable(core::NodeManager& manager);

		FunctionIncludeTable getFunctionIncludeTable();

		TypeHandlerList getTypeHandlerList();

		StmtHandlerList getStmtHandlerList();

	}

	OCLKernelBackendPtr OCLKernelBackend::getDefault() {
		return std::make_shared<OCLKernelBackend>();
	}

	OCLKernelBackendPtr OCLKernelBackend::getDefault(const std::string& kernelDumpPath) {
		return std::make_shared<OCLKernelBackend>(kernelDumpPath);
	}

	TargetCodePtr OCLKernelBackend::convert(const core::NodePtr& code) const {
		// create and set up the converter
		Converter converter("OpenCL Kernel Backend");

		// set up the node manager (for temporals)
		core::NodeManager& nodeManager = code->getNodeManager();
		converter.setNodeManager(&nodeManager);

		// set up the shared code fragment manager (for the result)
		c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
		converter.setFragmentManager(fragmentManager);

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
				getBasicPreProcessorSequence(SKIP_POINTWISE_EXPANSION),
				makePreProcessor<KernelPreprocessor>(kernelDumpPath)
		);
		converter.setPreProcessor(preprocessor);

		// set up post-processing
		PostProcessorPtr postprocessor = makePostProcessor<NoPostProcessing>();
		converter.setPostProcessor(postprocessor);

		// Prepare managers
		SimpleNameManager nameManager;
		converter.setNameManager(&nameManager);

		TypeManager typeManager(converter, getBasicTypeIncludeTable(), getTypeHandlerList());
		converter.setTypeManager(&typeManager);

		StmtConverter stmtConverter(converter, getStmtHandlerList());
		converter.setStmtConverter(&stmtConverter);

		FunctionManager functionManager(converter, getOperatorTable(nodeManager), getFunctionIncludeTable());
		converter.setFunctionManager(&functionManager);

		// dump kernel to binary file
//		std::fstream outFile("kernel.bin", std::fstream::out);

//		core::dump::binary::dumpIR(outFile, code);
//		outFile.close();

		// conduct conversion
		return converter.convert(code);
	}

	namespace {

		OperatorConverterTable getOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res = getBasicOperatorTable(manager);
			return addOpenCLKernelSpecificOps(manager, res);
		}

		FunctionIncludeTable getFunctionIncludeTable() {
			FunctionIncludeTable res = getBasicFunctionIncludeTable();
			
			// add OpenCL-specific includes

            res["get_local_id"]                     = "";
			res["get_global_id"] 					= "";
			res["get_local_size"]					= "";
			res["get_global_size"] 					= "";
			res["get_num_groups"] 					= "";
			res["barrier"]							= "";
			res["exp"]								= "";
			res["fabs"]								= "";
			res["sqrt"]								= "";
			res["log"]								= "";
            res["hypot"]                            = "";
            res["cos"]                              = "";
            res["sin"]                              = "";
            res["min"]                              = "";
            res["max"]                              = "";
            res["normalize"]                        = "";
            res["floor"]                            = "";
			return res;
		}

		TypeHandlerList getTypeHandlerList() {
			TypeHandlerList res;
			res.push_back(OclKernelTypeHandler);
			return res;
		}

		StmtHandlerList getStmtHandlerList() {
			StmtHandlerList res;
			res.push_back(OclKernelStmtHandler);
			return res;
		}


	}

} // end namespace ocl_kernel
} // end namespace backend
} // end namespace insieme




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

#include <cstdlib>
#include <iostream>

#include "insieme/core/ir_node.h"

#include "insieme/backend/preprocessor.h"
#include "insieme/backend/postprocessor.h"
#include "insieme/backend/converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/parallel_manager.h"
#include "insieme/backend/statement_converter.h"

#include "insieme/backend/ocl_host/host_backend.h"
#include "insieme/backend/ocl_host/host_operator.h"
#include "insieme/backend/ocl_host/host_preprocessor.h"
#include "insieme/backend/ocl_host/host_type_handler.h"
#include "insieme/backend/ocl_host/host_stmt_handler.h"

#include "insieme/backend/ocl_kernel/kernel_preprocessor.h"

#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_preprocessor.h"
#include "insieme/backend/runtime/runtime_operator.h"
#include "insieme/backend/runtime/runtime_type_handler.h"
#include "insieme/backend/runtime/runtime_stmt_handler.h"

#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {
namespace ocl_host {


	namespace {

		OperatorConverterTable getOperatorTable(core::NodeManager& manager);

		TypeHandlerList getTypeHandlerList();

		StmtHandlerList getStmtHandlerList();
	}

	OCLHostBackendPtr OCLHostBackend::getDefault() {
		return std::make_shared<OCLHostBackend>();
	}

	OCLHostBackendPtr OCLHostBackend::getDefault(const std::string& kernelDumpPath) {
		return std::make_shared<OCLHostBackend>(kernelDumpPath);
	}

	TargetCodePtr OCLHostBackend::convert(const core::NodePtr& code) const {

		// create and set up the converter
		Converter converter("OpenCL Host Backend");

		// set up the node manager (for temporals)
		core::NodeManager& nodeManager = code->getNodeManager();
		converter.setNodeManager(&nodeManager);

		// set up the shared code fragment manager (for the result)
		c_ast::SharedCodeFragmentManager fragmentManager = c_ast::CodeFragmentManager::createShared();
		converter.setFragmentManager(fragmentManager);

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
			getBasicPreProcessorSequence(SKIP_POINTWISE_EXPANSION),
			makePreProcessor<ocl_kernel::KernelPreprocessor>(kernelDumpPath),
			makePreProcessor<HostPreprocessor>(),
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

		StmtConverter stmtConverter(converter, getStmtHandlerList());
		converter.setStmtConverter(&stmtConverter);

		FunctionIncludeTable functionIncludeTable = getBasicFunctionIncludeTable();
		addOpenclHostFunctionIncludes(functionIncludeTable);
		runtime::addRuntimeFunctionIncludes(functionIncludeTable);
		FunctionManager functionManager(converter, getOperatorTable(nodeManager), functionIncludeTable);
		converter.setFunctionManager(&functionManager);

		// conduct conversion
		return converter.convert(code);
	}

	FunctionIncludeTable& addOpenclHostFunctionIncludes(FunctionIncludeTable& table) {

		// add OpenCL Host specific includes
		table["irt_ocl_rt_create_buffer"]		= "irt_all_impls.h";
		table["irt_ocl_write_buffer"]			= "irt_all_impls.h";
		table["irt_ocl_read_buffer"]			= "irt_all_impls.h";
		table["irt_ocl_release_buffer"]			= "irt_all_impls.h";

		return table;
	}


	namespace {
		OperatorConverterTable getOperatorTable(core::NodeManager& manager) {
			OperatorConverterTable res = getBasicOperatorTable(manager);
			runtime::addRuntimeSpecificOps(manager, res);
			return addOpenCLHostSpecificOps(manager, res);
		}

		TypeHandlerList getTypeHandlerList() {
			TypeHandlerList res;
			res.push_back(OclHostTypeHandler);
			res.push_back(runtime::RuntimeTypeHandler);
			return res;
		}

		StmtHandlerList getStmtHandlerList() {
			StmtHandlerList res;
			res.push_back(OclHostStmtHandler);
			res.push_back(runtime::RuntimeStmtHandler);
			return res;
		}
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme




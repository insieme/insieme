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

#include "insieme/backend/addons/cpp_references.h"
#include "insieme/backend/addons/complex_type.h"
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/addons/simd_vector.h"

#include "insieme/backend/c_ast/c_code.h"


namespace insieme {
namespace backend {
namespace ocl_host {


	namespace {

		FunctionIncludeTable& addOpenclHostFunctionIncludes(FunctionIncludeTable& table);

		void addOCLTypeIncludes(TypeIncludeTable& table);

	}

	OCLHostBackendPtr OCLHostBackend::getDefault(const std::string& kernelDumpPath, bool includeEffortEstimation) {
		auto res = std::make_shared<OCLHostBackend>(includeEffortEstimation, kernelDumpPath);
		res->addAddOn<addons::CppReferences>();
		res->addAddOn<addons::ComplexType>();
		res->addAddOn<addons::EnumTypes>();
		res->addAddOn<addons::SIMDVector>();
		return res;
	}

	Converter OCLHostBackend::buildConverter(core::NodeManager& manager) const {

		// get standard converter from runtime backend
		Converter converter = runtime::RuntimeBackend::buildConverter(manager);

		// update converter name
		converter.setConverterName("OpenCL Host Backend");

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
			getBasicPreProcessorSequence(SKIP_POINTWISE_EXPANSION),
			makePreProcessor<ocl_kernel::KernelPreprocessor>(kernelDumpPath),
			makePreProcessor<HostPreprocessor>(),
			makePreProcessor<runtime::WorkItemizer>(),
			makePreProcessor<runtime::StandaloneWrapper>()
		);
		converter.setPreProcessor(preprocessor);

		// update type manager
		TypeManager& typeManager = converter.getTypeManager();
		addOCLTypeIncludes(typeManager.getTypeIncludeTable());
		typeManager.addTypeHandler(OclHostTypeHandler);


		// update statement converter
		StmtConverter& stmtConverter = converter.getStmtConverter();
		stmtConverter.addStmtHandler(OclHostStmtHandler);

		// update function manager
		FunctionManager& functionManager = converter.getFunctionManager();
		addOpenclHostFunctionIncludes(functionManager.getFunctionIncludeTable());
		addOpenCLHostSpecificOps(manager, functionManager.getOperatorConverterTable());

		// done
		return converter;
	}

	namespace {

		FunctionIncludeTable& addOpenclHostFunctionIncludes(FunctionIncludeTable& table) {

			// add OpenCL Host specific includes
			table["irt_ocl_rt_create_buffer"]		= "irt_all_impls.h";
			table["irt_ocl_write_buffer"]			= "irt_all_impls.h";
			table["irt_ocl_read_buffer"]			= "irt_all_impls.h";
			table["irt_ocl_release_buffer"]			= "irt_all_impls.h";

			return table;
		}


		void addOCLTypeIncludes(TypeIncludeTable& table) {

			// for the following types no include is necessary (part of the runtime)
			table["cl_float2"] = "CL/cl.h";
			table["cl_float4"] = "CL/cl.h";
			table["cl_float8"] = "CL/cl.h";
			table["cl_float16"] = "CL/cl.h";

			table["cl_char2"] = "CL/cl.h";
			table["cl_char4"] = "CL/cl.h";
			table["cl_char8"] = "CL/cl.h";
			table["cl_char16"] = "CL/cl.h";

			table["cl_uchar2"] = "CL/cl.h";
			table["cl_uchar4"] = "CL/cl.h";
			table["cl_uchar8"] = "CL/cl.h";
			table["cl_uchar16"] = "CL/cl.h";

			table["cl_short2"] = "CL/cl.h";
			table["cl_short4"] = "CL/cl.h";
			table["cl_short8"] = "CL/cl.h";
			table["cl_short16"] = "CL/cl.h";

			table["cl_ushort2"] = "CL/cl.h";
			table["cl_ushort4"] = "CL/cl.h";
			table["cl_ushort8"] = "CL/cl.h";
			table["cl_ushort16"] = "CL/cl.h";

			table["cl_int2"] = "CL/cl.h";
			table["cl_int4"] = "CL/cl.h";
			table["cl_int8"] = "CL/cl.h";
			table["cl_int16"] = "CL/cl.h";

			table["cl_uint2"] = "CL/cl.h";
			table["cl_uint4"] = "CL/cl.h";
			table["cl_uint8"] = "CL/cl.h";
			table["cl_uint16"] = "CL/cl.h";

			table["cl_long2"] = "CL/cl.h";
			table["cl_long4"] = "CL/cl.h";
			table["cl_long8"] = "CL/cl.h";
			table["cl_long16"] = "CL/cl.h";

			table["cl_ulong2"] = "CL/cl.h";
			table["cl_ulong4"] = "CL/cl.h";
			table["cl_ulong8"] = "CL/cl.h";
			table["cl_ulong16"] = "CL/cl.h";

		}
	}

} // end namespace ocl_host
} // end namespace backend
} // end namespace insieme




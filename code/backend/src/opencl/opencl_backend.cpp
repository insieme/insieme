/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/function_manager.h"
#include "insieme/backend/parallel_manager.h"
#include "insieme/backend/backend_config.h"

#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/opencl/opencl_backend.h"
#include "insieme/backend/opencl/opencl_preprocessor.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_operator.h"
#include "insieme/backend/opencl/opencl_type_handler.h"

#include "insieme/backend/runtime/runtime_operator.h"
#include "insieme/backend/runtime/runtime_type_handler.h"
#include "insieme/backend/runtime/runtime_stmt_handler.h"
#include "insieme/backend/runtime/runtime_preprocessor.h"

#include "insieme/backend/sequential/sequential_preprocessor.h"

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/addons/cpp_casts.h"
#include "insieme/backend/addons/cpp_memb_ptr.h"
#include "insieme/backend/addons/complex_type.h"
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/addons/io.h"
#include "insieme/backend/addons/longlong_type.h"
#include "insieme/backend/addons/asm_stmt.h"
#include "insieme/backend/addons/varargs.h"
#include "insieme/backend/addons/static_variables.h"

namespace insieme {
namespace backend {
namespace opencl {

	OpenCLBackend::OpenCLBackend(bool includeEffortEstimation, const BackendConfigPtr& config ) :
		RuntimeBackend(includeEffortEstimation, config), includeEffortEstimation(includeEffortEstimation)
	{ }

	OpenCLBackendPtr OpenCLBackend::getDefault(bool includeEffortEstimation, bool isGemsclaim) {
		BackendConfigPtr config = std::make_shared<BackendConfig>();

		if(isGemsclaim) {
			config->mainFunctionName = "insieme_main";
			config->additionalHeaderFiles.push_back("input_file.h");
			// Jan fixed the issue but let's keep the code for a while
			// config->areShiftOpsSupported = false;
			config->instrumentMainFunction = true;
		}

		auto res = std::make_shared<OpenCLBackend>(includeEffortEstimation, config);
		res->addAddOn<addons::PointerType>();
		res->addAddOn<addons::CppCastsAddon>();
		res->addAddOn<addons::CppMembAddon>();
		res->addAddOn<addons::ComplexType>();
		res->addAddOn<addons::EnumType>();
		res->addAddOn<addons::InputOutput>();
		res->addAddOn<addons::LongLongType>();
		res->addAddOn<addons::AsmStmt>();
		res->addAddOn<addons::VarArgs>();
		res->addAddOn<addons::StaticVariables>();
		return res;
	}

	Converter OpenCLBackend::buildConverter(core::NodeManager& manager) const {
		// first of all, let the RuntimeBackend setup the converter
		Converter converter = runtime::RuntimeBackend::buildConverter(manager);
		// adjust the name of it
		converter.setConverterName("OpenCLBackend");

		// set up opencl pre-processing
		PreProcessorPtr preprocessor = makePreProcessor<PreProcessingSequence>(
		    getBasicPreProcessorSequence(),
		    makePreProcessor<runtime::InstrumentationSupport>(), // needs to be before the conversion to work-items
		    makePreProcessor<opencl::OffloadSupport>(),
		    makePreProcessor<runtime::WorkItemizer>(includeEffortEstimation), makePreProcessor<runtime::StandaloneWrapper>());
		converter.setPreProcessor(preprocessor);

		TypeManager& typeManager = converter.getTypeManager();
		typeManager.addTypeHandler(OpenCLTypeHandler);

		FunctionManager& functionManager = converter.getFunctionManager();
		addOpenCLSpecificOps(manager, functionManager.getOperatorConverterTable(), getConfiguration());
		addOpenCLSpecificHeaders(functionManager.getFunctionIncludeTable());
		return converter;
	}
	
	OpenCLKernelBackend::OpenCLKernelBackend(const BackendConfigPtr& config) :
		Backend(std::vector<AddOnPtr>(), config)
	{ }
	
	OpenCLKernelBackendPtr OpenCLKernelBackend::getDefault() {
		auto res = std::make_shared<OpenCLKernelBackend>(std::make_shared<BackendConfig>());
		res->addAddOn<addons::PointerType>();
		return res;
	}

	Converter OpenCLKernelBackend::buildConverter(core::NodeManager& manager) const {
		Converter converter(manager, "OpenCLKernelBackend", getConfiguration());
		
		PreProcessorPtr preprocessor = makePreProcessor<PreProcessingSequence>(
			makePreProcessor<sequential::Sequentializer>(),
			getBasicPreProcessorSequence());
		converter.setPreProcessor(preprocessor);
		
		return converter;
	}
	
} // end namespace opencl
} // end namespace backend
} // end namespace insieme

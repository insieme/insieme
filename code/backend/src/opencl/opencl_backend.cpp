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
#include "insieme/backend/opencl/opencl_postprocessor.h"
#include "insieme/backend/opencl/opencl_code_fragments.h"
#include "insieme/backend/opencl/opencl_operator.h"
#include "insieme/backend/opencl/opencl_type_handler.h"
#include "insieme/backend/opencl/opencl_transform.h"

#include "insieme/backend/runtime/runtime_operator.h"
#include "insieme/backend/runtime/runtime_type_handler.h"
#include "insieme/backend/runtime/runtime_stmt_handler.h"
#include "insieme/backend/runtime/runtime_preprocessor.h"

#include "insieme/backend/sequential/sequential_preprocessor.h"

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/addons/cpp_casts.h"
#include "insieme/backend/addons/complex_type.h"
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/addons/io.h"
#include "insieme/backend/addons/longlong_type.h"
#include "insieme/backend/addons/asm_stmt.h"
#include "insieme/backend/addons/varargs.h"
#include "insieme/backend/addons/static_variables.h"
#include "insieme/backend/addons/compound_operators.h"

namespace insieme {
namespace backend {
namespace opencl {

	OpenCLBackend::OpenCLBackend(const BackendConfigPtr& config) :
		RuntimeBackend(config)
	{ }

	OpenCLBackendPtr OpenCLBackend::getDefault(const BackendConfigPtr& config) {
		auto res = std::make_shared<OpenCLBackend>(config);
		res->addAddOn<addons::PointerType>();
		res->addAddOn<addons::CppCastsAddon>();
		res->addAddOn<addons::ComplexType>();
		res->addAddOn<addons::CompoundOps>();
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
		    makePreProcessor<OffloadSupportPre>(),
		    makePreProcessor<runtime::WorkItemizer>(),
			makePreProcessor<runtime::StandaloneWrapper>());
		converter.setPreProcessor(preprocessor);

		TypeManager& typeManager = converter.getTypeManager();
		typeManager.addTypeHandler(HostTypeHandler);

		FunctionManager& functionManager = converter.getFunctionManager();
		addOpenCLSpecificOps(manager, functionManager.getOperatorConverterTable(), getConfiguration());
		addOpenCLSpecificHeaders(functionManager.getFunctionIncludeTable());
		return converter;
	}

	KernelBackend::KernelBackend(const transform::StepContext& sc, const BackendConfigPtr& config) :
		Backend(std::vector<AddOnPtr>(), config), sc(sc)
	{ }

	KernelBackendPtr KernelBackend::getDefault(const transform::StepContext& sc, const BackendConfigPtr& config) {
		auto res = std::make_shared<KernelBackend>(sc, config);
		res->addAddOn<addons::PointerType>();
		res->addAddOn<addons::CompoundOps>();
		return res;
	}

	Converter KernelBackend::buildConverter(core::NodeManager& manager) const {
		Converter converter(manager, "OpenCLKernelBackend", getConfiguration());

		PreProcessorPtr preprocessor = makePreProcessor<PreProcessingSequence>(
			makePreProcessor<sequential::Sequentializer>(),
			getBasicPreProcessorSequence());
		converter.setPreProcessor(preprocessor);

		PostProcessorPtr postprocessor = makePostProcessor<PostProcessingSequence>(
			makePostProcessor<OffloadSupportPost>(boost::ref(sc)));
		converter.setPostProcessor(postprocessor);

		TypeManager& typeManager = converter.getTypeManager();
		typeManager.addTypeHandler(KrnlTypeHandler);

		FunctionManager& functionManager = converter.getFunctionManager();
		addOpenCLSpecificOps(manager, functionManager.getOperatorConverterTable(), getConfiguration());
		return converter;
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme

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
 *
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

#include "insieme/backend/addons/pointer_type.h"
#include "insieme/backend/addons/compound_operators.h"

#include "insieme/backend/sequential/sequential_preprocessor.h"

namespace insieme {
namespace backend {
namespace opencl {

	OpenCLBackend::OpenCLBackend(const BackendConfigPtr& config) : RuntimeBackend(config)
	{ }

	OpenCLBackendPtr OpenCLBackend::getDefault(const BackendConfigPtr& config) {
		auto res = std::make_shared<OpenCLBackend>(config);
		res->addDefaultAddons();
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

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

#include "insieme/backend/sequential/sequential_backend.h"

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

#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/sequential/sequential_preprocessor.h"
#include "insieme/backend/sequential/sequential_type_handler.h"

namespace insieme {
namespace backend {
namespace sequential {

	SequentialBackendPtr SequentialBackend::getDefault() {
		auto res = std::make_shared<SequentialBackend>();
		res->addDefaultAddons();
		return res;
	}

	Converter SequentialBackend::buildConverter(core::NodeManager& manager) const {
		// create and set up the converter
		Converter converter(manager, "SequentialBackend", getConfiguration());

		// set up pre-processing
		PreProcessorPtr preprocessor = makePreProcessor<PreProcessingSequence>(makePreProcessor<Sequentializer>(), getBasicPreProcessorSequence());
		converter.setPreProcessor(preprocessor);

		// update type manager
		TypeManager& typeManager = converter.getTypeManager();
		typeManager.addTypeHandler(SequentialTypeHandler);

		// done
		return converter;
	}

} // end namespace sequential
} // end namespace backend
} // end namespace insieme

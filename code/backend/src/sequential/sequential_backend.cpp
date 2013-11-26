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

#include "insieme/backend/addons/cpp_references.h"
#include "insieme/backend/addons/cpp_memb_ptr.h"
#include "insieme/backend/addons/complex_type.h"
#include "insieme/backend/addons/enum_type.h"
#include "insieme/backend/addons/simd_vector.h"
#include "insieme/backend/addons/asm_stmt.h"
#include "insieme/backend/addons/varargs.h"

namespace insieme {
namespace backend {
namespace sequential {

	SequentialBackendPtr SequentialBackend::getDefault() {
		auto res = std::make_shared<SequentialBackend>();
		res->addAddOn<addons::CppReferences>();
		res->addAddOn<addons::CppMembAddon>();
		res->addAddOn<addons::ComplexType>();
		res->addAddOn<addons::EnumTypes>();
		res->addAddOn<addons::SIMDVector>();
		res->addAddOn<addons::AsmStmt>();
		res->addAddOn<addons::VarArgs>();
		return res;
	}

	Converter SequentialBackend::buildConverter(core::NodeManager& manager) const {

		// create and set up the converter
		Converter converter(manager, "SequentialBackend");

		// set up pre-processing
		PreProcessorPtr preprocessor =  makePreProcessor<PreProcessingSequence>(
				makePreProcessor<Sequentializer>(),
				getBasicPreProcessorSequence()
		);
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




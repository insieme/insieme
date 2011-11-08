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

#include "insieme/simple_backend/simple_backend.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_node.h"

#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/statement_converter.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/function_manager.h"
#include "insieme/simple_backend/job_manager.h"

#include "insieme/simple_backend/formatting/operator_formatting.h"

namespace insieme {
namespace simple_backend {

	SimpleBackendPtr SimpleBackend::getDefault() {
		return std::make_shared<SimpleBackend>();
	}

	backend::TargetCodePtr SimpleBackend::convert(const core::NodePtr& source) const {

		// create and set up the converter
		Converter converter;

		// Prepare managers
		core::NodeManager& nodeManager = source->getNodeManager();
		converter.setNodeManager(&nodeManager);

		StmtConverter stmtConverter(converter, formatting::getBasicFormatTable(nodeManager.basic));
		converter.setStmtConverter(&stmtConverter);

		NameManager nameManager;
		converter.setNameManager(&nameManager);

		TypeManager typeManager(converter);
		converter.setTypeManager(&typeManager);

		VariableManager variableManager;
		converter.setVariableManager(&variableManager);

		FunctionManager functionManager(converter);
		converter.setFunctionManager(&functionManager);

		JobManager jobManager(converter);
		converter.setJobManager(&jobManager);

		// conduct conversion
		return converter.convert(source);
	}


} // end namespace simple_backend
} // end namespace insieme


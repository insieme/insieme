/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#pragma once

#include <string>

#include "insieme/utils/compiler/compiler.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/backend/backend.h"


namespace insieme {
namespace driver {
namespace utils {

	//***************************************************************************************
	// 				 Output formatting helpers
	//***************************************************************************************
	void openBoxTitle(const std::string title);

	void closeBox();


	//***************************************************************************************
	// 				 STATS: show statistics about the IR
	//***************************************************************************************
	void showStatistics(const core::ProgramPtr& program);


	//****************************************************************************************
	//                BENCHMARK CORE: Perform some performance benchmarks
	//****************************************************************************************
	void benchmarkCore(const core::NodePtr& program);


	//***************************************************************************************
	// 					SEMA: Performs semantic checks on the IR
	//***************************************************************************************
	int checkSema(const core::NodePtr& program, core::checks::MessageList& list, core::checks::CheckPtr checks = core::checks::getFullCheck());


	//***************************************************************************************
	//									Backend selection
	//***************************************************************************************
	insieme::backend::BackendPtr getBackend(const std::string& backendString, const std::string& dumpOclKernel);

	//***************************************************************************************
	//									Compiler selection
	//***************************************************************************************
	insieme::utils::compiler::Compiler getCompiler(const std::string& backendString, const bool isCpp);

} // end namespace utils
} // end namespace driver
} // end namespace insieme

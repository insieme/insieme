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

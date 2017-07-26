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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/backend/runtime/runtime_backend.h"

namespace insieme {
namespace backend {
namespace runtime {

	using namespace core;

	TEST(JobSupport, Simple) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();
		auto& parallel = mgr.getLangExtension<core::lang::ParallelExtension>();

		// ------------------- Constructions ---------------------

		// create a code snippet consisting of a job with a local declaration

		// step 1: create the job
		ExpressionPtr range = builder.getThreadNumRange(1);
		VariablePtr localVar = builder.variable(basic.getInt4(), 1);
		ExpressionPtr jobBody = builder.wrapLazy(builder.add(localVar, localVar));
		JobExprPtr job = builder.jobExpr(basic.getJob().as<GenericTypePtr>(), range, jobBody);


		// step 2: execute job
		ExpressionPtr run = builder.callExpr(parallel.getMerge(), builder.callExpr(parallel.getParallel(), job));

		// step 3: create surrounding body
		StatementPtr body = builder.compoundStmt(builder.declarationStmt(localVar, builder.intLit(4)), run);

		// step 4: encapsulate it into a function
		NodePtr code = builder.program(toVector<ExpressionPtr>(builder.lambdaExpr(basic.getUnit(), VariableList(), body)));


		// -------------------- Testing ---------------------------

		EXPECT_TRUE(core::checks::check(code).empty()) << core::checks::check(code);

		// convert the code
		auto c_code = RuntimeBackend::getDefault()->convert(code);
		EXPECT_TRUE(!!c_code);

		// try to compile the code
		EXPECT_TRUE(utils::compiler::compile(*c_code, utils::compiler::Compiler::getRuntimeCompiler()));

		//	std::cout << printer::PrettyPrinter(code) << "\n";
		//	std::cout << *c_code << "\n";
	}


} // end namespace runtime
} // end namespace backend
} // end namespace insieme

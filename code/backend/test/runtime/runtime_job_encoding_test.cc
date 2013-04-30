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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/transform/manipulation.h"

#include "insieme/utils/compiler/compiler.h"

#include "insieme/backend/runtime/runtime_backend.h"

namespace insieme {
namespace backend {
namespace runtime {

using namespace core;

TEST(JobSupport, LocalDeclarations) {

	NodeManager mgr;
	IRBuilder builder(mgr);
	auto& basic = mgr.getLangBasic();

	// ------------------- Constructions ---------------------

	// create a code snippet consisting of a job with a local declaration

	// step 1: create the job
	ExpressionPtr range = builder.getThreadNumRange(1);
	VariablePtr localVar = builder.variable(basic.getInt4(), 1);
	VariablePtr captured = builder.variable(basic.getInt4(), 2);
	DeclarationStmtsPtr localDecls = builder.declarationStmts(toVector(builder.declarationStmt(localVar, builder.intLit(12))));
	GuardedExprsPtr guardedExpr = builder.guardedExprs(toVector<GuardedExprPtr>());
	ExpressionPtr jobBody = builder.wrapLazy(builder.add(localVar, captured));
	JobExprPtr job = builder.jobExpr(basic.getJob().as<GenericTypePtr>(), range, localDecls, guardedExpr, jobBody);


	// step 2: execute job
	ExpressionPtr run = builder.callExpr(basic.getMerge(), builder.callExpr(basic.getParallel(), job));

	// step 3: create surrounding body
	StatementPtr body = builder.compoundStmt(
			builder.declarationStmt(captured, builder.intLit(4)),
			run
	);

	// step 4: encapsulate it into a function
	NodePtr code = builder.program(toVector<ExpressionPtr>(builder.lambdaExpr(body)));


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


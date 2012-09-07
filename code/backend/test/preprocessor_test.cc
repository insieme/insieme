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

#include <fstream>

#include "insieme/utils/compiler/compiler.h"
#include "insieme/utils/test/test_config.h"

#include "insieme/backend/preprocessor.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/transform/node_replacer.h"

namespace insieme {
namespace backend {

TEST(Preprocessor, GlobalElimination) {
	core::NodeManager manager;
	core::parse::IRParser parser(manager);

	core::ProgramPtr program = parser.parseProgram("main: fun ()->int<4>:"
			"mainfct in { ()->int<4>:mainfct = ()->int<4>{ {"
			"   decl ref<struct<a:vector<int<4>,20>,f:()->unit>>:v1 = (op<ref.new>(struct{a:(op<undefined>(lit<type<vector<int<4>,20>>, bla>)),f:(op<undefined>(lit<type<()->unit>, bla>))}));"
			"	(op<composite.ref.elem>(v1,lit<identifier,a>,lit<type<vector<int<4>,20>>, bla>));"
			"	(op<composite.member.access>((op<ref.deref>(v1)),lit<identifier,a>,lit<type<vector<int<4>,20>>, bla>));"
			"	(fun(ref<struct<a:vector<int<4>,20>,f:()->unit>>:v2) -> int<4> {{"
			"		(op<composite.ref.elem>(v2,lit<identifier,a>,lit<type<vector<int<4>,20>>, bla>));"
			"		(op<composite.member.access>((op<ref.deref>(v2)),lit<identifier,a>,lit<type<vector<int<4>,20>>, bla>));"
			"		return 0;"
			"	}}(v1) );"
			"   {"
			"		(op<ref.assign>((op<composite.ref.elem>(v1,lit<identifier,a>,lit<type<vector<int<4>,20>>, bla>)), lit<vector<int<4>,20>, X>));"
			"		((op<ref.deref>((op<composite.ref.elem>(v1,lit<identifier,f>,lit<type<()->unit>, bla>))))());"
			"   };"
			"	return 0;"
			"} } }");


	EXPECT_TRUE(program);

	program = core::transform::fixTypesGen(manager, program, core::VariableMap(), false);

//	std::cout << "Input: " << core::printer::PrettyPrinter(program) << "\n";

	// check for semantic errors
	auto errors = core::checks::check(program);
	EXPECT_EQ(core::checks::MessageList(), errors);


	// run preprocessor
	auto res = makePreProcessor<RestoreGlobals>()->process(manager, program);

//	std::cout << "Processed: " << core::printer::PrettyPrinter(res) << "\n";

	errors = core::checks::check(res);
	EXPECT_EQ(core::checks::MessageList(), errors);

	EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(res)), "__GLOBAL__1;");
	EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(res)), "__GLOBAL__1 := X;");
}



} // namespace backend
} // namespace insieme

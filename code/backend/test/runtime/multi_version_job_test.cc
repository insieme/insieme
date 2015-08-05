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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/backend/runtime/runtime_preprocessor.h"
#include "insieme/backend/runtime/runtime_backend.h"
#include "insieme/backend/runtime/runtime_extensions.h"
#include "insieme/backend/runtime/runtime_entities.h"

#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace backend {
namespace runtime {

namespace p = insieme::core::pattern;
namespace irp = insieme::core::pattern::irp;

TEST(MultiVersionJob, Generation) {

	core::NodeManager manager;
	core::IRBuilder builder(manager);
	
	// create a multi-version job
	auto code = builder.parseProgram(R"(
			unit main() {
				decl ref<int<4>> a;
				spawn lambda ()=> { a = 3; };
				spawn pick([
					lambda ()=> { a = 3; },
					lambda ()=> { a = 3*2; },
					lambda ()=> { a = 1+2; },
					lambda ()=> { a = 1*3; }
				]);
				syncAll;
			}
		)").as<core::ProgramPtr>();

	ASSERT_TRUE(code);
	
	auto res = core::checks::check(code);
	EXPECT_TRUE(res.empty()) << res;
	
	// generate code using the runtime backend
	auto targetCode = RuntimeBackend::getDefault()->convert(code);
	
	// print results if interested
	// std::cout << core::printer::PrettyPrinter(code) << "\n";
	// std::cout << *targetCode << "\n";
	
	auto compiler = utils::compiler::Compiler::getRuntimeCompiler();
	compiler.addFlag("-Werror");
	
	EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
}

void setMultiversionImplHint(core::NodePtr code, backend::runtime::PickImplementationHint hint) {
	irp::matchAllPairs(irp::pick(), code, [&](const core::NodePtr& match, p::NodeMatch m) {
		match.as<core::CallExprPtr>().attachValue(hint);
	});
}

TEST(MultiVersion, ImplementationHints) {

	core::NodeManager manager;
	core::IRBuilder builder(manager);
	
	// create a multi-version job
	auto code = builder.parseProgram(R"(
			int<4> main() {
				decl ref<int<4>> a;
				pick([
					lambda (ref<int<4>> a) -> unit { a = 3; },
					lambda (ref<int<4>> a) -> unit { a = 1+2; },
					lambda (ref<int<4>> a) -> unit { a = 1*3; }
				])(a);
				return ref_deref(a);
		}
		)").as<core::ProgramPtr>();

	ASSERT_TRUE(code);
	
	auto res = core::checks::check(code);
	EXPECT_TRUE(res.empty()) << res;
	
	auto compiler = utils::compiler::Compiler::getRuntimeCompiler();
	compiler.addFlag("-Werror");
	
	/////////////////////////////////// Test backend::runtime::PickImplementationHint::CALL
	
	setMultiversionImplHint(code, backend::runtime::PickImplementationHint::CALL);
	//dumpDetailColored(code);
	
	auto targetCode = RuntimeBackend::getDefault()->convert(code);
	string targetCodeString = toString(*targetCode);
	//std::cout << "\n===============================================\n\n" << targetCodeString << "\n";
	
	EXPECT_TRUE(targetCodeString.find("switch") == string::npos);
	EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));
	
	/////////////////////////////////// Test backend::runtime::PickImplementationHint::SWITCH
	
	setMultiversionImplHint(code, backend::runtime::PickImplementationHint::SWITCH);
	//dumpDetailColored(code);
	
	auto targetCodeSwitch = RuntimeBackend::getDefault()->convert(code);
	string targetCodeSwitchString = toString(*targetCodeSwitch);
	//std::cout << "\n===============================================\n\n" << targetCodeSwitchString << "\n";
	
	EXPECT_TRUE(targetCodeSwitchString.find("switch") != string::npos);
	EXPECT_TRUE(utils::compiler::compile(*targetCodeSwitch, compiler));
}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme


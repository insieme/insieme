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
#include <insieme/backend/runtime/runtime_extension.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/backend/runtime/runtime_preprocessor.h"
#include "insieme/backend/runtime/runtime_backend.h"
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
				var ref<int<4>> a;
				spawn ()=> { a = 3; };
				spawn pick([
					()=> { a = 3; },
					()=> { a = 3*2; },
					()=> { a = 1+2; },
					()=> { a = 1*3; }
				]);
				sync_all;
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
	irp::matchAllPairs(irp::pick(), code, [&](const core::NodePtr& match, p::NodeMatch m) { match.as<core::CallExprPtr>().attachValue(hint); });
}

TEST(MultiVersion, ImplementationHints) {
	core::NodeManager manager;
	core::IRBuilder builder(manager);

	// create a multi-version job
	auto code = builder.parseProgram(R"(
			int<4> main() {
				var ref<int<4>> a;
				pick([
					(a : ref<int<4>>) -> unit { a = 3; },
					(a : ref<int<4>>) -> unit { a = 1+2; },
					(a : ref<int<4>>) -> unit { a = 1*3; }
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
	// dumpDetailColored(code);

	auto targetCode = RuntimeBackend::getDefault()->convert(code);
	string targetCodeString = toString(*targetCode);
	// std::cout << "\n===============================================\n\n" << targetCodeString << "\n";

	EXPECT_TRUE(targetCodeString.find("switch") == string::npos);
	EXPECT_TRUE(utils::compiler::compile(*targetCode, compiler));

	/////////////////////////////////// Test backend::runtime::PickImplementationHint::SWITCH

	setMultiversionImplHint(code, backend::runtime::PickImplementationHint::SWITCH);
	// dumpDetailColored(code);

	auto targetCodeSwitch = RuntimeBackend::getDefault()->convert(code);
	string targetCodeSwitchString = toString(*targetCodeSwitch);
	// std::cout << "\n===============================================\n\n" << targetCodeSwitchString << "\n";

	EXPECT_TRUE(targetCodeSwitchString.find("switch") != string::npos);
	EXPECT_TRUE(utils::compiler::compile(*targetCodeSwitch, compiler));
}

} // end namespace runtime
} // end namespace backend
} // end namespace insieme

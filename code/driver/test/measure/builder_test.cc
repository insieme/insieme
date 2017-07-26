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

#include <boost/filesystem/operations.hpp>

#include "insieme/core/ir_builder.h"

#include "insieme/backend/runtime/runtime_backend.h"

#include "insieme/driver/measure/builder.h"

#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace driver {
namespace measure {

namespace bfs = boost::filesystem;

TEST(Dummy, MustNotBeEmpty) {}

//	TEST(RemoteCompilation, Localhost) {
//		// create a test program fragment
//		core::NodeManager manager;
//		core::IRBuilder builder(manager);
//		core::StatementPtr stmt = core::parse::parseStatement(manager,"{"
//			"decl ref<int<4>>:sum = (op<ref.var>(0));"
//			"for(decl uint<4>:i = 10 .. 50 : 1) {"
//			"	(sum = ((op<ref.deref>(sum))+1));"
//			"};}");
//
//		core::ProgramPtr prog = builder.program(
//				toVector<core::ExpressionPtr>(builder.lambdaExpr(stmt, core::VariableList()))
//		);
//		EXPECT_TRUE(prog);
//
//		auto code = backend::runtime::RuntimeBackend::getDefault()->convert(prog);
//
//		// build on local host
//		Host host("localhost");
//
//		// build some test program remotely
//		auto bin = buildRemote(*code, host);
//		EXPECT_TRUE(bin);
//
//		ASSERT_EQ("localhost:/tmp/work_dir_0/binary", toString(*bin));
//
//		system("/tmp/work_dir_0/binary");
//
//		// delete binary
//		if (bin) utils::net::remove_all((*bin).parent_path());
//
//	}
//
//	TEST(RemoteCompilation, Remote) {
//		// create a test program fragment
//		core::NodeManager manager;
//		core::IRBuilder builder(manager);
//		core::StatementPtr stmt = core::parse::parseStatement(manager,"{"
//			"decl ref<int<4>>:sum = (op<ref.var>(0));"
//			"for(decl uint<4>:i = 10 .. 50 : 1) {"
//			"	(sum = ((op<ref.deref>(sum))+1));"
//			"};}");
//
//		core::ProgramPtr prog = builder.program(
//				toVector<core::ExpressionPtr>(builder.lambdaExpr(stmt, core::VariableList()))
//		);
//		EXPECT_TRUE(prog);
//
//		auto code = backend::runtime::RuntimeBackend::getDefault()->convert(prog);
//
//		Host host("wildspitze", "csaf7445", "/insieme-libs/papi-latest");
//
//		// build some test program remotely
//		auto bin = buildRemote(*code, host);
//		EXPECT_TRUE(bin);
//
//		ASSERT_EQ("csaf7445@wildspitze:/tmp/work_dir_0/binary", toString(*bin));
//
//		// delete binary
//		if (bin) utils::net::remove_all((*bin).parent_path());
//
//	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme

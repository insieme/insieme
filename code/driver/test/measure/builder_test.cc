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
//		Logger::setLevel(WARNING);
//
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
//		Logger::setLevel(WARNING);
//
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

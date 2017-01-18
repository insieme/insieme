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

#include <gtest/gtest.h>

#include <set>
#include <boost/filesystem.hpp>

#include "insieme/core/ir_visitor.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/annotations/c/include.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/extensions/interceptor_extension.h"

#include "insieme/frontend/utils/conversion_test_utils.h"

namespace insieme {
namespace frontend {

	using namespace core;

	TEST(HeaderTagging, C89Declarations) {
		NodeManager man;

		boost::filesystem::path tmpFile;
		{
			// create a temporary source file
			utils::Source file(
			    R"(
				#include <stdio.h>

				int main() {
					stdout;
				}
				)");

			// check whether there is a temporary file
			tmpFile = file.getPath();
			EXPECT_TRUE(boost::filesystem::exists(tmpFile));

			// parse temporary file
			core::NodeManager manager;
			ConversionJob job(file);
			auto code = job.execute(manager);
			ASSERT_TRUE(code);

			// check that the target type of the stdout pointer has the correct attachment
			bool checked = false;
			visitDepthFirst(NodeAddress(code), [&checked](const core::LiteralAddress& lit) {
				if(lit->getStringValue() == "stdout") {
					EXPECT_TRUE(core::lang::isReference(lit->getType()));
					auto ptrT = core::lang::ReferenceType(lit->getType()).getElementType();
					EXPECT_TRUE(core::lang::isPointer(ptrT));
					auto elemT = core::lang::PointerType(ptrT).getElementType();
					ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(elemT));
					EXPECT_EQ("stdio.h", insieme::annotations::c::getAttachedInclude(elemT));
					checked = true;
				}
			});
			EXPECT_TRUE(checked);
		}
	}

	TEST(HeaderTagging, Intercepted) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp");
		job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		auto code = job.execute(manager);
		ASSERT_TRUE(code);

		// check that the intercepted "S" type exists and has the correct attachment
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::GenericTypePtr& genType) {
				if(genType->getName()->getValue() == "IMP_ns_colon__colon_S") {
					ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(genType));
					EXPECT_EQ("interceptor_header.h", insieme::annotations::c::getAttachedInclude(genType));
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}

		// check that the intercepted "IMP_ns_colon__colon_simpleFunc" literal exists and has the correct attachment
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::LiteralPtr& lit) {
				if(lit->getStringValue() == "IMP_ns_colon__colon_simpleFunc") {
					ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(lit));
					EXPECT_EQ("interceptor_header.h", insieme::annotations::c::getAttachedInclude(lit));
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}
	}

	TEST(HeaderTagging, GetInterceptedLibHeader) {
		// we have to test with existing files here, since the function will also use boost::filesystem::canonical
		auto canonicalize = [](const boost::filesystem::path& path) { return boost::filesystem::canonical(path); };

		// matching a path which isn't intercepted
		{
			std::set<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			std::set<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_classes.h"));
			EXPECT_FALSE(res);
		}

		// intercepting the same path as included
		{
			std::set<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			std::set<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting the same path as included, multiple includes
		{
			std::set<boost::filesystem::path> libDirs { canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                            canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			std::set<boost::filesystem::path> interceptedDirs{canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting the same path as included, multiple intercepted paths
		{
			std::set<boost::filesystem::path> libDirs { canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                            canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			std::set<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                                   canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting a sub-path of the include path - we always want the path relative to the include dir returned
		{
			std::set<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs") };
			std::set<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor/interceptor_header.h", res->string());
		}
	}

} // namespace frontend
} // namespace insieme

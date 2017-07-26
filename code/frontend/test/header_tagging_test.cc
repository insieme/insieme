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
			std::vector<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			std::vector<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion/cpp_basic_classes.h"));
			EXPECT_FALSE(res);
		}

		// intercepting the same path as included
		{
			std::vector<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			std::vector<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting the same path as included, multiple includes
		{
			std::vector<boost::filesystem::path> libDirs { canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                               canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			std::vector<boost::filesystem::path> interceptedDirs{canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting the same path as included, multiple intercepted paths
		{
			std::vector<boost::filesystem::path> libDirs { canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                               canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			std::vector<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor"),
			                                                      canonicalize(FRONTEND_TEST_DIR + "/inputs/conversion") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor_header.h", res->string());
		}

		// intercepting a sub-path of the include path - we always want the path relative to the include dir returned
		{
			std::vector<boost::filesystem::path> libDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs") };
			std::vector<boost::filesystem::path> interceptedDirs{ canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor") };
			auto res = utils::detail::getInterceptedLibHeader(libDirs, interceptedDirs, canonicalize(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_header.h"));
			ASSERT_TRUE(res);
			EXPECT_EQ("interceptor/interceptor_header.h", res->string());
		}
	}

} // namespace frontend
} // namespace insieme

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

#include "../independent_test_utils.h"

#include "insieme/frontend/extensions/interceptor_extension.h"

#include "insieme/core/annotations/naming.h"

namespace insieme {
namespace frontend {

	TEST(InterceptorTest, TrueInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp");
		job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		auto irTu = job.toIRTranslationUnit(manager);
		auto funs = irTu.getFunctions();
		EXPECT_FALSE(any(funs, [](decltype(funs)::value_type val) { return val.first->getStringValue() == "IMP_simpleFunc"; })) << "Function not intercepted!";
		EXPECT_TRUE(irTu.getTypes().empty());

		// check the attached name of the intercepted struct for correctness
		auto code = job.execute(manager);
		ASSERT_TRUE(code);
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::GenericTypePtr& genType) {
				if(genType->getName()->getValue() == "IMP_ns_colon__colon_S") {
					ASSERT_TRUE(core::annotations::hasAttachedName(genType));
					EXPECT_EQ(core::annotations::getAttachedName(genType), "struct ns::S");
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}
	}

	TEST(InterceptorTest, TrueTemplateInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/template_interception.cpp");
		job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		auto irTu = job.toIRTranslationUnit(manager);
		auto funs = irTu.getFunctions();
		EXPECT_FALSE(any(funs, [](decltype(funs)::value_type val) { return val.first->getStringValue() == "IMP_templateFun_int_returns_int"; })) << "Function not intercepted!";
		EXPECT_FALSE(any(funs, [](decltype(funs)::value_type val) { return val.first->getStringValue() == "IMP_templateFun_double_returns_double"; })) << "Function not intercepted!";
		EXPECT_FALSE(any(funs, [](decltype(funs)::value_type val) { return val.first->getStringValue() == "IMP_templateFun_unsigned_long_long_returns_unsigned_long_long"; })) << "Function not intercepted!";
		EXPECT_TRUE(irTu.getTypes().empty());

		// check the attached name of the intercepted structs for correctness
		auto code = job.execute(manager);
		ASSERT_TRUE(code);
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::GenericTypePtr& genType) {
				if(genType->getName()->getValue() == "IMP_TemplateClass_int") {
					ASSERT_TRUE(core::annotations::hasAttachedName(genType));
					EXPECT_EQ("TemplateClass<int>", core::annotations::getAttachedName(genType));
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::GenericTypePtr& genType) {
				if(genType->getName()->getValue() == "IMP_TemplateClass_double") {
					ASSERT_TRUE(core::annotations::hasAttachedName(genType));
					EXPECT_EQ("TemplateClass<double>", core::annotations::getAttachedName(genType));
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}
		{
			bool checked = false;
			visitDepthFirstOnce(code, [&checked](const core::GenericTypePtr& genType) {
				if(genType->getName()->getValue() == "IMP_TemplateClass_bool") {
					ASSERT_TRUE(core::annotations::hasAttachedName(genType));
					EXPECT_EQ("TemplateClass<bool>", core::annotations::getAttachedName(genType));
					checked = true;
				}
			}, true);
			EXPECT_TRUE(checked);
		}
	}

	TEST(InterceptorTest, CustomPath) {
		runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp", [](ConversionJob& job) {
			job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, Templates) {
		runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/template_interception.cpp", [](ConversionJob& job) {
			job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, SystemInterception) {
		runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/system_interception.cpp", [](ConversionJob& job) {
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	//TEST(InterceptorTest, NoInterception) {
	//	runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/not_interceptor_test.cpp", [](ConversionJob& job) {
	//		job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
	//	});
	//}

} // fe namespace
} // insieme namespace

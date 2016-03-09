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

#include "insieme/annotations/c/include.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/naming.h"

namespace insieme {
namespace frontend {

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

	TEST(InterceptorTest, NoInterception) {
		runIndependentTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/no_interceptor_test.cpp", [](ConversionJob& job) {
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	void checkForFunction(const core::tu::IRTranslationUnit& irTu, const std::string& name) {
		auto funs = irTu.getFunctions();
		EXPECT_FALSE(any(funs, [&](decltype(funs)::value_type val) { return val.first->getStringValue() == name; })) << "Function " << name << " has not intercepted!";
	}

	void checkForTypeName(const NodePtr& code, const std::string& typeNodeIrSpec, const std::string& expectedAttachedName, const std::string& expectedHeaderName) {
		bool checked = false;
		IRBuilder builder(code->getNodeManager());
		auto targetType = builder.parseType(typeNodeIrSpec).as<core::GenericTypePtr>();
		visitDepthFirstOnce(code, [&](const core::GenericTypePtr& genType) {
			if(genType == targetType) {
				ASSERT_TRUE(core::annotations::hasAttachedName(genType));
				EXPECT_EQ(expectedAttachedName, core::annotations::getAttachedName(genType));
				ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(genType));
				EXPECT_EQ(expectedHeaderName, insieme::annotations::c::getAttachedInclude(genType));
				checked = true;
			}
		}, true);
		EXPECT_TRUE(checked);
	}

	void checkForFunctionName(const NodePtr& code, const std::string& functionLiteralName, const std::string& expectedAttachedName, const std::string& expectedHeaderName) {
		bool checked = false;
		visitDepthFirstOnce(code, [&](const core::LiteralPtr& lit) {
			if(lit->getValue()->getValue() == functionLiteralName) {
				ASSERT_TRUE(core::annotations::hasAttachedName(lit));
				EXPECT_EQ(expectedAttachedName, core::annotations::getAttachedName(lit));
				ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(lit));
				EXPECT_EQ(expectedHeaderName, insieme::annotations::c::getAttachedInclude(lit));
				checked = true;
			}
		}, true);
		EXPECT_TRUE(checked);
	}

	TEST(InterceptorTest, TrueInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp");
		job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		auto irTu = job.toIRTranslationUnit(manager);
		auto funs = irTu.getFunctions();

		//check that functions/methods have been intercepted
		checkForFunction(irTu, "IMP_simpleFunc");

		//no types should have been translated
		EXPECT_TRUE(irTu.getTypes().empty());

		// check the attached name of the intercepted struct for correctness
		auto code = job.execute(manager);
		checkForTypeName(code, "IMP_ns_colon__colon_S", "struct ns::S", "interceptor_header.h");

		// check name of function/method literals
		checkForFunctionName(code, "IMP_ns_colon__colon_simpleFunc", "ns::simpleFunc", "interceptor_header.h");
		checkForFunctionName(code, "IMP_ns_colon__colon_S::IMP_memberFunc", "memberFunc", "interceptor_header.h");
	}

	TEST(InterceptorTest, TrueTemplateInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/template_interception.cpp");
		job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		job.setStandard(ConversionSetup::Standard::Cxx11);
		auto irTu = job.toIRTranslationUnit(manager);

		//check that functions/methods have been intercepted
		checkForFunction(irTu, "IMP_templateFunRet");
		checkForFunction(irTu, "IMP_templateFunRetParam");
		checkForFunction(irTu, "IMP_templateFun");
		checkForFunction(irTu, "IMP_TemplateWithMethod");

		//no types should have been translated
		EXPECT_TRUE(irTu.getTypes().empty());

		// check the attached name of the intercepted structs for correctness
		auto code = job.execute(manager);
		ASSERT_TRUE(code);
		checkForTypeName(code, "IMP_ClassWithTemplateMethod", "ClassWithTemplateMethod", "template_interception.h");
		checkForTypeName(code, "IMP_TemplateClass<int<4>>", "TemplateClass", "template_interception.h");
		checkForTypeName(code, "IMP_TemplateClass<real<8>>", "TemplateClass", "template_interception.h");
		checkForTypeName(code, "IMP_TemplateClass<bool>", "TemplateClass", "template_interception.h");
//		checkForTypeName(code, "IMP_TemplateClass_TemplateClass_lt_int_gt_", "TemplateClass<TemplateClass<int> >", "template_interception.h");

		// check name of function/method literals
		checkForFunctionName(code, "IMP_templateFunRet", "templateFunRet", "template_interception.h");
		checkForFunctionName(code, "IMP_templateFunRetParam", "templateFunRetParam", "template_interception.h");
		checkForFunctionName(code, "IMP_templateFun", "templateFun", "template_interception.h");
		checkForFunctionName(code, "IMP_TemplateWithMethod::IMP_get", "get", "template_interception.h");
		checkForFunctionName(code, "IMP_ClassWithTemplateMethod::IMP_get", "get", "template_interception.h");
//		checkForFunctionName(code, "IMP_templateTemplateFun_TemplateClass_int_returns_void", "templateTemplateFun<TemplateClass,int>", "template_interception.h");
//		checkForFunctionName(code, "IMP_templateTemplateFun_TemplateClass_TemplateClass_lt_int_gt__returns_void", "templateTemplateFun<TemplateClass,TemplateClass<int> >", "template_interception.h");
//		checkForFunctionName(code, "IMP_variadicTemplateFun_int_returns_int", "variadicTemplateFun<int>", "template_interception.h");
//		checkForFunctionName(code, "IMP_variadicTemplateFun_int_pack_begin_int_pack_end_returns_int", "variadicTemplateFun<int,int>", "template_interception.h");
	}

	TEST(InterceptorTest, TrueSystemInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/system_interception.cpp");
		job.registerFrontendExtension<extensions::InterceptorExtension>();
		job.registerFrontendExtension<extensions::TestPragmaExtension>(); // necessary to parse pragmas
		job.setStandard(ConversionSetup::Standard::Cxx11);
		auto irTu = job.toIRTranslationUnit(manager);
		auto funs = irTu.getFunctions();

		//check that functions/methods have been intercepted
		checkForFunction(irTu, "IMP_gettimeofday");
		checkForFunction(irTu, "IMP_std_colon__colon_vector::IMP_push_back");
		checkForFunction(irTu, "IMP_std_colon__colon__operator_lshift_");
		checkForFunction(irTu, "IMP_std_colon__colon_basic_ostream::IMP__operator_lshift_");

		//only our own simple type IMP_Trivial should have been translated to a type
		EXPECT_EQ(1, irTu.getTypes().size());

		// check the attached name of the intercepted structs for correctness
		auto code = job.execute(manager);
		ASSERT_TRUE(code);
		checkForTypeName(code, "IMP_timeval", "struct timeval", "sys/time.h");
		checkForTypeName(code, "IMP_std_colon__colon_vector<int<4>,IMP_std_colon__colon_allocator<int<4>>>", "std::vector", "vector");
		checkForTypeName(code, "IMP_std_colon__colon_vector<real<8>,IMP_std_colon__colon_allocator<real<8>>>", "std::vector", "vector");
		checkForTypeName(code, "def struct IMP_Trivial {}; IMP_std_colon__colon_vector<IMP_Trivial,IMP_std_colon__colon_allocator<IMP_Trivial>>", "std::vector", "vector");

		// check name of function/method literals as well as globals
		checkForFunctionName(code, "IMP_gettimeofday", "gettimeofday", "sys/time.h");
		checkForFunctionName(code, "IMP_std_colon__colon_vector::IMP_push_back", "push_back", "vector");
		checkForFunctionName(code, "IMP_std_colon__colon__operator_lshift_", "std::operator<<", "iostream");
		checkForFunctionName(code, "IMP_std_colon__colon_cout", "std::cout", "iostream");
		checkForFunctionName(code, "IMP_std_colon__colon_basic_ostream::IMP__operator_lshift_", "operator<<", "iostream");
		checkForFunctionName(code, "IMP_std_colon__colon_launch_colon__colon_async", "std::launch::async", "future");
		checkForFunctionName(code, "IMP_std_colon__colon_launch_colon__colon_deferred", "std::launch::deferred", "future");
	}

} // fe namespace
} // insieme namespace

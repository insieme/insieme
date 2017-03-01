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
 *
 */
#include "insieme/frontend/utils/conversion_test_utils.h"

#include "insieme/frontend/extensions/interceptor_extension.h"

#include "insieme/annotations/c/include.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/tu/ir_translation_unit_io.h"

namespace insieme {
namespace frontend {

	TEST(InterceptorTest, CustomPath) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp", [](ConversionJob& job) {
			job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, Templates) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/template_interception.cpp", [](ConversionJob& job) {
			job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, QualifiedTemplates) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/qualified_template_interception.cpp", [](ConversionJob& job) {
			job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.addInterceptedHeaderDir(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, SystemInterception) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/system_interception.cpp", [](ConversionJob& job) {
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	TEST(InterceptorTest, NoInterception) {
		utils::runConversionTestOn(FRONTEND_TEST_DIR + "/inputs/interceptor/no_interceptor_test.cpp", [](ConversionJob& job) {
			job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
			job.registerFrontendExtension<extensions::InterceptorExtension, extensions::TestPragmaExtension>();
		});
	}

	void checkForFunction(const core::tu::IRTranslationUnit& irTu, const std::string& name) {
		auto funs = irTu.getFunctions();
		EXPECT_FALSE(any(funs, [&](decltype(funs)::value_type val) { return val.first->getStringValue() == name; })) << "Function " << name << " has not intercepted!";
	}

	void checkForTypeName(const core::NodePtr& code, const std::string& typeNodeIrSpec, const std::string& expectedAttachedName, const std::string& expectedHeaderName) {
		bool checked = false;
		core::IRBuilder builder(code->getNodeManager());
		auto targetType = builder.parseType(typeNodeIrSpec).as<core::GenericTypePtr>();
		core::visitDepthFirstOnce(code, [&](const core::GenericTypePtr& genType) {
			if(genType == targetType) {
				ASSERT_TRUE(core::annotations::hasAttachedName(genType));
				EXPECT_EQ(expectedAttachedName, core::annotations::getAttachedName(genType));
				ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(genType));
				EXPECT_EQ(expectedHeaderName, insieme::annotations::c::getAttachedInclude(genType));
				checked = true;
			}
		}, true);
		EXPECT_TRUE(checked) << "checkForTypeName: " << typeNodeIrSpec;
	}

	void checkLiteralNameInternal(const core::NodePtr& code, const std::string& functionLiteralName, const std::string& expectedAttachedName,
	                              const std::string& expectedHeaderName, bool fun, const core::FunctionKind expectedKind) {
		bool checked = false;
		core::visitDepthFirstOnce(code, [&](const core::LiteralPtr& lit) {
			if(lit->getValue()->getValue() == functionLiteralName) {
				ASSERT_TRUE(core::annotations::hasAttachedName(lit));
				EXPECT_EQ(expectedAttachedName, core::annotations::getAttachedName(lit));
				ASSERT_TRUE(insieme::annotations::c::hasIncludeAttached(lit));
				EXPECT_EQ(expectedHeaderName, insieme::annotations::c::getAttachedInclude(lit));
				if(fun) {
					auto funType = lit->getType().isa<core::FunctionTypePtr>();
					ASSERT_TRUE(funType);
					EXPECT_EQ(funType->getKind(), expectedKind) << "Wrong kind for " << functionLiteralName;
				}
				checked = true;
			}
		}, true);
		EXPECT_TRUE(checked) << "checkForFunctionName: " << functionLiteralName;
	}


	void checkForFunctionName(const core::NodePtr& code, const std::string& functionLiteralName, const std::string& expectedAttachedName,
	                          const std::string& expectedHeaderName, const core::FunctionKind expectedKind = core::FK_PLAIN) {
		checkLiteralNameInternal(code, functionLiteralName, expectedAttachedName, expectedHeaderName, true, expectedKind);
	}

	void checkForLiteralName(const core::NodePtr& code, const std::string& functionLiteralName, const std::string& expectedAttachedName,
	                         const std::string& expectedHeaderName) {
		checkLiteralNameInternal(code, functionLiteralName, expectedAttachedName, expectedHeaderName, false, core::FK_PLAIN);
	}

	TEST(InterceptorTest, TrueInterception) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/interceptor_test.cpp");
		job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
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
		checkForTypeName(code, "IMP_ns_colon__colon_S", "ns::S", "interceptor_header.h");

		// check name of function/method literals
		checkForFunctionName(code, "IMP_ns_colon__colon_simpleFunc", "ns::simpleFunc", "interceptor_header.h");
		checkForFunctionName(code, "IMP_ns_colon__colon_S::IMP_memberFunc", "memberFunc", "interceptor_header.h", core::FK_MEMBER_FUNCTION);

		// check that 31337 is not in the code (from global init which should be intercepted)
		ASSERT_FALSE(core::analysis::contains(core::tu::toIR(manager, irTu), builder.stringValue("31337")));
	}

	TEST(InterceptorTest, TrueTemplateInterception) {
		core::NodeManager manager;
		ConversionJob job(FRONTEND_TEST_DIR + "/inputs/interceptor/template_interception.cpp");
		job.addIncludeDirectory(FRONTEND_TEST_DIR + "/inputs/interceptor");
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
		checkForTypeName(code, "IMP_TemplateClass<ref<int<4>,f,f,qualified>>", "TemplateClass", "template_interception.h");
		checkForTypeName(code, "IMP_TemplateClass<ref<real<8>,f,f,qualified>>", "TemplateClass", "template_interception.h");
		checkForTypeName(code, "IMP_TemplateClass<ref<bool,f,f,qualified>>", "TemplateClass", "template_interception.h");
		// generic type as template template param
		checkForTypeName(code, "IMP_TemplateClass<ref<'T_0_0,'T_0_0_a,'T_0_0_b,'T_0_0_c>>", "TemplateClass", "template_interception.h");

		// check name of function/method literals
		checkForFunctionName(code, "IMP_templateFunRet", "templateFunRet", "template_interception.h");
		checkForFunctionName(code, "IMP_templateFunRetParam", "templateFunRetParam", "template_interception.h");
		checkForFunctionName(code, "IMP_templateFun", "templateFun", "template_interception.h");
		checkForFunctionName(code, "IMP_TemplateWithMethod::IMP_get", "get", "template_interception.h", core::FK_MEMBER_FUNCTION);
		checkForFunctionName(code, "IMP_ClassWithTemplateMethod::IMP_get", "get", "template_interception.h", core::FK_MEMBER_FUNCTION);
		checkForFunctionName(code, "IMP_templateTemplateFun", "templateTemplateFun", "template_interception.h");
		checkForFunctionName(code, "IMP_variadicTemplateFun", "variadicTemplateFun", "template_interception.h");
		checkForFunctionName(code, "IMP_variadicTemplateTemplateFun", "variadicTemplateTemplateFun", "template_interception.h");
	}

	TEST(InterceptorTest, TrueSystemInterception) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);
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

		// only our own simple types IMP_Trivial and IMP_OtherTrivial should have been translated to a type
		EXPECT_EQ(3, irTu.getTypes().size());

		// check the attached name of the intercepted structs for correctness
		auto code = job.execute(manager);
		ASSERT_TRUE(code);
		checkForTypeName(code, "IMP_timeval", "timeval", "sys/time.h");
		checkForTypeName(code, "IMP_std_colon__colon_vector<ref<int<4>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<int<4>,f,f,qualified>>,f,f,qualified>>", "std::vector", "vector");
		checkForTypeName(code, "IMP_std_colon__colon_vector<ref<real<8>,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<real<8>,f,f,qualified>>,f,f,qualified>>", "std::vector", "vector");
		checkForTypeName(code, "def struct IMP_Trivial {}; IMP_std_colon__colon_vector<ref<IMP_Trivial,f,f,qualified>,ref<IMP_std_colon__colon_allocator<ref<IMP_Trivial,f,f,qualified>>,f,f,qualified>>", "std::vector", "vector");

		// check name and kind of function/method literals
		checkForFunctionName(code, "IMP_gettimeofday", "gettimeofday", "sys/time.h");
		checkForFunctionName(code, "IMP_std_colon__colon_vector::IMP_push_back", "push_back", "vector", core::FK_MEMBER_FUNCTION);
		checkForFunctionName(code, "IMP_std_colon__colon__operator_lshift_", "std::operator<<", "iostream");
		checkForFunctionName(code, "IMP_std_colon__colon_basic_ostream::IMP__operator_lshift_", "operator<<", "iostream", core::FK_MEMBER_FUNCTION);
		checkForFunctionName(code, "IMP_std_colon__colon_vector::IMP_size", "size", "vector", core::FK_MEMBER_FUNCTION);

		// check name of globals
		checkForLiteralName(code, "IMP_std_colon__colon_launch_colon__colon_async", "std::launch::async", "future");
		checkForLiteralName(code, "IMP_std_colon__colon_launch_colon__colon_deferred", "std::launch::deferred", "future");
		checkForLiteralName(code, "IMP_std_colon__colon_cout", "std::cout", "iostream");

		// check for std::function operator() return type
		EXPECT_TRUE(core::analysis::contains(code, builder.typeVariable("__std_fun_ret_type")));
	}

} // fe namespace
} // insieme namespace

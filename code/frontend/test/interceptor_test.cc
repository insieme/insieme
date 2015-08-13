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

#include "insieme/frontend/clang.h"

#include <gtest/gtest.h>

#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/annotations/c/include.h"

#include "insieme/utils/logging.h"

#include "insieme/utils/config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/extensions/interceptor_extension.h"
#include "insieme/frontend/tu/ir_translation_unit.h"

#include "insieme/utils/test/test_utils.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "test_utils.inc"

using namespace insieme::core;
using namespace insieme::driver;
using namespace insieme::core::checks;
using namespace insieme::utils::log;
using namespace insieme::annotations::c;
namespace fe = insieme::frontend;

void checkSemanticErrors(const NodePtr& node) {
	auto msgList = check(node, checks::getFullCheck()).getAll();
	EXPECT_EQ(static_cast<unsigned int>(0), msgList.size());
	std::sort(msgList.begin(), msgList.end());
	std::for_each(msgList.begin(), msgList.end(), [&node](const Message& cur) {
		LOG(INFO) << *node;
		LOG(INFO) << cur << std::endl;
	});
}

std::string getPrettyPrinted(const NodePtr& node) {
	std::ostringstream ss;
	ss << insieme::core::printer::PrettyPrinter(node,
	                                            insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL | insieme::core::printer::PrettyPrinter::NO_LET_BINDINGS);

	// Remove new lines and leading spaces
	std::vector<char> res;
	std::string prettyPrint = ss.str();
	for(auto it = prettyPrint.begin(), end = prettyPrint.end(); it != end; ++it)
		if(!(*it == '\n' || (it + 1 != end && *it == ' ' && *(it + 1) == ' '))) { res.push_back(*it); }

	return std::string(res.begin(), res.end());
}

TEST(Interception, SimpleInterception) {
	fe::Source src(
	    R"(
			#include "interceptor_header.h"
			void intercept_simpleFunc() {
				int a = 0;
				ns::simpleFunc(1);
				ns::simpleFunc(a);
			}

			void intercept_memFunc1() {
				int a = 0;
				ns::S s;
				s.memberFunc(a);
			}
			void intercept_memFunc2() {
				using namespace ns;
				int a = 0;
				S s;
				s.memberFunc(a);
			}

			// only for manual compilation
			int main() {
				intercept_simpleFunc();
				intercept_memFunc1();
				intercept_memFunc2();
			};
		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::string interception = "--intercept=ns::.*";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, "--std=c++03", interception};
	cmd::Options options = cmd::Options::parse(argv);

	auto tu = options.job.toIRTranslationUnit(mgr);
	// LOG(INFO) << tu;

	auto retTy = builder.getLangBasic().getUnit();
	auto funcTy = builder.functionType(TypeList(), retTy);

	// intercept_simpleFunc
	auto sf = builder.literal("intercept_simpleFunc", funcTy);
	auto sf_expected = "fun() -> unit { decl ref<int<4>> v1 = ( var(0)); ns::simpleFunc(1); ns::simpleFunc(( *v1));}";

	auto res = analysis::normalize(tu[sf]);
	auto code = toString(getPrettyPrinted(res));
	EXPECT_EQ(sf_expected, code);

	// intercept_memFunc1
	auto mf1 = builder.literal("intercept_memFunc1", funcTy);
	auto mf1_expected = "fun() -> unit { decl ref<int<4>> v1 = ( var(0)); decl ref<ns::S> v2 = ns::S(( var(undefined(type<ns::S>)))); memberFunc(v2, ( *v1));}";

	res = analysis::normalize(tu[mf1]);
	code = toString(getPrettyPrinted(res));

	EXPECT_EQ(mf1_expected, code);
	// intercept_memFunc2
	auto mf2 = builder.literal("intercept_memFunc2", funcTy);
	auto mf2_expected = "fun() -> unit { decl ref<int<4>> v1 = ( var(0)); decl ref<ns::S> v2 = ns::S(( var(undefined(type<ns::S>)))); memberFunc(v2, ( *v1));}";

	res = analysis::normalize(tu[mf2]);
	code = toString(getPrettyPrinted(res));
	EXPECT_EQ(mf2_expected, code);
}

TEST(Interception, SimpleFunction1) {
	fe::Source src(
	    R"(
			namespace ns{
				void intercept_simpleFunc() {
					int a,b,c,d;
				}
			}

			// only for manual compilation
			int main() {
				ns::intercept_simpleFunc();
			};
		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, "--std=c++03"};
	cmd::Options option = cmd::Options::parse(argv);

	auto tu = option.job.toIRTranslationUnit(mgr);

	auto retTy = builder.getLangBasic().getUnit();
	auto funcTy = builder.functionType(TypeList(), retTy);


	// intercept_simpleFunc
	auto sf = builder.literal("ns_intercept_simpleFunc", funcTy);
	EXPECT_TRUE(tu[sf]);
	if(!tu[sf]) { std::cout << "the translation unit contains: \n" << tu; }
}

TEST(Interception, SimpleFunction2) {
	fe::Source src(
	    R"(
			namespace ns{
				void intercept_simpleFunc() {
					int a,b,c,d;
				}
			}

			// only for manual compilation
			int main() {
				ns::intercept_simpleFunc();
			};
		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::string interception = "--intercept=ns::.*";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, interception, "--std=c++03"};
	cmd::Options option = cmd::Options::parse(argv);

	auto tu = option.job.toIRTranslationUnit(mgr);
	// LOG(INFO) << tu;

	auto retTy = builder.getLangBasic().getUnit();
	auto funcTy = builder.functionType(TypeList(), retTy);

	// intercept_simpleFunc
	auto sf = builder.literal("intercept_simpleFunc", funcTy);
	EXPECT_FALSE(tu[sf]);
}


/////////////////////////////////////////////////////////////////////
//   This check just assures that everithing is in there, so
//   the interceped one can be thrusted

TEST(Interception, Types) {
	fe::Source src(
	    R"(
			namespace ns{

				enum SomeEnum { One =15, Two =2 };

				struct SomeStruct {
					int a,b,c;
				};

				class SomeClass {
					int b,c,d;
				public:
					int sum() { return b+c+d; }
				};

				union SomeUnion{
					int a;
					SomeClass obj;
				};

				typedef int BuiltinAllias;
			}

		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, "--std=c++03"};
	cmd::Options option = cmd::Options::parse(argv);

	auto tu = option.job.toIRTranslationUnit(mgr);

	{
		auto t = builder.genericType("ns_SomeStruct");
		EXPECT_TRUE(tu[t]);
		if(!tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto t = builder.genericType("ns_SomeUnion");
		EXPECT_TRUE(tu[t]);
		if(!tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto t = builder.genericType("ns_SomeClass");
		EXPECT_TRUE(tu[t]);
		if(!tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto owner = builder.genericType("ns_SomeClass");
		TypeList list;
		list.push_back(builder.refType(owner));
		auto retTy = builder.getLangBasic().getInt4();
		auto funcTy = builder.functionType(list, retTy, FK_MEMBER_FUNCTION);
		auto func = builder.literal("ns_SomeClass_sum", funcTy);

		EXPECT_TRUE(tu[func]);
		if(!tu[func]) { std::cout << "the translation unit contains: \n" << tu; }
	}
}

TEST(Interception, TypesIntercepted) {
	fe::Source src(
	    R"(
			namespace ns{

				enum SomeEnum { One =15, Two =2 };

				struct SomeStruct {
					int a,b,c;
				};

				class SomeClass {
					int b,c,d;
				public:
					int sum() { return b+c+d; }
				};

				union SomeUnion{
					int a;
					SomeClass obj;
				};

				typedef int BuiltinAllias;
			}


			void function(){
				ns::SomeEnum e = ns::One;

				ns::SomeStruct a;
				ns::SomeClass b;
				ns::SomeUnion c;
			}

		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::string interception = "--intercept=ns::.*";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, interception, "--std=c++03"};
	cmd::Options option = cmd::Options::parse(argv);

	auto tu = option.job.toIRTranslationUnit(mgr);

	{
		auto t = builder.genericType("ns_SomeStruct");
		EXPECT_FALSE(tu[t]);
		if(tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto t = builder.genericType("ns_SomeUnion");
		EXPECT_FALSE(tu[t]);
		if(tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto t = builder.genericType("ns_SomeClass");
		EXPECT_FALSE(tu[t]);
		if(tu[t]) { std::cout << "the translation unit contains: \n" << tu; }
	}
	{
		auto owner = builder.genericType("ns_SomeClass");
		TypeList list;
		list.push_back(builder.refType(owner));
		auto retTy = builder.getLangBasic().getInt4();
		auto funcTy = builder.functionType(list, retTy, FK_MEMBER_FUNCTION);
		auto func = builder.literal("ns_SomeClass_sum", funcTy);

		EXPECT_FALSE(tu[func]);
		if(tu[func]) { std::cout << "the translation unit contains: \n" << tu; }
	}

	// ok, now check who do the ussages look like:
	auto retTy = builder.getLangBasic().getUnit();
	auto funcTy = builder.functionType(TypeList(), retTy);

	// intercept_simpleFunc
	auto func = tu[builder.literal("function", funcTy)];
	EXPECT_TRUE(func);

	auto IRcode = getPrettyPrinted(func);
	EXPECT_TRUE(IRcode.find("ref<__insieme_enum<ns::SomeEnum>>") != std::string::npos);
	EXPECT_TRUE(IRcode.find("ns::One") != std::string::npos);
	EXPECT_TRUE(IRcode.find("ref<ns::SomeStruct>") != std::string::npos);
	EXPECT_TRUE(IRcode.find("ref<ns::SomeClass>") != std::string::npos);
	EXPECT_TRUE(IRcode.find("ref<ns::SomeUnion>") != std::string::npos);
}

//		contain the appropiate header????
TEST(Interception, AttachedHeader) {
	fe::Source src(
	    R"(
			#include "interceptor_header.h"

			// only for manual compilation
			void func() {
				ns::S obj;
			};
		)",
	    fe::CPP);

	NodeManager mgr;
	IRBuilder builder(mgr);
	const boost::filesystem::path& fileName = src;
	std::string include = "-I" FRONTEND_TEST_DIR "inputs/interceptor/";
	std::string interception = "--intercept=ns::.*";
	std::vector<std::string> argv = {"compiler", fileName.string(), include, interception, "--std=c++03"};
	cmd::Options option = cmd::Options::parse(argv);

	auto tu = option.job.toIRTranslationUnit(mgr);
	// LOG(INFO) << tu;

	auto retTy = builder.getLangBasic().getUnit();
	auto funcTy = builder.functionType(TypeList(), retTy);

	// intercept_simpleFunc
	auto symb = builder.literal("func", funcTy);

	auto node = tu[symb];
	auto checkDecl = [&](const NodePtr& node) {
		if(node.isa<LambdaExprPtr>()) { return true; }
		if(auto var = node.isa<VariablePtr>()) {
			auto inTy = var->getType().as<RefTypePtr>()->getElementType();
			EXPECT_TRUE(hasIncludeAttached(inTy)) << "var: " << inTy << " has no header annotation when used in var: " << var << std::endl;
		}
		return false;
	};

	visitDepthFirstPrunable(node.as<LambdaExprPtr>()->getBody(), checkDecl);

	std::cout << tu << std::endl;
}

// TODO:
//    initialization
//    templates
//
//    casts?

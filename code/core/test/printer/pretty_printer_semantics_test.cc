#include <gtest/gtest.h>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace parser {

	namespace {
		void runTest(const std::string& code) {
			NodeManager mgr;
			IRBuilder builder(mgr);

			auto parsed = builder.parseStmt(code);
			auto printed = toString(dumpPretty(parsed));
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberBasic) {
		runTest(R"(
			def GenericTypeWithFreeMember :: function free = () -> unit {
			};
			{
				var ref<GenericTypeWithFreeMember,f,f,plain> v0;
				GenericTypeWithFreeMember::free(v0);
			}
		)");
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberThis) {
		runTest(R"(
			def GenericTypeWithFreeMember :: function free = () -> unit {
				this;
			};
			{
				var ref<GenericTypeWithFreeMember,f,f,plain> v0;
				GenericTypeWithFreeMember::free(v0);
			}
		)");
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberConst) {
		runTest(R"(
			def GenericTypeWithFreeMember :: const function free = () -> unit {
				this;
			};
			{
				var ref<GenericTypeWithFreeMember,f,f,plain> v0;
				GenericTypeWithFreeMember::free(v0);
			}
		)");
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberNoCall) {
		runTest(R"(
			def GenericTypeWithFreeMember :: volatile function free = () -> unit {
			};
			{
				(GenericTypeWithFreeMember::free);
			}
		)");
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberOfGenTypeWithTypeParams) {
		runTest(R"(
			def GenericTypeWithFreeMember<TypeParam1> :: const function free = () -> unit {
				this;
			};
			{
				var ref<GenericTypeWithFreeMember<TypeParam1>,f,f,plain> v0;
				v0.free();
			}
		)");
	}

	TEST(PrettyPrinterSemanticsTest, FreeMemberOfGenTypeWithTypeParamsNoCall) {
		runTest(R"(
			def GenericTypeWithFreeMember<TypeParam1> :: const function free = () -> unit {
				this;
			};
			{
				lambda_name GenericTypeWithFreeMember<TypeParam1>::free;
			}
		)");
	}

} // parser
} // core
} // insieme
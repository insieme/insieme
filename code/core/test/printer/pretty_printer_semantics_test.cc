#include <gtest/gtest.h>

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace parser {

	TEST(PrettyPrinterSemanticsTest, FreeMembers) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		// most basic free member
		{
			auto freeMember = R"(
					def GenericTypeWithFreeMember :: function free = () -> unit {
					};
					{
						var ref<GenericTypeWithFreeMember,f,f,plain> v0;
						GenericTypeWithFreeMember::free(v0);
					}
				)";
			auto parsed = builder.parseStmt(freeMember);
			auto printed = toString(dumpPretty(parsed));
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}

		// free member with "this" usage
		{
			auto freeMember = R"(
					def GenericTypeWithFreeMember :: function free = () -> unit {
						this;
					};
					{
						var ref<GenericTypeWithFreeMember,f,f,plain> v0;
						GenericTypeWithFreeMember::free(v0);
					}
				)";
			auto parsed = builder.parseStmt(freeMember);
			auto printed = toString(dumpPretty(parsed));
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}

		// const free member with "this" usage
		{
			auto freeMember = R"(
					def GenericTypeWithFreeMember :: const function free = () -> unit {
						this;
					};
					{
						var ref<GenericTypeWithFreeMember,f,f,plain> v0;
						GenericTypeWithFreeMember::free(v0);
					}
				)";
			auto parsed = builder.parseStmt(freeMember);
			auto printed = toString(dumpPretty(parsed));
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}

		// reference to free member with no call
		{
			auto freeMember = R"(
					def GenericTypeWithFreeMember :: volatile function free = () -> unit {
					};
					{
						(GenericTypeWithFreeMember::free);
					}
				)";
			auto parsed = builder.parseStmt(freeMember);
			auto printed = toString(dumpPretty(parsed));
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}

		// free member of a generic type with type parameters
		{
			auto freeMember = R"(
					def GenericTypeWithFreeMember<TypeParam1> :: const function free = () -> unit {
						this;
					};
					{
						var ref<GenericTypeWithFreeMember<TypeParam1>,f,f,plain> v0;
						v0.free();
					}
				)";
			auto parsed = builder.parseStmt(freeMember);
			auto printed = toString(dumpPretty(parsed));
			std::cout << "printed: " << printed;
			auto reparsed = builder.parseStmt(printed);

			EXPECT_EQ(core::analysis::normalize(parsed), core::analysis::normalize(reparsed));
		}
	}

} // parser
} // core
} // insieme
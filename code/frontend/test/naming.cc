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

#include "insieme/frontend/frontend.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/frontend/tu/ir_translation_unit_check.h"

#include "insieme/core/annotations/naming.h"

#include "insieme/utils/test/test_utils.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "test_utils.inc"

using namespace insieme::driver;

namespace insieme {
namespace frontend {

#define RUN_SEMA(irCode) \
		auto msg = insieme::core::checks::check(irCode);\
		if(!msg.empty()) {\
			for(auto m: msg.getErrors()) {\
				std::cout << m.getMessage() << "\n\t" << m.getLocation() << " code: " << m.getErrorCode() << std::endl;\
			}\
		}\
		EXPECT_TRUE(msg.empty());

	TEST(AnonymousTypes, StructNaming) {

		Source src(
				R"(

					struct name{
						int a;
					};

					typedef struct oldname{
						int a;
					} renamed;

					typedef struct {
						int a;
					} anon_renamed;

					int main() {
						struct name A;
						renamed B;
						anon_renamed C;
					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		// parse temporary file
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string() };
        cmd::Options options = cmd::Options::parse(argv);

		core::ProgramPtr res = builder.normalize(options.job.execute(mgr));
		RUN_SEMA(res);

		dumpPretty(res);

		core::CompoundStmtPtr body = (*res)[0].as<core::LambdaExprPtr>()->getBody();

		EXPECT_TRUE(body[0].isa<core::DeclarationStmtPtr>());
		EXPECT_TRUE(body[1].isa<core::DeclarationStmtPtr>());
		EXPECT_TRUE(body[2].isa<core::DeclarationStmtPtr>());

		{
			core::TypePtr type = body[0].as<core::DeclarationStmtPtr>()->getVariable()->getType();
			type = type.as<core::RefTypePtr>()->getElementType();
			EXPECT_EQ (toString(type), "AP(struct name <a:int<4>>)");
			EXPECT_TRUE(core::annotations::hasNameAttached(type));
			EXPECT_EQ (core::annotations::getAttachedName(type), "name");
		}
		{
			core::TypePtr type = body[1].as<core::DeclarationStmtPtr>()->getVariable()->getType();
			type = type.as<core::RefTypePtr>()->getElementType();
			EXPECT_EQ (toString(type), "AP(struct oldname <a:int<4>>)");
			EXPECT_TRUE(core::annotations::hasNameAttached(type));
			EXPECT_EQ (core::annotations::getAttachedName(type), "oldname");
		}
		{
			core::TypePtr type = body[2].as<core::DeclarationStmtPtr>()->getVariable()->getType();
			type = type.as<core::RefTypePtr>()->getElementType();
			EXPECT_EQ (toString(type), "AP(struct anon_renamed <a:int<4>>)");
			EXPECT_TRUE(core::annotations::hasNameAttached(type));
			EXPECT_EQ (core::annotations::getAttachedName(type), "anon_renamed");
		}

	}
} // end namespace frontend
} // end namespace insieme

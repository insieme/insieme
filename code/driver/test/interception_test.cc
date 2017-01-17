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

#include "insieme/backend/sequential/sequential_backend.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/frontend.h"
#include "insieme/utils/config.h"
#include "insieme/utils/compiler/compiler.h"


namespace insieme {
namespace driver {

	namespace {
		string testCompilation(const string& filename) {
			core::NodeManager manager;

			frontend::ConversionJob job(utils::getInsiemeSourceRootDir() + filename);
			job.addIncludeDirectory(utils::getInsiemeSourceRootDir() + "frontend/test/inputs/interceptor/");
			job.addInterceptedHeaderDir(utils::getInsiemeSourceRootDir() + "frontend/test/inputs/interceptor/");
			job.registerDefaultExtensions();
			job.setStandard(frontend::ConversionSetup::Standard::Cxx11);
			core::ProgramPtr program = job.execute(manager);

			//dumpColor(program);

			//std::cout << "Converting IR to C...\n";
			auto converted = backend::sequential::SequentialBackend::getDefault()->convert(program);
			//std::cout << "Printing converted code:\n" << *converted;

			// check C code for absence of any pointers/derefs
			auto codeString = toString(*converted);
			codeString = insieme::utils::removeCppStyleComments(codeString);

			// try compiling the code fragment
			utils::compiler::Compiler compiler = utils::compiler::Compiler::getDefaultCppCompiler();
			compiler.addFlag("-c"); // do not run the linker
			compiler.addIncludeDir(utils::getInsiemeSourceRootDir() + "frontend/test/inputs/interceptor/");
			EXPECT_TRUE(utils::compiler::compile(*converted, compiler)) << "in " << filename;

			return codeString;
		}
	}

	TEST(DriverInterceptionTest, Basic) {
		auto codeString = testCompilation("frontend/test/inputs/interceptor/interceptor_test.cpp");
		EXPECT_PRED2(notContainsSubString, codeString, "*");
	}

	TEST(DriverInterceptionTest, Templates) {
		auto codeString = testCompilation("frontend/test/inputs/interceptor/template_interception.cpp");
		EXPECT_PRED2(notContainsSubString, codeString, "*");
		EXPECT_PRED2(containsSubString, codeString, "templateTemplateFun<TemplateClass,");
	}

	TEST(DriverInterceptionTest, QualifiedTemplates) {
		auto codeString = testCompilation("frontend/test/inputs/interceptor/qualified_template_interception.cpp");
		EXPECT_PRED2(notContainsSubString, codeString, "*");
		EXPECT_PRED2(containsSubString, codeString, "trivialTemplateFun<int32_t&");
		EXPECT_PRED2(containsSubString, codeString, "trivialTemplateFun<const int32_t");
		EXPECT_PRED2(containsSubString, codeString, "trivialTemplateFun<int32_t volatile&&");
	}

	TEST(DriverInterceptionTest, System) {
		auto codeString = testCompilation("frontend/test/inputs/interceptor/system_interception.cpp");
	}

} // end namespace driver
} // end namespace insieme

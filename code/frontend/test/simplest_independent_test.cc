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

#include <string>
#include <fstream>

#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/utils/config.h"

namespace insieme {
namespace frontend {
	using namespace extensions;
	using namespace core;
	using insieme::annotations::ExpectedIRAnnotation;

	void runTestOn(const string& fn) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
		ConversionJob job(fn);
		job.registerFrontendExtension<TestPragmaExtension>([](conversion::Converter& converter, int num) {
			EXPECT_EQ(num, converter.getVarMan()->numVisibleDeclarations()) << "Location: " << converter.getLastTrackableLocation() << "\n";
		});
				
		auto res = builder.normalize(job.execute(mgr));

		// iterate over res and check pragma expectations
		size_t visited = 0;
		visitDepthFirstOnce(NodeAddress(res), [&](const NodeAddress& addr) {
			auto node = addr.getAddressedNode();
			if(node->hasAnnotation(ExpectedIRAnnotation::KEY)) {
				auto ann = node->getAnnotation(ExpectedIRAnnotation::KEY);
				auto ex = ann->getExpected();
				boost::replace_all(ex, R"(\")", R"(")");
				NodePtr expected;
				if(node.isa<ExpressionPtr>()) {
					expected = builder.parseExpr(ex);
				} else {
					expected = builder.parseStmt(ex);
				}
				EXPECT_EQ(builder.normalize(expected), builder.normalize(node)) 
					<< "Location: " << *core::annotations::getLocation(addr) << "\n"
					<< "Actual Pretty: " << dumpColor(builder.normalize(node), std::cout, true) << "\n";
				//if(builder.normalize(expected) != builder.normalize(node)) {
				//	dumpText(builder.normalize(expected));
				//	std::cout << "MUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHAMUAHAHA\n";
				//	dumpText(builder.normalize(node));
				//}
				visited++;
			}
		});

		std::ifstream tf(fn);
		std::string fileText((std::istreambuf_iterator<char>(tf)), std::istreambuf_iterator<char>());
		std::string searchString{"#pragma test expect_ir"};
		string::size_type start = 0;
		unsigned occurrences = 0;
		while((start = fileText.find(searchString, start)) != string::npos) { 
			++occurrences; 
			start += searchString.length();
		}
		EXPECT_EQ(visited, occurrences);

		dumpColor(res);
	}
	
	TEST(IndependentTest, BasicTypes) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_basic_types.c");
	}

	TEST(IndependentTest, Globals) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_globals.c");
	}

	TEST(IndependentTest, Statements) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_statements.c");
	}

	TEST(IndependentTest, VariableScopes) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_variable_scopes.c");
	}
	
	TEST(IndependentTest, FunCalls) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_fun_calls.c");
	}

	TEST(IndependentTest, Expressions) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_expressions.c");
	}

	TEST(IndependentTest, HelloWorld) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_hello_world.c");
	}

	TEST(IndependentTest, Casts) {
		runTestOn(FRONTEND_TEST_DIR "/inputs/conversion/c_casts.c");
	}

} // fe namespace
} // insieme namespace

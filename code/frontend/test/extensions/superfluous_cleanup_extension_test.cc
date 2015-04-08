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

#include <gtest/gtest.h>

#include "insieme/frontend/extensions/superfluous_cleanup_extension.h"

#include "insieme/core/parser2/ir_parser.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/frontend/frontend.h"
#include "../test_utils.inc"

#include "insieme/driver/cmd/insiemecc_options.h"

namespace insieme {
namespace frontend {

using namespace core;
using namespace driver;

TEST(SuperfluousCleanup, Simple) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					ref<int<4>> i = ref.var(0);
					ref<int<4>> j = ref.var(4);
					for(int<4> v = 0 .. 100) {
						j = j + 6;
						i = i + 3;
					}
					while (j!=0) {
						j = j - 2;
					}
					return j;
				}
				)"
			);

	ASSERT_TRUE(program);

	auto lambdaExp = program->getEntryPoints()[0].as<LambdaExprPtr>();
	auto cleaned = extensions::cleanup::removeObviouslySuperfluousCode(lambdaExp);
	//dumpPretty(lambdaExp);
	//dumpPretty(cleaned);

	// check if superfluous decl got removed
	int declCount = 0;
	visitDepthFirstOnce(cleaned, [&](const DeclarationStmtPtr& d) { declCount++;});
	EXPECT_EQ(declCount, 3);
}

TEST(SuperfluousCleanup, EnclosingLoops) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
		R"(
		int<4> main() {
			ref<int<4>> i = ref.var(0);
			ref<int<4>> j = ref.var(4);
			while (i<6) {
				for(int<4> v = 0 .. 100) {
					j = j + 6;
					i = i + 3;
				}
			}
			return j;
		}
		)"
	);

	ASSERT_TRUE(program);

	auto lambdaExp = program->getEntryPoints()[0].as<LambdaExprPtr>();
	auto cleaned = extensions::cleanup::removeObviouslySuperfluousCode(lambdaExp);
	//dumpPretty(lambdaExp);
	//dumpPretty(cleaned);

	// check if we didn't remove anything we shouldn't have
	EXPECT_EQ(*lambdaExp, *cleaned);
}

TEST(SuperfluousCleanup, MMul) {
	NodeManager man;
	IRBuilder builder(man);

	fs::path tmpFile;
	{

// create a temporary source file
Source file(
R"(
#include <stdio.h>
#define N 1000
#define M N
#define K N

#define MIN(X,Y) ((X)<(Y)?(X):(Y))
#define MAX(X,Y) ((X)>(Y)?(X):(Y))

#define VALUE double

// create the matices
VALUE A[N][M];
VALUE B[M][K];
VALUE C[N][K];

int main() {
	int i,j,k;
	// A contains real values
	for (i=0; i<N; i++) {
		for (j=0; j<M; j++) {
			A[i][j] = i*j;
		}
	}

	// B is the identity matrix
	for (i=0; i<M; i++) {
		for (j=0; j<K; j++) {
			B[i][j] = (i==j)?1:0;
		}
	}

	// conduct multiplication
	for (i=0; i<N; i++) {
		for (j=0; j<K; j++) {
			// to be handleable by the polyhedral model
			for (k=0; k<M; k++) {
				C[i][j] += A[i][k] * B[k][j];
			}
		}
	}
}
)"
);

		// check whether there is a temporary file
		tmpFile = file.getPath();
		EXPECT_TRUE(fs::exists(tmpFile));

		// parse temporary file
		core::NodeManager manager;
        const boost::filesystem::path& fileName = file;
        std::vector<std::string> argv = { "compiler",  fileName.string() };
        cmd::Options options = cmd::Options::parse(argv);

		auto code = options.job.execute(manager);
		EXPECT_TRUE(code);

		auto lambdaExp = code->getEntryPoints()[0].as<LambdaExprPtr>();
		auto cleaned = extensions::cleanup::removeObviouslySuperfluousCode(lambdaExp);
		//dumpPretty(lambdaExp);
		//dumpPretty(cleaned);

		// check if all superfluous "if"s got removed
		int ifCount = 0;
		visitDepthFirstOnce(cleaned, [&](const IfStmtPtr& f) { ifCount++;});
		EXPECT_EQ(ifCount, 1);
		// check if we still got all our "for"s
		int forCount = 0;
		visitDepthFirstOnce(cleaned, [&](const ForStmtPtr& f) { forCount++;});
		EXPECT_EQ(forCount, 7);
	}
}

} // namespace frontend
} // namespace insieme

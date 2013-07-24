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

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/clang_config.h"

#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/insieme.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"

using namespace insieme::frontend;
using namespace insieme::frontend::pragma;
using namespace insieme::core;
namespace fe = insieme::frontend;

TEST(Cilk, Pragmas) {

	NodeManager manager;
	IRBuilder builder(manager);

	ConversionSetup setup;
	setup.setOption(ConversionSetup::Cilk);
	insieme::frontend::Program prog(manager, SRC_DIR "/inputs/hello.cilk", setup);

	const auto& pl = prog.getPragmaList();
	const ClangCompiler& comp = prog.getCompiler();


	// check number of annotations
	EXPECT_EQ((size_t) 4, pl.size());

	// check version before cilk-sema is applied
//	auto raw = prog.convert();
//	dump(raw);
//	dumpDetail(raw);

	{
		auto p = pl[0];

		// check pragma start location
		EXPECT_EQ((size_t)7, utils::Line(p->getStartLocation(), comp.getSourceManager()));

		EXPECT_EQ(p->getType(), "cilk::spawn");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		auto node = p->getStatement();

		// check stmt start location
		EXPECT_EQ((size_t)7, utils::Line(node->getLocStart(), comp.getSourceManager()));
	}

	{
		auto p = pl[1];

		// check pragma start location
		EXPECT_EQ((size_t)9, utils::Line(p->getStartLocation(), comp.getSourceManager()));

		EXPECT_EQ(p->getType(), "cilk::spawn");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		auto node = p->getStatement();

		// check stmt start location
		EXPECT_EQ((size_t)9, utils::Line(node->getLocStart(), comp.getSourceManager()));
	}

	{
		auto p = pl[2];

		// check pragma start location
		EXPECT_EQ((size_t)11, utils::Line(p->getStartLocation(), comp.getSourceManager()));

		EXPECT_EQ(p->getType(), "cilk::spawn");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		auto node = p->getStatement();

		// check stmt start location
		EXPECT_EQ((size_t)11, utils::Line(node->getLocStart(), comp.getSourceManager()));
	}

	{
		auto p = pl[3];

		// check pragma start location
		EXPECT_EQ((size_t)13, utils::Line(p->getStartLocation(), comp.getSourceManager()));

		EXPECT_EQ(p->getType(), "cilk::sync");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		auto node = p->getStatement();

		// check stmt start location
		EXPECT_EQ((size_t)14, utils::Line(node->getLocStart(), comp.getSourceManager()));

	}

}


TEST(Cilk, Sema) {

	NodeManager manager;
	IRBuilder builder(manager);

	ConversionJob job(SRC_DIR "/inputs/hello.cilk");
	job.setOption(ConversionJob::Cilk);


	// check proper encoding of cilk primitives
	auto code = builder.normalize(job.execute(manager));
	dump(code);

	auto str = toString(printer::PrettyPrinter(code));

	EXPECT_PRED2(containsSubString, str, "v5 := v0(v4-1);");
	EXPECT_PRED2(containsSubString, str, "v8 := v0(v7-2);");
	EXPECT_PRED2(containsSubString, str, "v0(v10-3);");

	EXPECT_PRED2(containsSubString, str, "decl ref<int<4>> v2 =  var(undefined(type<int<4>>));");

	EXPECT_PRED2(containsSubString, str, "default: bind(){fun000(v1, v2)}");
	EXPECT_PRED2(containsSubString, str, "default: bind(){fun001(v1, v6)}");
	EXPECT_PRED2(containsSubString, str, "default: bind(){fun002(v1)}");

	EXPECT_PRED2(containsSubString, str, "mergeAll()");
	EXPECT_PRED2(containsSubString, str, "return v2+v6");
}

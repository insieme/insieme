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

#include "programs.h"

#include "clang_compiler.h"
#include "pragma_handler.h"
#include "utils/source_locations.h"
#include "clang_config.h"

#include "omp/omp_pragma.h"
#include <iostream>
#include <clang/AST/Stmt.h>

using namespace insieme::frontend;
using namespace insieme::core;

#define CHECK_LOCATION(loc, srcMgr, line, col) \
	EXPECT_EQ(util::Line(loc, srcMgr), (size_t)line); \
	EXPECT_EQ(util::Column(loc, srcMgr), (size_t)col);

TEST(PragmaMatcherTest, HandleOmpParallel) {

	ProgramPtr program = Program::create();
	InsiemeTransUnitPtr TU = InsiemeTransUnit::ParseFile(std::string(SRC_DIR) + "/omp_parallel.c", *program);
	const PragmaList& pl = TU->getPragmaList();

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 2);

	// first pragma is at location [(4:2) - (4:22)]
	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), TU->getCompiler().getSourceManager(), 4, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), TU->getCompiler().getSourceManager(), 4, 22);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), TU->getCompiler().getSourceManager(), 5, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), TU->getCompiler().getSourceManager(), 9, 2);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->mMap.empty());
	}

	// CHECK SECOND PRAGMA
	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), TU->getCompiler().getSourceManager(), 12, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), TU->getCompiler().getSourceManager(), 12, 60);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), TU->getCompiler().getSourceManager(), 13, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), TU->getCompiler().getSourceManager(), 13, 4);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_FALSE(omp->mMap.empty());

		ValueList& values = omp->mMap["private"];
		// only 1 variable in the private construct
		EXPECT_EQ(values.size(), (size_t) 2);

		// check first variable name
		{
			clang::DeclRefExpr* varRef =  dyn_cast<clang::DeclRefExpr>(values[0]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "a");
		}

		// check second variable name
		{
			clang::DeclRefExpr* varRef =  dyn_cast<clang::DeclRefExpr>(values[1]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "b");
		}

		// check default(shared)
		EXPECT_FALSE(omp->mMap["default"].empty());
		EXPECT_EQ(*omp->mMap["default"][0]->get<std::string*>(), "shared");
	}
}

TEST(PragmaMatcherTest, HandleOmpFor) {

	ProgramPtr program = Program::create();
	InsiemeTransUnitPtr TU = InsiemeTransUnit::ParseFile(std::string(SRC_DIR) + "/omp_for.c", *program);
	const PragmaList& pl = TU->getPragmaList();

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 4);

	// first pragma is at location [(6:2) - (6:37)]
	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), TU->getCompiler().getSourceManager(), 6, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), TU->getCompiler().getSourceManager(), 6, 37);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), TU->getCompiler().getSourceManager(), 7, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), TU->getCompiler().getSourceManager(), 9, 2);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_FALSE(omp->mMap.empty());

		// look for 'for' keyword in the map
		EXPECT_TRUE(omp->mMap.find("for") != omp->mMap.end());

		ValueList& values = omp->mMap["private"];
		// only 1 variable in the private construct
		EXPECT_EQ(values.size(), (size_t) 1);

		// check first variable name
		{
			clang::DeclRefExpr* varRef =  dyn_cast<clang::DeclRefExpr>(values[0]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "a");
		}
	}

	// pragma is at location [(11:2) - (11:22)]
	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), TU->getCompiler().getSourceManager(), 11, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), TU->getCompiler().getSourceManager(), 11, 22);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), TU->getCompiler().getSourceManager(), 12, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), TU->getCompiler().getSourceManager(), 18, 2);

		// check empty map
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->mMap.empty());
	}

	// pragma is at location [(13:3) - (14:14)]
	p = pl[2];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), TU->getCompiler().getSourceManager(), 13, 3);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), TU->getCompiler().getSourceManager(), 14, 14);

		EXPECT_EQ(p->getType(), "omp::for");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), TU->getCompiler().getSourceManager(), 15, 3);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), TU->getCompiler().getSourceManager(), 17, 3);

	}

}

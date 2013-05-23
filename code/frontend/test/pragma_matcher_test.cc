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

#define CHECK_LOCATION(loc, srcMgr, line, col) \
	EXPECT_EQ((size_t)line, utils::Line(loc, srcMgr)); \
	EXPECT_EQ((size_t)col, utils::Column(loc, srcMgr));


TEST(PragmaMatcherTest, PragmaPossitions) {

	// This test checks that everithing is allright with the pragmas matching
	// we experienced some issues related to finding the possition of the pragma when using macros
	//clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R,
	NodeManager manager;
	ConversionJob job;

	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile( SRC_DIR "/inputs/pragmas.c" );
	TranslationUnit& tu = prog.addTranslationUnit( file );

	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	std::cout << "****************************************" << std::endl;
	std::cout << "****************************************" << std::endl;

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 8);

	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 43, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 43, 22);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"first\"");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 44, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 44, 7);
	}

	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 46, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 46, 21);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"macro\"");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 47, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 49, 14);

	}

	p = pl[2];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 49, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 49, 20);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"solo\"");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

        // test was changed due to changes in the pragma handling (see InsiemeSema::ActOnCompoundStmt)
        // we have to check if the sub statement of the AttributedStmt is a NullStmt
		EXPECT_TRUE ( llvm::isa<clang::AttributedStmt>(stmt) &&
                      llvm::isa<clang::NullStmt>( ((clang::AttributedStmt *)stmt)->getSubStmt() ) );
		// we dont check injected stmt possition, might be wrong
//		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 12, 2);
//		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 14, 14);
	}

	p = pl[3];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 55, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 55, 24);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"function\"");

		// pragma associated to a function
		EXPECT_FALSE(p->isStatement());
		EXPECT_TRUE(p->isDecl());
		const clang::Decl* decl = p->getDecl();

		CHECK_LOCATION(decl->getLocStart(), comp.getSourceManager(), 56, 1);
		CHECK_LOCATION(decl->getLocEnd(), comp.getSourceManager(), 59, 1);
	}

	p = pl[4];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 65, 3);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 66, 19);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"two lines\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 67, 3);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 68, 3);
	}


	/// three pragmas in a raw
	p = pl[5];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 74, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 74, 19);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"one\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 77, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 78, 2);
	}
	p = pl[6];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 75, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 75, 19);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"two\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 77, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 78, 2);
	}
	p = pl[7];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 76, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 76, 21);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"three\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 77, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 78, 2);
	}
}

TEST(PragmaMatcherTest, PragmaPossitions2) {

	// This test checks that everithing is allright with the pragmas matching
	// we experienced some issues related to finding the possition of the pragma when using macros
	//clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R,
	NodeManager manager;
	ConversionJob job;

	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile( SRC_DIR "/inputs/pragma2.c" );
	TranslationUnit& tu = prog.addTranslationUnit( file );

	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	std::cout << "****************************************" << std::endl;
	std::cout << "****************************************" << std::endl;

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 3);

	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 42, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 43, 9);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"first\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 44, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 44, 11);
	}

	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 46, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 47, 10);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"second\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 48, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 48, 16);
	}

	p = pl[2];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 50, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 51, 9);

		EXPECT_EQ(p->getType(), "test");
		const fe::TestPragma& tp = static_cast<const fe::TestPragma&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"third\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 52, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 9);
	}
}


TEST(PragmaMatcherTest, HandleOmpParallel) {

	NodeManager manager;
	ConversionJob job;

	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile( SRC_DIR "/inputs/omp_parallel.c" );
	TranslationUnit& tu = prog.addTranslationUnit( file );

	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 4);

	// first pragma is at location [(4:2) - (4:22)]
	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 40, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 40, 22);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 41, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 45, 2);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->getMap().empty());
	}

	// CHECK SECOND PRAGMA
	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 48, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 48, 60);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 49, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 51, 3);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_FALSE(omp->getMap().empty());

		auto fit = omp->getMap().find("private");
		EXPECT_TRUE( fit != omp->getMap().end() );
		const ValueList& values = fit->second;
		// only 1 variable in the private construct
		EXPECT_EQ(values.size(), (size_t) 2);

		// check first variable name
		{
			clang::DeclRefExpr* varRef =  llvm::dyn_cast<clang::DeclRefExpr>(values[0]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			// ASSERT_EQ(varRef->getDecl()->getNameAsString(), "a");
		}

		// check second variable name
		{
			clang::DeclRefExpr* varRef = llvm::dyn_cast<clang::DeclRefExpr>(values[1]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "b");
		}

		// check default(shared)
		auto dit = omp->getMap().find("default");
		EXPECT_TRUE(dit != omp->getMap().end());
		EXPECT_FALSE(dit->second.empty());
		EXPECT_EQ(*dit->second[0]->get<std::string*>(), "shared");
	}

	p = pl[2];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 51, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 51, 20);

		EXPECT_EQ(p->getType(), "omp::master");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 52, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 24);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->getMap().empty());
	}

	p = pl[3];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 55, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 55, 20);

		EXPECT_EQ(p->getType(), "omp::single");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 56, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 56, 23);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->getMap().empty());
	}

}


TEST(PragmaMatcherTest, HandleOmpFor) {

	NodeManager manager;
	ConversionJob job;
	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile(SRC_DIR "/inputs/omp_for.c");
	prog.addTranslationUnit( file );

	const PragmaList& pl = (*prog.getTranslationUnits().begin())->getPragmaList();
	const ClangCompiler& comp = (*prog.getTranslationUnits().begin())->getCompiler();

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 4);

	// first pragma is at location [(6:2) - (6:37)]
	PragmaPtr p = pl[0];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 40, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 40, 37);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 41, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 45, 22);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_FALSE(omp->getMap().empty());

		// look for 'for' keyword in the map
		EXPECT_TRUE(omp->getMap().find("for") != omp->getMap().end());

		auto fit = omp->getMap().find("private");
		EXPECT_TRUE(fit != omp->getMap().end());
		const ValueList& values = fit->second;
		// only 1 variable in the private construct
		EXPECT_EQ(values.size(), (size_t) 1);

		// check first variable name
		{
			clang::DeclRefExpr* varRef = llvm::dyn_cast<clang::DeclRefExpr>(values[0]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "a");
		}
	}

	// pragma is at location [(11:2) - (11:22)]
	p = pl[1];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 45, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 45, 22);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 48, 14);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 2);

		// check empty map
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());
		EXPECT_TRUE(omp->getMap().empty());
	}

	// pragma is at location [(13:3) - (14:14)]
	p = pl[2];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 47, 3);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 48, 14);

		EXPECT_EQ(p->getType(), "omp::for");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 49, 3);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 51, 3);

		// check the omp parallel is empty
		omp::OmpPragma* omp = static_cast<omp::OmpPragma*>(p.get());

		auto fit = omp->getMap().find("firstprivate");
		EXPECT_TRUE(fit != omp->getMap().end());
		const ValueList& values = fit->second;
		// only 1 variable in the private construct
		EXPECT_EQ(values.size(), (size_t) 1);

		// check first variable name
		{
			clang::DeclRefExpr* varRef =  llvm::dyn_cast<clang::DeclRefExpr>(values[0]->get<clang::Stmt*>());
			ASSERT_TRUE(varRef);
			ASSERT_EQ(varRef->getDecl()->getNameAsString(), "a");
		}

		// look for 'nowait' keyword in the map
		EXPECT_TRUE(omp->getMap().find("nowait") != omp->getMap().end());
	}

	// pragma is at location [(16:5) - (16:24)]
	p = pl[3];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 50, 5);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 50, 24);

		EXPECT_EQ(p->getType(), "omp::barrier");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = ((clang::AttributedStmt *) (p->getStatement()))->getSubStmt();

		EXPECT_TRUE( llvm::dyn_cast<clang::NullStmt>(stmt) != NULL );
		EXPECT_TRUE( stmt->getLocStart().isInvalid() );
	}

}

TEST(PragmaMatcherTest, RecursiveFunctions) {

	NodeManager manager;
	ConversionJob job;
	insieme::frontend::Program prog(manager, job);

	ConversionJob file = job;
	file.setFile(SRC_DIR "/inputs/rec.c");
	prog.addTranslationUnit( file );

	const PragmaList& pl = (*prog.getTranslationUnits().begin())->getPragmaList();
	const ClangCompiler& comp = (*prog.getTranslationUnits().begin())->getCompiler();

	EXPECT_FALSE(pl.empty());
	EXPECT_EQ(pl.size(), (size_t) 2);

	// first pragma is at location [(6:2) - (6:37)]
	PragmaPtr p = pl[0];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 40, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 41, 133);

		EXPECT_EQ(p->getType(), "test");

		// pragma associated to a statement
		EXPECT_TRUE(p->isDecl());
		const clang::Decl* decl = p->getDecl();

		// check stmt start location
		CHECK_LOCATION(decl->getLocStart(), comp.getSourceManager(), 42, 1);
		// check stmt end location
		CHECK_LOCATION(decl->getLocEnd(), comp.getSourceManager(), 47, 2);
	}

	// first pragma is at location [(6:2) - (6:37)]
	p = pl[1];
	{
		std::cout << "****************************************" << std::endl;

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 46, 1);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 47, 133);

		EXPECT_EQ(p->getType(), "test");

		// pragma associated to a statement
		EXPECT_TRUE(p->isDecl());
		const clang::Decl* decl = p->getDecl();

		// check stmt start location
		CHECK_LOCATION(decl->getLocStart(), comp.getSourceManager(), 48, 1);
		// check stmt end location
		CHECK_LOCATION(decl->getLocEnd(), comp.getSourceManager(), 50, 1);
	}
}


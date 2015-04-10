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
#include <vector>
#include <memory>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/compiler.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/extensions/omp_frontend_extension.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/translation_unit.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/utils/config.h"


#include "clang/AST/Expr.h"
#include "clang/AST/Type.h"

using namespace insieme::frontend;
using namespace insieme::frontend::extensions;
using namespace insieme::frontend::pragma;
using namespace insieme::core;

#define CHECK_LOCATION(loc, srcMgr, line, col) \
	EXPECT_EQ((size_t)line, utils::Line(loc, srcMgr)); \
	EXPECT_EQ((size_t)col, utils::Column(loc, srcMgr));

namespace {

    /**
     *  Checks given match object for all indentifiers
     *  that are contained in the expression or variable list of
     *  the match object.
     */
    std::vector<insieme::core::ExpressionPtr> handleIdentifierList(MatchObject& m, const std::string& key) {
        std::vector<insieme::core::ExpressionPtr> ret;
        for(insieme::core::ExpressionPtr p : m.getExprs(key))
            ret.push_back(p);
        for(insieme::core::VariablePtr p : m.getVars(key))
            ret.push_back(p.as<insieme::core::ExpressionPtr>());
        return ret;
    }

}


TEST(PragmaMatcherTest, DISABLED_PragmaPositions) {

	// This test checks that everything is alright with the pragma matching
	// we experienced some issues related to finding the position of the pragma when using macros
	//clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R,
	NodeManager manager;
	ConversionSetup setup;
	setup.frontendExtensionInit();
	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/pragmas.c", setup);

	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	EXPECT_EQ(pl.size(), (size_t) 8);

	if (pl.size()==8) {
	PragmaPtr p;
	p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 43, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 43, 22);

		EXPECT_EQ(p->getType(), "test");
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"solo\"");

		// pragma associated to a statement
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

        EXPECT_TRUE ( llvm::isa<clang::NullStmt>(stmt));
//      we don't check injected stmt position, might be wrong
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"three\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 77, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 78, 2);
	}
	}
}

TEST(PragmaMatcherTest, DISABLED_PragmaPositions2) {

	// This test checks that everything is alright with the pragma matching
	// we experienced some issues related to finding the position of the pragma when using macros
	//clang::StmtResult InsiemeSema::ActOnCompoundStmt(clang::SourceLocation L, clang::SourceLocation R,
	NodeManager manager;
	ConversionSetup setup;
	setup.frontendExtensionInit();

	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/pragma2.c", setup);
	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	EXPECT_EQ(pl.size(), (size_t) 3);

	if (pl.size()==3) {
	PragmaPtr p = pl[0];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 42, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 43, 9);

		EXPECT_EQ(p->getType(), "test");
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
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
		const TestPragmaExtension& tp = static_cast<const TestPragmaExtension&>(*p);
		EXPECT_EQ(tp.getExpected(), "\"third\"");

		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 52, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 9);
	}
	}
}


TEST(PragmaMatcherTest, HandleOmpParallel) {

	NodeManager manager;

    insieme::frontend::ConversionJob job(CLANG_SRC_DIR "/inputs/omp_parallel.c");
    job.registerFrontendExtension<insieme::frontend::extensions::OmpFrontendExtension>();

	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/omp_parallel.c", job);
	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();
	insieme::frontend::conversion::Converter convFactory(manager, tu);

	EXPECT_EQ(pl.size(), (size_t) 4);

	if (pl.size()==4) {
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
		const clang::Stmt* stmt = p->getStatement();

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 41, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 45, 2);

		// check the omp parallel is empty
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
		auto mo = omp->getMatchObject(convFactory);
		EXPECT_TRUE(mo.empty());
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
		const clang::Stmt* stmt = (p->getStatement());

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 49, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 51, 3);

		// check the omp parallel is empty
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
		auto mo = omp->getMatchObject(convFactory);
		EXPECT_FALSE(mo.empty());

		auto fit = handleIdentifierList(mo, "private");
		EXPECT_FALSE( fit.empty() );
		// only 1 variable in the private construct
		EXPECT_EQ(fit.size(), (size_t) 2);

		// check first variable name
		{
		    EXPECT_TRUE(toString(fit[0].as<VariablePtr>()) == "AP(v1)");
		    EXPECT_TRUE(toString(fit[1].as<VariablePtr>()) == "AP(v2)");
        }

		// check default(shared)
        auto def = mo.getString("default");
        EXPECT_FALSE(def.empty());
        EXPECT_TRUE(def == "shared");
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
		const clang::Stmt* stmt = (p->getStatement());

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 52, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 24);

		// check the omp parallel is empty
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
        auto mo = omp->getMatchObject(convFactory);
		EXPECT_TRUE(mo.empty());
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
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
        auto mo = omp->getMatchObject(convFactory);
		EXPECT_TRUE(mo.empty());
	}
}
}


TEST(PragmaMatcherTest, HandleOmpFor) {

	NodeManager manager;

    insieme::frontend::ConversionJob job(CLANG_SRC_DIR "/inputs/omp_for.c");
    job.registerFrontendExtension<insieme::frontend::extensions::OmpFrontendExtension>();

	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/omp_for.c", job);
	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();
	insieme::frontend::conversion::Converter convFactory(manager, tu);

	EXPECT_EQ(pl.size(), (size_t) 4);

	if (pl.size()==4) {
	// first pragma is at location [(6:2) - (6:37)]
	PragmaPtr p = pl[0];
	{

		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 40, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 40, 37);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = (p->getStatement());

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 41, 2);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 45, 22);

		// check the omp parallel is empty
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
		auto mo = omp->getMatchObject(convFactory);

		EXPECT_FALSE(mo.empty());

		// look for 'for' keyword in the map
		EXPECT_TRUE(mo.stringValueExists("for"));

		auto fit = handleIdentifierList(mo, "private");
		EXPECT_FALSE(fit.empty());
		// only 1 variable in the private construct
		EXPECT_EQ(fit.size(), (size_t) 1);

		// check first variable name
		{
            EXPECT_TRUE(toString(fit[0].as<VariablePtr>()) == "AP(v1)");
		}
	}

	// pragma is at location [(11:2) - (11:22)]
	p = pl[1];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 45, 2);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 45, 22);

		EXPECT_EQ(p->getType(), "omp::parallel");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = (p->getStatement());

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 48, 14);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 52, 2);

		// check empty map
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
        auto mo = omp->getMatchObject(convFactory);

		EXPECT_TRUE(mo.empty());
	}

	// pragma is at location [(13:3) - (14:14)]
	p = pl[2];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 47, 3);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 48, 14);

		EXPECT_EQ(p->getType(), "omp::for");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = (p->getStatement());

		// check stmt start location
		CHECK_LOCATION(stmt->getLocStart(), comp.getSourceManager(), 49, 3);
		// check stmt end location
		CHECK_LOCATION(stmt->getLocEnd(), comp.getSourceManager(), 51, 3);

		// check the omp parallel is empty
		FrontendExtensionPragma* omp = static_cast<FrontendExtensionPragma*>(p.get());
        auto mo = omp->getMatchObject(convFactory);

		auto ex = handleIdentifierList(mo, "firstprivate");
		EXPECT_FALSE(ex.empty());
		// only 1 variable in the private construct
		EXPECT_EQ(ex.size(), (size_t) 1);

		// check first variable name
		{
            EXPECT_TRUE(toString(ex[0]) == "AP(v1)");
		}

		// look for 'nowait' keyword in the map
		EXPECT_TRUE(mo.stringValueExists("nowait"));
	}

	// pragma is at location [(16:5) - (16:24)]
	p = pl[3];
	{
		// check pragma start location
		CHECK_LOCATION(p->getStartLocation(), comp.getSourceManager(), 50, 5);
		// check pragma end location
		CHECK_LOCATION(p->getEndLocation(), comp.getSourceManager(), 50, 24);

		EXPECT_EQ(p->getType(), "omp::barrier");

		// pragma associated to a subStmt of an AttributedStmt
		// test was changed due to a change in the InsiemeSema (see InsiemeSema::ActOnCompoundStmt)
		EXPECT_TRUE(p->isStatement());
		const clang::Stmt* stmt = (p->getStatement());

		EXPECT_TRUE( llvm::dyn_cast<clang::NullStmt>(stmt) != NULL );
		EXPECT_TRUE( stmt->getLocStart().isInvalid() );
	}
}
}

TEST(PragmaMatcherTest, DISABLED_RecursiveFunctions) {

	NodeManager manager;
	ConversionSetup setup;
	setup.frontendExtensionInit();

	insieme::frontend::TranslationUnit tu(manager, CLANG_SRC_DIR "/inputs/rec.c", setup);
	const PragmaList& pl = tu.getPragmaList();
	const ClangCompiler& comp = tu.getCompiler();

	EXPECT_EQ(pl.size(), (size_t) 2);

	if (pl.size()==2){
	// first pragma is at location [(6:2) - (6:37)]
	PragmaPtr p = pl[0];
	{
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
}

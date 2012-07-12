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
#include <limits>
#include <algorithm>

#include "insieme/analysis/cfg.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

TEST(CFGBuilder, Single) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"(ref<int<4>>:a = 0)"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(1u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, std::distance(entry->successors_begin(), entry->successors_end()));

	const auto& decl = *entry->successors_begin();
	EXPECT_EQ(1u, decl->size());
	EXPECT_TRUE((*decl)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, std::distance(decl->successors_begin(), decl->successors_end()));
	
	const auto& exit = *decl->successors_begin();
	EXPECT_EQ(exit, cfg->getBlockPtr(cfg->exit()));

}

TEST(CFGBuilder, IfThen) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 0;"
		"	if ( true ) { "
		"		(a = 0); "
		"	};"
		"	decl int<4>:c = 1;"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(4u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, std::distance(entry->successors_begin(), entry->successors_end()));

	const auto& decl = *entry->successors_begin();
	EXPECT_EQ(1u, decl->size());
	EXPECT_TRUE((*decl)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);
	EXPECT_EQ(1u, std::distance(decl->successors_begin(), decl->successors_end()));

	const auto& ifHead = *decl->successors_begin();
	EXPECT_EQ(1u, ifHead->size());
	EXPECT_TRUE(ifHead->hasTerminator());
	EXPECT_TRUE((*ifHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, std::distance(ifHead->successors_begin(), ifHead->successors_end()));

	const auto& ifThen = *ifHead->successors_begin();
	EXPECT_EQ(1u, ifThen->size());
	EXPECT_TRUE((*ifThen)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, std::distance(ifThen->successors_begin(), ifThen->successors_end()));

	EXPECT_EQ(*ifThen->successors_begin(), *(++ifHead->successors_begin()));

	const auto& end = *ifThen->successors_begin();
	EXPECT_EQ(1u, end->size());
	EXPECT_TRUE((*end)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);

}

TEST(CFGBuilder, IfThenElse) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 0;"
		"	if ( true ) { "
		"		(a = 0); "
		"	} else {"
		"		(a = 1); "
		"	};"
		"	decl int<4>:c = 1;"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(5u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, std::distance(entry->successors_begin(), entry->successors_end()));

	const auto& decl = *entry->successors_begin();
	EXPECT_EQ(1u, decl->size());
	EXPECT_TRUE((*decl)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);
	EXPECT_EQ(1u, std::distance(decl->successors_begin(), decl->successors_end()));

	const auto& ifHead = *decl->successors_begin();
	EXPECT_EQ(1u, ifHead->size());
	EXPECT_TRUE(ifHead->hasTerminator());
	EXPECT_TRUE((*ifHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, std::distance(ifHead->successors_begin(), ifHead->successors_end()));

	const auto& ifThen = *ifHead->successors_begin();
	EXPECT_EQ(1u, ifThen->size());
	EXPECT_TRUE((*ifThen)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, std::distance(ifThen->successors_begin(), ifThen->successors_end()));

	EXPECT_NE(*ifThen->successors_begin(), *(++ifHead->successors_begin()));

	const auto& ifElse = *(++ifHead->successors_begin());
	EXPECT_EQ(1u, ifElse->size());
	EXPECT_TRUE((*ifElse)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, std::distance(ifElse->successors_begin(), ifElse->successors_end()));

	EXPECT_EQ(*ifThen->successors_begin(), *ifElse->successors_begin());

	const auto& end = *ifThen->successors_begin();
	EXPECT_EQ(1u, end->size());
	EXPECT_TRUE((*end)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);

}



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

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;

TEST(CFGBuilder, Single) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt("a = 0;", symbols);

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(1u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count() );

	const auto& decl = entry->successor(0);
	EXPECT_EQ(1u, decl->size());
	EXPECT_EQ(core::NT_CallExpr, (*decl)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, decl->successors_count());
	
	const auto& exit = decl->successor(0);
	EXPECT_EQ(exit, cfg->getBlockPtr(cfg->exit()));

}

TEST(CFGBuilder, Compound) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"{ "
		"	a = 0; "
		"	a = 0; "
		"	a = 0; "
		"}", 
		symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(3u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& s1 = entry->successor(0);
	EXPECT_EQ(1u, s1->size());
	EXPECT_EQ(core::NT_CallExpr, (*s1)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, s1->successors_count());
	
	const auto& s2 = s1->successor(0);
	EXPECT_EQ(1u, s2->size());
	EXPECT_EQ(core::NT_CallExpr, (*s2)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, s2->successors_count());

	const auto& s3 = s2->successor(0);
	EXPECT_EQ(1u, s3->size());
	EXPECT_EQ(core::NT_CallExpr, (*s3)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, s3->successors_count());

	const auto& exit = s3->successor(0);
	EXPECT_EQ(exit, cfg->getBlockPtr(cfg->exit()));

}

TEST(CFGBuilder, IfThen) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto code = builder.parseStmt(
		"{"
		"	ref<int<4>> a = 0;"
		"	if ( true ) { "
		"		a = 0; "
		"	}"
		"	int<4> c = 1;"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);
	std::cout << *cfg << std::endl;

	EXPECT_EQ(4u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& decl = entry->successor(0);
	EXPECT_EQ(1u, decl->size());
	EXPECT_EQ(core::NT_DeclarationStmt, (*decl)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, decl->successors_count());

	const auto& ifHead = decl->successor(0);
	EXPECT_EQ(1u, ifHead->size());
	EXPECT_TRUE(ifHead->hasTerminator());
	EXPECT_EQ(core::NT_Literal, (*ifHead)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(2u, ifHead->successors_count());

	const auto& ifThen = ifHead->successor(0);
	EXPECT_EQ(1u, ifThen->size());
	EXPECT_EQ(core::NT_CallExpr, (*ifThen)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, ifThen->successors_count());

	EXPECT_EQ(ifThen->successor(0), ifHead->successor(1));

	const auto& end = ifThen->successor(0);
	EXPECT_EQ(1u, end->size());
	EXPECT_EQ(core::NT_DeclarationStmt,(*end)[0].getStatementAddress()->getNodeType());

}

TEST(CFGBuilder, IfThenElse) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto code = builder.parseStmt(
		"{"
		"	ref<int<4>> a = 0;"
		"	if ( true ) { "
		"		a = 0; "
		"	} else {"
		"		a = 1; "
		"	}"
		"	intt<4> c = 1;"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(5u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& decl = entry->successor(0);
	EXPECT_EQ(1u, decl->size());
	EXPECT_EQ(core::NT_DeclarationStmt, (*decl)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, decl->successors_count());

	const auto& ifHead = decl->successor(0);
	EXPECT_EQ(1u, ifHead->size());
	EXPECT_TRUE(ifHead->hasTerminator());
	EXPECT_EQ(core::NT_Literal, (*ifHead)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(2u, ifHead->successors_count());

	const auto& ifThen = ifHead->successor(0);
	EXPECT_EQ(1u, ifThen->size());
	EXPECT_EQ(core::NT_CallExpr, (*ifThen)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, ifThen->successors_count());

	EXPECT_NE(ifThen->successor(0), ifHead->successor(1));

	const auto& ifElse = ifHead->successor(1);
	EXPECT_EQ(1u, ifElse->size());
	EXPECT_EQ(core::NT_CallExpr, (*ifElse)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, ifElse->successors_count());

	EXPECT_EQ(ifThen->successor(0), ifElse->successor(0));

	const auto& end = ifThen->successor(0);
	EXPECT_EQ(1u, end->size());
	EXPECT_EQ(core::NT_DeclarationStmt, (*end)[0].getStatementAddress()->getNodeType());

}

TEST(CFGBuilder, IfThenCmp) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto code = builder.parseStmt(
		"{"
		"	ref<int<4>> a = 0;"
		"	if ( true ) { "
		"		a = 0; "
		"		a = 1; "
		"	}"
		"	int<4> c = 1;"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(5u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& decl = entry->successor(0);
	EXPECT_EQ(1u, decl->size());
	EXPECT_EQ(core::NT_DeclarationStmt, (*decl)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, decl->successors_count());

	const auto& ifHead = decl->successor(0);
	EXPECT_EQ(1u, ifHead->size());
	EXPECT_TRUE(ifHead->hasTerminator());
	EXPECT_EQ(core::NT_Literal, (*ifHead)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(2u, ifHead->successors_count());

	const auto& ifThen1 = ifHead->successor(0);
	EXPECT_EQ(1u, ifThen1->size());
	EXPECT_EQ(core::NT_CallExpr, (*ifThen1)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, ifThen1->successors_count());

	const auto& ifThen2 = ifThen1->successor(0);
	EXPECT_EQ(1u, ifThen2->size());
	EXPECT_EQ(core::NT_CallExpr, (*ifThen2)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, ifThen2->successors_count());

	EXPECT_EQ(ifThen2->successor(0), ifHead->successor(1));

	const auto& end = *ifThen2->successors_begin();
	EXPECT_EQ(1u, end->size());
	EXPECT_EQ(core::NT_DeclarationStmt, (*end)[0].getStatementAddress()->getNodeType());

}

TEST(CFGBuilder, WhileSimple) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"{"
		"	while ( true ) { "
		"		a = 0; "
		"	}"
		"}", symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(2u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& whileHead = entry->successor(0);
	EXPECT_EQ(1u, whileHead->size());
	EXPECT_TRUE(whileHead->hasTerminator());
	EXPECT_EQ(core::NT_Literal, (*whileHead)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(2u, whileHead->successors_count());

	const auto& body = whileHead->successor(0);
	EXPECT_EQ(1u, body->size());
	EXPECT_EQ(core::NT_CallExpr, (*body)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(1u, body->successors_count());

	EXPECT_EQ(body->successor(0), whileHead);

	const auto& end = whileHead->successor(1);
	EXPECT_EQ(0u, end->size());
	EXPECT_EQ(0u, end->successors_count());
}

TEST(CFGBuilder, WhileBreak) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"{"
		"	while ( true ) { "
		"		a = 0; "
		"		break; "
		"	}"
		"}", symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(2u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& whileHead = *entry->successors_begin();
	EXPECT_EQ(1u, whileHead->size());
	EXPECT_TRUE(whileHead->hasTerminator());
	EXPECT_EQ(core::NT_Literal, (*whileHead)[0].getStatementAddress()->getNodeType());
	EXPECT_EQ(2u, whileHead->successors_count());

	const auto& body = *whileHead->successors_begin();
	EXPECT_EQ(1u, body->size());
	EXPECT_TRUE((*body)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, body->successors_count());

	const auto& end = *(++whileHead->successors_begin());
	EXPECT_EQ(*body->successors_begin(), end);

	EXPECT_EQ(0u, end->size());
	EXPECT_EQ(0u, end->successors_count());

}

TEST(CFGBuilder, WhileIfBreakCont) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"{"
		"	while ( true ) { "
		"		a = 0; "
		"		if (false) break; else continue;"
		"	}"
		"}", symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(5u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& whileHead = *entry->successors_begin();
	EXPECT_EQ(1u, whileHead->size());
	EXPECT_TRUE(whileHead->hasTerminator());
	EXPECT_TRUE((*whileHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, whileHead->successors_count());

	const auto& body1 = *whileHead->successors_begin();
	EXPECT_EQ(1u, body1->size());
	EXPECT_TRUE((*body1)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, body1->successors_count());

	const auto& ifstmt = *body1->successors_begin();
	EXPECT_EQ(1u, ifstmt->size());
	EXPECT_TRUE(ifstmt->hasTerminator());
	EXPECT_TRUE((*ifstmt)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, ifstmt->successors_count());

	const auto& breakStmt = *ifstmt->successors_begin();
	EXPECT_EQ(0u, breakStmt->size());
	EXPECT_TRUE(breakStmt->hasTerminator());
	EXPECT_EQ(1u, breakStmt->successors_count());

	const auto& contStmt = *(++ifstmt->successors_begin());
	EXPECT_EQ(0u, contStmt->size());
	EXPECT_TRUE(contStmt->hasTerminator());
	EXPECT_EQ(1u, contStmt->successors_count());

	const auto& end = *(++whileHead->successors_begin());
	EXPECT_EQ(*breakStmt->successors_begin(), end);
	EXPECT_EQ(*contStmt->successors_begin(), whileHead);

	EXPECT_EQ(0u, end->size());
	EXPECT_EQ(0u, end->successors_count());

}


//TEST(CFGBuilder, SwitchSimple) {
//
//	NodeManager mgr;
//	parse::IRParser parser(mgr);
//
//    auto code = parser.parseStatement(
//		"switch ( true ) { "
//		"	case 2:	(int<4>:a = 0)"
//		"}"
//    );
//
//    EXPECT_TRUE(code);
//	CFGPtr cfg = CFG::buildCFG(code);
//
//	EXPECT_EQ(2u+2u, cfg->size());
//
//	const auto& entry = cfg->getBlockPtr( cfg->entry() );
//	// entry point 1 single child
//	EXPECT_EQ(1u, entry->successors_count());
//
//	const auto& switchHead = *entry->successors_begin();
//	EXPECT_EQ(1u, switchHead->size());
//	EXPECT_TRUE(switchHead->hasTerminator());
//	EXPECT_TRUE((*switchHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
//	EXPECT_EQ(2u, switchHead->successors_count());
//
//	const auto& case2 = *switchHead->successors_begin();
//	EXPECT_EQ(1u, case2->size());
//	EXPECT_TRUE((*case2)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
//	EXPECT_EQ(1u, case2->successors_count());
//
//	const auto& end = *(++switchHead->successors_begin());
//	EXPECT_EQ(*case2->successors_begin(), end);
//
//	EXPECT_EQ(0u, end->size());
//	EXPECT_EQ(0u, end->successors_count());
//
//}

TEST(CFGBuilder, ForSimple) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"for ( int<4> i = 0 .. 20 : 1 ) "
		"	a = i;", symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(4u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& decl = *entry->successors_begin();
	EXPECT_EQ(1u, decl->size());
	EXPECT_TRUE((*decl)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);
	EXPECT_EQ(1u, decl->successors_count());

	const auto& forHead = *decl->successors_begin();
	EXPECT_EQ(1u, forHead->size());
	EXPECT_TRUE(forHead->hasTerminator());
	EXPECT_TRUE((*forHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, forHead->successors_count());

	// exit node (false edge)
	const auto& body = *(++forHead->successors_begin());
	EXPECT_EQ(1u, body->size());
	EXPECT_TRUE((*body)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, body->successors_count());
	
	const auto& inc = *body->successors_begin();
	EXPECT_EQ(1u, inc->size());
	EXPECT_EQ(1u, inc->successors_count());

	const auto& end = *forHead->successors_begin();
	EXPECT_EQ(*inc->successors_begin(), forHead);

	EXPECT_EQ(0u, end->size());
	EXPECT_EQ(0u, end->successors_count());

}

TEST(CFGBuilder, ForBreak) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));

    auto code = builder.parseStmt(
		"for ( int<4> i=0 .. 20 : 1 ) { "
		"	if (true) break; "
		"	a = i; "
		"}", symbols
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	EXPECT_EQ(6u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& decl = *entry->successors_begin();
	EXPECT_EQ(1u, decl->size());
	EXPECT_TRUE((*decl)[0].getStatementAddress()->getNodeType() == core::NT_DeclarationStmt);
	EXPECT_EQ(1u, decl->successors_count());

	const auto& forHead = *decl->successors_begin();
	EXPECT_EQ(1u, forHead->size());
	EXPECT_TRUE(forHead->hasTerminator());
	EXPECT_TRUE((*forHead)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, forHead->successors_count());

	const auto& body1 = *(++forHead->successors_begin());
	EXPECT_EQ(1u, body1->size());
	EXPECT_TRUE(body1->hasTerminator());
	EXPECT_TRUE((*body1)[0].getStatementAddress()->getNodeType() == core::NT_Literal);
	EXPECT_EQ(2u, body1->successors_count());

	const auto& breakStmt = *body1->successors_begin();
	EXPECT_EQ(0u, breakStmt->size());
	EXPECT_TRUE(breakStmt->hasTerminator());
	EXPECT_EQ(1u, breakStmt->successors_count());

	const auto& end = *forHead->successors_begin();
	EXPECT_EQ(*breakStmt->successors_begin(), end);

	const auto& body2 = *(++body1->successors_begin());
	EXPECT_EQ(1u, body2->size());
	EXPECT_TRUE((*body2)[0].getStatementAddress()->getNodeType() == core::NT_CallExpr);
	EXPECT_EQ(1u, body2->successors_count());

	const auto& inc = *body2->successors_begin();
	EXPECT_EQ(1u, inc->size());
	EXPECT_EQ(1u, inc->successors_count());

	EXPECT_EQ(*inc->successors_begin(), forHead);

	EXPECT_EQ(0u, end->size());
	EXPECT_EQ(0u, end->successors_count());

}

TEST(CFGBuilder, CallExpr) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["c"] = builder.variable(builder.parseType("int<4>"));
	symbols["d"] = builder.variable(builder.parseType("int<4>"));

    auto addresses = builder.parseAddresses("$a=$b+$c+d$$;$", symbols);

	EXPECT_EQ(3u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0]);

	EXPECT_EQ(3u+2u, cfg->size());

	const auto& entry = cfg->getBlockPtr( cfg->entry() );
	// entry point 1 single child
	EXPECT_EQ(1u, entry->successors_count());

	const auto& sum_c_d = entry->successor(0);
	EXPECT_EQ(1u, sum_c_d->size());
	core::StatementAddress addr = (*sum_c_d)[0].getStatementAddress();
	EXPECT_EQ(core::NT_CallExpr, addr->getNodeType());
	EXPECT_EQ(addr, addresses[2]);
	EXPECT_EQ(1u, sum_c_d->successors_count());

	const auto& sum_b_c_d = sum_c_d->successor(0);
	EXPECT_EQ(1u, sum_b_c_d->size());
	core::StatementAddress addr1 = (*sum_b_c_d)[0].getStatementAddress();
	EXPECT_EQ(core::NT_CallExpr, addr1->getNodeType());
	EXPECT_EQ(addr1, addresses[1]);
	EXPECT_EQ(1u, sum_b_c_d->successors_count());

	const auto& assign = sum_b_c_d->successor(0);
	EXPECT_EQ(1u, assign->size());
	core::StatementAddress addr2 = (*assign)[0].getStatementAddress();
	EXPECT_EQ(core::NT_CallExpr, addr2->getNodeType());
	EXPECT_EQ(addr2, addresses[0]);
	EXPECT_EQ(1u, assign->successors_count());

	const auto& exit = assign->successor(0);
	EXPECT_EQ(0u, exit->size());
	EXPECT_EQ(0u, exit->successors_count());


}

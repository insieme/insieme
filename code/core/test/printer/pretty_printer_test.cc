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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/attributes.h"

using namespace insieme::core;
using namespace insieme::core::printer;

TEST(PrettyPrinter, Basic) {

	// check setup
	EXPECT_EQ(static_cast<unsigned>(0), PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::PRINT_BRACKETS | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS | PrettyPrinter::PRINT_MARKERS | PrettyPrinter::PRINT_ATTRIBUTES | PrettyPrinter::NO_EVAL_LAZY ),
			PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_EQ(static_cast<unsigned>(PrettyPrinter::OPTIONS_DETAIL | PrettyPrinter::PRINT_SINGLE_LINE),
			PrettyPrinter::OPTIONS_SINGLE_LINE);

	NodePtr ptr;

	PrettyPrinter printerA(ptr, PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_FALSE(printerA.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerB(ptr, PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_TRUE(printerB.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_FALSE(printerB.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	PrettyPrinter printerC(ptr, PrettyPrinter::OPTIONS_SINGLE_LINE);
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_CASTS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_BRACKETS));
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_SINGLE_LINE));

	printerC.setOption(PrettyPrinter::PRINT_DEREFS, false);
	EXPECT_FALSE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	printerC.setOption(PrettyPrinter::PRINT_DEREFS);
	EXPECT_TRUE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));
	printerC.setOption(PrettyPrinter::PRINT_DEREFS, false);
	EXPECT_FALSE(printerC.hasOption(PrettyPrinter::PRINT_DEREFS));

}

TEST(PrettyPrinter, Wrapper) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	LiteralPtr lit = Literal::get(mgr, mgr.getLangBasic().getString(), "\"this is a string literal\"");
	LiteralPtr one = Literal::get(mgr, mgr.getLangBasic().getInt4(), "1");
	VariablePtr iter = Variable::get(mgr, mgr.getLangBasic().getInt4());
	ForStmtPtr forStmt = builder.forStmt(iter, one, one, one, lit);

	PrettyPrinter printerA(forStmt, PrettyPrinter::OPTIONS_DEFAULT);

	std::ostringstream ss1;
	SourceLocationMap srcMap = printAndMap(ss1, printerA);

	std::ostringstream ss2;
	ss2 << printerA;

	// EXPECT_EQ(ss2.str(), ss1.str());

	// print the map
	std::cout << ss2.str() << std::endl;
	std::cout << srcMap;

	// ForStmt loc
	SourceLocationMap::const_iterator it = srcMap.begin();
	EXPECT_EQ(forStmt, it->second);
	EXPECT_EQ(SourceLocation(0,0), it->first.first );
	EXPECT_EQ(SourceLocation(2,1), it->first.second );
	
	++it;

	// int<4> type loc
	EXPECT_EQ(mgr.getLangBasic().getInt4(), it->second);
	EXPECT_EQ(SourceLocation(0,9), it->first.first );
	EXPECT_EQ(SourceLocation(0,15), it->first.second );

	++it;

	// variable loc
	EXPECT_EQ(iter, it->second);
	EXPECT_EQ(SourceLocation(0,16), it->first.first );
	EXPECT_EQ(SourceLocation(0,18), it->first.second );
	
	++it;

	// init value (1) loc
	EXPECT_EQ(forStmt->getStart(), it->second);
	EXPECT_EQ(SourceLocation(0,21), it->first.first );
	EXPECT_EQ(SourceLocation(0,22), it->first.second );

	++it;

	// for loop end condition (1) loc
	EXPECT_EQ(forStmt->getEnd(), it->second);
	EXPECT_EQ(SourceLocation(0,26), it->first.first );
	EXPECT_EQ(SourceLocation(0,27), it->first.second );

	++it;

	// for loop step (1) loc
	EXPECT_EQ(forStmt->getStep(), it->second);
	EXPECT_EQ(SourceLocation(0,30), it->first.first );
	EXPECT_EQ(SourceLocation(0,31), it->first.second );

	++it;

	// for loop body (compound) loc
	EXPECT_EQ(forStmt->getBody(), it->second);
	EXPECT_EQ(SourceLocation(0,33), it->first.first );
	EXPECT_EQ(SourceLocation(2,1), it->first.second );

	++it;

	// for loop body (lit) loc
	EXPECT_EQ(forStmt->getBody()->getStatements()[0], it->second);
	EXPECT_EQ(SourceLocation(1,4), it->first.first );
	EXPECT_EQ(SourceLocation(1,30), it->first.second );

}

TEST(PrettyPrinter, HiddenAttributes) {

	NodeManager manager;
	IRBuilder builder(manager);
	auto& ext = manager.getLangExtension<analysis::AttributeExtension>();

	analysis::AttributePtr a1 = ext.getUnordered();

	ExpressionPtr expr = builder.intLit(1);
	expr = analysis::addAttribute(expr, a1);

	EXPECT_EQ("1", toString(PrettyPrinter(expr)));
	EXPECT_EQ("attr(1, ([unordered]))", toString(PrettyPrinter(expr, PrettyPrinter::OPTIONS_DETAIL)));
}

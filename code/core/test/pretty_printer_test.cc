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

#include "insieme/core/ast_node.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme::core;
using namespace insieme::core::printer;

TEST(PrettyPrinter, Basic) {

	// check setup
	EXPECT_EQ(0, PrettyPrinter::OPTIONS_DEFAULT);
	EXPECT_EQ(PrettyPrinter::PRINT_BRACKETS | PrettyPrinter::PRINT_CASTS | PrettyPrinter::PRINT_DEREFS, PrettyPrinter::OPTIONS_DETAIL);
	EXPECT_EQ(PrettyPrinter::OPTIONS_DETAIL | PrettyPrinter::PRINT_SINGLE_LINE, PrettyPrinter::OPTIONS_SINGLE_LINE);

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

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

#include "insieme/analysis/dfa/analyses/def_use.h"
#include "insieme/analysis/cfg.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

//=============================================================================
// Scalars 
//=============================================================================
TEST(DefUse, ScalarNoControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$ref<int<4>> a = 0;$ "
		"	$a$ = i+b; "
		"	int<4> c = *$a$;"
		"}$"
    );
    EXPECT_EQ(4u, addresses.size());

	analyses::DefUse du(addresses[0].getAddressedNode());
	core::VariableAddress aRef = addresses[3].as<VariableAddress>();
		
	auto addrSet = du.getDefinitions(aRef);
	EXPECT_EQ(1u, addrSet.size());

	auto addrIt = addrSet.begin();
	EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
	EXPECT_EQ(addresses[2], *addrIt);
}

//=============================================================================
// Members
//=============================================================================
TEST(DefUse, VectorsWithControl) {

	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, core::NodePtr> symbols;
	symbols["v"] = builder.variable(
			builder.parseType("ref<vector<struct{int<4> a; int<2> b;}, 10>>")
		);

    auto addresses = builder.parseAddresses(
		"${"
		"	int<4> i = 2; "
		"	int<4> b = 3; "
		"	$v[1u].a$ = i+b; "
		"	while (i<b) { "
		"		$v[2u].a$ = i+b; "
		"	}"
		"	int<4> c1 = *$v[1u].a$;"
		"	int<4> c2 = *$v[2u].b$;"
		"	int<4> c3 = *$v[2u].a$;"
		"}$", symbols
    );
	EXPECT_EQ(6u, addresses.size());

	analyses::DefUse du(addresses[0].getAddressedNode());

	{
		// lookup address of variable A
		ExpressionAddress aRef = addresses[3].as<ExpressionAddress>();
		auto addrSet = du.getDefinitions(aRef);
		EXPECT_EQ(1u, addrSet.size());

		auto addrIt = addrSet.begin();
		EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
		EXPECT_EQ(addresses[1], *addrIt);
	}

	{
		// lookup address of variable A
		ExpressionAddress aRef = addresses[4].as<ExpressionAddress>();
		auto addrSet = du.getDefinitions(aRef);
		EXPECT_EQ(0u, addrSet.size());
	}

	{
		// lookup address of variable A
		ExpressionAddress aRef = addresses[5].as<ExpressionAddress>();
		auto addrSet = du.getDefinitions(aRef);
		EXPECT_EQ(1u, addrSet.size());

		auto addrIt = addrSet.begin();
		EXPECT_EQ(addresses[0].getRootNode(), addrIt->getRootNode());
		EXPECT_EQ(addresses[2], *addrIt);
	}

}

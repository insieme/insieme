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

TEST(CFGAddress, Simple1) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddressesStatement(
		"$decl ref<int<4>> a = 10+$20$;$"
    );

    EXPECT_EQ(2u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0]);
	
	auto address = cfg->find(addresses[1]);

	EXPECT_TRUE( address );
	EXPECT_EQ( 2u, address.getBlock().getBlockID() );
	EXPECT_EQ( 0u, address.getStmtIdx() );

	EXPECT_EQ("<2:0:0-1-3>", toString(address));

	EXPECT_EQ( addresses[1].getAddressedNode(), address.getAddressedNode() );

}

TEST(CFGAddress, Simple2) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddressesStatement(
		"${ "
		"	decl int<4> a = 10;"
		"	decl int<4> b = 20;"
		"	$decl ref<int<4>> c = $a+$b$$;$"
		"}$ "
    );

    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0]);
	// std::cout << *cfg << std::endl;
	{ 
		auto address = cfg->find(addresses[3]);

		EXPECT_TRUE( address );
		EXPECT_EQ( 2u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<2:0:0-1-3>", toString(address));
		EXPECT_EQ( addresses[3].getAddressedNode(), address.getAddressedNode() );
	}
	{ 
		auto address = cfg->find(addresses[2]);

		EXPECT_TRUE( address );
		EXPECT_EQ( 2u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<2:0:0-1>", toString(address));
		EXPECT_EQ( addresses[2].getAddressedNode(), address.getAddressedNode() );
	}
	{ 
		auto address = cfg->find(addresses[1]);

		EXPECT_TRUE( address );
		EXPECT_EQ( 2u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<2:0:0>", toString(address));
		EXPECT_EQ( addresses[1].getAddressedNode(), address.getAddressedNode() );
	}
}

TEST(CFGAddress, Simple3) {

	NodeManager mgr;
	IRBuilder builder(mgr);

    auto addresses = builder.parseAddressesStatement(
		"${ "
		"	decl int<4> a = 10;"
		"	decl int<4> b = 20;"
		"	$decl ref<int<4>> c = $a+$b$+b$;$"
		"}$ "
    );

    EXPECT_EQ(4u, addresses.size());

	CFGPtr cfg = CFG::buildCFG(addresses[0]);
	// std::cout << *cfg << std::endl;
	{ 
		auto address = cfg->find(addresses[3]);

		EXPECT_TRUE( address );

		EXPECT_EQ( 3u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<3:0:0-1-3>", toString(address));
		EXPECT_EQ( addresses[3].getAddressedNode(), address.getAddressedNode() );
		EXPECT_EQ( addresses[3], address.toAbsoluteAddress(cfg->getTmpVarMap()) );
	}
	{ 
		auto address = cfg->find(addresses[2]);

		EXPECT_TRUE( address );
		EXPECT_EQ( 2u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<2:0:0-1>", toString(address));
		EXPECT_NE( addresses[2].getAddressedNode(), address.getAddressedNode() );
		EXPECT_EQ("int_add(v8, v2)", toString(*address.getAddressedNode()));
		EXPECT_EQ( addresses[2], address.toAbsoluteAddress(cfg->getTmpVarMap()) );
	}
	{ 
		auto address = cfg->find(addresses[1]);

		EXPECT_TRUE( address );
		EXPECT_EQ( 2u, address.getBlock().getBlockID() );
		EXPECT_EQ( 0u, address.getStmtIdx() );
		EXPECT_EQ("<2:0:0>", toString(address));
		EXPECT_NE( addresses[1].getAddressedNode(), address.getAddressedNode() );
		EXPECT_EQ("ref<int<4>> v3 = int_add(v8, v2)", toString(*address.getAddressedNode()));
		EXPECT_EQ( addresses[1], address.toAbsoluteAddress(cfg->getTmpVarMap()) );
	}
}


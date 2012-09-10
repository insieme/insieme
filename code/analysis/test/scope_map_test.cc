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

#include "insieme/core/ir_builder.h"
#include "insieme/analysis/scopes_map.h"

using namespace insieme::utils;
using namespace insieme::core;
using namespace insieme::analysis;

TEST(VariableScopeMap, Simple) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);


	auto addresses = builder.parseAddresses(
			"${ "
			"	int<4> a = 0; "
			"	${ "
			"		int<4> a = 0; "
			"	}$ "
			"}$"
		);

	auto ret = mapVariablesToScopes(NodeAddress(addresses[0]));

	auto it = ret.begin(), end=ret.end();
	EXPECT_EQ(it->second, addresses[0]);
	++it;

	EXPECT_NE(end, it);
	EXPECT_EQ(it->second, addresses[1]);
	
	++it;
	EXPECT_EQ(end, it);

}

TEST(VariableScopeMap, Simple2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);


	auto addresses = builder.parseAddresses(
			"${ "
			"	int<4> a = 0; "
			"	a = 1; "
			"	${ "
			"		a = 3; "
			"		int<4> a = 0; "
			"		a = 4; "
			"	}$ "
			"	a = 6; "
			"}$"
		);

	//std::cout << *addresses[0] << std::endl;
	auto ret = mapVariablesToScopes(NodeAddress(addresses[0]));

	auto it = ret.begin(), end=ret.end();
	EXPECT_EQ(it->second, addresses[0]);
	++it;

	EXPECT_NE(end, it);
	EXPECT_EQ(it->second, addresses[1]);
	
	++it;
	EXPECT_EQ(end, it);

}

TEST(VariableScopeMap, Simple3) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);


	auto code = builder.parse(
			"{"
			"	int<4> a = 0; "
			"	a = 4; "
			"}"
		).as<StatementPtr>();

	code = builder.compoundStmt(code, code, builder.compoundStmt(code));

	auto ret = mapVariablesToScopes(NodeAddress(code));

	EXPECT_EQ(3u, ret.size());

	auto it = ret.begin(), end=ret.end();
	EXPECT_EQ(it->second, StatementAddress(code).getAddressOfChild(0));
	++it;

	EXPECT_NE(end, it);
	EXPECT_EQ(it->second, StatementAddress(code).getAddressOfChild(1));
	
	++it;

	EXPECT_NE(end, it);
	EXPECT_EQ(it->second, StatementAddress(code).getAddressOfChild(2).getAddressOfChild(0));
	++it;

	EXPECT_EQ(end, it);
}


TEST(VariableScopeMap, Lambda) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);


	auto addresses = builder.parseAddresses(
			"${ "
			"	int<4> a = 0; "
			"	a = 1; "
			"	${ "
			"		(int<4> a) -> int<4> ${ return a+1; }$ (3);"
			"		a = 4; "
			"	}$ "
			"	a = 6; "
			"}$"
		);

	auto ret = mapVariablesToScopes(NodeAddress(addresses[0]));

	auto it = ret.begin(), end=ret.end();

	//std::cout << *it->first << std::endl;
	//std::cout << *it->second << std::endl;
	EXPECT_EQ(it->second, addresses[0]);
	++it;

	EXPECT_NE(end, it);
	//std::cout << *it->first << std::endl;
	//std::cout << *it->second << std::endl;
	EXPECT_EQ(it->second, addresses[2].getParentAddress()); // because the parser automatically add a compound stmt
	EXPECT_NE(it->second, addresses[1]);
	
	++it;
	EXPECT_EQ(end, it);

}

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

#include "insieme/analysis/dfa/analyses/reaching_defs.h"
#include "insieme/analysis/dfa/analyses/def_use.h"

#include "insieme/analysis/dfa/entity.h"
#include "insieme/analysis/dfa/value.h"
#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/cfg.h"

#include "insieme/analysis/dfa/lattice.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/dfa/analyses/reaching_defs.h"

#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

TEST(Problem, ReachingDefinitions) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

	typedef utils::set::PointerSet<VariablePtr> VarSet; 

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 0;"
		"	if ( (a<=0) ) { "
		"		(a = (int<4>:i+int<4>:b)); "
		"	};"
		"	decl int<4>:c = (op<ref.deref>(a));"
		"}"
    );

    EXPECT_TRUE(code);
	CFGPtr cfg = CFG::buildCFG(code);

	Solver<dfa::analyses::ReachingDefinitions> s(*cfg);
	auto ret = s.solve();
	
	// lookup address of variable A
	NodeAddress aRef = NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2);
	// std::cout << *aRef << std::endl;
	
	const cfg::BlockPtr& b = cfg->find(aRef);
	EXPECT_EQ(2u, b->getBlockID());

	std::vector<core::VariableAddress> addrList;
	 
	for( auto def : ret[b->getBlockID()] ) {
		if (std::get<0>(def) == analyses::makeVarEntity(aRef.as<VariableAddress>())) {
			core::NodeAddress block = (*std::get<1>(def))[0].getStatementAddress();
			core::NodeAddress addr = Address<const Node>::find( aRef, block.getAddressedNode());
			addrList.push_back( core::static_address_cast<core::VariableAddress>(core::concat(block, addr)) );
		}
	}

	// std::cout << addrList << std::endl;
	EXPECT_EQ(2u, addrList.size());
	EXPECT_EQ(addrList[0], NodeAddress(code).getAddressOfChild(0).getAddressOfChild(0));
	EXPECT_EQ(addrList[1], NodeAddress(code).getAddressOfChild(1).getAddressOfChild(1).
							getAddressOfChild(0).getAddressOfChild(2));
}

TEST(Problem, ReachingDefinitions2) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 0;"
		"	if ( (a<=0) ) { "
		"		(a = (int<4>:i+int<4>:b)); "
		"	};"
		"	decl int<4>:c = (op<ref.deref>(a));"
		"}"
    );

    EXPECT_TRUE(code);

	analyses::DefUse du(code);

	VariableAddress aRef = core::static_address_cast<const core::Variable>( 
			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2)
		);
	
	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));
	
	auto comp = [](const ExpressionAddress& lhs, const ExpressionAddress& rhs) -> bool { 
		assert(lhs.getRootNode() == rhs.getRootNode());
		return lhs.getPath() < rhs.getPath();
	};

	std::set<ExpressionAddress, decltype(comp)> definitions(du.defs_begin(aRef), du.defs_end(aRef), comp);
	
	auto it = definitions.begin(), end = definitions.end();

	{
		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
			NodeAddress(code).getAddressOfChild(0).getAddressOfChild(0)
		);

		EXPECT_EQ(trgRef, *it);
		EXPECT_EQ(*trgRef, **it);
	}
	++it;
	{
		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
			NodeAddress(code).getAddressOfChild(1).getAddressOfChild(1).getAddressOfChild(0).getAddressOfChild(2)
		);
	
		EXPECT_EQ(trgRef, *it);
		EXPECT_EQ(*trgRef, **it);
	}
	++it;

	EXPECT_EQ(end,it);
}


TEST(Problem, ReachingDefinitions3) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<int<4>>:a = 0;"
		"	if ( (a<=0) ) { "
		"		(a = (int<4>:i+int<4>:b)); "
		"	} else {"
		"		(a = int<4>:b);"
		"   };"
		"	decl int<4>:c = (op<ref.deref>(a));"
		"}"
    );

    EXPECT_TRUE(code);

	analyses::DefUse du(code);

	VariableAddress aRef = core::static_address_cast<const core::Variable>( 
			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1).getAddressOfChild(2)
		);
	
	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));

	analyses::DefUse::iterator it = du.defs_begin(aRef), end = du.defs_end(aRef);

	std::vector<NodeAddress> l { 
		NodeAddress(code).getAddressOfChild(1).getAddressOfChild(1).getAddressOfChild(0).getAddressOfChild(2),
		NodeAddress(code).getAddressOfChild(1).getAddressOfChild(2).getAddressOfChild(0).getAddressOfChild(2)
	};

	auto fit1 = std::find(it, end, l[0]);
	EXPECT_NE(end, fit1);

	auto fit2 = std::find(it, end, l[1]);
	EXPECT_NE(end, fit2);
}


TEST(Problem, ReachingDefinitionsMember) {

	NodeManager mgr;
	parse::IRParser parser(mgr);

    auto code = parser.parseStatement(
		"{"
		"	decl ref<struct<a:int<4>, b:int<4>>>:s=0;"
	//	"	if ( (int<4>:b <= 0) ) { "
		"	((op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>,int>)) = 3);"
	//	"   };"
		"	decl ref<int<4>>:c = (op<composite.ref.elem>(s, lit<identifier,a>, lit<type<int<4>>, int>));"
		"}"
    );

    EXPECT_TRUE(code);

	analyses::DefUse du(code);

	ExpressionAddress aRef = core::static_address_cast<const core::Expression>( 
			NodeAddress(code).getAddressOfChild(2).getAddressOfChild(1)
		);
	
	auto var = analyses::makeVarEntity(aRef);

	EXPECT_EQ(2u, std::distance(du.defs_begin(aRef), du.defs_end(aRef)));

	auto comp = [](const ExpressionAddress& lhs, const ExpressionAddress& rhs) -> bool { 
		assert(lhs.getRootNode() == rhs.getRootNode());
		return lhs.getPath() < rhs.getPath();
	};

	std::set<ExpressionAddress, decltype(comp)> definitions(du.defs_begin(aRef), du.defs_end(aRef), comp);
	
	auto it = definitions.begin(), end = definitions.end();

	{
		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
			NodeAddress(code).getAddressOfChild(0).getAddressOfChild(0)
		);

		EXPECT_EQ(trgRef, *it);
		EXPECT_EQ(*trgRef, **it);
	}
	++it;
	{
		ExpressionAddress trgRef = core::static_address_cast<const core::Expression>( 
			NodeAddress(code).getAddressOfChild(1).getAddressOfChild(2)
		);
	
		EXPECT_EQ(trgRef, *it);
		EXPECT_EQ(*trgRef, **it);
	}
	++it;

	EXPECT_EQ(end,it);

}

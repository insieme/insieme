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

#include "insieme/analysis/dfa/solver.h"
#include "insieme/analysis/mpi/dfa/rank_prop.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statements.h"

#include "insieme/analysis/polyhedral/scop.h"

#include "insieme/core/printer/pretty_printer.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::analysis;
using namespace insieme::analysis::dfa;

typedef dfa::analyses::RankPropagation::value_type AnalysisData;

TEST(RankPropagation, PropagateConstantNoControl) {
 
 	NodeManager mgr;
 	IRBuilder builder(mgr);
 
    auto addresses = builder.parseAddresses(
		"${"
		"	let MPI_Comm_rank = lit(\"MPI_Comm_rank\" : (int, ref<int>) -> int); "
		"	let MPI_Comm_size = lit(\"MPI_Comm_size\" : (int, ref<int>) -> int); "

		"	ref<int<4>> a = 0;"
		"	ref<int<4>> s = 0;"
		"   MPI_Comm_rank(0, a); "
		"	MPI_Comm_size(0, s); "
		"	int<4> c = (a+1)%s;"
		"	int<4> d = c+a*c;"
		"	$d$; "
		"}$"
    );
 
	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

 	Solver<dfa::analyses::RankPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	auto addr = cfg->find( addresses[1] );

 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());
	auto accPtr = getImmediateAccess(mgr, addresses[1]);

	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	LOG(INFO) << consts;

	//auto cons = find_constant_value(accClasses, consts);

	//EXPECT_EQ( builder.intLit(1), std::get<1>(cons).value() );
}


TEST(RankPropagation, PropagateConstantControl) {
 
 	NodeManager mgr;
 	IRBuilder builder(mgr);
 
    auto addresses = builder.parseAddresses(
		"${"
		"	let MPI_Comm_rank = lit(\"MPI_Comm_rank\" : (int, ref<int>) -> int); "
		"	let MPI_Comm_size = lit(\"MPI_Comm_size\" : (int, ref<int>) -> int); "

		"	ref<int<4>> a = 0;"
		"	ref<int<4>> s = 0;"
		"   MPI_Comm_rank(0, a); "
		"	MPI_Comm_size(0, s); "
		"	int<4> c = (a+1)%s;"
		"	ref<int<4>> d = a; "
		"	if (c>0) { "
		"		d = c+a*c;"
		"	}"
		"	$d$; "
		"}$"
    );
 
	EXPECT_EQ(2u, addresses.size());
	CFGPtr cfg = CFG::buildCFG(addresses[0].getAddressedNode());

	LOG(INFO) << *cfg;

 	Solver<dfa::analyses::RankPropagation> s(*cfg);
 	auto ret = s.solve();

 	// lookup address of variable A
 	auto addr = cfg->find( addresses[1] );

 	EXPECT_EQ(2u, addr.getBlockPtr()->getBlockID());
	auto accPtr = getImmediateAccess(mgr, addresses[1]);
	LOG(INFO) << s.getProblemInstance().getAccessManager();

	auto accClasses = s.getProblemInstance().getAccessManager().getClassFor(accPtr);
	assert( !accClasses.empty() );

	auto consts = ret[addr.getBlockPtr()->getBlockID()];
	LOG(INFO) << consts;

	//auto cons = find_constant_value(accClasses, consts);

	//EXPECT_EQ( builder.intLit(1), std::get<1>(cons).value() );
}

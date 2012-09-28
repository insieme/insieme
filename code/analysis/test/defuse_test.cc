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

#include "insieme/analysis/defuse_collect.h"
#include "insieme/core/ir_builder.h"

using namespace insieme::core;
using namespace insieme::analysis;

TEST(DefUseCollect, Scalar) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);
	
	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));
	symbols["b"] = builder.variable(builder.parseType("ref<int<4>>"));
	symbols["c"] = builder.variable(builder.parseType("ref<int<4>>"));

	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
    auto compStmt = builder.parseStmt(
		"{ "
		"	a+b*c; "
		"} ", symbols
	);

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(3u, refs.size());

	// all the refs are usages 
	std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ 
			EXPECT_TRUE(cur->getUsage() == Ref::USE);
		});

}

TEST(DefUseCollect, SimpleArray) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["v"] = builder.variable(builder.parseType("ref<vector<int<4>,4>>"));
	symbols["a"] = builder.variable(builder.parseType("int<4>"));
	symbols["b"] = builder.variable(builder.parseType("ref<int<4>>"));

	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
	auto compStmt = builder.parseStmt(
		"{ "
		"	v[a+b]; "
		"} ", symbols
	);
	// std::cout << *compStmt << std::endl;

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(2u, refs.size());

	// all the refs are usages 
	std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ 
			EXPECT_TRUE(cur->getUsage() == Ref::USE);
			if (cur->getType() == Ref::ARRAY) {
				EXPECT_EQ(1u, static_cast<ArrayRef&>(*cur).getIndexExpressions().size());
			} else {
				EXPECT_TRUE(cur->getType() == Ref::SCALAR);
			}
		});

}

TEST(DefUseCollect, Assignment) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<int<4>>"));
	symbols["c"] = builder.variable(builder.parseType("int<4>"));

	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
	auto compStmt = builder.parseStmt(
		"{ "
		"	a = c; "
		"} ", symbols
	);

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(1u, refs.size());
	const Ref& ref = **refs.begin();
	EXPECT_TRUE(ref.getUsage() == Ref::DEF);
	
	EXPECT_TRUE(ref.getType() == Ref::SCALAR);

}

TEST(DefUseCollect, ArrayAccess) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["c"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));

	// even if the expression is completely wrong (because it works with refs), still valid as a
	// test case 
	auto compStmt =builder.parseStmt(
		"{ "
		"	a[c[b]]; "
		"} ", symbols
	);
	// std::cout << *compStmt << std::endl;

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(2u, refs.size());

	for_each(refs.arrays_begin(), refs.arrays_end(),
		[](const RefPtr& cur) {
			EXPECT_TRUE(cur->getUsage() == Ref::USE);
		}
	);

}

TEST(DefUseCollect, ArrayAssignment) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["c"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["d"] = builder.variable(builder.parseType("ref<int<4>>"));

	// even if the expression is completely wrong (because it works with refs), still valid as a
	// test case 
	auto compStmt = builder.parseStmt(
		"{ "
		"	a[c[b]] = d; "
		"}", symbols
	);
	// std::cout << *compStmt << std::endl;

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(3u, refs.size());

	RefList::ref_iterator<ArrayRef> it = refs.arrays_begin(), end = refs.arrays_end();
	EXPECT_TRUE((*it)->getUsage() == Ref::USE);
	++it;
	EXPECT_TRUE(it != end);
	EXPECT_TRUE((*it)->getUsage() == Ref::DEF);
	++it;
	EXPECT_TRUE(it == end);

}

TEST(DefUseCollect, ArrayAssignment2) {
	
	NodeManager mgr;
	IRBuilder builder(mgr);

	std::map<std::string, NodePtr> symbols;
	symbols["a"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["c"] = builder.variable(builder.parseType("ref<vector<int<4>,10>>"));
	symbols["b"] = builder.variable(builder.parseType("int<4>"));
	symbols["d"] = builder.variable(builder.parseType("ref<int<4>>"));

	// even if the expression is completely wrong (because it works with refs), still valid as a
	// test case 
	auto compStmt = builder.parseStmt(
		"{ "
		"	a[c[b]] = d; "
		"} ", symbols
	);
	// std::cout << *compStmt << std::endl;

	RefList&& refs = collectDefUse(compStmt);
	EXPECT_EQ(3u, refs.size());

	RefList::ref_iterator<ArrayRef> it = refs.arrays_begin(), end = refs.arrays_end();
	EXPECT_TRUE((*it)->getUsage() == Ref::USE);
	++it;
	EXPECT_TRUE(it != end);
	EXPECT_TRUE((*it)->getUsage() == Ref::DEF);
	++it;
	EXPECT_TRUE(it == end);

}



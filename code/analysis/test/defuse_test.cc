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

#include "insieme/core/program.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/statements.h"

#include "insieme/core/parser/ir_parse.h"

using namespace insieme::core;
using namespace insieme::analysis;

TEST(DefUseCollect, Scalar) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);
	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
    auto compStmt = parser.parseStatement(
		"{\
			((ref<int<4>>:a+ref<int<4>>:b)*ref<int<4>>:c);\
		}"
	);

	RefSet&& refs = collectDefUse(compStmt);
	EXPECT_EQ(static_cast<size_t>(3), refs.size());

	// all the refs are usages 
	std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ 
			EXPECT_TRUE(cur->getUsage() == Ref::USE);
		});

}

TEST(DefUseCollect, SimpleArray) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);
	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
    auto compStmt = parser.parseStatement(
		"{\
			(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, (int<4>:a+ref<int<4>>:b)));\
		}"
	);
	// std::cout << *compStmt << std::endl;

	RefSet&& refs = collectDefUse(compStmt);
	EXPECT_EQ(static_cast<size_t>(2), refs.size());

	// std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ std::cout << *cur << std::endl; });

	// all the refs are usages 
	std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ 
			EXPECT_TRUE(cur->getUsage() == Ref::USE);
			if (cur->getType() == Ref::ARRAY) {
				EXPECT_EQ(static_cast<size_t>(1), static_cast<ArrayRef&>(*cur).getIndexExpressions().size());
			} else {
				EXPECT_TRUE(cur->getType() == Ref::VAR);
			}
		});

}

TEST(DefUseCollect, Assignment) {
	
	NodeManager mgr;
	parse::IRParser parser(mgr);
	// even if the expression is completely wrong (because it works with refs),
	// still valid as a test case 
    auto compStmt = parser.parseStatement(
		"{\
			(ref<int<4>>:a = int<4>:c);\
		}"
	);
	// std::cout << *compStmt << std::endl;

	RefSet&& refs = collectDefUse(compStmt);
	EXPECT_EQ(static_cast<size_t>(1), refs.size());
	const Ref& ref = **refs.begin();
	EXPECT_TRUE(ref.getUsage() == Ref::DEF);

//	std::for_each(refs.begin(), refs.end(), [](const RefPtr& cur){ std::cout << *cur << std::endl; });
	
	EXPECT_TRUE(ref.getType() == Ref::VAR);

}



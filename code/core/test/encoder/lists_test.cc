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

#include <boost/lexical_cast.hpp>

#include "insieme/core/encoder/lists.h"

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {

TEST(Lists, languageExtension) {

	NodeManager manager;
	const ListExtension& ext = manager.getLangExtension<ListExtension>();

	EXPECT_EQ("(('a,list<'a>)->list<'a>)", toString(*ext.cons->getType()));
	EXPECT_EQ("((type<'a>)->list<'a>)", toString(*ext.empty->getType()));

}


TEST(Lists, listConversion) {

	NodeManager manager;

	// create a list

	vector<int> list = toVector(1,2,3);
	core::ExpressionPtr irList = toIR(manager, list);
	vector<int> back = toValue<vector<int>>(irList);

	EXPECT_EQ("[1,2,3]", toString(list));
	EXPECT_EQ("cons(1, cons(2, cons(3, empty(int<4>))))", toString(*irList));

	EXPECT_TRUE(isEncodingOf<vector<int>>(irList));
	EXPECT_EQ(list, back);

	EXPECT_EQ("[]", toString(check(irList, checks::getFullCheck())));


	// test another type
	EXPECT_EQ("cons(3.75, cons(1.47, empty(real<8>)))", toString(*toIR(manager, toVector<double>(3.75, 1.47))));

}


} // end namespace lists
} // end namespace core
} // end namespace insieme


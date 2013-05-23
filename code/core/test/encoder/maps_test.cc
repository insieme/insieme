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

#include <utility>

#include "insieme/core/encoder/maps.h"

#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme {
namespace core {
namespace encoder {


TEST(Maps, MapsConversion) {

	NodeManager manager;

	// create a pair

	map<string, int> value;

	core::ExpressionPtr ir = toIR(manager, value);
	auto back = toValue<map<string,int>>(ir);

	EXPECT_EQ("{}", toString(value));
	EXPECT_EQ("empty(pair<ref<array<char,1>>,int<4>>)", toString(*ir));


	EXPECT_TRUE((isEncodingOf<map<string,int>>(ir)));
	EXPECT_TRUE((isEncodingOf<vector<pair<string,int>>>(ir)));
	EXPECT_FALSE((isEncodingOf<pair<int,int>>(ir)));
	EXPECT_EQ(value, back);
	EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));


	// fill the map with something

	value["hello"] = 12;
	value["world"] = 14;

	ir = toIR(manager, value);
	back = toValue<map<string,int>>(ir);

	EXPECT_EQ("{hello=12, world=14}", toString(value));
	EXPECT_EQ("cons(pair(hello, 12), cons(pair(world, 14), empty(pair<ref<array<char,1>>,int<4>>)))", toString(*ir));

	EXPECT_TRUE((isEncodingOf<map<string,int>>(ir)));
	EXPECT_EQ(value, back);

	EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));

}


} // end namespace lists
} // end namespace core
} // end namespace insieme


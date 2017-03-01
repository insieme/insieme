/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
		auto back = toValue<map<string, int>>(ir);

		EXPECT_EQ("{}", toString(value));
		EXPECT_EQ("list_empty(type<pair<ref<array<char,inf>,f,f,plain>,int<4>>>)", toString(*ir));


		EXPECT_TRUE((isEncodingOf<map<string, int>>(ir)));
		EXPECT_TRUE((isEncodingOf<vector<pair<string, int>>>(ir)));
		EXPECT_FALSE((isEncodingOf<pair<int, int>>(ir)));
		EXPECT_EQ(value, back);
		EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));


		// fill the map with something

		value["hello"] = 12;
		value["world"] = 14;

		ir = toIR(manager, value);
		back = toValue<map<string, int>>(ir);

		EXPECT_EQ("{hello=12, world=14}", toString(value));
		EXPECT_EQ("list_cons(pair(hello, 12), list_cons(pair(world, 14), list_empty(type<pair<ref<array<char,inf>,f,f,plain>,int<4>>>)))", toString(*ir));

		EXPECT_TRUE((isEncodingOf<map<string, int>>(ir)));
		EXPECT_EQ(value, back);

		EXPECT_EQ("[]", toString(check(ir, checks::getFullCheck())));
	}


} // end namespace lists
} // end namespace core
} // end namespace insieme

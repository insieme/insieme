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

#include "insieme/core/inspyer/inspyer.h"

#include <sstream>

#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/json_parser.hpp>

#include "insieme/core/ir_address.h"
#include "insieme/core/ir_builder.h"

using namespace std;
using namespace boost::property_tree;

namespace insieme {
namespace core {
namespace inspyer {


	TEST(InspyerMeta, Base) {
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1>>"));
		NodePtr code = builder.parseStmt("{ "
			"	for(uint<4> i = 10u .. 50u) { "
			"		v[i]; "
			"	} "
			"	for(uint<4> j = 5u .. 25u) { "
			"		v[j]; "
			"	} "
			"}",
			symbols);

		NodeAddress root(code);
		MetaGenerator meta(code);
		meta.addBookmark(root.getAddressOfChild(1, 3));
		meta.addExpand(root.getAddressOfChild(1, 3));
		meta.addHighlight(root.getAddressOfChild(1, 3));
		meta.addLabel(root.getAddressOfChild(1, 3), "my label");
		meta.addBody(root.getAddressOfChild(1, 3), "my body");

		stringstream buffer;
		meta.dump(buffer);

		ptree load;
		read_json(buffer, load);

		ASSERT_STREQ("0-1-3", load.get_child("bookmarks").back().second.get_value<string>().c_str());
		ASSERT_STREQ("0-1-3", load.get_child("expands").back().second.get_value<string>().c_str());
		ASSERT_STREQ("0-1-3", load.get_child("highlights").back().second.get_value<string>().c_str());
		ASSERT_STREQ("0-1-3", load.get_child("labels").back().first.c_str());
		ASSERT_STREQ("my label", load.get_child("labels").back().second.get_value<string>().c_str());
		ASSERT_STREQ("0-1-3", load.get_child("bodies").back().first.c_str());
		ASSERT_STREQ("my body", load.get_child("bodies").back().second.get_value<string>().c_str());
	}

} // end namespace inspyer
} // end namespace core
} // end namespace insieme

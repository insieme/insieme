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
 */

#include <gtest/gtest.h>

#include <sstream>

#include "insieme/driver/integration/properties.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace driver {
namespace integration {


	TEST(Properties, Basic) {
		Properties p;

		EXPECT_EQ("Properties { }\n", toString(p));


		EXPECT_EQ("", p.get(""));
		EXPECT_EQ("", p.get("a"));
		EXPECT_EQ("", p.get("a", "b"));

		p.set("a", "v1");
		EXPECT_EQ("Properties {\n\ta=v1\n}\n", toString(p));

		EXPECT_EQ("", p.get(""));
		EXPECT_EQ("v1", p.get("a"));
		EXPECT_EQ("v1", p.get("a", "b"));
		EXPECT_EQ("v1", p.get("a", "c"));
		EXPECT_EQ("", p.get("x"));

		p.set("a", "b", "v2");
		EXPECT_EQ("Properties {\n\ta=v1\n\ta[b]=v2\n}\n", toString(p));

		EXPECT_EQ("", p.get(""));
		EXPECT_EQ("v1", p.get("a"));
		EXPECT_EQ("v2", p.get("a", "b"));
		EXPECT_EQ("v1", p.get("a", "c"));
		EXPECT_EQ("", p.get("x"));

		p.set("a", "c", "v3");
		EXPECT_EQ("Properties {\n\ta=v1\n\ta[b]=v2\n\ta[c]=v3\n}\n", toString(p));

		EXPECT_EQ("", p.get(""));
		EXPECT_EQ("v1", p.get("a"));
		EXPECT_EQ("v2", p.get("a", "b"));
		EXPECT_EQ("v3", p.get("a", "c"));
		EXPECT_EQ("", p.get("x"));
	}

	TEST(Properties, Getter) {
		Properties p;
		p.set("a", "5");
		p.set("b", "1,2,3,2,1");
		p.set("c", "1,2,3,,2,1");

		EXPECT_EQ("5", p.get("a"));
		EXPECT_EQ("1,2,3,2,1", p.get("b"));
		EXPECT_EQ("1,2,3,,2,1", p.get("c"));

		EXPECT_EQ(5, p.get<int>("a"));
		EXPECT_THROW(p.get<int>("b"), boost::bad_lexical_cast);

		EXPECT_EQ(toVector(5), p.get<vector<int>>("a"));
		EXPECT_EQ(std::set<int>({5}), p.get<set<int>>("a"));

		EXPECT_EQ(toVector(1, 2, 3, 2, 1), p.get<vector<int>>("b"));
		EXPECT_EQ(std::set<int>({1, 2, 3}), p.get<set<int>>("b"));

		EXPECT_EQ(toVector(1, 2, 3, 2, 1), p.get<vector<int>>("c"));
		EXPECT_EQ(std::set<int>({1, 2, 3}), p.get<set<int>>("c"));
	}

	TEST(Properties, VarSubstitute) {
		Properties p;
		p.set("a", "x");
		p.set("b", "y");

		EXPECT_EQ("test", p.mapVars("test"));
		EXPECT_EQ("testx", p.mapVars("test${a}"));
		EXPECT_EQ("testy", p.mapVars("test${b}"));
		EXPECT_EQ("testxsdydsxdf", p.mapVars("test${a}sd${b}ds${a}df"));
	}

	TEST(Properties, Merge) {
		{
			// a simple case
			Properties p1;
			p1.set("a", "v1");

			Properties p2;
			p2.set("b", "v2");

			auto r = p1 << p2;
			EXPECT_EQ(2, r.size());
			EXPECT_EQ("v1", r.get("a")) << r;
			EXPECT_EQ("v2", r.get("b")) << r;
		}

		{
			// a case with collisions
			Properties p1;
			p1.set("a", "v1");

			Properties p2;
			p2.set("a", "v2");

			auto r = p1 << p2;
			EXPECT_EQ(1, r.size());
			EXPECT_EQ("v2", r.get("a")) << r;
		}

		{
			// a case with a category
			Properties p1;
			p1.set("a", "v1");

			Properties p2;
			p2.set("a", "c1", "v2");

			auto r = p1 << p2;
			EXPECT_EQ(1, r.size());
			EXPECT_EQ("v1", r.get("a")) << r;
			EXPECT_EQ("v2", r.get("a", "c1")) << r;
		}

		{
			// a case with variable substitution
			Properties p1;
			p1.set("a", "v1");
			p1.set("a", "c", "v2");

			Properties p2;
			p2.set("a", "x ${a} ${a[c]}");

			auto r = p1 << p2;
			EXPECT_EQ(1, r.size());
			EXPECT_EQ("x v1 v2", r.get("a")) << r;
		}
	}

	TEST(PropertyView, Merge) {
		Properties p;
		p.set("a", "v1");
		p.set("a", "c1", "v2");
		p.set("a", "c2", "v3");

		p.set("b", "v4");
		p.set("b", "c1", "v5");

		p.set("c", "v6");

		// create views
		auto view1 = p.getView("c1");
		auto view2 = p.getView("c2");
		auto view3 = p.getView("c3");

		// test views
		EXPECT_EQ("v2", view1["a"]);
		EXPECT_EQ("v5", view1["b"]);
		EXPECT_EQ("v6", view1["c"]);
		EXPECT_EQ("", view1["d"]);

		EXPECT_EQ("v3", view2["a"]);
		EXPECT_EQ("v4", view2["b"]);
		EXPECT_EQ("v6", view2["c"]);
		EXPECT_EQ("", view2["d"]);

		EXPECT_EQ("v1", view3["a"]);
		EXPECT_EQ("v4", view3["b"]);
		EXPECT_EQ("v6", view3["c"]);
		EXPECT_EQ("", view3["d"]);

		// and test print operation
		EXPECT_EQ("Properties {\n\ta=v2\n\tb=v5\n\tc=v6\n}\n", toString(view1));
		EXPECT_EQ("Properties {\n\ta=v3\n\tb=v4\n\tc=v6\n}\n", toString(view2));
		EXPECT_EQ("Properties {\n\ta=v1\n\tb=v4\n\tc=v6\n}\n", toString(view3));
	}


	TEST(Properties, IO) {
		std::stringstream buffer;

		Properties p;
		p.set("a", "v1");
		p.set("a", "c1", "v2");
		p.set("b", "v3");

		// safe stuff in buffer
		p.store(buffer);


		// load stuff from buffer
		Properties p2 = Properties::load(buffer);

		EXPECT_EQ(p, p2);
	}

	TEST(Properties, IO_empty) {
		std::stringstream buffer;

		Properties p;

		// safe stuff in buffer
		p.store(buffer);

		// load stuff from buffer
		Properties p2 = Properties::load(buffer);

		EXPECT_EQ(p, p2);
	}


} // end namespace integration
} // end namespace driver
} // end namespace insieme

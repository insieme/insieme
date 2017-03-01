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

#include <string>

#include "insieme/utils/difference_constraints.h"
#include "insieme/utils/set_utils.h"

namespace insieme {
namespace utils {

	TEST(DifferenceConstraints, Basic) {

		DifferenceConstraints<std::string> c;
		EXPECT_EQ("{}", toString(c));

		c.addConstraint("X","Y",2);
		EXPECT_EQ("{X-Y=2}", toString(c));

		c.addConstraint("X","Z",-2);
		EXPECT_EQ("{X-Y=2,Z-X=2}", toString(c));

		c.markUnsatisfiable();
		EXPECT_EQ("-unsatisfiable-", toString(c));

	}

	TEST(DifferenceConstraints, Solve) {

		DifferenceConstraints<std::string> c;
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{}",toString(c.solve()));

		c.addConstraint("X","Y",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{X=2,Y=0}",toString(c.solve()));

		c.addConstraint("Y","X",2);
		EXPECT_FALSE(c.solve());
		EXPECT_EQ("-invalid-",toString(c.solve()));

	}

	TEST(DifferenceConstraints, Solve_Transitive) {

		DifferenceConstraints<std::string> c;
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{}",toString(c.solve()));

		c.addConstraint("X","Y",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{X=2,Y=0}",toString(c.solve()));

		c.addConstraint("Y","Z",1);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{X=3,Y=1,Z=0}",toString(c.solve()));

		c.addConstraint("Z","W",4);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{W=0,X=7,Y=5,Z=4}",toString(c.solve()));

	}

	TEST(DifferenceConstraints, Solve_Minimum) {

		DifferenceConstraints<std::string> c;
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{}",toString(c.solve()));

		c.addConstraint("X","Y",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{X=2,Y=0}",toString(c.solve()));

		c.addConstraint("X","Z",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{X=2,Y=0,Z=0}",toString(c.solve()));

		c.addConstraint("X","W",4);
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{W=0,X=4,Y=2,Z=2}",toString(c.solve()));

	}


	TEST(DifferenceConstraints, Solve_Minimum_2) {

		DifferenceConstraints<std::string> c;
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{}",toString(c.solve()));

		c.addConstraint("W","X",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{W=2,X=0}",toString(c.solve()));

		c.addConstraint("W","Y",2);
		EXPECT_TRUE(c.solve());
		EXPECT_EQ("{W=2,X=0,Y=0}",toString(c.solve()));

		c.addConstraint("W","Z",4);
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{W=4,X=2,Y=2,Z=0}",toString(c.solve()));

	}

	TEST(DifferenceConstraints, Solve_Components) {

		DifferenceConstraints<std::string> c;
		c.addConstraint("W","X",2);
		c.addConstraint("X","Z",1);
		c.addConstraint("R","S",1);

		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{R=1,S=0,W=3,X=1,Z=0}",toString(c.solve()));

	}

	TEST(DifferenceConstraints, Unsatisfiable_Constraints) {

		DifferenceConstraints<std::string> c;
		c.addConstraint("W","X",1);
		c.addConstraint("X","Y",1);
		c.addConstraint("Y","Z",1);
		c.addConstraint("Z","W",1);

		EXPECT_FALSE(c.solve());
	}


	TEST(DifferenceConstraints, FixedValues_1) {

		DifferenceConstraints<std::string> c;
		c.addConstraint("X",5);
		EXPECT_EQ("{X=5}",toString(c));
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{X=5}",toString(c.solve()));

		c.addConstraint("Y",7);
		EXPECT_EQ("{X=5,Y=7}",toString(c));
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{X=5,Y=7}",toString(c.solve()));

		c.addConstraint("Y","X",2);
		c.addConstraint("Z","Y",1);
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{X=5,Y=7,Z=8}",toString(c.solve()));

		c.addConstraint("X","Y",1);
		EXPECT_FALSE(c.solve()) << c;

	}


	TEST(DifferenceConstraints, FixedValues_2) {

		DifferenceConstraints<std::string> c;
		c.addConstraint("X",5);
		c.addConstraint("Y",7);
		EXPECT_EQ("{X=5,Y=7}",toString(c));
		EXPECT_TRUE(c.solve()) << c;
		EXPECT_EQ("{X=5,Y=7}",toString(c.solve()));

		c.addConstraint("X","Y",1);
		EXPECT_FALSE(c.solve()) << c << " => " << c.solve();

	}


	TEST(DifferenceConstraints, FixedValues_3) {

		DifferenceConstraints<std::string> c;
		c.addConstraint("X",5);
		c.addConstraint("X","Y",1);
		c.addConstraint("Y","Z",1);
		c.addConstraint("Z",5);
		EXPECT_EQ("{X=5,Z=5,X-Y=1,Y-Z=1}",toString(c));
		EXPECT_FALSE(c.solve()) << c;

	}

} // end namespace utils
} // end namespace insieme

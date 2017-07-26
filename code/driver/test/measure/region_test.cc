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

#include <boost/filesystem.hpp>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/analysis/region/for_selector.h"
#include "insieme/core/analysis/region/fun_call_selector.h"
#include "insieme/core/analysis/region/mpi_selector.h"
#include "insieme/frontend/frontend.h"
#include "insieme/utils/config.h"

namespace insieme {
namespace driver {
namespace measure {

	using namespace std;
	using namespace core;

	TEST(Region, Loops) {
		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt("{"
			                                  "	 for(int<4> i = 10 .. 50 : 1) { "
			                                  "    for(int<4> j = 10 .. 50 : 1) { "
			                                  "    }; "
			                                  "  }; "
			                                  "}");

		ASSERT_TRUE(stmt);

		analysis::region::ForSelector forSelector;

		analysis::region::RegionList regionList = forSelector.getRegions(NodeAddress(stmt));

		EXPECT_EQ(2, regionList.size());

	}

	TEST(Region, FunctionCalls) {
		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);
		StatementPtr stmt = builder.parseStmt(""
			                                  "def some_function = (n : int<4>)->int<4> {"
			                                  "	var ref<int<4>> m = n;"
			                                  "	return n;"
			                                  "};"
			                                  "{"
			                                  "some_function(10);"
			                                  "lit(\"some_other_function\": ()->int<4>)();"
			                                  "}");

		ASSERT_TRUE(stmt);

		analysis::region::FunctionCallSelector funSelector("some_function");
		analysis::region::RegionList regionList = funSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(1, regionList.size());

		analysis::region::FunctionCallSelector otherSelector("some_other_function");
		regionList = otherSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(1, regionList.size());

		analysis::region::FunctionCallSelector allSelector("some.*function");
		regionList = allSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(2, regionList.size());
	}

	TEST(Region, FunWithLoops) {
		// create a small example code fragment
		NodeManager manager;
		IRBuilder builder(manager);

		StatementPtr stmt = builder.parseStmt(""
			                                  "def some_function = (n : int<4>)->int<4> {"
			                                  "	var ref<int<4>> m = n;"
			                                  "	return n;"
			                                  "};"
			                                  "def some_other_function = (n : int<4>)->int<4> {"
			                                  "	var ref<int<4>> m = n;"
			                                  "	return n;"
			                                  "};"
			                                  "{"
			                                  "	var ref<int<4>> res = some_function(10);"
			                                  "	$for(int<4> i = 0 .. 50) {"
			                                  "		$for(int<4> j = 0 .. 70) {"
			                                  "			res = res + some_other_function(100000);"
			                                  "		}$"
			                                  "		$for(int<4> k = 0 .. 60) {"
			                                  "			$for(int<4> l = 0 .. 100) {"
			                                  "				res = some_function(*res);"
			                                  //"				res = lit(\"some_function\": (int<4>)->int<4>)(*res);"
			                                  "			}$"
			                                  "		}$"
			                                  "	}$"
			                                  "}");

		ASSERT_TRUE(stmt);

		analysis::region::ForSelector forSelector;
		analysis::region::RegionList forList = forSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(4, forList.size());

		analysis::region::FunctionCallSelector someSelector = analysis::region::FunctionCallSelector("some_function");
		analysis::region::RegionList someList = someSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(2, someList.size());

		analysis::region::FunctionCallSelector otherSelector = analysis::region::FunctionCallSelector("some_other_function");
		analysis::region::RegionList otherList = otherSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(1, otherList.size());

		analysis::region::FunctionCallSelector allSelector = analysis::region::FunctionCallSelector("some.*function");
		analysis::region::RegionList allList = allSelector.getRegions(NodeAddress(stmt));
		EXPECT_EQ(3, allList.size());

	}

	TEST(Region, MPIExample) {
		NodeManager manager;
		insieme::frontend::ConversionJob job(insieme::utils::getInsiemeSourceRootDir() + "driver/test/inputs/mpi.c");
		job.registerDefaultExtensions();
		ProgramPtr program = job.execute(manager);

		ASSERT_TRUE(program);

		analysis::region::ForSelector forSelector;
		analysis::region::RegionList forList = forSelector.getRegions(NodeAddress(program));
		EXPECT_EQ(1, forList.size());

		analysis::region::MPISelector mpiSelector;
		analysis::region::RegionList mpiList = mpiSelector.getRegions(NodeAddress(program));
		ASSERT_EQ(10, mpiList.size());

		//std::cout << mpiList << "\n";

		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-0", toString(mpiList[0].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-2", toString(mpiList[0].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-3", toString(mpiList[1].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-4", toString(mpiList[1].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-5", toString(mpiList[2].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-7", toString(mpiList[2].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-8", toString(mpiList[3].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-8", toString(mpiList[3].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-9", toString(mpiList[4].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-0-1-9", toString(mpiList[4].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-0", toString(mpiList[5].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-1", toString(mpiList[5].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-2", toString(mpiList[6].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-4", toString(mpiList[6].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-5", toString(mpiList[7].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-6", toString(mpiList[7].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-7", toString(mpiList[8].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-7", toString(mpiList[8].getEnd()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-8", toString(mpiList[9].getBegin()));
		EXPECT_EQ("0-0-2-0-1-2-8-0-3-0-1-1-8", toString(mpiList[9].getEnd()));
	}

} // end namespace measure
} // end namespace driver
} // end namespace insieme

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

#include <cstdlib>

#include "insieme/driver/measure/measure.h"
#include "insieme/utils/logging.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

namespace insieme {
namespace driver {
namespace measure {


	using namespace core;

	TEST(Measuring, Metrics) {
		Logger::setLevel(WARNING);

		// play a little using units
		auto time = Metric::TOTAL_EXEC_TIME_NS;

		EXPECT_EQ(time, Metric::TOTAL_EXEC_TIME_NS);
		EXPECT_NE(time, Metric::TIMESTAMP_END_NS);

		EXPECT_EQ("total_exec_time[ns]", toString(time));

		EXPECT_EQ(Metric::TOTAL_EXEC_TIME_NS, Metric::getForName(Metric::TOTAL_EXEC_TIME_NS->getName()));
	}


	TEST(Measuring, Measure) {
		Logger::setLevel(WARNING);

		// create a small example code fragment
		NodeManager manager;
		StatementPtr stmt = parse::parseStatement(manager,"{"
			"decl ref<int<4>>:sum = (op<ref.var>(0));"
			"for(decl uint<4>:i = 10 .. 50 : 1) {"
			"	(sum = ((op<ref.deref>(sum))+1));"
			"};}");

		EXPECT_TRUE(stmt);

		StatementAddress addr(stmt);

		// measure execution time of this fragment
		auto time = measure(addr, Metric::TOTAL_EXEC_TIME_NS);

		EXPECT_TRUE(time.isValid());
		EXPECT_TRUE(time > 0 * s) << "Actual time: " << time;
	}


} // end namespace measure
} // end namespace driver
} // end namespace insieme

/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include "insieme/analysis/datalog/integer_analysis.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace datalog {

	using namespace core;

	using SymbolTable = std::map<std::string,core::NodePtr>;

	IRBuilder& getBuilder() {
		static NodeManager mgr;
		static IRBuilder builder(mgr);
		return builder;
	}

	bool isInt32(const std::string &str) {
		auto &builder = getBuilder();
		auto type = builder.parseType("int<4>");
		auto lit = builder.literal(type, str);
		auto &opt_num = lit->template getValueAs<int32_t>();
		return !!opt_num;
	}

	int32_t getAsInt32(const std::string &str) {
		auto &builder = getBuilder();
		auto type = builder.parseType("int<4>");
		auto lit = builder.literal(type, str);
		auto &opt_num = lit->template getValueAs<int32_t>();
		assert(opt_num);
		return *opt_num;
	}

	TEST(IntegerAnalysis, Int32Check) {
		EXPECT_TRUE(isInt32("-10"));
		EXPECT_TRUE(isInt32("-1"));
		EXPECT_TRUE(isInt32("0"));
		EXPECT_TRUE(isInt32("1"));
		EXPECT_TRUE(isInt32("1234"));
		EXPECT_TRUE(isInt32("-2147483648")); /* int32 min */
		EXPECT_TRUE(isInt32("2147483647"));  /* int32 max */

		const std::string str = "267100722";
		printf("Resulting number from string '%s': %d\n", str.c_str(), getAsInt32(str));

		EXPECT_FALSE(isInt32(""));            /* empty */
		EXPECT_FALSE(isInt32(" "));
		EXPECT_FALSE(isInt32("hello world")); /* string */
		EXPECT_FALSE(isInt32("a"));
		EXPECT_FALSE(isInt32("1a"));          /* num is part of string */
		EXPECT_FALSE(isInt32("a1"));
		EXPECT_FALSE(isInt32(" 1"));
		EXPECT_FALSE(isInt32("1 "));
		EXPECT_FALSE(isInt32(" 1 "));
		EXPECT_FALSE(isInt32("1."));         /* floating points */
		EXPECT_FALSE(isInt32("1.0"));
		EXPECT_FALSE(isInt32("1.337"));
	}

	TEST(IntegerAnalysis, DISABLED_Int32OverflowDetection) {
		EXPECT_FALSE(isInt32("-2147483649")); /* invalid value range - int32 min - 1*/
		EXPECT_FALSE(isInt32("2147483648"));  /* int32 max + 1 */
		EXPECT_FALSE(isInt32("-4294967296")); /* int32 min << 2 */
		EXPECT_FALSE(isInt32("4294967294"));  /* int32 max << 2 */
	}

} // end namespace datalog
} // end namespace analysis
} // end namespace insieme


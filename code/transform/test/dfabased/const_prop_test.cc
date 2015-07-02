/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/ir_builder.h"
#include "insieme/transform/dfabased/const_prop.h"
#include "insieme/transform/dfabased/dead_variables.h"
#include "insieme/utils/logging.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/transform/simplify.h"

namespace insieme {
namespace transform {

	using namespace core;

	TEST(ConstProp, Simple) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	decl int<4> b = a+2; "
			"	a = b+a;"
			"	*a;"
			"}"
		);

		NodePtr ret = doConstProp(mgr,code);

		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = 10; "
				"int<4> v2 = int_add(10, 2); "
				"ref_assign(v1, 22); "
				"22;"
			"}", toString(*ret));

		ret = removeDeadVariables(mgr, ret);

		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl int<4> v2 = undefined(type<int<4>>);{ };22;}",
				toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));

		ret = insieme::core::transform::simplify(mgr, ret);

		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl int<4> v2 = undefined(type<int<4>>);22;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
	}


	TEST(ConstProp, Liveness) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	decl ref<int<4>> c = 20; "
			"	if (a < c) { "
			"		decl int<4> b = a+2; "
			"		a = b+a;"
			"	}"
			"	c = *a;"
			"	*c; "
			"}"
		);

		NodePtr ret = doConstProp(mgr,code);

		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = 10; "
				"ref<int<4>> v2 = 20; "
				"if(int_lt(10, 20)) {"
					"int<4> v3 = int_add(10, 2); "
					"ref_assign(v1, 22);"
				"} else {}; "
				"ref_assign(v2, ref_deref(v1)); "
				"ref_deref(v2);"
			"}", toString(*ret));

		ret = doConstProp(mgr,insieme::core::transform::simplify(mgr, ret));

		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = 10; "
				"ref<int<4>> v2 = 20; "
				"{"
					"int<4> v3 = 12; "
					"ref_assign(v1, 22);"
				"}; "
				"ref_assign(v2, 22); "
				"22;"
			"}", toString(*ret));

		ret = removeDeadVariables(mgr, ret);

		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl ref<int<4>> v2 = ( var(undefined(type<int<4>>)));{decl int<4> v3 = undefined(type<int<4>>);{ };};{ };22;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));

		ret = insieme::core::transform::simplify(mgr, ret);

		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl ref<int<4>> v2 = ( var(undefined(type<int<4>>)));{decl int<4> v3 = undefined(type<int<4>>);};22;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
	}


//	TEST(ConstProp, Vector) {
//
//		NodeManager mgr;
//		IRBuilder builder(mgr);
//
//		std::map<std::string, core::NodePtr> symbols;
//		symbols["v"] = builder.variable(
//			builder.parseType("ref<vector<int<4>,10>>")
//		);
//
//		auto code = builder.parse(
//			"{"
//			"	v[2u] = 10; "
//			"	v[3u] = 3; "
//			"	v[1u] = v[2u] + v[3u]; "
//			"	v[2u] = v[1u]; "
//			"	int<4> a = v[2u]; "
//			"	a; "
//			"}", symbols
//		);
//
//		NodePtr ret = doConstProp(mgr,code);
//
//		EXPECT_EQ(
//			"{"
//				"ref_assign(vector.ref_elem(v1, 2u), 10); "
//				"ref_assign(vector.ref_elem(v1, 3u), 3); "
//				"ref_assign(vector.ref_elem(v1, 1u), 13); "
//				"ref_assign(vector.ref_elem(v1, 2u), 13); "
//				"int<4> v2 = 13; "
//				"13;"
//			"}", toString(*ret));
//
//		ret = removeDeadVariables(mgr, ret);
//
//		EXPECT_EQ(
//			"{"
//				"{}; "
//				"{}; "
//				"{}; "
//				"{}; "
//				"int<4> v2 = undefined(int<4>); "
//				"13;"
//			"}", toString(*ret));
//
//		ret = insieme::core::transform::simplify(mgr, ret);
//
//		EXPECT_EQ("{int<4> v2 = undefined(int<4>); 13;}",toString(*ret));
//
//	}

} // end transform namespace 
} // end insieme namespace 

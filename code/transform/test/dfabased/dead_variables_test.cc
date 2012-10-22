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

#include "insieme/core/ir_builder.h"
#include "insieme/transform/dfabased/dead_variables.h"
#include "insieme/utils/logging.h"

#include "insieme/core/transform/simplify.h"

namespace insieme {
namespace transform {

	using namespace core;

	TEST(DeadAssignments, Scalars) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	ref<int<4>> a = 10; "
			"	ref<int<4>> c = 20; "
			"	if (a < c) { "
			"		int<4> b = a+2; "
			"		a = b+3; "
			"		a = b+3;"
			"	}"
			"	c = a;"
			"	c; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = 10; "
				"ref<int<4>> v2 = 20; "
				"if(int.lt(ref.deref(v1), ref.deref(v2))) {"
					"int<4> v3 = int.add(ref.deref(v1), 2); "
					"{}; "
					"ref.assign(v1, int.add(v3, 3));"
				"} else {}; "
				"ref.assign(v2, v1); "
				"v2;"
			"}"
			, toString(*ret));

	}

	TEST(DeadAssignments, ScalarsTransitive) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	ref<int<4>> a = 10; "
			"	ref<int<4>> b = 10; "
			"	ref<int<4>> c = 20; "
			"	a = c;"
			"	c = b;"
			"	b = a;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = ref.var(undefined(int<4>)); "
				"ref<int<4>> v2 = ref.var(undefined(int<4>)); "
				"ref<int<4>> v3 = 20; "
				"ref.assign(v1, v3); "
				"{}; "
				"{}; "
				"v1;"
			"}"
			, toString(*ret));

	}

	TEST(DeadAssignments, ScalarsWhileControl) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	ref<int<4>> a = 10; "
			"	ref<int<4>> i=0; "
			"	while( i < 2 ) { "
			"		a = i; "
			"		i = i+1; "
			"	} "
			"	ref<int<4>> c = 20; "
			"	a = c;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = ref.var(undefined(int<4>)); "
				"ref<int<4>> v2 = 0; "
				"while(int.lt(ref.deref(v2), 2)) {"
					"{}; "
					"ref.assign(v2, int.add(ref.deref(v2), 1));"
				"}; "
				"ref<int<4>> v3 = 20; "
				"ref.assign(v1, v3); v1;"
			"}"
			, toString(*ret));
	}

	TEST(DeadAssignments, ScalarsForControl) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parse(
			"{"
			"	ref<int<4>> a = 10; "
			"	for(int<4> i=0 .. 10 : 2) {"
			"		a = i; "
			"	} "
			"	ref<int<4>> c = 20; "
			"	a = c;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = ref.var(undefined(int<4>)); "
				"for(int<4> v2 = 0 .. 10 : 2) {"
					"{};"
				"}; "
				"ref<int<4>> v3 = 20; "
				"ref.assign(v1, v3); "
				"v1;"
			"}"
			, toString(*ret));
	}


	TEST(DeadAssignments, Structus) {

		NodeManager mgr;
		IRBuilder builder(mgr);
		
		std::map<std::string, core::NodePtr> symbols;
		symbols["s"] = builder.variable(
			builder.parseType("ref<struct{ int<4> a; }>")
		);

		auto code = builder.parse(
			"{"
			"	ref<int<4>> a = 10; "
			"	ref<int<4>> c = 20; "
			"	s.a = a+c; "
			"	c = a;"
			"	s.a = c; "
			"	s.a; "
			"}", symbols
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v2 = 10; "
				"ref<int<4>> v3 = 20; "
				"{}; "
				"ref.assign(v3, v2); "
				"ref.assign(composite.ref.elem(v1, a, int<4>), v3); "
				"composite.ref.elem(v1, a, int<4>);"
			"}"
			, toString(*ret));

	}

} // end transform namespace 
} // end insieme namespace



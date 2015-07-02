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
#include "insieme/transform/dfabased/dead_variables.h"
#include "insieme/utils/logging.h"

#include "insieme/core/transform/simplify.h"

#include "insieme/analysis/polyhedral/scopregion.h"

namespace insieme {
namespace transform {

	using namespace core;

	TEST(DeadAssignments, Scalars) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(R"(
			{
				decl ref<int<4>> a = 10; 
				decl ref<int<4>> c = 20; 
				if (a < c) { 
					decl int<4> b = a+2; 
					a = b+3; 
					a = b+3;
				}
				c = *a;
				c; 
			}
		)");

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ(
			"{"
				"ref<int<4>> v1 = 10; "
				"ref<int<4>> v2 = 20; "
				"if(int_lt(ref_deref(v1), ref_deref(v2))) {"
					"int<4> v3 = int_add(ref_deref(v1), 2); "
					"{}; "
					"ref_assign(v1, int_add(v3, 3));"
				"} else {}; "
				"ref_assign(v2, ref_deref(v1)); "
				"v2;"
			"}"
			, toString(*ret));

	}

	TEST(DeadAssignments, ScalarsTransitive) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	decl ref<int<4>> b = 10; "
			"	decl ref<int<4>> c = 20; "
			"	a = *c;"
			"	c = *b;"
			"	b = *a;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl ref<int<4>> v2 = ( var(undefined(type<int<4>>)));decl ref<int<4>> v3 = 20;(v1 := (v3));{ };{ };v1;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
	}

	TEST(DeadAssignments, ScalarsWhileControl) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	decl ref<int<4>> i=0; "
			"	while( i < 2 ) { "
			"		a = *i; "
			"		i = i+1; "
			"	} "
			"	decl ref<int<4>> c = 20; "
			"	a = *c;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));decl ref<int<4>> v2 = 0;while(((v2)<2)) {{ };(v2 := ((v2)+1));};decl ref<int<4>> v3 = 20;(v1 := (v3));v1;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
	}

	TEST(DeadAssignments, ScalarsForControl) {

		NodeManager mgr;
		IRBuilder builder(mgr);

		auto code = builder.parseStmt(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	for(int<4> i=0 .. 10 : 2) {"
			"		a = i; "
			"	} "
			"	decl ref<int<4>> c = 20; "
			"	a = *c;"
			"	a; "
			"}"
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ("{decl ref<int<4>> v1 = ( var(undefined(type<int<4>>)));for(decl int<4> v2 = 0 .. 10 : 2) {{ };};decl ref<int<4>> v3 = 20;(v1 := (v3));v1;}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));
	}


	TEST(DeadAssignments, Structus) {

		NodeManager mgr;
		IRBuilder builder(mgr);
		
		std::map<std::string, core::NodePtr> symbols;
		symbols["s"] = builder.variable(
			builder.parseType("ref<struct{ int<4> a; }>")
		);

		auto code = builder.parseStmt(
			"{"
			"	decl ref<int<4>> a = 10; "
			"	decl ref<int<4>> c = 20; "
			"	s.a = a+c; "
			"	c = *a;"
			"	s.a = *c; "
			"	s.a; "
			"}", symbols
		);

		NodePtr ret = removeDeadVariables(mgr,code);
		
		EXPECT_EQ("{decl ref<int<4>> v2 = 10;decl ref<int<4>> v3 = ( var(undefined(type<int<4>>)));{ };(v3 := (v2));((v1->a) := (v3));(v1->a);}",
			toString(printer::PrettyPrinter(ret, printer::PrettyPrinter::PRINT_SINGLE_LINE)));

	}

//	TEST(DeadAssignments, VectorsForControl) {
//
//		NodeManager mgr;
//		IRBuilder builder(mgr);
//
//		auto code = builder.parse(
//			"{"
//			"	ref<vector<int<4>,10>> v; "
//			"	for(uint<4> i=0u .. 10u : 2u) {"
//			"		v[i] = i; "
//			"	} "
//			"	ref<int<4>> a = v[1u];"
//			"	a; "
//			"}"
//		);
//
//		// mark SCoPs
//		insieme::analysis::polyhedral::scop::mark(code);
//
//		NodePtr ret = removeDeadVariables(mgr,code);
//		
//		EXPECT_EQ(
//			"{"
//				"ref<vector<int<4>,10>> v1 = ref_var(undefined(vector<int<4>,10>)); "
//				"for(uint<4> v2 = 0u .. 10u : 2u) {"
//					"{};"
//				"}; "
//				"ref<int<4>> v3 = vector.ref_elem(v1, 1u); "
//				"v3;"
//			"}"
//			, toString(*ret));
//	}

//	TEST(DeadAssignments, VectorsForControlNotDead) {
//
//		NodeManager mgr;
//		IRBuilder builder(mgr);
//
//		auto code = builder.parse(
//			"{"
//			"	ref<vector<int<4>,10>> v; "
//			"	for(uint<4> i=0u .. 10u : 3u) {"
//			"		v[i] = i; "
//			"	} "
//			"	ref<int<4>> a = v[3u];"
//			"	a; "
//			"}"
//		);
//
//		// mark SCoPs
//		insieme::analysis::polyhedral::scop::mark(code);
//
//		NodePtr ret = removeDeadVariables(mgr,code);
//		
//		EXPECT_EQ(
//			"{"
//				"ref<vector<int<4>,10>> v1 = ref_var(undefined(vector<int<4>,10>)); "
//				"for(uint<4> v2 = 0u .. 10u : 3u) {"
//					"{};"
//				"}; "
//				"ref<int<4>> v3 = vector.ref_elem(v1, 1u); "
//				"v3;"
//			"}"
//			, toString(*ret));
//	}

} // end transform namespace 
} // end insieme namespace



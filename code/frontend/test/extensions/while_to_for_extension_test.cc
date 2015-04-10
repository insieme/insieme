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

#include "insieme/frontend/extensions/while_to_for_extension.h"

namespace insieme {
namespace core {
namespace pattern {

using namespace core;

TEST(WhileToFor, Simple) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					ref<int<4>> i = 0;
					ref<int<4>> j = 4;
					while (i < 10) {
						ref<int<4>> i2 = i;
						i = i + 3;
					}
					while (j!=0) {
						j = j - 2;
					}
					return i;
				}
				)"
			);

	ASSERT_TRUE(program);

	frontend::extensions::WhileToForExtension extension;
	auto str=toString(extension.IRVisit(program));
	EXPECT_PRED2(containsSubString, str, "{{}; {}; for(int<4> v5 = 0 .. 10 : 3) {ref<int<4>> v3 = v1; {};}; for(int<4> v4 = 4 .. 0 : -2) {{};}; return v1;}}");
}

TEST(WhileToFor, MultipleAss) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					ref<int<4>> i = 0;
					ref<int<4>> j = 4;
					while (i < 10 && j!=0) {
						ref<int<4>> i2 = i;
						i = 1 + i + 1;
						j = j - 2;
						i = i - 1;
					}
					return 0;
				}
				)"
			);

	ASSERT_TRUE(program);

	frontend::extensions::WhileToForExtension extension;
	auto str=toString(extension.IRVisit(program));
	EXPECT_PRED2(containsSubString, str, "{ref<int<4>> v1 = 0; ref<int<4>> v2 = 4; while(rec v0.{v0=fun(bool v1, (()=>bool) v2) {if(v1) {return v2();} else {}; return false;}}(int.lt(ref.deref(v1), 10), bind(){rec v0.{v0=fun(ref<int<4>> v2) {return int.ne(ref.deref(v2), 0);}}(v2)})) {ref<int<4>> v5 = v1; ref.assign(v1, int.add(int.add(1, ref.deref(v1)), 1)); ref.assign(v2, int.sub(ref.deref(v2), 2)); ref.assign(v1, int.sub(ref.deref(v1), 1));}; return 0;}");
}

TEST(WhileToFor, DISABLED_ConfusedMultipleAss) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					ref<int<4>> i = 0;
					ref<int<4>> j = 4;
					while (i < 10 && j!=0) {
						ref<int<4>> i2 = i;
						i = i + 2 - i;
						if (j == 2) { i = i + 1; }
						j = j - 1;
					}
					return 0;
				}
				)"
			);

	ASSERT_TRUE(program);

	frontend::extensions::WhileToForExtension extension;
	extension.IRVisit(program);
}

TEST(WhileToFor, Nested) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				R"(
				int<4> main() {
					ref<int<4>> i1 = 0;
					while (i1 < 10) {
						ref<int<4>> i2 = 8;
						while(i2 > 5) {
							i2 = i2 - 1;
						}
						i1 = i1 + 2;
					}
					ref<int<4>> i3 = 2;
					while (i3 > 4) {
						i3 = i3 - 2;
					}
					return 0;
				}
				)"
			);

	ASSERT_TRUE(program);

	frontend::extensions::WhileToForExtension extension;
	auto str=toString(extension.IRVisit(program));
	EXPECT_PRED2(containsSubString, str, "{{}; for(int<4> v6 = 0 .. 10 : 2) {{}; for(int<4> v5 = 8 .. 5 : -1) {{};}; {};}; {}; for(int<4> v4 = 2 .. 4 : -2) {{};}; return 0;}");
}

TEST(WhileToFor, DISABLED_NestedDeps) {
	NodeManager man;
	IRBuilder builder(man);

	core::ProgramPtr program = builder.parseProgram(
				"int<4> main() {"
				"	ref<int<4>> i1 = 0;"
				"	while (i1 < 10) {"
				"		ref<int<4>> i2 = i1;"
				"		while(i2 > 5) {"
				"			i2 = i2 - 1;"
				"		}"
				"		i1 = i1 + 2;"
				"	}"
				"   ref<int<4>> i3 = 2;"
				"	while (i1 > 4) {"
				"      i1 = i1 - i3;"
				"   }"
				"	return 0;"
				"}"
				);
		
	ASSERT_TRUE(program);

	frontend::extensions::WhileToForExtension extension;
	extension.IRVisit(program);
}
	
	TEST(WhileToFor, DISABLED_NonConvertible) {
		NodeManager man;
		IRBuilder builder(man);

		core::ProgramPtr program = builder.parseProgram(
			"int<4> main() {"
			"   ref<int<4>> i1 = 0;"
			"   ref<int<4>> j1 = 10;"
			"	while (i1 < 10 && j1 > 7) {"  // not convertible -- 2 iteration variables
			"		i1 = i1 + 2;"
			"		j1 = j1 - 2;"
			"	}"
			"	while (i1 < 10) {"  // not convertible (for now!) -- 2 assignments to iteration variable
			"		i1 = i1 + 2;"
			"		i1 = i1 + 1;"
			"	}"
			"	while (i1 + j1 < 10) {"  // not convertible (for now!) -- 2 assignments to iteration variable
			"		i1 = i1 + 2;"
			"		i1 = i1 + 1;"
			"	}"
			"   ref<int<4>> i3 = 2;" // not convertible -- break
			"	while (i1 > 4) {"
			"      i1 = i1 - i3;"
			"      if(i1==2) break;"
			"   }"
			"	return 0;"
			"}"
		);
		
		ASSERT_TRUE(program);

		frontend::extensions::WhileToForExtension extension;
		extension.IRVisit(program);
	}
} // namespace pattern
} // namespace core
} // namespace insieme

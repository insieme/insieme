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

#include "insieme/core/parser2/ir_parser.h"

namespace insieme {
namespace core {
namespace pattern {

	using namespace core;

	TEST(WhileToFor, Basic) {
		NodeManager man;
		IRBuilder builder(man);

		core::ProgramPtr program = builder.parseProgram(
			"int<4> main() {"
			"   ref<int<4>> i1 = 0;"
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

		frontend::WhileToForPlugin plugin;
		plugin.IRVisit(program);
	}
	
	TEST(WhileToFor, NonConvertible) {
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

		frontend::WhileToForPlugin plugin;
		plugin.IRVisit(program);
	}
} // namespace pattern
} // namespace core
} // namespace insieme
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

#include "insieme/transform/rulebased/stmt_transformations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/parser/ir_parse.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/checks/ir_checks.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace transform {
namespace rulebased {



	TEST(Transformations, CompoundElimination) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::StatementPtr one = builder.intLit(1);
		core::StatementPtr two = builder.intLit(2);

		CompoundElimination trans(parameter::emptyValue);


		core::StatementPtr stmt = builder.compoundStmt(builder.compoundStmt(one));
		EXPECT_EQ("{{1;};}", toString(*stmt));
		EXPECT_EQ("{1;}", toString(*trans.apply(stmt)));

		stmt = builder.compoundStmt(
				one, two,
				builder.compoundStmt(),
				two
		);

		EXPECT_EQ("{1; 2; {}; 2;}", toString(*stmt));
		EXPECT_EQ("{1; 2; 2;}", toString(*trans.apply(stmt)));

	}


	TEST(Transformations, LoopUnrolling) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::parse::IRParser parser(manager);

		auto forStmt = static_pointer_cast<core::ForStmtPtr>( parser.parseStatement("\
			for(decl int<4>:i = 10 .. 50 : 2) { \
				(op<array.ref.elem.1D>(ref<array<int<4>,1>>:v, i)); \
			}") );


		std::cout << core::check(forStmt, core::checks::getFullCheck()) << "\n";

		EXPECT_TRUE(forStmt);

		LoopUnrolling trans(parameter::makeValue<unsigned>(4));
		auto transformed = trans.apply(forStmt);
		auto res = toString(core::printer::PrettyPrinter(transformed, core::printer::PrettyPrinter::OPTIONS_DETAIL));

//		std::cout << res;

		// check transformed code
		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = 10 .. (50-((50-10)%(4*2))) : (2*4))");
		EXPECT_PRED2(containsSubString, res, "v2[&(v1+(2*0))]");
		EXPECT_PRED2(containsSubString, res, "v2[&(v1+(2*1))]");
		EXPECT_PRED2(containsSubString, res, "v2[&(v1+(2*2))]");
		EXPECT_PRED2(containsSubString, res, "v2[&(v1+(2*3))]");
		EXPECT_PRED2(notContainsSubString, res, "v2[&(v1+(1*4)]");

		EXPECT_PRED2(containsSubString, res, "for(decl int<4> v1 = (50-((50-10)%(4*2))) .. 50 : 2)");

//		auto list = core::check(transformed, core::checks::getFullCheck());
//		EXPECT_TRUE(list.empty()) << list;

	}

} // end namespace rulebased
} // end namespace transform
} // end namespace insieme



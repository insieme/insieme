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

#include "insieme/transform/versioning.h"
#include "insieme/transform/primitives.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/utils/test/test_utils.h"

namespace insieme {
namespace transform {


	TEST(Versioning, SimpleVersioning) {

		core::NodeManager manager;
		core::IRBuilder builder(manager);

		core::StatementPtr one = builder.intLit(1);
		core::StatementPtr two = builder.intLit(2);


		core::StatementPtr in = builder.compoundStmt(one, two);
		core::StatementPtr out;

		out = versioning(makeNoOp(), makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(out)), "pick(([0,1]))");

		// try special case - on transformation only
		out = versioning(makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_EQ(*in, *out);

		// and a large number of versions
		out = versioning(makeNoOp(), makeNoOp(), makeNoOp(), makeNoOp())->apply(in);
		EXPECT_TRUE(core::checks::check(out).empty());
		EXPECT_PRED2(containsSubString, toString(core::printer::PrettyPrinter(out)), "pick(([0,1,2,3]))");

	}


} // end namespace transform
} // end namespace insieme



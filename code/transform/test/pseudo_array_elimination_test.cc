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

#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/transform/ir_cleanup.h"
#include "insieme/utils/logging.h"

using namespace insieme::utils::log;

namespace insieme {
	using namespace core;

namespace transform {

TEST(PseudoArrayElimination, Basic) {

	NodeManager manager;
	IRBuilder builder(manager);

	StatementPtr stmt = builder.parseStmt( 
		"lambda () -> unit { "
		"	decl array<int<4>,1> arr = array_create_1D( lit(int<4>), 1u ); "
		"};");
	
	LOG(INFO) << printer::PrettyPrinter(stmt);

	eliminatePseudoArrays(stmt);

	//EXPECT_EQ("(a,b,())", toString(tree));

}

} // end namespace transform
} // end namespace insieme


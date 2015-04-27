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
#include "insieme/core/checks/semantic_checks.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {
namespace checks {

bool containsMSG(const MessageList& list, const Message& msg) {
	return contains(list.getAll(), msg);
}

TEST(ScalarArrayIndexRangeCheck, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	{
		StatementPtr stmt_err = builder.parseStmt(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit { 
                    decl ref<uint<8>> i = var(0u); 
                    lambda (ref<array<uint<8>,1>> arr) -> unit { 
                        decl uint<8> b = 1u; 
                        arr[b]; 
                    } (scalar_to_array(i)); 
                 };
			}
            )1N5P1RE");
		auto addrlist = builder.parseAddressesStatement(R"1N5P1RE(
            {
                let uint = uint<8>;
                lambda () -> unit { 
                    decl ref<uint<8>> i = var(0u); 
                    lambda (ref<array<uint<8>,1>> arr) -> unit { 
                        decl uint<8> b = 1u; 
                        $ arr[b] $; 
                    } (scalar_to_array(i)); 
                 };
			}
            )1N5P1RE");

        EXPECT_TRUE(stmt_err) << "parsing error";
        EXPECT_EQ(addrlist.size(), 1) << "parsing error";

		CheckPtr scalarArrayIndexRangeCheck = makeRecursive(make_check<ScalarArrayIndexRangeCheck>());

		//NodeAddress errorAdr = NodeAddress(stmt_err).getAddressOfChild(2,0,1,2,1,1,2,0,1,2,1);
		NodeAddress errorAdr = addrlist[0];

		EXPECT_EQ("0-0-2-0-1-2-1-1-2-0-1-2-1", toString(errorAdr));
		EXPECT_TRUE(dynamic_pointer_cast<CallExprPtr>(errorAdr.getAddressedNode())) << errorAdr.getAddressedNode();

		EXPECT_EQ(toString(check(stmt_err, scalarArrayIndexRangeCheck)[0]), 
			toString(Message(errorAdr, EC_SEMANTIC_ARRAY_INDEX_OUT_OF_RANGE, "Potentially unsafe indexing of single-element array v5 using formula v6", Message::WARNING)));
	}

	{
		StatementPtr stmt_pass = builder.parseExpr( 
            "let uint = uint<8>;"
			"lambda () -> unit { "
			"	decl ref<uint<8>> i = var(0u); "
			"	lambda (ref<array<uint<8>,1>> arr) -> unit { "
			"		decl uint<8> b = 1; "
			"		arr[0u]; "
			"	} (scalar_to_array(i)); "
			"}");
        EXPECT_TRUE(stmt_pass) << "parsing error";

		CheckPtr scalarArrayIndexRangeCheck = makeRecursive(make_check<ScalarArrayIndexRangeCheck>());
		EXPECT_TRUE(check(stmt_pass, scalarArrayIndexRangeCheck).empty());
	}
}


} // end namespace checks
} // end namespace core
} // end namespace insieme


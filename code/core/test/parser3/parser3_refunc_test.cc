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

#include <iostream>
#include <gtest/gtest.h>
#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/checks/full_check.h"

namespace insieme{
namespace core{
namespace parser3{

    using namespace detail;

    bool test_program(NodeManager& nm, const std::string& x){
        InspireDriver driver(x, nm);
        driver.parseProgram();
        if (driver.result) {
            std::cout << " ============== TEST ============ " << std::endl;
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        return driver.result;

    }


    // test comment
    TEST(IR_Parser3, Program) {
        NodeManager nm;
        std::cout << "Starting testing parser" << std::endl;
    EXPECT_TRUE(test_program(nm, "int<4> main () { return 1; }"));
    EXPECT_TRUE(test_program(nm, "let int = int<4>; int main () { return 1; }"));

    EXPECT_TRUE(test_program(nm,
                "let int = int<4>;"
                "let fun000 = recFunc function_name {"
                        "function_name = lambda(int a) -> int {"
                        "   return 1;"
                        "};"
                        "v001 = lambda(int<5> b, int<8> c) -> int {"
                        "   return 2;"
                        "};"
                "};"
                "int main() { decl int x = 10; fun000(1); return 0; }"
        ));
/*
        EXPECT_TRUE(test_program(nm,
                "let h = lambda((int<4>) -> int<4> f) -> int<4> {"
                "    return f(5);"
                "};"
                "let fun000 = recFun v187{ "
                "   v187 = lambda(int<4> a) -> int<4> {"
                "       h(v187);"
                "       v187(4);"
                "       v188(v187(4));"
                "       h(v188);"
                "   };"
                "   v188 = lambda(int<4> a) -> int<4> {"
                "       h(v187);"
                "       v187(v188(4));"
                "       v188(4);"
                "       h(v188);"
                "   };"
                "};"
                "unit main(){"
                "    fun000(1);"
                "}"
        ));*/
    }
} // parser3
} // core
} // insieme

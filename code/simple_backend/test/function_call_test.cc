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

#include "insieme/simple_backend/backend_convert.h"

#include "insieme/core/program.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/parser/ir_parse.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

TEST(FunctionCall, templates) {
    core::NodeManager manager;
    core::parse::IRParser parser(manager);

    core::ProgramPtr program = parser.parseProgram("main: fun ()->int<4>:\
            mainfct in { ()->int<4>:mainfct = ()->int<4>{ { \
                (fun(type<'a>:dtype, uint<4>:size) -> array<'a, 1> {{ \
                    return (op<array.create.1D>( dtype, size )); \
                }}(lit<type<real<4> >, type(real(4)) >, lit<uint<4>, 7>) ); \
                \
                \
                return 0; \
            } } }");
/*
    (fun(array<'a, 1>:in) -> int<4> {{ \
        decl array<'a, 1>:local = (op<undefined>(lit<type<array<'a> >, type(array('a)) > )); \
        return 0; \
    }}(lit<array<int<4>, 1>, x>)); \
*/

    LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

    LOG(INFO) << "Converting IR to C...";
    auto converted = simple_backend::SimpleBackend::getDefault()->convert(program);
    LOG(INFO) << "Printing converted code: " << *converted;

    string code = toString(*converted);

    EXPECT_FALSE(code.find("<?>") != string::npos);
//    EXPECT_FALSE(code.find("[[unhandled_simple_type") != string::npos);
}


TEST(FunctionCall, VectorReduction) {
    core::NodeManager manager;
    core::parse::IRParser parser(manager);

    core::ASTBuilder builder(manager);
    const core::lang::BasicGenerator& basic = manager.basic;

    // Operation: vector.reduction
    // Type: (vector<'elem,#l>, 'res, ('elem, 'res) -> 'res) -> 'res

//    core::ProgramPtr program = parser.parseProgram("main: fun ()->unit:\
//            mainfct in { ()->unit:mainfct = ()->unit{ { \
//                (fun() -> unit {{ \
//    		        decl vector<int<4>,4>:x = vector<vector<int<4>,4>>(1,2,3,4);\
//                    return (op<vector.reduction>( vector<int<4>,4>:x, lit<int<4>, 0>, op<int.add> )); \
//                }}() ); \
//                return 0; \
//            } } }");


    core::TypePtr int4 = basic.getInt4();
    core::VectorTypePtr vectorType = builder.vectorType(int4, builder.concreteIntTypeParam(4));

    core::ExpressionPtr vector = builder.vectorExpr(vectorType,
    		toVector<core::ExpressionPtr>(
    				builder.literal(int4, "1"),
    				builder.literal(int4, "2"),
    				builder.literal(int4, "3"),
    				builder.literal(int4, "4")
    		)
    );

    core::ExpressionPtr call = builder.callExpr(basic.getVectorReduction(), vector, builder.literal(int4, "0"), basic.getSignedIntAdd());

    core::FunctionTypePtr funType = builder.functionType(toVector<core::TypePtr>(), basic.getUnit());
    core::ExpressionPtr lambda = builder.lambdaExpr(funType, toVector<core::VariablePtr>(), builder.returnStmt(call));

    core::ProgramPtr program = builder.createProgram(toVector(lambda), true);

/*
    (fun(array<'a, 1>:in) -> int<4> {{ \
        decl array<'a, 1>:local = (op<undefined>(lit<type<array<'a> >, type(array('a)) > )); \
        return 0; \
    }}(lit<array<int<4>, 1>, x>)); \
*/

    LOG(INFO) << "Printing the IR: " << core::printer::PrettyPrinter(program);

    LOG(INFO) << "Converting IR to C...";
    auto converted = simple_backend::SimpleBackend::getDefault()->convert(program);
    LOG(INFO) << "Printing converted code: " << *converted;

    string code = toString(*converted);

//    EXPECT_FALSE(code.find("<?>") != string::npos);
//    EXPECT_FALSE(code.find("[[unhandled_simple_type") != string::npos);
}

} // namespace backend
} // namespace insieme

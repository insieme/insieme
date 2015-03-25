#include <iostream>
#include <gtest/gtest.h>
#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme{
namespace core{

    using namespace parser3::detail;

    bool test_expression(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseExpression();
        return driver.result; 
    }

    TEST(IR_Parser3, Expressions) {

        EXPECT_TRUE(test_expression("1"));
        EXPECT_TRUE(test_expression("1u"));

        EXPECT_TRUE(test_expression("1.0f"));
        EXPECT_TRUE(test_expression("1.0"));

        EXPECT_TRUE(test_expression("x"));

    }

} // core
} // insieme

#include <iostream>
#include <gtest/gtest.h>
#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme{
namespace core{

    using namespace parser3::detail;

    bool test_type(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseType();

        if (driver.result) dumpColor(driver.result);
        return driver.result; 
    }

    TEST(IR_Parser3, Types) {

        EXPECT_TRUE(test_type("int<4>"));
        EXPECT_TRUE(test_type("vector<int<4>>"));
        EXPECT_TRUE(test_type("vector<int<4>, 4>"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b;}"));

        EXPECT_TRUE(test_type("let int = int<4>; int"));

    }

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

        EXPECT_TRUE(test_expression("1 + 3"));
        EXPECT_TRUE(test_expression("1 - 0"));
        EXPECT_TRUE(test_expression("1 * 0"));
        EXPECT_TRUE(test_expression("1 / 0"));
        EXPECT_TRUE(test_expression("1 | 0"));
        EXPECT_TRUE(test_expression("1 & 0"));
        EXPECT_TRUE(test_expression("1 ^ 0"));

        EXPECT_TRUE(test_expression("(1.0)"));
        EXPECT_TRUE(test_expression("((1.0))"));

        EXPECT_TRUE(test_expression("let int = int<4>;(int)1.0"));
        EXPECT_TRUE(test_expression("(int<4>)(1.0)"));

        //EXPECT_TRUE(test_expression("x"));

    }

} // core
} // insieme

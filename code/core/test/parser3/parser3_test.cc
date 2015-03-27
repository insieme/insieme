#include <iostream>
#include <gtest/gtest.h>
#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"
#include "insieme/core/ir_builder.h"

namespace insieme{
namespace core{
namespace parser3{

    using namespace detail;

    bool test_type(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseType();

        if (driver.result) dumpColor(driver.result);
        return driver.result; 
    }

    TEST(IR_Parser3, Types) {

        EXPECT_TRUE(test_type("int<4>"));
        EXPECT_TRUE(test_type("someweirdname<>"));
        EXPECT_TRUE(test_type("vector<int<4>>"));
        EXPECT_TRUE(test_type("vector<int<4>, 4>"));
        EXPECT_TRUE(test_type("vector<'a, 4>"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name :: int<4> : vector<int<4>, 45> { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b;}"));
        EXPECT_TRUE(test_type("let int = int<4>; int"));

        EXPECT_TRUE(test_type("( int<4> , ref<int<4>>) -> int<4>"));
        EXPECT_TRUE(test_type("( int<4> , ref<int<4>>) => int<4>"));

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
        EXPECT_TRUE(test_expression("((1.0) + 4.0)"));

        EXPECT_FALSE(test_expression("x"));

        EXPECT_TRUE(test_expression("lambda ('a _) -> true"));
        EXPECT_TRUE(test_expression("lambda ('a x) -> x+CAST('a) 3"));
        EXPECT_TRUE(test_expression("lambda ('a x) => x+CAST('a) 3"));
    }

    bool test_statement(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseStmt();
        if (driver.result) dumpColor(driver.result);
        return driver.result; 
    }

    TEST(IR_Parser3, Statements) {

        EXPECT_TRUE(test_statement(";"));

        EXPECT_TRUE(test_statement("1;"));
        EXPECT_TRUE(test_statement("1u;"));

        EXPECT_TRUE(test_statement("1.0f;"));
        EXPECT_TRUE(test_statement("1.0;"));

        EXPECT_TRUE(test_statement("1 + 3;"));
        EXPECT_TRUE(test_statement("1 - 0;"));
        EXPECT_TRUE(test_statement("1 * 0;"));
        EXPECT_TRUE(test_statement("1 / 0;"));
        EXPECT_TRUE(test_statement("1 | 0;"));
        EXPECT_TRUE(test_statement("1 & 0;"));
        EXPECT_TRUE(test_statement("1 ^ 0;"));

        EXPECT_TRUE(test_statement("(1.0);"));
        EXPECT_TRUE(test_statement("((1.0));"));

        EXPECT_FALSE(test_statement("x"));
        EXPECT_FALSE(test_statement("x;"));

        EXPECT_TRUE(test_statement("{ decl int<4> x = 0; x+1; }"));
        EXPECT_TRUE(test_statement("{ decl auto x = 0; x+1; }"));

        EXPECT_TRUE(test_statement("if ( true ) {}"));
        EXPECT_TRUE(test_statement("if ( true ) {} else {}"));
        EXPECT_TRUE(test_statement("if ( true ) if ( false ) { } else 1 ;"));
        EXPECT_TRUE(test_statement("if ( true ) if ( false ) { } else 1 ; else 2; "));
        EXPECT_TRUE(test_statement("while ( true ) 1+1;"));
        EXPECT_TRUE(test_statement("while ( 1+2 ) 1+1;"));
        EXPECT_TRUE(test_statement("while ( 1+2 ) { 1+1; }"));
        EXPECT_TRUE(test_statement("for ( int<4> it 1 .. 3) 1+1;"));
        EXPECT_TRUE(test_statement("for ( int<4> it 1 .. 3: 2) 1+1;"));
        EXPECT_TRUE(test_statement("for ( int<4> it 1 .. 3: 2) { 1+1; }"));

        EXPECT_TRUE(test_statement("switch (2) { case 1: 1; case 2: 2; }"));
        EXPECT_FALSE(test_statement("switch (2) { case 1: 1; case 2: 2; default 2;}"));
        EXPECT_FALSE(test_statement("switch (2) { case 1+1: 1; case 2: 2; default: 2;}"));

        EXPECT_FALSE(test_statement("try  2; catch( int<4> e) { 1+1; }"));
        EXPECT_FALSE(test_statement("try  {2;} catch( int<4> e)  1+1; "));

        EXPECT_TRUE(test_statement("try  {2;} catch( int<4> e) { 1+1; }"));
        EXPECT_TRUE(test_statement("try  {2;} catch( int<4> e) { 1+1; } catch (ref<int<4>> r) { 3+4; }"));

    }
    
    bool test_program(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseProgram();
        if (driver.result) dumpColor(driver.result);
        return driver.result; 
    }

    TEST(IR_Parser3, Program) {

        EXPECT_TRUE(test_program("int<4> main (int<4> a, int<4> b)  { 1+1; }"));
        EXPECT_TRUE(test_program("let int = int<4>; int main (int a, int b) { 1+1; }"));
    }

} // parser3
} // core
} // insieme

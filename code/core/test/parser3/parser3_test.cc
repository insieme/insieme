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

    bool test_type(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseType();
        if (driver.result) {  
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        std::cout << " ========================================== " << std::endl;
        return driver.result; 

    }

    TEST(IR_Parser3, Types) {
        EXPECT_TRUE(test_type("int<4>"));
        EXPECT_TRUE(test_type("someweirdname<>"));
        EXPECT_FALSE(test_type("vector<int<4>>"));
        EXPECT_TRUE(test_type("vector<int<4>, 4>"));
        EXPECT_TRUE(test_type("vector<'a, 4>"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name :: papa<1> : mama<2> { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct name :: papa<4> { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type("struct { int<4> a; int<5> b;}"));
        EXPECT_TRUE(test_type("let int = int<4>; int"));

        EXPECT_TRUE(test_type("( int<4> , ref<int<4>>) -> int<4>"));
        EXPECT_TRUE(test_type("( int<4> , ref<int<4>>) => int<4>"));
        EXPECT_TRUE(test_type("(array<'elem,#n>, vector<uint<8>,#n>) -> 'elem"));
    }

    bool test_expression(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseExpression();
        if (driver.result) {  
            std::cout << driver.result << std::endl;
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        std::cout << " ========================================== " << std::endl;
        return driver.result; 
    }

    TEST(IR_Parser3, Expressions) {

        EXPECT_TRUE(test_expression("true?1:0"));
        EXPECT_TRUE(test_expression("param(1)"));

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

        // precedence
        EXPECT_TRUE(test_expression("1 + 0 * 5"));
        EXPECT_TRUE(test_expression("1 * 0 + 5"));

        EXPECT_TRUE(test_expression("(1.0)"));
        EXPECT_TRUE(test_expression("((1.0))"));
        EXPECT_TRUE(test_expression("((1.0) + 4.0)"));

        EXPECT_TRUE(test_expression("struct { i= 4.0}"));
        EXPECT_TRUE(test_expression("let x = struct{ int<4> a }; struct x { a= 4}"));

        EXPECT_FALSE(test_expression("x"));

        EXPECT_TRUE(test_expression("lambda ('a _) -> bool : true"));
        EXPECT_TRUE(test_expression("lambda ('a x) -> 'a :  x+CAST('a) 3"));
        EXPECT_TRUE(test_expression("lambda ('a x) -> 'a : (x+CAST('a) 3)"));
        EXPECT_TRUE(test_expression("lambda ('a x) -> 'a { return x+CAST('a) 3; }"));
        EXPECT_TRUE(test_expression("lambda ('a x) => x+CAST('a) 3"));

        EXPECT_TRUE(test_expression("type(int<4>)"));
    }

    bool test_statement(const std::string& x){
        NodeManager nm;
        inspire_driver driver(x, nm);
        driver.parseStmt();
        if (driver.result) {  
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        std::cout << " ========================================== " << std::endl;
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
        EXPECT_TRUE(test_statement("while ( false ) 1+1;"));
        EXPECT_TRUE(test_statement("while ( false || true ) { 1+1; }"));
        EXPECT_TRUE(test_statement("for ( int<4> it = 1 .. 3) 1+1;"));
        EXPECT_TRUE(test_statement("for ( int<4> it = 1 .. 3: 2) 1+1;"));
        EXPECT_TRUE(test_statement("for ( int<4> it = 1 .. 3: 2) { 1+1; }"));

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
        if (driver.result) {  
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        std::cout << " ========================================== " << std::endl;
        return driver.result; 

    }

    TEST(IR_Parser3, Let) {
        EXPECT_TRUE(test_program("let int = int<4>; int main () { return 1; }"));
        EXPECT_TRUE(test_program("let int = int<4>; let long = int<8>; long main (int a) { return 1; }"));
        EXPECT_TRUE(test_program("let int , long = int<4> ,int<8>; int<4> main () { return 1; }"));
        EXPECT_TRUE(test_program("let f = lambda () -> unit { }; int<4> main () { f(); return 1; }"));
        EXPECT_TRUE(test_program("let int = int<4>; let f = lambda (int a) -> int { return a; }; int<4> main () { f(1); return 1; }"));

            
        EXPECT_FALSE(test_program("let int , long = int<4>; int<4> main () { return 1; }"));
        EXPECT_FALSE(test_program("let a , b = a<4>; int<4> main () { return 1; }"));

        EXPECT_TRUE(test_program("let a , b = a<b>, b<a>; int<4> main () { decl a x = undefined(a); decl b y = undefined(b); return 1; }"));
        EXPECT_TRUE(test_program("let f,g = lambda()->unit{g();},lambda()->unit{f();}; unit main() { f(); }"));
            
    }
    
    TEST(IR_Parser3, Program) {

        EXPECT_TRUE(test_program("int<4> main (int<4> a, int<4> b)  { 1+1; }"));
        EXPECT_TRUE(test_program("let int = int<4>; int main (int a, int b) { 1+1; }"));
    }

} // parser3
} // core
} // insieme

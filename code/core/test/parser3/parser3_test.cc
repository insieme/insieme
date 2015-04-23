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

    bool test_type(NodeManager& nm, const std::string& x){
        inspire_driver driver(x, nm);
        driver.parseType();
        if (driver.result) {  
            dumpColor(driver.result);
            std::cout << " ============== TEST ============ " << std::endl;
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        return driver.result; 

    }

    TEST(IR_Parser3, Types) {
        NodeManager nm;
        EXPECT_TRUE(test_type(nm, "int<4>"));
        EXPECT_TRUE(test_type(nm, "someweirdname<>"));
        EXPECT_TRUE(test_type(nm, "vector<int<4>, 4>"));
        EXPECT_TRUE(test_type(nm, "vector<'a, 4>"));
        EXPECT_TRUE(test_type(nm, "struct { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type(nm, "struct name { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type(nm, "let papa, mama = t<11>, t<4>; struct name : papa, mama { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type(nm, "let papa = t<11>; struct name : papa { int<4> a; int<5> b}"));
        EXPECT_TRUE(test_type(nm, "struct { int<4> a; int<5> b;}"));
        EXPECT_TRUE(test_type(nm, "let int = int<4>; int"));

        EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) -> int<4>"));
        EXPECT_TRUE(test_type(nm, "( int<4> , ref<int<4>>) => int<4>"));
        EXPECT_TRUE(test_type(nm, "(array<'elem,#n>, vector<uint<8>,#n>) -> 'elem"));

        EXPECT_TRUE(test_type(nm, 
                    "let class = struct name { int<4> a; int<5> b};"
                    "method class::()->int<4> "));
        EXPECT_TRUE(test_type(nm, 
                    "let class = struct name { int<4> a; int<5> b};"
                    "~class::()" ));
        EXPECT_TRUE(test_type(nm, 
                    "let class = struct name { int<4> a; int<5> b};"
                    "ctor class::()"));

        EXPECT_TRUE(test_type(nm, "struct C { int<4> field; }" ));
        EXPECT_TRUE(test_type(nm, "(ref<array<ref<array<struct{int<4> int; real<4> float },1>>,1>>,"
                                  " ref<array<ref<array<real<4>,1>>,1>>,"
                                  " ref<array<uint<8>,1>>)"));

        // failing types
        EXPECT_FALSE(test_type(nm, "vector<int<4>>"));


    }

    bool test_expression(NodeManager& nm, const std::string& x){
        inspire_driver driver(x, nm);
        driver.parseExpression();
        if (driver.result) {  
            std::cout << driver.result << std::endl;
     //       dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        else{
            driver.print_errors();
        }
     //   std::cout << " ============== TEST ============ " << std::endl;
        return driver.result; 
    }

    TEST(IR_Parser3, Expressions) {
        NodeManager nm;
        EXPECT_TRUE(test_expression(nm, "true?1:0"));
        EXPECT_TRUE(test_expression(nm, "param(1)"));

        EXPECT_TRUE(test_expression(nm, "1"));
        EXPECT_TRUE(test_expression(nm, "1u"));
        EXPECT_TRUE(test_expression(nm, "1l"));
        EXPECT_TRUE(test_expression(nm, "1ul"));
        EXPECT_TRUE(test_expression(nm, "1ll"));
        EXPECT_TRUE(test_expression(nm, "1ull"));

        EXPECT_TRUE(test_expression(nm, "1.0f"));
        EXPECT_TRUE(test_expression(nm, "1.0"));

        EXPECT_TRUE(test_expression(nm, "1 + 3"));
        EXPECT_TRUE(test_expression(nm, "1 - 0"));
        EXPECT_TRUE(test_expression(nm, "1 * 0"));
        EXPECT_TRUE(test_expression(nm, "1 / 0"));
        EXPECT_TRUE(test_expression(nm, "1 | 0"));
        EXPECT_TRUE(test_expression(nm, "1 & 0"));
        EXPECT_TRUE(test_expression(nm, "1 ^ 0"));

        // precedence
        EXPECT_TRUE(test_expression(nm, "1 + 0 * 5"));
        EXPECT_TRUE(test_expression(nm, "1 * 0 + 5"));

        EXPECT_TRUE(test_expression(nm, "(1.0)"));
        EXPECT_TRUE(test_expression(nm, "((1.0))"));
        EXPECT_TRUE(test_expression(nm, "((1.0) + 4.0)"));

        EXPECT_TRUE(test_expression(nm, "let x = struct{ int<4> a }; struct x { 4}"));

        EXPECT_FALSE(test_expression(nm, "x"));

        EXPECT_TRUE(test_expression(nm, "lambda ('a _) -> bool { return true; }"));
        EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return x+CAST('a) 3; }"));
        EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return(x+CAST('a) 3); }"));
        EXPECT_TRUE(test_expression(nm, "lambda ('a x) -> 'a { return x+CAST('a) 3; }"));
        EXPECT_TRUE(test_expression(nm, "lambda ('a x) => x+CAST('a) 3"));

        EXPECT_TRUE(test_expression(nm, "type(int<4>)"));

        EXPECT_TRUE(test_expression(nm, "let f = lambda ()->unit {  5;  lambda ()->unit { f(); } (); }; f"));

        EXPECT_TRUE(test_expression(nm, 
    	"lambda (int<4> v, int<4> exp) -> int<4> { "
    	"	let one = lambda(int<4> _)=>4; "
    	"	let two = lambda(int<4> x)=>x+exp; "
        "    return one(two(exp));"
    	"}  "));
        EXPECT_TRUE(test_expression(nm, 
    	"lambda (int<4> v, int<4> exp) -> int<4> { "
    	"	let one = lambda(int<4> _)-> int<4> { return 4;  };"
    	"	let two = lambda(int<4> x)-> int<4> { return x+5; };"
        "    return one(two(exp));"
    	"}  "));
            
        EXPECT_TRUE(test_expression(nm, 
        "    let class = struct name { int<4> a; int<5> b};"
        "    lambda ctor class::() { }"));

        EXPECT_TRUE(test_expression(nm, 
        "    let class = struct name { int<4> a; int<5> b};"
        "    lambda class::()->int<4> { return 1; }"));

        EXPECT_TRUE(test_expression(nm, 
        "    let class = struct name { int<4> a; int<5> b};"
        "    lambda  ~class::() { }"));
    }

    bool test_statement(NodeManager& nm, const std::string& x){
        inspire_driver driver(x, nm);
        driver.parseStmt();
        if (driver.result) {  
            dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
        else{
            driver.print_errors();
        }
        std::cout << " ============== TEST ============ " << std::endl;
        return driver.result; 
    }

    TEST(IR_Parser3, Statements) {

        NodeManager nm;
        EXPECT_TRUE(test_statement(nm, ";"));

        EXPECT_TRUE(test_statement(nm, "1;"));
        EXPECT_TRUE(test_statement(nm, "1u;"));

        EXPECT_TRUE(test_statement(nm, "1.0f;"));
        EXPECT_TRUE(test_statement(nm, "1.0;"));

        EXPECT_TRUE(test_statement(nm, "1 + 3;"));
        EXPECT_TRUE(test_statement(nm, "1 - 0;"));
        EXPECT_TRUE(test_statement(nm, "1 * 0;"));
        EXPECT_TRUE(test_statement(nm, "1 / 0;"));
        EXPECT_TRUE(test_statement(nm, "1 | 0;"));
        EXPECT_TRUE(test_statement(nm, "1 & 0;"));
        EXPECT_TRUE(test_statement(nm, "1 ^ 0;"));

        EXPECT_TRUE(test_statement(nm, "(1.0);"));
        EXPECT_TRUE(test_statement(nm, "((1.0));"));

        EXPECT_FALSE(test_statement(nm, "x"));
        EXPECT_FALSE(test_statement(nm, "x;"));

        EXPECT_TRUE(test_statement(nm, "{ decl int<4> x = 0; x+1; }"));
        EXPECT_TRUE(test_statement(nm, "{ decl auto x = 0; x+1; }"));

        EXPECT_TRUE(test_statement(nm, "if ( true ) {}"));
        EXPECT_TRUE(test_statement(nm, "if ( true ) {} else {}"));
        EXPECT_TRUE(test_statement(nm, "if ( true ) if ( false ) { } else 1 ;"));
        EXPECT_TRUE(test_statement(nm, "if ( true ) if ( false ) { } else 1 ; else 2; "));
	    EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; } else { return 1+2; }"));
	    EXPECT_TRUE(test_statement(nm, "if( false ) { return 0; }"));
	    EXPECT_TRUE(test_statement(nm, "if(1 != 0) { return 0; }"));
        EXPECT_TRUE(test_statement(nm, "while ( true ) 1+1;"));
        EXPECT_TRUE(test_statement(nm, "while ( false ) 1+1;"));
        EXPECT_TRUE(test_statement(nm, "while ( false || true ) { 1+1; }"));
        EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3) 1+1;"));
        EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) 1+1;"));
        EXPECT_TRUE(test_statement(nm, "for ( int<4> it = 1 .. 3: 2) { 1+1; }"));

        EXPECT_TRUE(test_statement(nm, "switch (2) { case 1: 1; case 2: 2; }"));
        EXPECT_FALSE(test_statement(nm,"switch (2) { case 1: 1; case 2: 2; default 2;}"));
        EXPECT_FALSE(test_statement(nm,"switch (2) { case 1+1: 1; case 2: 2; default: 2;}"));

        EXPECT_FALSE(test_statement(nm,"try  2; catch( int<4> e) { 1+1; }"));
        EXPECT_FALSE(test_statement(nm,"try  {2;} catch( int<4> e)  1+1; "));

        EXPECT_TRUE(test_statement(nm, "try  {2;} catch( int<4> e) { 1+1; }"));
        EXPECT_TRUE(test_statement(nm, "try  {2;} catch( int<4> e) { 1+1; } catch (ref<int<4>> r) { 3+4; }"));

        EXPECT_TRUE(test_statement(nm, "{ }"));


        EXPECT_TRUE(test_statement(nm, 
        "{"
        "    let type = struct a { int<4> a; int<8> b; };"
        "    decl type varable;"
        "    decl ref<type> var2;"
        "    decl auto var3 = undefined(type);"
        "}"
        ));

        EXPECT_TRUE(test_statement(nm, 
        "{"
        "    let int = int<4>;    "
        "    let A = struct { int a; };  "
        "    let B = struct : A { int b; };  "
        "    decl ref<B> b;  "
        "    decl auto x = ref_narrow( b, dp_parent( dp_root, lit(A) ), lit(A) );"
        "}"
        ));

        EXPECT_TRUE(test_statement(nm, 
        "{"
        "    let class = struct name { int<2> a};"
        "    let collection = vector<class, 10>;"
        "    decl ref<collection> x;"
        "    decl int<2> y;"
        "    x[5].a = y;"
        "}"
        ));
    }

    bool test_program(NodeManager& nm, const std::string& x){
        inspire_driver driver(x, nm);
        driver.parseProgram();
        if (driver.result) {  
     //       dumpColor(driver.result);
            auto msg = checks::check(driver.result);
            EXPECT_TRUE(msg.empty()) << msg;
        }
     //   std::cout << " ============== TEST ============ " << std::endl;
        return driver.result; 

    }

    TEST(IR_Parser3, Let) {
        NodeManager mgr;
        EXPECT_TRUE(test_program(mgr, "let int = int<4>; int main () { return 1; }"));
        EXPECT_TRUE(test_program(mgr, "let int = int<4>; let long = int<8>; long main (int a) { return 1; }"));
        EXPECT_TRUE(test_program(mgr, "let int , long = int<4> ,int<8>; int<4> main () { return 1; }"));
        EXPECT_TRUE(test_program(mgr, "let f = lambda () -> unit { }; int<4> main () { f(); return 1; }"));
        EXPECT_TRUE(test_program(mgr, "let int = int<4>; let f = lambda (int a) -> int { return a; }; int<4> main () { f(1); return 1; }"));

            
        EXPECT_FALSE(test_program(mgr, "let int , long = int<4>; int<4> main () { return 1; }"));
        EXPECT_FALSE(test_program(mgr, "let a , b = a<4>; int<4> main () { return 1; }"));

        EXPECT_TRUE(test_program(mgr, "let a , b = a<b>, b<a>; int<4> main () { decl a x = undefined(a); decl b y = undefined(b); return 1; }"));
        EXPECT_TRUE(test_program(mgr, "let f,g = lambda()->unit{g();},lambda()->unit{f();}; unit main() { f(); }"));

        EXPECT_TRUE(test_program(mgr, 
            "let class = struct name { int<4> a; int<5> b};"
            "let f,g = lambda class :: ()->unit{"
            "        g(this);"
            "    },"
            "    lambda class ::()->unit{"
            "        f(this);"
            "    }; "
            "unit main() {  "
            "    decl ref<class> x;"
            "    f(x);"
            "    g(x);"
            "}" ));
    }
    
    TEST(IR_Parser3, Program) {
        NodeManager nm;
        EXPECT_TRUE(test_program(nm, "int<4> main (int<4> a, int<4> b)  { 1+1; }"));
        EXPECT_TRUE(test_program(nm, "let int = int<4>; int main (int a, int b) { 1+1; }"));
        EXPECT_TRUE(test_program(nm, "let int = int<4>; let f = lambda (int a) ->int { return a; }; int main (int a, int b) { f(1); }"));
        EXPECT_TRUE(test_program(nm, 
                "let int = int<4> ; "
                "let h = lambda ((int)->int f)->int { return f(5); } ; "
                "let f,g = "
                "    lambda (int a)->int {"
                "        h(f);"
                "        f(4);"
                "        g(f(4));"
                "        h(g);"
                "    },"
                "    lambda (int a)->int {"
                "        h(f);"
                "        f(g(4));"
                "        g(4);"
                "        h(g);"
                "    };"
                "unit main() { f(1); }"
        ));

         EXPECT_TRUE(test_program(nm,
                "let int = int<4>;"
                "let uint = uint<4>;"
                "let differentbla = lambda ('b x) -> unit {"
                "    decl auto m = x;"
                "    decl auto l = m;"
                "};"
                "let bla = lambda ('a f) -> unit {"
                "    let anotherbla = lambda ('a x) -> unit {"
                "        decl auto m = x;"
                "    };"
                "    anotherbla(f);"
                "    differentbla(f);"
                "    parallel(job { decl auto l = f; });"
                "};"
                "int main() {"
                "    decl int x = 10;"
                "    bla(x);"
                "    return 0;"
                "}"
        ));
    }


    TEST(IR_Parser3, Scopes) {

        NodeManager nm;
//        EXPECT_FALSE (test_expression(nm, R"(
//            let a = lambda()->unit {};  let a = lambda ()->int<4>{ return 0; };  a
//        )"));
//        EXPECT_TRUE (test_expression(nm, R"(
//            let a = lambda()->unit {};  let b = ()->int{ return 0; };  a
//        )"));
//
        EXPECT_FALSE (test_expression(nm, " let a = int<4>;  let a = float<4>;  1 "));
    }

} // parser3
} // core
} // insieme

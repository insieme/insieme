#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

// this last one is generated and the path will be provided to the command
#include "inspire_parser.hpp"

namespace insieme{
namespace core{
namespace parser3{
namespace detail{


    inspire_driver::inspire_driver (const std::string &f, NodeManager& nm)
      : scanner( new scanner_string(this, f)),  mgr(nm), builder(mgr), file("global scope"), str(f), result(nullptr), glob_loc(&file)
    {
    }

    inspire_driver::~inspire_driver ()
    {
        delete scanner;
    }

    NodePtr inspire_driver::parseProgram ()
    {
      scanner->scan_begin ();
      auto ssymb = inspire_parser::make_FULL_PROGRAM(glob_loc);
      auto* ptr = &ssymb;
      inspire_parser parser (*this, &ptr);
      int res = parser.parse ();
      scanner->scan_end ();
      return result;
    }

    TypePtr inspire_driver::parseType ()
    {
      scanner->scan_begin ();
      auto ssymb = inspire_parser::make_TYPE_ONLY(glob_loc);
      auto* ptr = &ssymb;
      inspire_parser parser (*this, &ptr);
      int res = parser.parse ();
      scanner->scan_end ();
      return result.as<TypePtr>();
    }

    StatementPtr inspire_driver::parseStmt ()
    {
      scanner->scan_begin ();
      auto ssymb = inspire_parser::make_STMT_ONLY(glob_loc);
      auto* ptr = &ssymb;
      inspire_parser parser (*this, &ptr);
      int res = parser.parse ();
      scanner->scan_end ();
      return result.as<StatementPtr>();
    }

    ExpressionPtr inspire_driver::parseExpression ()
    {
      scanner->scan_begin ();
      auto ssymb = inspire_parser::make_EXPRESSION_ONLY(glob_loc);
      auto* ptr = &ssymb;
      inspire_parser parser (*this, &ptr);
      int res = parser.parse ();
      scanner->scan_end ();
      return result.as<ExpressionPtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ExpressionPtr inspire_driver::findSymbol(const location& l, const std::string& name)  const{
        auto x = scopes.find(name);
        if (!x) {
            error(l, format("the symbol %s was not declared in this context", name));
            return nullptr; 
        }
        if (!x.isa<ExpressionPtr>()){
            error(l, format("the symbol %s is not an expression (var/func)", name));
            return nullptr; 
        }

        return x.as<ExpressionPtr>();
    }

    TypePtr inspire_driver::findType(const location& l, const std::string& name)  const{
        auto x = scopes.find(name);
        if (!x) {
            error(l, format("the symbol %s was not declared in this context", name));
            return nullptr; 
        }
        if (!x.isa<TypePtr>()){
            error(l, format("the symbol %s is not a type", name));
            return nullptr; 
        }
        return x.as<TypePtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Error management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::error (const location& l, const std::string& m)const {
      std::cerr << l << ": " << m << std::endl;
      //int lineb = l.begin.line;
      //int linee = l.end.line;
      int colb = l.begin.column;
      int cole = l.end.column;

      //TODO: multiline?

      std::cerr << "  => " << str << std::endl;
      std::cerr << "     ";
      for (int i =0; i < colb-1; ++i) std::cerr << " ";
      std::cerr << "^";
      for (int i =0; i < cole - colb; ++i) std::cerr << "~";
      std::cerr << std::endl;
    }



    void inspire_driver::error (const std::string& m)const {
      std::cerr << m << std::endl;
    }


} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

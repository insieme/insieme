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
      if (!res) return nullptr;
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
      if (!res) return nullptr;
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
      if (!res) return nullptr;
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
      if (!res) return nullptr;
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


    ExpressionPtr inspire_driver::getOperand(ExpressionPtr expr){
	    return builder.tryDeref(expr);
    }

    ExpressionPtr inspire_driver::genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right){
        // Interpret operator

        auto a = getOperand(left);
        auto b = getOperand(right);

        // assign

        // bitwise 
        if (op == "&") return builder.bitwiseAnd(a,b);
        if (op == "|") return builder.bitwiseOr(a,b);
        if (op == "^") return builder.bitwiseXor(a,b);
        
        // logic
        if (op == "||") return builder.logicOr(a,b);
        if (op == "&&") return builder.logicAnd(a,b);

        // arithm
        if (op == "-") return builder.add(a,b);
        if (op == "+") return builder.sub(a,b);

        // geom
        if (op == "*") return builder.mul(a,b);
        if (op == "/") return builder.div(a,b);
        if (op == "%") return builder.mod(a,b);

        error(l, format("the symbol %s is not a operator", op));
        return nullptr;
    }

    TypePtr inspire_driver::genGenericType(const location& l, const std::string& name, const TypeList& parents,
                                           const TypeList& params, const IntParamList& iparamlist){
        
        if (name == "struct"){
        }
        if (name == "union"){
        }
        if (name == "vector"){
            if (iparamlist.size() != 1 || params.size() != 1) error(l, "mal-form vector type");
        }
        if (name == "array"){
            if (iparamlist.size() != 1 || params.size() != 1) error(l, "mal-form array type");
        }
        if (name == "ref"){
            if (!iparamlist.empty() || params.size() != 1) error(l, "mal-form ref type");
        }
        if (name == "int"){
            if (iparamlist.size() != 1) error(l, "wrong int size");
        }
        if (name == "real"){
            
            if (iparamlist.size() != 1) error(l, "wrong real size");
        }

		return builder.genericType(name, params, iparamlist);

        error(l, "this does not look like a type");
        return nullptr;
    }

    TypePtr inspire_driver::genFuncTypeType(const location& l, const TypeList& params, const TypePtr& retType, bool closure){
        
        return builder.functionType(params, retType, closure?FK_PLAIN:FK_CLOSURE);
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
      for (int i =0; i < cole - colb -1; ++i) std::cerr << "~";
      std::cerr << std::endl;
    }


    void inspire_driver::error (const std::string& m)const {
      std::cerr << m << std::endl;
    }


} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

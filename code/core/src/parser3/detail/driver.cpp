#include <algorithm>
#include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"

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

    ProgramPtr inspire_driver::parseProgram ()
    {
        scanner->scan_begin ();
        auto ssymb = inspire_parser::make_FULL_PROGRAM(glob_loc);
        auto* ptr = &ssymb;

        inspire_parser parser (*this, &ptr);
        int fail = parser.parse ();
        scanner->scan_end ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ProgramPtr>();
    }

    TypePtr inspire_driver::parseType ()
    {
        scanner->scan_begin ();
        auto ssymb = inspire_parser::make_TYPE_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr);
        int fail = parser.parse ();
        scanner->scan_end ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<TypePtr>();
    }

    StatementPtr inspire_driver::parseStmt ()
    {
        scanner->scan_begin ();
        auto ssymb = inspire_parser::make_STMT_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr);
        int fail = parser.parse ();
        scanner->scan_end ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<StatementPtr>();
    }

    ExpressionPtr inspire_driver::parseExpression ()
    {
        scanner->scan_begin ();
        auto ssymb = inspire_parser::make_EXPRESSION_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr);
        int fail = parser.parse ();
        scanner->scan_end ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ExpressionPtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ExpressionPtr inspire_driver::findSymbol(const location& l, const std::string& name)  const{
        auto x = scopes.find(name);

        if(!x) {
            try{ 
               x = builder.getLangBasic().getBuiltIn(name);
            }catch(...)
            {
            }
        }

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

        // keyword types (is this the right place?) 
        if (!x) {
            if (name == "unit") return builder.getLangBasic().getUnit();
        }

        if (!x) {
            try{ 
            x = builder.getLangBasic().getBuiltIn(name);
            }catch(...)
            {
            }
        }

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
       // std::cout << op << std::endl;
       // std::cout << " " << left << " : " << left->getType() << std::endl;
       // std::cout << " " << right << " : " << right->getType() << std::endl;

        // right side must be always a value
        auto b = getOperand(right);

        // assign
        if (op == "="){
            return builder.assign(left, b);
        }

        // if not assign, then left operand must be a value as well
        auto a = getOperand(left);

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

    TypePtr inspire_driver::genGenericType(const location& l, const std::string& name, 
                                           const TypeList& params, const IntParamList& iparamlist){
        
        if (name == "ref"){
            if (iparamlist.size() != 0 || params.size() != 1) error(l, "malform ref type");
            else return builder.refType(params[0]);
        }
        if (name == "channel"){
            if (iparamlist.size() != 1 || params.size() != 1) error(l, "malform channel type");
            else return builder.channelType(params[0], iparamlist[0]);
        }
        if (name == "struct"){
        }
        if (name == "union"){
        }
        if (name == "vector"){
            if (iparamlist.size() != 1 || params.size() != 1) error(l, "malform vector type");
            else return builder.vectorType(params[0], iparamlist[0]);
        }
        if (name == "array"){
            if (iparamlist.size() != 1 || params.size() != 1) error(l, "malform array type");
            else return builder.arrayType(params[0], iparamlist[0]);
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

    TypePtr inspire_driver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, bool closure){
        return builder.functionType(params, retType, closure?FK_CLOSURE:FK_PLAIN);
    }

    ExpressionPtr inspire_driver::genLambda(const location& l, const VariableList& params, StatementPtr body){
        TypeList paramTys;
        for (const auto& var : params) paramTys.push_back(var.getType());

        TypePtr retType;
        std::set<TypePtr> allRetTypes;
        visitDepthFirstOnce (body, [&allRetTypes] (const ReturnStmtPtr& ret){
            allRetTypes.insert(ret.getReturnExpr().getType());
        });
        if (allRetTypes.size() > 1){
            error(l, "the lambda returns more than one type");
            return ExpressionPtr();
        }
        if (!allRetTypes.empty()) retType = *allRetTypes.begin();
        else                   retType = builder.getLangBasic().getUnit();

        auto funcType = genFuncType(l, paramTys, retType); 
        return builder.lambdaExpr(funcType.as<FunctionTypePtr>(), params, body);
    }

    ExpressionPtr inspire_driver::genClosure(const location& l, const VariableList& params, StatementPtr stmt){

        CallExprPtr call;
        if (stmt.isa<CallExprPtr>()){
            call = stmt.as<CallExprPtr>();
        } 
        else if( stmt->getNodeCategory() == NC_Expression){
            call = builder.id(stmt.as<ExpressionPtr>());
        }
        else if (transform::isOutlineAble(stmt)) {
            call = transform::outline(builder.getNodeManager(), stmt);  
        }

        // check whether call-conversion was successful
        if (!call) {
            error(l, "Not an outline-able context!");
            return ExpressionPtr();
        }

        // build bind expression
        return builder.bindExpr(params, call);
    }

    ExpressionPtr inspire_driver::genCall(const location& l, const ExpressionPtr& func, ExpressionList args){


        auto ftype = func->getType();
        if (!ftype.isa<FunctionTypePtr>()) error(l, "attempt to call non function expression");    

        auto funcParamTypes = ftype.as<FunctionTypePtr>()->getParameterTypeList();
        if (builder.getLangBasic().isVarList(*funcParamTypes.rbegin())){

            ExpressionList newParams (args.begin(), args.begin() + funcParamTypes.size()-1);
            ExpressionList packParams(args.begin() + funcParamTypes.size(), args.end());
            newParams.push_back(builder.pack(packParams));
            std::swap(args, newParams);
        }

        if (args.size() != funcParamTypes.size())  error(l, "invalid number of arguments in function call"); 
    
        return builder.callExpr(func, args);
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::add_symb(const location& l, const std::string& name, NodePtr ptr){
        if (!scopes.add_symb(name, ptr)) {
            error(l, format("symbol %s redefined", name));
        }
    }
    void inspire_driver::add_symb(const std::string& name, NodePtr ptr){
        scopes.add_symb(name, ptr);
    }

    void inspire_driver::add_unfinish_symbol(const location& l, const std::string& name){
        scopes.add_unfinish_symbol(name);
        // generate a temporary callable variable to be fixed once the bodies are visited
        scopes.add_symb(name, builder.variable(builder.functionType(TypeList(), builder.getLangBasic().getUnit()))); 
    }
    void inspire_driver::close_unfinish_symbol(const location& l, const NodePtr& node){
        auto name = scopes.get_unfinish_symbol();
        scopes.add_symb(name, node);
    }
    void inspire_driver::all_symb_defined(const location& l){
        if (!scopes.all_symb_defined()) error(l, "not all let names were defined");
    }

    void inspire_driver::open_scope(const location& l, const std::string& name){
        scopes.open_scope(name);
    }

    void inspire_driver::close_scope(const location& l, const std::string& name){
        scopes.close_scope(name);
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Error management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::error (const location& l, const std::string& m)const {

      errors.push_back(t_error(l, m));
    }


    void inspire_driver::error (const std::string& m)const {
      std::cerr << m << std::endl;
    }

namespace {
    std::vector<std::string> split_string(const std::string& s){
        std::vector<std::string> res;
        std::string delim = "\n";

        auto start = 0U;
        auto end = s.find(delim);
        if (end == std::string::npos) {
            res.push_back(s);
        }
        while (end != std::string::npos)
        {
            auto tmp = s.substr(start, end - start);
            std::replace( tmp.begin(), tmp.end(), '\t', ' ');
            res.push_back(tmp);
            start = end + delim.length();
            end = s.find(delim, start);
        }
        assert(res.size() > 0);
        return res;
    }
}

    void inspire_driver::print_errors(std::ostream& out)const {

        auto buffer = split_string(str);
        int line = 1;
        for (const auto& err : errors){

            int lineb = err.l.begin.line;
            int linee = err.l.end.line;

            out << "ERROR: " << err.l << " " << err.msg << std::endl;
            for (; line< lineb; ++line); 
            for (; line< linee; ++line); out << buffer[line-1] << std::endl;

            int colb = err.l.begin.column;
            int cole = err.l.end.column;

            for (int i =0; i < colb-1; ++i) std::cerr << " ";
            out << "^";
            for (int i =0; i < cole - colb -1; ++i) std::cerr << "~";
            out << std::endl;

        }
    }


} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

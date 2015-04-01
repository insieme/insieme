#include <algorithm>
#include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/parser3/detail/scanner.h"

// this last one is generated and the path will be provided to the command
#include "inspire_parser.hpp"

namespace insieme{
namespace core{
namespace parser3{
namespace detail{

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ scope manager ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


    void DeclarationContext::open_scope(const std::string& msg){
        //for (unsigned i =0; i< scope_stack.size(); ++i) std::cout << " ";
        scope_stack.push_back(ctx_map_type());
    }
    void DeclarationContext::close_scope(const std::string& msg){
        scope_stack.pop_back();
        //for (unsigned i =0; i< scope_stack.size(); ++i) std::cout << " ";
    }

    bool DeclarationContext::add_symb(const std::string& name, NodePtr node){
        if (scope_stack.empty()) {
            if (global_scope.find(name) != global_scope.end()) { 
                return false;
            }
            global_scope[name] =  node;
        }
        else  {
            if (scope_stack.back().find(name) != scope_stack.back().end()) { 
                return false;
            }
            scope_stack.back()[name] =  node;
        }
        return true;
    }

    NodePtr DeclarationContext::find(const std::string& name) const{
        ctx_map_type::const_iterator mit;
        for (auto it = scope_stack.rbegin(); it != scope_stack.rend(); ++it){
            if ((mit = it->find(name)) != it->end()) return mit->second;
        }
        if ((mit = global_scope.find(name)) != global_scope.end()) return mit->second;
        return nullptr;
    }

    void DeclarationContext::add_unfinish_symbol(const std::string& name){
        unfinished_symbols.push_back(name);
        let_symbols.push_back(name);
    }

    std::string DeclarationContext::get_unfinish_symbol(){
        auto x = unfinished_symbols.front();
        unfinished_symbols.erase(unfinished_symbols.begin());
        return x;
    }

    bool DeclarationContext::all_symb_defined(){
        let_symbols.clear();
        return unfinished_symbols.empty();
    }

    bool DeclarationContext::is_unfinished(const std::string& name)const {
        return (std::find (let_symbols.begin(), let_symbols.end(), name) != let_symbols.end());
    }

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ inspire_driver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


    inspire_driver::inspire_driver (const std::string &f, NodeManager& nm)
      : mgr(nm), builder(mgr), file("global scope"), str(f), result(nullptr), glob_loc(&file)
    {
    }

    inspire_driver::~inspire_driver ()
    {
    }

    ProgramPtr inspire_driver::parseProgram ()
    {
        std::stringstream ss(str);
        inspire_scanner scanner(&ss);
        auto ssymb = inspire_parser::make_FULL_PROGRAM(glob_loc);
        auto* ptr = &ssymb;

        inspire_parser parser (*this, &ptr, scanner);
        int fail = parser.parse ();

        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ProgramPtr>();
    }

    TypePtr inspire_driver::parseType ()
    {
        std::stringstream ss(str);
        inspire_scanner scanner(&ss);
        auto ssymb = inspire_parser::make_TYPE_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr, scanner);
        int fail = parser.parse ();
        //scanner->scan_end ();
        //delete scanner;
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<TypePtr>();
    }

    StatementPtr inspire_driver::parseStmt ()
    {
        std::stringstream ss(str);
        inspire_scanner scanner(&ss);
        auto ssymb = inspire_parser::make_STMT_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr, scanner);
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<StatementPtr>();
    }

    ExpressionPtr inspire_driver::parseExpression ()
    {
        std::stringstream ss(str);
        inspire_scanner scanner(&ss);
        auto ssymb = inspire_parser::make_EXPRESSION_ONLY(glob_loc);
        auto* ptr = &ssymb;
        inspire_parser parser (*this, &ptr, scanner);
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ExpressionPtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


    LiteralPtr inspire_driver::get_tag_function(const std::string& name){

        if (!rec_function_vars[name].first){
            LiteralPtr lit = builder.literal(builder.stringValue(name), 
                                             builder.functionType(TypeList(), builder.getLangBasic().getUnit()));   
            return rec_function_vars[name].first = lit;
        }
        else{
            return rec_function_vars[name].first;
        }
    }




    ExpressionPtr inspire_driver::findSymbol(const location& l, const std::string& name) {

        if (scopes.is_unfinished(name)) {
            // this is the use of a recursive lambda call before is defined, return an callable type and wait for ammend
            auto flit =  get_tag_function(name);
            // touch the map, the real type will not be known until the prototype is parsed
            rec_function_vars[name] = {flit, nullptr}; 
            return flit;
        }

        auto x = scopes.find(name);

        if(!x) {
            try{ 
            std::cout << "                                                  leaving" << std::endl;
               x = builder.getLangBasic().getBuiltIn(name);
            std::cout << "                                                  come back" << x << std::endl;
            }catch(...)
            {
                std::cout <<" exception thrown " << std::endl;
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

    TypePtr inspire_driver::findType(const location& l, const std::string& name){

        if (scopes.is_unfinished(name)) {
            // this is the use of a recursive lambda call before is defined, return an callable type and wait for ammend
            return builder.typeVariable(name);
        }

        auto x = scopes.find(name);

        // keyword types (is this the right place?) 

        
        return x.as<TypePtr>();

 //       if (!x) {
 //           if (name == "unit") return builder.getLangBasic().getUnit();
 //       }
 //       if (!x) {
 //           if (name == "bool") return builder.getLangBasic().getBool();
 //       }

        if (!x) {
            try{ 
                x = builder.getLangBasic().getBuiltIn(name);
            }catch(...) { }
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
        std::cout << op << std::endl;
        std::cout << " " << left << " : " << left->getType() << std::endl;
        std::cout << " " << right << " : " << right->getType() << std::endl;

        // right side must be always a value
        auto b = getOperand(right);

        // assign
        if (op == "="){
            return builder.assign(left, b);
        }
        if (op == "["){
            if (builder.getLangBasic().isSignedInt(b->getType())) {
                b = builder.castExpr(builder.getLangBasic().getUInt8(), b);
            }
			if (left->getType()->getNodeType() == NT_RefType) {
			    return builder.arrayRefElem(left, b);
			}
			return builder.arraySubscript(left, b);		// works for arrays and vectors
        }


        // if not assign, then left operand must be a value as well
        auto a = getOperand(left);

        // comparators
        if (op == "==") return builder.eq(a,b);
        if (op == "!=") return builder.ne(a,b);
        if (op == "<") return builder.lt(a,b);
        if (op == ">") return builder.gt(a,b);
        if (op == "<=") return builder.le(a,b);
        if (op == ">=") return builder.ge(a,b);

        // bitwise 
        if (op == "&") return builder.bitwiseAnd(a,b);
        if (op == "|") return builder.bitwiseOr(a,b);
        if (op == "^") return builder.bitwiseXor(a,b);
        
        // logic
        if (op == "||") return builder.logicOr(a,b);
        if (op == "&&") return builder.logicAnd(a,b);

        // arithm
        if (op == "+") return builder.add(a,b);
        if (op == "-") return builder.sub(a,b);

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

        return genLambda(l, params, retType, body);
    }

    ExpressionPtr inspire_driver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body){
        // TODO: cast returns to apropiate type
        TypeList paramTys;
        for (const auto& var : params) paramTys.push_back(var.getType());

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

    ExpressionPtr inspire_driver::genCall(const location& l, const ExpressionPtr& callable, ExpressionList args){

        ExpressionPtr func = callable;
        //std::cout << "call @" << l << std::endl;

        // if the call is to a not yet defined symbol, we might want to change the type (now can be completed)
        if (func.isa<LiteralPtr>()){
		    auto name = func.as<LiteralPtr>().getValue()->getValue();
            if (rec_function_vars.find(name) != rec_function_vars.end()){
                // we can redefine the symbol now
                TypeList l;
                for(const auto& e : args) l.push_back(e.getType());
                
                func = builder.literal(name, builder.functionType(l, builder.getLangBasic().getUnit()));
                rec_function_vars[name].first = func.as<LiteralPtr>();
            }

        }

        auto ftype = func->getType();
        if (!ftype.isa<FunctionTypePtr>()) error(l, "attempt to call non function expression");    

        auto funcParamTypes = ftype.as<FunctionTypePtr>()->getParameterTypeList();
        if (!funcParamTypes.empty() ){
            // fix variadic arguments
            if (builder.getLangBasic().isVarList(*funcParamTypes.rbegin())){

                ExpressionList newParams (args.begin(), args.begin() + funcParamTypes.size()-1);
                ExpressionList packParams(args.begin() + funcParamTypes.size(), args.end());
                newParams.push_back(builder.pack(packParams));
                std::swap(args, newParams);
            }
        }

        if (args.size() != funcParamTypes.size())  error(l, "invalid number of arguments in function call"); 
    
        return builder.callExpr(func, args);
    }

    ExpressionPtr inspire_driver::genTagExpression(const location& l, const TypePtr& type, const NamedValueList& fields){
        if (!type.isa<StructTypePtr>()) {
            error (l, "not struct type"); 
            return nullptr;
        }
	    return builder.structExpr(type.as<StructTypePtr>(), fields);
    }
    ExpressionPtr inspire_driver::genTagExpression(const location& l, const NamedValueList& fields){
        // build up a struct type and call the other func
        NamedTypeList fieldTypes;
        for (const auto& f : fields) fieldTypes.push_back(builder.namedType(f->getName(), f->getValue()->getType()));

        TypePtr type =  builder.structType(builder.stringValue(""), fieldTypes);  
        return genTagExpression(l, type, fields);
       
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::add_symb(const location& l, const std::string& name, NodePtr ptr){
        if (!scopes.add_symb(name, ptr)) {
            error(l, format("symbol %s redefined", name));
        }
    }

    void inspire_driver::add_symb(const std::string& name, NodePtr ptr){
        add_symb(glob_loc, name, ptr);
    }

    void inspire_driver::add_unfinish_symbol(const location& l, const std::string& name){
        scopes.add_unfinish_symbol(name);
    }

    bool inspire_driver::close_unfinish_symbol(const location& l, const NodePtr& node){
        if (!node) return false;
        auto name = scopes.get_unfinish_symbol();

        // this node may be modified by every unfinished node that it might have used
        recursive_symbols[name] = node;
        return true;
    }

namespace {
    bool contains_type_variables (const TypePtr& t){
        
        bool contains = false;
        visitDepthFirstOnce(t, [&](const TypePtr& t){
            if (t.isa<TypeVariablePtr>()) contains = true;
        });
        return contains;
    }
}

    bool inspire_driver::all_symb_defined(const location& l){

        if (!scopes.all_symb_defined()) {
            //TODO: nested  recursion not supported, add scoping mechanisms to this thing.
            error(l, "not all let identifiers were defined");
            return false;
        }

        // amend all nodes that might be recursive
        std::map<std::string, TypePtr> rec_types;
        std::map<std::string, LambdaExprPtr> rec_funcs;
        for (const auto& rec : recursive_symbols){
            if (rec.second.isa<TypePtr>())            rec_types[rec.first] = rec.second.as<TypePtr>();
            else if (rec.second.isa<LambdaExprPtr>()) rec_funcs[rec.first] = rec.second.as<LambdaExprPtr>();
            else {
                error(l, format("the recursive symbol %s is neither a type or function", rec.first));
                return false;
            }
        }


        // FIRST TYPES
        {
            std::vector<RecTypeBindingPtr> type_defs;
            std::vector<std::string> names;
            for (const auto& rec : rec_types){
                if(!contains_type_variables(rec.second.as<TypePtr>())) {
                    add_symb(l, rec.first, rec.second);
                }
                else {
                    type_defs.push_back(builder.recTypeBinding(builder.typeVariable(rec.first), rec.second.as<TypePtr>()));
                    names.push_back(rec.first);
                }
            }
            RecTypeDefinitionPtr fullType = builder.recTypeDefinition(type_defs);
            for (const auto& n : names) add_symb(l, n, builder.recType(builder.typeVariable(n), fullType));
        }


        // THEN FUNCTIONS
        {
            NodeMap replacements;
           
            // collect al symbols that need to be replaced
            for (const auto& rec : rec_funcs){
                // build literal (to match the used one)
                LiteralPtr flit = get_tag_function(rec.first);

                // build variable with the right typing
                VariablePtr fvar = builder.variable(rec.second.getType());
                replacements.insert({flit, fvar});
             //   count++;
            }

            std::vector<LambdaBindingPtr> lambda_defs;
            // replace symbols and create lambda bindings
            for (const auto& rec : rec_funcs){
                LiteralPtr flit = get_tag_function(rec.first);
                StatementPtr body = transform::replaceAllGen(mgr, rec.second->getBody(), replacements, true);
                auto params = rec.second->getParameterList();
                auto type = rec.second->getType();

                VariablePtr var = replacements[flit].as<VariablePtr>();
                lambda_defs.push_back(builder.lambdaBinding(var, builder.lambda(type.as<FunctionTypePtr>(), params, body)));
            }
            LambdaDefinitionPtr lambdaDef = builder.lambdaDefinition(lambda_defs);

            // generate a callable symbol for each name
            for (const auto& rec : rec_funcs){
                LiteralPtr flit = get_tag_function(rec.first);
                VariablePtr var = replacements[flit].as<VariablePtr>();

                add_symb(l, rec.first, builder.lambdaExpr(var, lambdaDef));
            }
        }

        // clear the scope
        recursive_symbols.clear();
        return true;
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

    bool inspire_driver::where_errors()const{
        return !errors.empty();
    }

namespace {

    //TODO: move this to string utils
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

    //TODO: move this to  utils
    const std::string RED   ="\033[31m";
    const std::string GREEN ="\033[32m";
    const std::string BLUE  ="\033[34m";
    const std::string BLACK ="\033[30m";
    const std::string CYAN  ="\033[96m";
    const std::string YELLOW="\033[33m";
    const std::string GREY  ="\033[37m";

    const std::string RESET ="\033[0m";
    const std::string BOLD  ="\033[1m";

} // annon
    
    void inspire_driver::print_errors(std::ostream& out)const {

        auto buffer = split_string(str);
        int line = 1;
        for (const auto& err : errors){

            int lineb = err.l.begin.line;
            int linee = err.l.end.line;

            out << RED << "ERROR: "  << RESET << err.l << " " << err.msg << std::endl;
            for (; line< lineb; ++line); 
            for (; line< linee; ++line); out << buffer[line-1] << std::endl;

            int colb = err.l.begin.column;
            int cole = err.l.end.column;

            for (int i =0; i < colb-1; ++i) std::cerr << " ";
            out << GREEN << "^";
            for (int i =0; i < cole - colb -1; ++i) std::cerr << "~";
            out << RESET << std::endl;


        }
    }


} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

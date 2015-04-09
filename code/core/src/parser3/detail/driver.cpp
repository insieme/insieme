#include <algorithm>
#include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
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

  /* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ inspire_driver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


    inspire_driver::inspire_driver (const std::string& str, NodeManager& mgr, const DeclarationContext& ctx)
      : scopes(ctx), mgr(mgr), builder(mgr), file("global scope"), str(str), result(nullptr), glob_loc(&file),
        ss(str), scanner(&ss),
        parser(*this, scanner),
        let_count(0), inhibit_building_count(false)
    {
        // std::cout << "parse: " << str << std::endl;
    }

    inspire_driver::~inspire_driver () {
    }

    ProgramPtr inspire_driver::parseProgram () {
        scanner.set_start_program();
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ProgramPtr>();
    }

    TypePtr inspire_driver::parseType () {
        scanner.set_start_type();
        inspire_parser parser (*this, scanner);
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<TypePtr>();
    }

    StatementPtr inspire_driver::parseStmt () {
        scanner.set_start_statement();
        inspire_parser parser (*this, scanner);
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<StatementPtr>();
    }

    ExpressionPtr inspire_driver::parseExpression () {
        scanner.set_start_expression();
        inspire_parser parser (*this, scanner);
        int fail = parser.parse ();
        if (fail) {
            print_errors();
            return nullptr;
        }
        return result.as<ExpressionPtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ExpressionPtr inspire_driver::findSymbol(const location& l, const std::string& name) {

        auto x = scopes.find(name);

        if(!x) {
            try{ 
               x = builder.getLangBasic().getBuiltIn(name);
            }catch(...)
            {
               // pass, nothing to do really
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

        if (std::find(let_names.begin(), let_names.end(), name) != let_names.end()){
            // this is a type in a let binding usage, it might be recursive, so we "mark" it
            return builder.typeVariable(name);
        }

        auto x = scopes.find(name);
        if (x && !x.isa<TypePtr>()){
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
      //  std::cout << op << std::endl;
      //  std::cout << " " << left << " : " << left->getType() << std::endl;
      //  std::cout << " " << right << " : " << right->getType() << std::endl;

        // assign
        // left side must be a ref, right side must be untouched
        if (op == "="){

            if (!left.getType().isa<RefTypePtr>()) {
                error(l, format("left side on assignment must be a reference and is %s", toString(left.getType())));
            }

            return builder.assign(left, right);
        }

        auto b = getOperand(right);
        // left side is untouched because of reference subscript operators
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
    ExpressionPtr inspire_driver::genFieldAccess(const location& l, const ExpressionPtr& expr, const std::string& fieldname){

            StructTypePtr structType;
            if (expr->getType().isa<StructTypePtr>()) {
                structType = expr->getType().as<StructTypePtr>();
            } else if (expr->getType().isa<RefTypePtr>()) {
                TypePtr type = expr->getType().as<RefTypePtr>()->getElementType();

                if ( type.isa<RecTypePtr>() ) {
                    type = type.as<RecTypePtr>()->unroll(mgr);
                }

                structType = type.isa<StructTypePtr>();
                if (!structType) {
                    error(l, "Accessing element of non-struct type!");
                    return nullptr;
                }
            } else {
                error(l, "Accessing element of non-struct type!");
                return nullptr;
            }

            // check field
            if (!structType->getNamedTypeEntryOf(fieldname)) {
                error(l, format("Accessing unknown field %s", fieldname));
                return nullptr;
            }

            // create access
            if (expr->getType().isa<RefTypePtr>()) {
                return builder.refMember(expr, fieldname);
            }
            return builder.accessMember(expr, fieldname);
    }

    TypePtr inspire_driver::genGenericType(const location& l, const std::string& name, const TypeList& parents,
                                           const TypeList& params, const IntParamList& iparamlist){
        
        if (name == "ref"){
            if (iparamlist.size() != 0 || params.size() != 1) error(l, "malform ref type");
            else return builder.refType(params[0]);
        }
        if (name == "src"){
            if (iparamlist.size() != 0 || params.size() != 1) error(l, "malform ref type");
            else return builder.refType(params[0], RK_SOURCE);
        }
        if (name == "sink"){
            if (iparamlist.size() != 0 || params.size() != 1) error(l, "malform ref type");
            else return builder.refType(params[0], RK_SINK);
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
        for (const auto& p : params){
            if(!p){
                std::cout <<  "wrong parameter in paramenter list" << std::endl;
                abort();
            }
        }
        for (const auto& p : iparamlist){
            if(!p){
                std::cout <<  "wrong parameter in paramenter list" << std::endl;
                abort();
            }
        }

		return builder.genericType(name, params, iparamlist);

        error(l, "this does not look like a type");
        return nullptr;
    }

    TypePtr inspire_driver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk){
        return builder.functionType(params, retType, fk);
    }

    ExpressionPtr inspire_driver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, 
                                            const StatementPtr& body, const FunctionKind& fk){
        // TODO: cast returns to apropiate type
        TypeList paramTys;
        for (const auto& var : params) paramTys.push_back(var.getType());
        auto funcType = genFuncType(l, paramTys, retType, fk); 
        return builder.lambdaExpr(funcType.as<FunctionTypePtr>(), params, body);
    }

    ExpressionPtr inspire_driver::genClosure(const location& l, const VariableList& params, StatementPtr stmt){

        if (!stmt) {
            error(l, "closure statement malformed");
            return nullptr;
        }
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
            return nullptr;
        }


        // build bind expression
        return builder.bindExpr(params, call);
    }

    ExpressionPtr inspire_driver::genCall(const location& l, const ExpressionPtr& callable, ExpressionList args){

        ExpressionPtr func = callable;

        auto ftype = func->getType();
        if (!ftype.isa<FunctionTypePtr>()) error(l, "attempt to call non function expression");    

        auto funcParamTypes = ftype.as<FunctionTypePtr>()->getParameterTypeList();
        if (!funcParamTypes.empty() ){
            // fix variadic arguments
            if (builder.getLangBasic().isVarList(*funcParamTypes.rbegin())){

                if (args.size() < funcParamTypes.size()){
                    args.push_back(builder.pack(ExpressionList()));
                }
                else if (!builder.getLangBasic().isVarList(args.rbegin()->getType())){
                    ExpressionList newParams (args.begin(), args.begin() + funcParamTypes.size()-1);
                    ExpressionList packParams(args.begin() + funcParamTypes.size(), args.end());
                    newParams.push_back(builder.pack(packParams));
                    std::swap(args, newParams);
                }
            }
        }

        if (args.size() != funcParamTypes.size()) {
            error(l, "invalid number of arguments in function call"); 
            return nullptr;
        }

        ExpressionPtr res;
        try{
            res = builder.callExpr(func, args);
        }catch(...){
            error(l, "malformed call expression");
            return nullptr;
        }
        if(!res) error(l, "malformed call expression");
        return res;
    }

    ExpressionPtr inspire_driver::genTagExpression(const location& l, const TypePtr& type, const ExpressionList& list){

		if (!type){  error(l, "Not a struct type!"); return nullptr; }

        // retrieve struct type, (unroll rec types)
        StructTypePtr structType = type.isa<StructTypePtr>();
        if (type.isa<RecTypePtr>()){
	        structType = type.as<RecTypePtr>()->unroll(type.getNodeManager()).isa<StructTypePtr>();
        }

		if (!structType){  error(l, format("Not a struct type: %s", toString(type))); return nullptr; }
		if (structType->size() != list.size()) { error(l, "init list does not match number of fields"); return nullptr; }

		// build up struct expression
		auto begin = make_paired_iterator(structType->begin(), list.begin());
		auto end = make_paired_iterator(structType->end(), list.end());

		// extract name / value pairs
		NamedValueList values;
		for (auto it = begin; it != end; ++it) {
			values.push_back(builder.namedValue(it->first->getName(), it->second.as<ExpressionPtr>()));
		}

		// build struct expression
		return builder.structExpr(structType, values);
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Address marking   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    ExpressionPtr inspire_driver::mark_address(const location& l, const ExpressionPtr& expr){
        NodePtr res = builder.markerExpr(expr);
        res->attachValue<AddressMark>();
        return res.as<ExpressionPtr>();
    }

    StatementPtr  inspire_driver::mark_address(const location& l, const StatementPtr& stmt){
        NodePtr res = builder.markerStmt(stmt);
        res->attachValue<AddressMark>();
        return res.as<StatementPtr>();
    }

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::add_symb(const location& l, const std::string& name, NodePtr ptr){

        if (!ptr) {
            error(l, format("symbol %s is not well form", name));
            return;
        }

        // ignore wildcard for unused variables
        if (name == "_") return;

        if (!scopes.add_symb(name, ptr)) {
            error(l, format("symbol %s redefined", name));
        }
    }

    void inspire_driver::add_symb(const std::string& name, NodePtr ptr){
        add_symb(glob_loc, name, ptr);
    }

    VariableIntTypeParamPtr inspire_driver::gen_type_param_var(const location& l, const std::string& name){
        auto x = scopes.find(name);
        if(!x) {
            if(name.size() != 2) error(l, format("variable %s needs to have lenght 1", name));
            x = builder.variableIntTypeParam(name[1]);
            add_symb(l, name, x);
        }
        if(!x.isa<VariableIntTypeParamPtr>()) {
            error(l, format("variable %s is not an int type param var", name));
        }
        return x.as<VariableIntTypeParamPtr>();
    }

    VariableIntTypeParamPtr inspire_driver::find_type_param_var(const location& l, const std::string& name){
        auto x = scopes.find(name);
        if (!x) error(l, format("variable %s is not defined in context", name));
        if (!x.isa<VariableIntTypeParamPtr>()) {
                error(l, format("variable %s is not a type param variable", name));
        }
        return x.as<VariableIntTypeParamPtr>();
    }

namespace {
    bool contains_type_variables (const TypePtr& t){
        
        bool contains = false;
        visitDepthFirstOnce(t, [&](const TypePtr& t){
            if (t.isa<TypeVariablePtr>()) contains = true;
        });
        return contains;
    }

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

    std::string get_body_string(const std::string& text, const location& b, const location& e){
        auto tmp = text;

        auto strings = split_string(text);
        for (auto& s : strings) s.append("\n");

        std::vector<std::string> subset (strings.begin()+b.begin.line-1, strings.begin()+e.end.line);
        std::string res;

        subset[subset.size()-1] = subset[subset.size()-1].substr(0, e.end.column-1);
        for (auto it = subset.begin(); it < subset.end(); ++it){
            res.append(*it);
        }
        res = res.substr(b.begin.column-1, res.size());
        // the lambda keyword is lost during parsing, ammend
        return std::string("lambda ").append(res);
    }

}
    void inspire_driver::add_let_name(const location& l, const std::string& name){
        let_names.push_back(name);
    }

    void inspire_driver::add_let_lambda(const location& l, const location& begin, const location& end, 
                                        const TypePtr& retType, const VariableList& params, const FunctionKind& fk){

        // if there are lambdas inside of the derefed lambda, those will be handled by the nested driver
        if (inhibit_building_count >1) return;
        lambda_lets.push_back(Lambda_let(retType, params, get_body_string(str, begin, end), fk));
    }

    void inspire_driver::add_this (const location& l, const TypePtr& classType){
        // gen ref type
        auto refThis = builder.refType(classType);
        // gen var
        auto thisVar = builder.variable(refThis);
        // save in scope
        add_symb(l, "this", thisVar);
    }

    void inspire_driver::add_let_type(const location& l, const TypePtr& type){
        type_lets.push_back(type);
    }

    void inspire_driver::add_let_expression(const location& l, const ExpressionPtr& expr){
        if (!expr) {
            error(l, "no expression translated");
            return;
        }
        expr_lets.insert(expr_lets.begin(), expr);
    }

    void inspire_driver::close_let_statement(const location& l){

         //std::cout << "finish let "<< std::endl;
         // print_location(l);
         // std::cout << "    let count" << let_count << std::endl;
         // std::cout << "    inhibitcount" << inhibit_building_count << std::endl;
         // std::cout << "    let_mames " << let_names << std::endl;
         // std::cout << "    let_type " << type_lets << std::endl;
         // std::cout << "    let_lambd " << lambda_lets.size() << std::endl;

  
        // LAMBDA LETS (functions):
        if(let_names.size() == lambda_lets.size()){
            DeclarationContext temp_scope (scopes);

            std::map<std::string, VariablePtr> funcVars;
            unsigned count = 0;
            for (const auto& name :  let_names) {
                Lambda_let& tmp = lambda_lets[count];

                TypeList types;
                for(const auto& v : tmp.params) types.push_back(v->getType());

                funcVars[name] = builder.variable(builder.functionType(types, tmp.retType, tmp.fk));
                count ++;

            }

            // generate a nested parser to parse the body with the right types
            std::vector<std::pair<VariablePtr, LambdaExprPtr>> funcs;
            count =0;
            for(const auto& let : lambda_lets){
                inspire_driver let_driver(let.expression, mgr, temp_scope);
                for (const auto pair : funcVars) let_driver.add_symb(pair.first, pair.second);
                try{
                    ExpressionPtr lambda = let_driver.parseExpression();
                    if (!lambda) { 
                        error(l, "lambda expression is wrong");
                        return;
                    }
                    funcs.push_back({funcVars[let_names[count]],lambda.as<LambdaExprPtr>()});
                }catch(...){
                    error(l, "something went really wrong parsing lambda body");
                }

                count ++;
            }
        
                // if all variables in body are defined, this is regular function
            if (funcs.size() == 1 && analysis::getFreeVariables(funcs[0].second).empty()){
                add_symb(l, let_names[0], funcs[0].second);
            }
            else{
                // generate funcs and bound them to the var types
                std::vector<LambdaBindingPtr> lambdas;
                for (const auto& lf : funcs){
                    auto var = lf.first;
                    auto params = lf.second->getParameterList();
                    auto type = lf.second->getType();
                    auto body = lf.second->getBody();
                    lambdas.push_back(builder.lambdaBinding(var, builder.lambda(type.as<FunctionTypePtr>(), params, body)));
                }
                LambdaDefinitionPtr lambdaDef = builder.lambdaDefinition(lambdas);
                for (const auto& fv : funcVars){
                    add_symb(l, fv.first, builder.lambdaExpr(fv.second, lambdaDef));
                }

            }

            lambda_lets.clear();
        }
        // TYPE LETS
        else if(let_names.size() == type_lets.size()){

            std::vector<RecTypeBindingPtr> type_defs;
            std::vector<std::string> names;
            unsigned count = 0;
            for (const auto& type : type_lets){
            
                const std::string& name = let_names[count];
                if(!contains_type_variables(type)) {
                    add_symb(l, name, type);
                }
                else {
                    type_defs.push_back(builder.recTypeBinding(builder.typeVariable(name), type));
                    names.push_back(name);
                }
                count ++;
            }
            RecTypeDefinitionPtr fullType = builder.recTypeDefinition(type_defs);
            for (const auto& n : names) add_symb(l, n, builder.recType(builder.typeVariable(n), fullType));

            type_lets.clear();
        }
        // Expression LETS (includes closures)
        else if(let_names.size() == expr_lets.size()){
            unsigned count = 0;
            for (const auto& name :  let_names) {
                add_symb(l, name, expr_lets[count]);
                count++;
            }

            expr_lets.clear();
        }
        else{ 
    
            std::cout << "let_mames " << let_names << std::endl;
            std::cout << "let_type " << type_lets << std::endl;
            std::cout << "let_expr " << expr_lets << std::endl;
            std::cout << "let_lambd " << lambda_lets.size() << std::endl;
            error(l, "mixed type/function/closure let not allowed");
        }
       
        let_names.clear();
        let_count --;
        inhibit_building_count = false;
    }

    void inspire_driver::open_scope(const location& l, const std::string& name){
        scopes.open_scope(name);
    }

    void inspire_driver::close_scope(const location& l, const std::string& name){
        scopes.close_scope(name);
    }


    void inspire_driver::set_inhibit(bool flag){
        if (!flag ) { 
            if(inhibit_building_count>0) inhibit_building_count --;
        }
        else{
            inhibit_building_count ++;
        }
    }
    bool inspire_driver::inhibit_building()const{
        return inhibit_building_count > 0;
    }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Debug tools  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

namespace {

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

    void inspire_driver::print_location(const location& l)const{
        auto buffer = split_string(str);
        int line = 1;

        int lineb = l.begin.line;
        int linee = l.end.line;

        for (; line< lineb; ++line); 
        for (; line< linee; ++line); std::cout << buffer[line-1] << std::endl;

        int colb = l.begin.column;
        int cole = l.end.column;

        for (int i =0; i < colb-1; ++i) std::cout << " ";
        std::cout << GREEN << "^";
        for (int i =0; i < cole - colb -1; ++i) std::cout << "~";
        std::cout << RESET << std::endl;
    }
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Error management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    void inspire_driver::error (const location& l, const std::string& m)const {
      errors.push_back(t_error(l, m));
    }

    void inspire_driver::error (const std::string& m)const {
      std::cerr << m << std::endl;
    }

    bool inspire_driver::where_errors()const{
        if( !errors.empty()) print_errors();
        return !errors.empty();
    }

    
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

            for (int i =0; i < colb-1; ++i) out << " ";
            out << GREEN << "^";
            for (int i =0; i < cole - colb -1; ++i) out << "~";
            out << RESET << std::endl;


        }
    }


} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme

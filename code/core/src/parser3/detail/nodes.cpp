#include<string>
#include<iostream>
#include<cassert>
#include<vector>

#include "insieme/core/parser3/detail/nodes.hpp"


std::ostream& operator<< (std::ostream& out, const op_code& code){
    switch(code){
        case SUM:
            out << "+";
            break;
        case SUB: 
            out << "-";
            break;
        case MUL:
            out << "*";
            break;
        case DIV:
            out << "/";
            break;
        case DEREF: 
            out << "* (deref)";
            break;
        case NOT:
            out << "!+";
            break;
        case MOD:
            out << "MOD";
            break;
        case MINUS:
            out << "-(change sign)";
            break;
        case SUBSCRIPT: 
            out << " subscript";
            break;
        case MEMBACCESS:
            out << " memaccess";
            break;
        case ASSIGN:
            out << " = ";
            break;
        default:
            out << "UNKNOWN OP";
    }
    return out;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

std::ostream& NLitType::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << name;
    return os;
}

std::ostream& NIntTypeParam::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << "#" << val;
    return os;
}

std::ostream& NComposedType::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << name;
    os << "<";
    for (const auto t : tparams) t->print(os,"");
    os << ">";
    return os;
}

std::ostream& NFuncType::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << "(";
    for (const auto t : tparams) t->print(os,"");
    os << ") -> ";
    ret->print(os, "");
    return os;
}

std::ostream& NClosureType::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << "(";
    for (const auto t : tparams) t->print(os,"");
    os << ") => ";
    ret->print(os, "");
    return os;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

std::ostream& NVariableDecl::print (std::ostream& os, const std::string& prefix)const{
    
    std::string bodyindent = prefix;
    bodyindent.append(" |-");

    os << prefix << "var decl\n";
    type->print(os,bodyindent); os << std::endl;
    os << name;
    if(initialization) {
        os << " = ";
        initialization->print(os,bodyindent); os << std::endl;
    }
    return os;
}

std::ostream& NWhileLoop::print (std::ostream& os, const std::string& prefix) const{

    std::string bodyindent = prefix;
    bodyindent.append(" |-");

    os << prefix << "while\n";
    cond->print(os,bodyindent); os << std::endl;
    body->print(os,bodyindent); os << std::endl;
    
    return os;
}

std::ostream& NForLoop::print (std::ostream& os, const std::string& prefix) const{
    std::string bodyindent = prefix;
    bodyindent.append("  ");

    os << prefix;
    iteratorType->print(os,"for (");
    it->print(os," ");
    lbound->print(os, " = ");
    ubound->print(os, " .. ");
    os << ")\n";
    body->print(os,bodyindent);
    return os;
}

std::ostream& NCompound::print (std::ostream& os, const std::string& prefix) const{
    std::string bodyindent = prefix;
    bodyindent.append("  ");
    os << prefix << "{\n";
    for (const auto s : stmts) {
        s->print(os, bodyindent);
        os << "\n";
    }
    os << prefix << "}";
    return os;
}
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

std::ostream& NLiteralExpr::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << val;
    return os;
}

std::ostream& NSynbolExpr::print (std::ostream& os, const std::string& prefix) const{
    os << prefix << name;
    return os;
}

std::ostream& NCallExpr::print (std::ostream& os, const std::string& prefix) const{
    func->print(os, prefix);
    os << "(";
    for (const auto arg: args) {
        arg->print(os, "");
        os << ", ";
    }
    os << ")";
    return os;
}

std::ostream& NUnaryExpr::print (std::ostream& os, const std::string& prefix) const{
    std::string indent = prefix;
    indent.append(" |-");

    os << prefix << "Unary operator: " << op << "\n";
    expr->print(os, indent);
    os << "\n";
    return os;
}

std::ostream& NBinaryExpr::print (std::ostream& os, const std::string& prefix) const{
    std::string indent = prefix;
    indent.append(" |-");

    os << prefix << "Binary operator: " << op << "\n";
    left->print(os, indent);
    os << "\n";
    right->print(os, indent);
    return os;
}

std::ostream& NTernaryExpr::print (std::ostream& os, const std::string& prefix) const{
    
    std::string indent = prefix;
    indent.append(" |-");

    os << prefix << "Ternary operator: ? \n" ;
    cond->print(os, indent);
    os << "\n";
    yes->print(os, indent);
    os << "\n";
    no->print(os, indent);
    return os;
}

std::ostream& NLambdaExpression::print (std::ostream& os, const std::string& prefix) const{
    
    std::string indent = prefix;
    indent.append(" |-");

    os << prefix << "function: ? \n" ;
    retTy->print(os, indent);
    os << "\n{";
    for (const auto p : paramList) {
        p->print(os, indent);
        os << "\n";
    }
    os << "}";
    body->print(os, indent);
    return os;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


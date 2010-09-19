/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "backend_convert.h"

#include "annotated_ptr.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;

CodePtr FunctionManager::getFunction(const LambdaExprPtr& lambda, const Identifier& ident) {
	auto codeIt = functionMap.find(ident);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	auto funType = dynamic_pointer_cast<const FunctionType>(lambda->getType());

	// generate a new function from the lambda expression
	CodePtr cptr = std::make_shared<CodeFragment>(string("fundef_codefragment_") + ident.getName());
	CodeStream& cs = cptr->getCodeStream();
	// write the function header
	cs << cc.getTypeMan().getTypeName(funType->getReturnType()) << " " << ident.getName() << "(";
	// handle arguments
	cs << join(", ", lambda->getParams(), [this](std::ostream& os, const ParamExprPtr& param) -> std::ostream& {
		return (os << this->cc.getTypeMan().getTypeName(param->getType()) << " " << param->getIdentifier().getName());
	});
	cs << ") {" << CodeStream::indR << "\n";
	// generate the function body
	ConvertVisitor visitor(cc, cptr);
	visitor.visit(lambda->getBody());
	cs << CodeStream::indL << "\n}\n\n";
	// insert into function map and return
	functionMap.insert(std::make_pair(ident, cptr));
	return cptr;
}

CodePtr FunctionManager::getFunctionLiteral(const core::FunctionTypePtr& type, const string& name) {
	// TODO refactor duplication w/ above
	CodePtr cptr = std::make_shared<CodeFragment>(string("fundecl_codefragment_") + name);
	CodeStream& cs = cptr->getCodeStream();
	cs << cc.getTypeMan().getTypeName(type->getReturnType()) << " " << name << "(";
	auto argType = type->getArgumentType();
	if(auto tupleArgType = dynamic_pointer_cast<const TupleType>(argType)) {
		cs << join(", ", tupleArgType->getElementTypes(), [this](std::ostream& o, const TypePtr& cur) -> std::ostream& {
			return (o << this->cc.getTypeMan().getTypeName(cur));
		});
	} // TODO handle other argument types
	cs << ");\n";
	return cptr;
}


ConversionContext::ConvertedCode ConversionContext::convert(const core::ProgramPtr& prog) {
	ConvertedCode converted;
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {
		ConvertVisitor convVisitor(*this);
		convVisitor.visit(ep);
		converted.insert(std::make_pair(ep, convVisitor.getCode()));
	});
	return converted;
}


void ConvertVisitor::visitLambdaExpr( const LambdaExprPtr& ptr ) {
	if(auto cname = ptr.getAnnotation(c_info::CNameAnnotation::KEY)) { // originally a named C function
		defCodePtr->addDependency(cc.getFuncMan().getFunction(ptr, cname->getIdent()));
		// TODO print function name
	}
	else { // an unnamed lambda
		assert(0 && "Unnamed lambda not yet implemented");
	}
}

void ConvertVisitor::visitCallExpr(const CallExprPtr& ptr) {
	const std::vector<ExpressionPtr>& args = ptr->getArguments();
	visit(ptr->getFunctionExpr());
	cStr << "(";
	if(args.size()>0) {
		visit(args.front());
		for_each(args.cbegin()+1, args.cend(), [&, this](const ExpressionPtr& curArg) {
			this->cStr << ", ";
			this->visit(curArg);
		});
	}
	cStr << ")";
}

void ConvertVisitor::visitLiteral(const LiteralPtr& ptr) {
	auto typePtr = ptr->getType();
	const string& val = ptr->getValue();
	if(*typePtr == lang::TYPE_STRING_VAL) {
		// TODO change once the decision is made how string literals should be represented int the AST
		if(val.empty() || val[0] != '"' || val[val.length()-1] != '"') {
			cStr << "\"" << val << "\"";
		} else {
			cStr << val;
		}
	} 
	else if(auto funType = dynamic_pointer_cast<const FunctionType>(typePtr)) {
		auto funLiteralDeclCode = cc.getFuncMan().getFunctionLiteral(funType, val); 
		defCodePtr->addDependency(funLiteralDeclCode);
		cStr << val;
	}
	else {
		cStr << val;
	}
}

void ConvertVisitor::visitReturnStmt( const ReturnStmtPtr& ptr )
{
	cStr << "return ";
	if(*ptr->getReturnExpr()->getType() != lang::TYPE_UNIT_VAL) {
		visit(ptr->getReturnExpr());
	}
	cStr << ";";
}



string TypeManager::getTypeName(const core::TypePtr type) {
	SimpleTypeConverter conv(cc.getNameGen());
	return conv.visit(type);
	// TODO handle complex types
}

string TypeManager::getTypeDecl(const core::TypePtr type) {
	//TODO
	return string();
}

CodePtr TypeManager::getTypeDefinition(const core::TypePtr type) {
	//TODO
	return CodePtr();
}


string SimpleTypeConverter::visitGenericType(const GenericTypePtr& ptr) {
	if(lang::isUnitType(*ptr)) {
		return "void";
	} else
	if(lang::isIntType(*ptr)) {
		// TODO better handling for int sizes
		string qualifier = lang::isUIntType(*ptr) ? "unsigned " : "";
		switch(lang::getNumBytes(*ptr)) {
		case 1: return qualifier + "char";
		case 2: return qualifier + "short";
		case 4: return qualifier + "int";
		case 8: return qualifier + "long";
		default: return ptr->getName();
		}
	} else
	if(lang::isBoolType(*ptr)) {
		return "bool";
	} else
	if(lang::isRealType(*ptr)) {
		return ptr->getName();
	} else
	if(*ptr == lang::TYPE_STRING_VAL) {
		return "string";
	} else
	if(*ptr == lang::TYPE_CHAR_VAL) {
		return "char";
	} else
	if(*ptr == lang::TYPE_VAR_LIST_VAL) {
		return "...";
	}
	//assert(0 && "Unhandled generic type.");
	return string("[[unhandled_simple_type: ") + ptr->getName() + "]]";
}

string SimpleTypeConverter::visitRefType(const RefTypePtr& ptr) {
	return visit(ptr->getElementType()) + "*";
}


}
}

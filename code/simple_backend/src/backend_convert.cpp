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

#include <glog/logging.h>

#include "annotated_ptr.h"
#include "types.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;

CodePtr FunctionManager::getFunction(const LambdaExprPtr& lambda, const Identifier& ident) {
	auto codeIt = functionMap.find(ident);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	auto funType = dynamic_pointer_cast<const FunctionType>(lambda->getType());
	auto body = lambda->getBody();
	bool isCompoundBody = !!dynamic_pointer_cast<const CompoundStmt>(body);

	// generate a new function from the lambda expression
	CodePtr cptr = std::make_shared<CodeFragment>(string("fundef_codefragment_") + ident.getName());
	CodeStream& cs = cptr->getCodeStream();
	// write the function header
	cs << cc.getTypeMan().getTypeName(funType->getReturnType()) << " " << ident.getName() << "(";
	// handle arguments
	cs << join(", ", lambda->getParams(), [this](std::ostream& os, const VariablePtr& param) -> std::ostream& {
		return (os << this->cc.getTypeMan().getTypeName(param->getType()) << " " << cc.getNameGen().getVarName(param));
	});
	cs << ")";
	if(!isCompoundBody) cs << " {" << CodeStream::indR << "\n";
	// generate the function body
	ConvertVisitor visitor(cc, cptr);
	visitor.visit(lambda->getBody());
	if(!isCompoundBody) cs << CodeStream::indL << "\n}\n";
	cs << "\n";
	// insert into function map and return
	functionMap.insert(std::make_pair(ident, cptr));
	return cptr;
}

CodePtr FunctionManager::getFunctionLiteral(const core::FunctionTypePtr& type, const string& name) {
	// TODO refactor duplication w/ above
	auto ident = Identifier(string("fundecl_codefragment_") + name);
	auto codeIt = functionMap.find(ident);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}
	CodePtr cptr = std::make_shared<CodeFragment>(ident.getName());
	CodeStream& cs = cptr->getCodeStream();
	cs << cc.getTypeMan().getTypeName(type->getReturnType()) << " " << name << "(";
	auto argType = type->getArgumentType();
	if(auto tupleArgType = dynamic_pointer_cast<const TupleType>(argType)) {
		cs << join(", ", tupleArgType->getElementTypes(), [this](std::ostream& o, const TypePtr& cur) -> std::ostream& {
			return (o << this->cc.getTypeMan().getTypeName(cur));
		});
	} // TODO handle other argument types
	cs << ");\n";
	// insert into function map and return
	functionMap.insert(std::make_pair(ident, cptr));
	return cptr;
}

void FunctionManager::writeFunctionCall(const Identifier& funId, const LambdaExprPtr& ptr) {
	 // TODO
}


ConvertedCode ConversionContext::convert(const core::ProgramPtr& prog) {
	ConvertedCode converted(prog);
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {
		ConvertVisitor convVisitor(*this);
		convVisitor.visit(ep);
		converted.insert(std::make_pair(ep, convVisitor.getCode()));
	});
	return converted;
}


void ConvertVisitor::visitLambdaExpr(const LambdaExprPtr& ptr) {
	string cFunName = cc.getNameGen().getName(ptr);
	if(auto cnameAnn = ptr.getAnnotation(c_info::CNameAnnotation::KEY)) { // originally a named C function
		cFunName = cnameAnn->getName();
	}
	defCodePtr->addDependency(cc.getFuncMan().getFunction(ptr, cFunName));
	cStr << cFunName;
}

void ConvertVisitor::visitCallExpr(const CallExprPtr& ptr) {
	const std::vector<ExpressionPtr>& args = ptr->getArguments();
	auto funExp = ptr->getFunctionExpr();
	// generic built in C operator handling
	if(auto cOpAnn = funExp->getAnnotation(c_info::COpAnnotation::KEY)) { 
		string op = cOpAnn->getOperator();
		cStr << "(";
		visit(args.front());
		cStr << " " << op << " ";
		visit(args.back());
		cStr << ")";
		return;
	}
	// special built in function handling -- TODO make more generic
	if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {
		//LOG(INFO) << "+++++++ visitCallExpr dyncastLit\n";
		auto funName = literalFun->getValue();
		//LOG(INFO) << "+++++++ val: " << funName << "\n";
		if(funName == "ref.deref") {
			// TODO decide whether no-op or *
			visit(ptr->getArguments().front());
			return;
		} else if(funName == "subscript") {
			visit(ptr->getArguments().front());
			cStr << "[";
			visit(ptr->getArguments().back());
			cStr << "]";
			return;
		}
	}
	// non built-in handling
	visit(funExp);
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

void ConvertVisitor::visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
	auto var = ptr->getVariable();
	// handle fixed size vectors of simple types (C arrays)
	vector<unsigned> vecLengths;
	auto innerType = var->getType();
	if(auto innerRefType = dynamic_pointer_cast<const RefType>(innerType)) {
		innerType = innerRefType->getElementType();
	}
	while(auto innerVecType = dynamic_pointer_cast<const VectorType>(innerType)) {
		//LOG(INFO) << "+++++++ innerVec\n";
		assert(innerVecType->getSize().isConcrete() && "Vectors with non-concrete size not yet supported");
		vecLengths.push_back(innerVecType->getSize().getValue());
		innerType = innerVecType->getElementType();
		if(auto innerRefType = dynamic_pointer_cast<const RefType>(innerType)) {
			innerType = innerRefType->getElementType();
		}
	}
	if(!vecLengths.empty()) { // TODO check that innerType is "simple" enough to be part of C array
		//LOG(INFO) << "+++++++ innerType " << innerType << "\n";
		cStr << printTypeName(innerType) << " " << nameGen.getVarName(var);
		for_each(vecLengths, [this](unsigned vl) { this->cStr << "[" << vl << "]"; });
		// TODO initialization
		return;
	}
	// standard handling
	cStr << printTypeName(var->getType()) << " " << nameGen.getVarName(var) << " = ";
	visit(ptr->getInitialization());
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
	firstRef = true;
	if(lang::isUnitType(*ptr)) {
		return "void";
	} else
	if(lang::isIntegerType(*ptr)) {
		string qualifier = lang::isUIntType(*ptr) ? "unsigned " : "";
		switch(lang::getNumBytes(*ptr)) {
			case 1: return qualifier + "char";
			case 2: return qualifier + "short";
			case 4: return qualifier + "int";
			case 8: return qualifier + "long"; // long long ?
			default: return ptr->getName();
		}
	} else
	if(lang::isBoolType(*ptr)) {
		return "bool";
	} else
	if(lang::isRealType(*ptr)) {
		switch(lang::getNumBytes(*ptr)) {
			case 4: return "float";
			case 8: return "double";
			default: return ptr->getName();
		}
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
	if(firstRef) {
		firstRef = false;
		return visit(ptr->getElementType());
	}
	return visit(ptr->getElementType()) + "*";
}

string SimpleTypeConverter::visitStructType(const StructTypePtr& ptr) {
	firstRef = true;
	string structName;
	if(auto annotation = ptr.getAnnotation(c_info::CNameAnnotation::KEY)) {
		structName = annotation->getName();
	} else {
		structName = nameGen.getName(ptr, "unnamed_struct");
	}
	std::ostringstream ret; // TODO use code stream
	ret << "struct " << structName << " {\n";
	for_each(ptr->getEntries(), [&ret, this](const NamedCompositeType::Entry& entry) {
		ret << this->visit(entry.second) << " " << entry.first.getName() << ";";
	});
	return ret.str();
}


const ProgramPtr& ConvertedCode::getProgram() const {
	return fromProg;
}


string NameGenerator::getName( const NodePtr& ptr, const char* fragment /*= "unnamed"*/ ) {
	auto it = nameMap.find(ptr);
	if(it != nameMap.end()) return string("__insieme_") + fragment + "_" + it->second;
	// generate a new name string
	std::stringstream name;
	switch(ptr->getNodeCategory()) {
	case NC_Support:
		name << "supp"; break;
	case NC_Type:
		name << "type"; break;
	case NC_Expression:
		name << "expr"; break;
	case NC_Statement:
		name << "stat"; break;
	case NC_Program:
		name << "prog"; break;
	}
	name << "_" << num++;
	nameMap.insert(make_pair(ptr, name.str()));
	return getName(ptr, fragment);
}

string NameGenerator::getVarName(const VariablePtr& var) {
	if(auto annotation = var.getAnnotation(c_info::CNameAnnotation::KEY)) {
		return annotation->getName();
	} else {
		return string("unnamed_var_") + toString(var->getId());
	}
}

}
}

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code) {
	for_each(code.getProgram()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "---\n" << *code.find(ep);
	});
	return out;
}

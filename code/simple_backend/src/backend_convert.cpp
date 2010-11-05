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

std::ostream& printFunctionParamter(std::ostream& out, const VariablePtr& param, ConversionContext& cc) {

	// register ref-based variable within the variable manager
	if (param->getType()->getNodeType() == NT_RefType) {
		VariableManager::VariableInfo info;
		info.location = VariableManager::HEAP;
		cc.getVariableManager().addInfo(param, info);
	}

	// create output ...
	TypePtr type = param->getType();
	if (RefTypePtr ref = dynamic_pointer_cast<const RefType>(type)) {
		TypePtr element = ref->getElementType();
		if (element->getNodeType() == NT_VectorType) {

			// special handling for references to vectors ...
			// -- result has to look like float(* var)[5][5]

			// assemble parameter entry ...
			string res = "";
			TypePtr cur = element;
			while (cur->getNodeType() == NT_VectorType) {
				VectorTypePtr curVec = static_pointer_cast<const VectorType>(cur);
				res = res + "[" + toString(curVec->getSize()) + "]";
				cur = curVec->getElementType();
				if (cur->getNodeType() == NT_RefType) {
					cur = static_pointer_cast<const RefType>(cur)->getElementType();
				}
			}
			return out << cc.getTypeMan().getTypeName(cur) << "(* " << cc.getNameGen().getVarName(param) << ")" << res;
		}
	}

	return out << cc.getTypeMan().getTypeName(param->getType()) << " " << cc.getNameGen().getVarName(param);
}


CodePtr FunctionManager::getFunction(const LambdaExprPtr& lambda) {
	auto codeIt = functionMap.find(lambda);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	// get the name for the new function
	string ident = cc.getNameGen().getName(lambda);

	auto funType = dynamic_pointer_cast<const FunctionType>(lambda->getType());
	auto body = lambda->getBody();
	bool isCompoundBody = !!dynamic_pointer_cast<const CompoundStmt>(body);

	// generate a new function from the lambda expression
	CodePtr cptr = std::make_shared<CodeFragment>(string("fundef_codefragment_") + ident);
	CodeStream& cs = cptr->getCodeStream();
	// write the function header
	cs << cc.getTypeMan().getTypeName(funType->getReturnType()) << " " << ident << "(";
	// handle arguments
	cs << join(", ", lambda->getParams(), [this](std::ostream& os, const VariablePtr& param) {
		printFunctionParamter(os, param, this->cc);
	});
	cs << ")";
	if(!isCompoundBody) cs << " {" << CodeStream::indR << "\n";
	// generate the function body
	ConvertVisitor visitor(cc, cptr);
	visitor.visit(lambda->getBody());
	if(!isCompoundBody) cs << CodeStream::indL << "\n}\n";
	cs << "\n";
	// insert into function map and return
	functionMap.insert(std::make_pair(lambda, cptr));
	return cptr;
}

CodePtr FunctionManager::getFunction(const RecLambdaExprPtr& lambda, const CodePtr& surrounding) {

	// check whether code has already been generated
	auto codeIt = functionMap.find(lambda);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	// generate forward declarations for all functions within this recursive type
	const RecLambdaDefinitionPtr& definition = lambda->getDefinition();
	typedef RecLambdaDefinition::RecFunDefs::value_type Pair;
	for_each(definition->getDefinitions(), [&](const Pair& cur){

		// create forward declaration
		auto funType = dynamic_pointer_cast<const FunctionType>(cur.second->getType());
		auto body = cur.second->getBody();

		// get a name (for the defined recursive function)
		RecLambdaExprPtr recLambda = RecLambdaExpr::get(cc.getNodeManager(), cur.first, definition);
		string ident = cc.getNameGen().getName(recLambda);

		// generate a new function from the lambda expression
		CodePtr cptr(new CodeFragment(string("fundecl_codefragment_") + ident));
		CodeStream& cs = cptr->getCodeStream();
		// write the function header
		cs << cc.getTypeMan().getTypeName(funType->getReturnType()) << " " << ident << "(";
		// handle arguments
		auto &cci = cc;
		cs << join(", ", cur.second->getParams(), [&](std::ostream& os, const VariablePtr& param) -> std::ostream& {
			return (os << cci.getTypeMan().getTypeName(param->getType()) << " " << cci.getNameGen().getVarName(param));
		});
		cs << ");\n";

		// bind forward declaration to name
		functionMap.insert(std::make_pair(recLambda, cptr));
	});

	// generate the actual definition of the functions
	for_each(definition->getDefinitions(), [&](const Pair& cur){

		// construct current recursive function
		RecLambdaExprPtr recLambda = RecLambdaExpr::get(cc.getNodeManager(), cur.first, definition);

		// use normal function generator for this work (by printing unrolled version)
		LambdaExprPtr unrolled = definition->unrollOnce(cc.getNodeManager(), cur.first);

		// get name for function
		string name = cc.getNameGen().getName(recLambda);
		unrolled.addAnnotation(std::make_shared<c_info::CNameAnnotation>(name));
		CodePtr cptr = this->getFunction(unrolled);

		// add dependency to code definition
		cptr->addDependency(this->getFunction(recLambda, surrounding));

		// make surrounding depending on function definition
		surrounding->addDependency(cptr);
	});

	// return lambda now registered within function map
	return getFunction(lambda, surrounding);
}

CodePtr FunctionManager::getFunctionLiteral(const LiteralPtr& literal) {
	// TODO refactor duplication w/ above
	auto codeIt = functionMap.find(literal);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	const FunctionTypePtr type = dynamic_pointer_cast<const FunctionType>(literal->getType());
	assert(type && "Literal is not a function!");

	const string& name = literal->getValue();
	CodePtr cptr = std::make_shared<CodeFragment>("fundef_codefragment_" + name);
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
	functionMap.insert(std::make_pair(literal, cptr));
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

	// obtain a name for the function ...
	string cFunName = cc.getNameGen().getName(ptr);

	// add a dependency to the function definition
	defCodePtr->addDependency(cc.getFuncMan().getFunction(ptr));
}

void ConvertVisitor::visitRecLambdaExpr(const RecLambdaExprPtr& ptr) {

	// get name of lambda Expr ...
	string cFunName = cc.getNameGen().getName(ptr);

	// add a dependency to the function definition
	defCodePtr->addDependency(cc.getFuncMan().getFunction(ptr, defCodePtr));
}

namespace {

	/**
	 * Determines whether using the given expression as a LHS expression within an assignment or within a
	 * RHS read requires a de-referencing within C.
	 */
	bool requiresDeref(const ExpressionPtr& target, ConversionContext& cc) {
		switch (target->getNodeType()) {
			case NT_Variable:
				// check location of memory allocation for variable (only HEAP needs to be dereferenced)
				return (cc.getVariableManager().getInfo(static_pointer_cast<const Variable>(target)).location == VariableManager::HEAP);
			case NT_CallExpr:
				// for only a small number of functions (build ins) returning a ref does not mean it is a pointer
				return !((*(static_pointer_cast<const CallExpr>(target)->getFunctionExpr()) == lang::OP_SUBSCRIPT_VAL) ||
						  (*(static_pointer_cast<const CallExpr>(target)->getFunctionExpr()) == lang::OP_SUBSCRIPT_SINGLE_VAL));
			default:
				return true;
		}
	}
}

void ConvertVisitor::visitCallExpr(const CallExprPtr& ptr) {
	//DLOG(INFO) << "CALLEXPR - " << ptr->getFunctionExpr() << ". prev cStr: \n" << cStr.getString();
	const std::vector<ExpressionPtr>& args = ptr->getArguments();
	auto funExp = ptr->getFunctionExpr();

	// special built in function handling -- TODO make more generic
	if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {
		//LOG(INFO) << "+++++++ visitCallExpr dyncastLit\n";
		auto funName = literalFun->getValue();
		//LOG(INFO) << "+++++++ val: " << funName << "\n";
		// TODO: do not check against name - use full literal!
		if(funName == "ref.assign") {
			// print assignment
			if (requiresDeref(args.front(), cc)) cStr << "*";
			visit(args.front());
			cStr << "=";
			visit(args.back());
			return;
		}
		if(funName == "ref.deref") {

			// test whether a deref is required
			bool deref = requiresDeref(args.front(), cc);

			// add operation
			if (deref) cStr << "(*";
			visit(args.front());
			if (deref) cStr << ")";
			return;
		} if(funName == "ref.var") {
			// TODO handle case where not RHS of local var decl
			visit(args.front());
			return;
		} else if(funName == "subscript_single") {
			visit(args.front());
			cStr << "[";
			visit(args.back());
			cStr << "]";
			return;
		} else if(funName == "pack") {
			//DLOG(INFO) << cStr.getString();
			// if the arguments are a tuple expression, use the expressions within the tuple ...
			if (args.size() == 1) { // should actually be implicit if all checks are satisfied
				if (TupleExprPtr arguments = dynamic_pointer_cast<const TupleExpr>(args[0])) {
					// print elements of the tuple directly ...
					functionalJoin([&]{ this->cStr << ", "; }, arguments->getExpressions(), [&](const ExpressionPtr& ep) { this->visit(ep); });
					return;
				}
			}

			functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			//DLOG(INFO) << cStr.getString();
			//LOG(INFO) << "\n=========================== " << args.front() << " ---->> " << args.back() << "\n";
			return;
		}
	}

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

	// non built-in handling (generate function body if necessary)
	visit(funExp);

	// add method invocation
	if (funExp->getNodeType() != NT_Literal) {
		cStr << cc.getNameGen().getName(funExp);
	}

	cStr << "(";
	functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
	cStr << ")";
}

void ConvertVisitor::visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
	auto var = ptr->getVariable();

	// investigate initialization to determine whether variable is a pointer / skalar
	VariableManager::VariableInfo info;
	info.location = VariableManager::NONE;
	if (var->getType()->getNodeType() == NT_RefType) {

		ExpressionPtr initialization = ptr->getInitialization();
		switch (initialization->getNodeType()) {
		case NT_Variable:
			info = varManager.getInfo(static_pointer_cast<const Variable>(initialization));
			break;
		case NT_CallExpr: {
			// distinguish between var and new
			CallExprPtr call = static_pointer_cast<const CallExpr>(initialization);
			ExpressionPtr function = call->getFunctionExpr();
			if (function->getNodeType() == NT_Literal) {
				// mark as a stack variable only if created using var.new => otherwise always a pointer (conservative)
				info.location = (*function == lang::OP_REF_VAR_VAL)?VariableManager::STACK:VariableManager::HEAP;
			}
			break;
		}
		default: ;// nothing
		}
	}
	varManager.addInfo(var, info);


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
		cStr << cc.getTypeMan().getTypeName(innerType, true) << " " << nameGen.getVarName(var);
		for_each(vecLengths, [this](unsigned vl) { this->cStr << "[" << vl << "]"; });
		// TODO initialization
		return;
	}

	// standard handling
	cStr << cc.getTypeMan().getTypeName(var->getType(), info.location == VariableManager::STACK) << " " << nameGen.getVarName(var) << " = ";

	// generate initializer expression
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
		auto funLiteralDeclCode = cc.getFuncMan().getFunctionLiteral(ptr);
		defCodePtr->addDependency(funLiteralDeclCode);
		cStr << val;
	}
	else {
		cStr << val;
	}
}

void ConvertVisitor::visitReturnStmt(const ReturnStmtPtr& ptr)
{
	cStr << "return ";
	if(*ptr->getReturnExpr()->getType() != lang::TYPE_UNIT_VAL) {
		visit(ptr->getReturnExpr());
	}
	cStr << ";";
}

void ConvertVisitor::visitSwitchStmt(const SwitchStmtPtr& ptr) {
		cStr << "switch(";
		visit(ptr->getSwitchExpr());
		cStr << ") {\n";
		for_each(ptr->getCases(), [&](const SwitchStmt::Case& curCase) { // GCC sucks
			this->cStr << "case ";
			this->visit(curCase.first);
			this->cStr << ":" << CodeStream::indR << "\n";
			this->visit(curCase.second);
			this->cStr << "; break;" << CodeStream::indL << "\n";
		});
		cStr << "}";
	}

void ConvertVisitor::visitCompoundStmt(const CompoundStmtPtr& ptr) {
	if(ptr->getStatements().size() > 0) {
		cStr << "{" << CodeStream::indR << "\n";
		for_each(ptr->getChildList(), [&](const NodePtr& cur) { 
			this->visit(cur); 
			cStr << ";";
			if(cur != ptr->getChildList().back()) cStr << "\n";
		});
		cStr << CodeStream::indL << "\n}";
	}
}

void ConvertVisitor::visitCastExpr(const CastExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(ptr->getType()) << ")(";
	visit(ptr->getSubExpression());
	cStr << "))";
}



string TypeManager::getTypeName(const core::TypePtr type, bool inDecl /* = false */) {
	visitStarting = true;
	declVisit = inDecl;
	return visit(type);
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


string TypeManager::visitGenericType(const GenericTypePtr& ptr) {
	visitStarting = false;
	if(lang::isUnitType(*ptr)) {
		return "void";
	}
	if(lang::isIntegerType(*ptr)) {
		string qualifier = lang::isUIntType(*ptr) ? "unsigned " : "";
		switch(lang::getNumBytes(*ptr)) {
			case 1: return qualifier + "char";
			case 2: return qualifier + "short";
			case 4: return qualifier + "int";
			case 8: return qualifier + "long"; // long long ?
			default: return ptr->getName();
		}
	}
	if(lang::isBoolType(*ptr)) {
		return "bool";
	}
	if(lang::isRealType(*ptr)) {
		switch(lang::getNumBytes(*ptr)) {
			case 4: return "float";
			case 8: return "double";
			default: return ptr->getName();
		}
	}
	if(*ptr == lang::TYPE_STRING_VAL) {
		return "string";
	}
	if(*ptr == lang::TYPE_CHAR_VAL) {
		return "char";
	}
	if(*ptr == lang::TYPE_VAR_LIST_VAL) {
		return "...";
	}

	// handle arrays
	if (ArrayTypePtr arrayType = dynamic_pointer_cast<const ArrayType>(ptr)) {

		// test whether dimension is final
		IntTypeParam dim = arrayType->getDimension();
		if (dim.getType() == IntTypeParam::CONCRETE) {
			TypePtr elementType = arrayType->getElementType();
			string res = visit(elementType);
			int numStars = dim.getValue();
			if (elementType->getNodeType() == NT_RefType) {
				numStars -= 1;
			}

			for (int i=0; i < numStars; i++) {
				res += "*";
			}

			// res += " /* " + toString(*arrayType) + " */ ";

			return res;
		}
	}

	//assert(0 && "Unhandled generic type.");
	return string("[[unhandled_simple_type: ") + ptr->getName() + "]]";
}

string TypeManager::visitRefType(const RefTypePtr& ptr) {
	auto elemType = ptr->getElementType();
	// special handling for void* type
	if (*ptr == lang::TYPE_REF_ALPHA_VAL) {
		return "void*";
	}
	if((declVisit && visitStarting) || elemType->getNodeType() == NT_VectorType ) {
		visitStarting = false;
		return visit(elemType);
	}
	visitStarting = false;
	return visit(elemType) + "*";
}

string TypeManager::visitStructType(const StructTypePtr& ptr) {
	visitStarting = false;
	string structName;
	if(auto annotation = ptr.getAnnotation(c_info::CNameAnnotation::KEY)) {
		structName = annotation->getName();
	} else {
		structName = cc.getNameGen().getName(ptr, "unnamed_struct");
	}
	std::ostringstream ret; // TODO use code stream
	ret << "struct " << structName << " {\n";
	for_each(ptr->getEntries(), [&ret, this](const NamedCompositeType::Entry& entry) {
		ret << this->visit(entry.second) << " " << entry.first.getName() << ";";
	});
	return ret.str();
}

string TypeManager::visitVectorType(const VectorTypePtr& ptr) {
	visitStarting = false;
	return visit(ptr->getElementType()) + "[" + toString(ptr->getSize()) + "]";
}

// -------------------------------- Variable Manager -----------------------------------------

const VariableManager::VariableInfo& VariableManager::getInfo(const VariablePtr& variable) const {
	auto pos = variableMap.find(variable);
	assert(pos != variableMap.end() && "Tried to look up undefined Variable!");
	return (*pos).second;
}

void VariableManager::addInfo(const VariablePtr& variable, const VariableManager::VariableInfo& info) {
	auto res = variableMap.insert(std::make_pair(variable, info));
	if (res.second) {
		return;
	}
	variableMap.erase(res.first);
	res = variableMap.insert(std::make_pair(variable,info));
	assert(res.second && "Replacement failed!");
}

void VariableManager::removeInfo(const VariablePtr& variable) {
	variableMap.erase(variable);
}

bool VariableManager::hasInfoFor(const VariablePtr& variable) const {
	return variableMap.find(variable) != variableMap.end();
}



const ProgramPtr& ConvertedCode::getProgram() const {
	return fromProg;
}


string NameGenerator::getName( const NodePtr& ptr, const string fragment) {

	// test whether a name has already been picked
	auto it = nameMap.find(ptr);
	if(it != nameMap.end()) return it->second;

	// test whether a name is attached ...
	if(auto cnameAnn = ptr.getAnnotation(c_info::CNameAnnotation::KEY)) {
		// => take original c name
		string name = cnameAnn->getName();
		nameMap.insert(make_pair(ptr, name));
		return name;
	}

	// test whether recursive function name is attached
	if (RecLambdaExprPtr recLambda = dynamic_pointer_cast<const RecLambdaExpr>(ptr)) {
		if(auto cnameAnn = recLambda->getVariable()->getAnnotation(c_info::CNameAnnotation::KEY)) {
			// => take original c name
			string name = cnameAnn->getName();
			nameMap.insert(make_pair(ptr, name));
			return name;
		}
	}

	// generate a new name string
	std::stringstream name;
	name << string("__insieme_");
	if (!fragment.empty()) {
		name << fragment << "_";
	}

	switch(ptr->getNodeCategory()) {
	case NC_Support:
		name << "supp"; break;
	case NC_Type:
		name << "type"; break;
	case NC_Expression:
		switch(ptr->getNodeType()) {
		case NT_LambdaExpr: name << "fun"; break;
		case NT_RecLambdaExpr: name << "recFun"; break;
		default: name << "expr"; break;
		} ; break;
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
	if(auto annotation = var->getAnnotation(c_info::CNameAnnotation::KEY)) {
		return annotation->getName();
	} else {
		return string("var_") + toString(var->getId());
	}
}

}
}

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code) {
	out << "// --- Generated Inspire Code ---\n";
	out << "#define bool int\n";
	out << "#define true 1\n";
	out << "#define false 0\n";

	for_each(code.getProgram()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "// --- Entry Point ---\n";
		out << (*code.find(ep)).second;
	});
	return out;
}

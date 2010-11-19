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

#include "insieme/simple_backend/function_manager.h"

#include "insieme/simple_backend/backend_convert.h"

namespace insieme {
namespace simple_backend {

namespace {

	std::ostream& printFunctionParamter(std::ostream& out, CodePtr& context, const VariablePtr& param,
			VariableManager& varManager, TypeManager& typeManager, NameGenerator& nameGenerator) {

		// register ref-based variable within the variable manager
		if (param->getType()->getNodeType() == NT_RefType) {
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			varManager.addInfo(param, info);
		}

		// format parameter using type manager
		return out << typeManager.formatParamter(context, param->getType(), nameGenerator.getVarName(param));
	}

	std::ostream& printFunctionParamter(std::ostream& out, CodePtr& context, const VariablePtr& param, ConversionContext& cc) {
		return printFunctionParamter(out, context, param, cc.getVariableManager(), cc.getTypeMan(), cc.getNameGen());
	}
}


CodePtr OldFunctionManager::getFunction(const LambdaPtr& lambda) {
//	auto codeIt = functionMap.find(lambda);
//	if(codeIt != functionMap.end()) {
//		return codeIt->second;
//	}

	// get the name for the new function
	string ident = cc.getNameGen().getName(lambda);

	auto funType = dynamic_pointer_cast<const FunctionType>(lambda->getType());
	auto body = lambda->getBody();

	// generate a new function from the lambda expression
	CodePtr cptr = std::make_shared<CodeFragment>(string("fundef_codefragment_") + ident);
	CodeStream& cs = cptr->getCodeStream();
	// write the function header
	cs << cc.getTypeMan().getTypeName(cptr, funType->getReturnType()) << " " << ident << "(void* _capture";
	// handle arguments
	if (!lambda->getParameterList().empty()) {
		cs << ", " << join(", ", lambda->getParameterList(), [&, this](std::ostream& os, const VariablePtr& param) {
			printFunctionParamter(os, cptr, param, this->cc);
		});
	}
	cs << ")";
	cs << " {" << CodeStream::indR << "\n";

	ConvertVisitor visitor(cc, cptr);

	// extract capture list
	cs << "// --------- Captured Stuff - Begin -------------\n";

	string name = cc.getNameGen().getName(lambda, "lambda");
	string structName = "struct " + name + "_closure";

	for_each(lambda->getCaptureList(), [&](const VariablePtr& cur) {
//		VariableManager::VariableInfo info;
//		info.location = VariableManager::HEAP;
//
//		VariablePtr var = cur->getVariable();
//		cc.getVariableManager().addInfo(var, info);
//
//		// standard handling
//		cs << cc.getTypeMan().getTypeName(cptr, var->getType(), false);
//		string name = cc.getNameGen().getVarName(var);
//		cs << " " << name << " = ((" << structName << "*)_capture)->" << name << ";\n";

	});

	cs << "// --------- Captured Stuff -  End  -------------\n";


	// generate the function body
	visitor.visit(lambda->getBody());
	cs << CodeStream::indL << "\n}\n";
	cs << "\n";

	// insert into function map and return
//	functionMap.insert(std::make_pair(lambda, cptr));
	return cptr;
}

CodePtr OldFunctionManager::getFunction(const LambdaExprPtr& lambda, const CodePtr& surrounding) {

	// check whether code has already been generated
	auto codeIt = functionMap.find(lambda);
	if(codeIt != functionMap.end()) {
		return codeIt->second;
	}

	// generate forward declarations for all functions within this recursive type
	const LambdaDefinitionPtr& definition = lambda->getDefinition();
	typedef LambdaDefinition::Definitions::value_type Pair;
	for_each(definition->getDefinitions(), [&](const Pair& cur){

		// create forward declaration
		auto funType = dynamic_pointer_cast<const FunctionType>(cur.second->getType());
		auto body = cur.second->getBody();

		// get a name (for the defined recursive function)
		LambdaExprPtr recLambda = LambdaExpr::get(cc.getNodeManager(), cur.first, definition);
		string ident = cc.getNameGen().getName(recLambda);

		// generate a new function from the lambda expression
		CodePtr cptr(new CodeFragment(string("fundecl_codefragment_") + ident));
		CodeStream& cs = cptr->getCodeStream();
		// write the function header
		cs << cc.getTypeMan().getTypeName(surrounding, funType->getReturnType()) << " " << ident << "(";
		// handle arguments
		auto &cci = cc;
		cs << join(", ", cur.second->getParameterList(), [&](std::ostream& os, const VariablePtr& param) -> std::ostream& {
			return (os << cci.getTypeMan().getTypeName(cptr, param->getType()) << " " << cci.getNameGen().getVarName(param));
		});
		cs << ");\n";

		// bind forward declaration to name
		functionMap.insert(std::make_pair(recLambda, cptr));
	});

	// generate the actual definition of the functions
	for_each(definition->getDefinitions(), [&](const Pair& cur){

		// construct current recursive function
		LambdaExprPtr recLambda = LambdaExpr::get(cc.getNodeManager(), cur.first, definition);

		// use normal function generator for this work (by printing unrolled version)
		LambdaExprPtr unrolled = definition->unrollOnce(cc.getNodeManager(), cur.first);

		// get name for function
//		string name = cc.getNameGen().getName(recLambda);
//		unrolled.addAnnotation(std::make_shared<c_info::CNameAnnotation>(name));
//		CodePtr cptr = this->getFunction(unrolled);
//
//		// add dependency to code definition
//		cptr->addDependency(this->getFunction(recLambda, surrounding));
//
//		// make surrounding depending on function definition
//		surrounding->addDependency(cptr);
	});

	// return lambda now registered within function map
	return getFunction(lambda, surrounding);
}

CodePtr OldFunctionManager::getFunctionLiteral(const LiteralPtr& literal) {
	// TODO refactor duplication w/ above
//	auto codeIt = functionMap.find(literal);
//	if(codeIt != functionMap.end()) {
//		return codeIt->second;
//	}

	const FunctionTypePtr type = dynamic_pointer_cast<const FunctionType>(literal->getType());
	assert(type && "Literal is not a function!");

	const string& name = literal->getValue();
	CodePtr cptr = std::make_shared<CodeFragment>("fundef_codefragment_" + name);
	CodeStream& cs = cptr->getCodeStream();
	cs << cc.getTypeMan().getTypeName(cptr, type->getReturnType()) << " " << name << "(";
	cs << join(", ", type->getArgumentTypes(), [&, this](std::ostream& o, const TypePtr& cur) -> std::ostream& {
		return (o << this->cc.getTypeMan().getTypeName(cptr, cur));
	});
	cs << ");\n";
	// insert into function map and return
//	functionMap.insert(std::make_pair(literal, cptr));
	return cptr;
}

void OldFunctionManager::writeFunctionCall(const Identifier& funId, const LambdaExprPtr& ptr) {
	 // TODO
}

void FunctionManager::createCallable(const CodePtr& context, const core::LiteralPtr& external) {

	// get prototype definition
	CodePtr protoType = resolve(external);

	// add dependency
	context->addDependency(protoType);

	// print function name (which then will be call-able)
	context->getCodeStream() << external->getValue();
}

CodePtr FunctionManager::resolve(const LiteralPtr& literal) {
	// lookup element
	auto pos = externalFunctions.find(literal);
	if (pos != externalFunctions.end()) {
		return pos->second;
	}

	// extract function type
	auto type = dynamic_pointer_cast<const FunctionType>(literal->getType());
	assert(type && "Literal is not a function!");

	const string& name = literal->getValue();
	CodePtr protoType = std::make_shared<CodeFragment>("Prototype for external function: " + name);
	CodeStream& cs = protoType->getCodeStream();
	cs << cc.getTypeMan().getTypeName(protoType, type->getReturnType()) << " " << name << "(";
	cs << join(", ", type->getArgumentTypes(), [&, this](std::ostream& out, const TypePtr& cur) {
		out << cc.getTypeMan().getTypeName(protoType, cur);
	});
	cs << ");\n";

	// insert into function map and return
	externalFunctions.insert(std::make_pair(literal, protoType));
	return protoType;
}


void FunctionManager::createCallable(const CodePtr& context, const core::CaptureInitExprPtr& capture) {

//	// extract sub-expression
//	auto subExpr = capture->getLambda();
//	assert(subExpr->getNodeType() == NT_LambdaExpr && "Simple backend can't handle capture init expressions");
//
//	// convert to lambda expression
//	LambdaExprPtr lambda = static_pointer_cast<const LambdaExpr>(subExpr);
//
//	// lookup code information
//	const LambdaCode& code = resolve(lambda);
//
//	// use code to generate initialization


}

void FunctionManager::createCallable(const CodePtr& context, const core::LambdaExprPtr& lambda) {

	// NEXT: create proto-types if not recursive!

	// resolve lambda
	const LambdaPtr& def = lambda->getLambda();
	assert(def->getCaptureList().empty() && "This method cannot support lambdas with capture lists!");

	// copy c-name annotation to lambda - if there is one
	if(auto cnameAnn = lambda->getVariable()->getAnnotation(c_info::CNameAnnotation::KEY)) {
		std::cout << "Name found: " << cnameAnn->getName() << " on " << toString(*(lambda->getVariable())) << std::endl;
		def->addAnnotation(cnameAnn);
	}


	// obtain code for lambda
	const LambdaCode& code = resolve(def);

	// add dependencies
	context->addDependency(code.function);

	// add function name
	context->getCodeStream() << code.function->getName();
}


const FunctionManager::LambdaCode& FunctionManager::resolve(const LambdaPtr& lambda) {

	// lookup definition
	auto pos = functionDefinitions.find(lambda);
	if (pos != functionDefinitions.end()) {
		return pos->second;
	}

	// get name
	string name = cc.getNameGen().getName(lambda);
	FunctionTypePtr funType = lambda->getType();

	VariableManager varManager;

	// create function code for lambda
	CodePtr function = std::make_shared<CodeFragment>(name);
	CodeStream& cs = function->getCodeStream();
	cs << cc.getTypeMan().getTypeName(function, funType->getReturnType()) << " " << name << "(";
	cs << join(", ", lambda->getParameterList(), [&, this](std::ostream& out, const VariablePtr& param) {
		printFunctionParamter(out, function, param, cc);
	});
	cs << ") {" << CodeStream::indR << "\n";

	ConvertVisitor visitor(cc, function);

	if (lambda->isCapturing()) {
		// extract capture list
		cs << "// --------- Captured Stuff - Begin -------------\n";

		string structName = "struct " + name + "_closure";

		for_each(lambda->getCaptureList(), [&](const VariablePtr& var) {
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;

			cc.getVariableManager().addInfo(var, info);

			// standard handling
			cs << cc.getTypeMan().getTypeName(function, var->getType(), false);
			string name = cc.getNameGen().getVarName(var);
			cs << " " << name << " = ((" << structName << "*)_capture)->" << name << ";\n";

		});

		cs << "// --------- Captured Stuff -  End  -------------\n";
	}

	// generate the function body
	visitor.visit(lambda->getBody());
	cs << CodeStream::indL << "\n}\n";
	cs << "\n";

	// create lambda code object
	LambdaCode code;
	code.function = function;

	// register and return reference to inserted element
	return functionDefinitions.insert(std::make_pair(lambda, code)).first->second;
}


} // end namespace simple_backend
} // end namespace insieme


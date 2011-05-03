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

#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/statement_converter.h"

namespace insieme {
namespace simple_backend {

namespace {
	string getSignatureOf(const CodeFragmentPtr& context, const FunctionTypePtr& funType, const string& name, TypeManager& typeManager) {
		std::stringstream ss;

		ss << typeManager.getTypeName(context, funType->getReturnType()) << " " << name << "(";

		auto parameter = funType->getParameterTypes();

//		if (!captures.empty()) {
//			ss << "void *" << ((!arguments.empty())?", ":"");
//		}
		ss << join(", ", parameter, [&](std::ostream& out, const TypePtr& cur) {
			out << typeManager.getTypeName(context, cur);
		});
		ss << ")";

		return ss.str();
	}
}

void FunctionManager::appendFunctionParameter(const CodeFragmentPtr& fragment, const VariablePtr& param) {
	// register ref-based variable within the variable manager
	if (param->getType()->getNodeType() == NT_RefType) {
		VariableManager::VariableInfo info;
		info.location = VariableManager::HEAP;
		(cc.getVariableManager()).addInfo(param, info);
	}

	// format parameter using type manager
	fragment << (cc.getTypeManager()).formatParamter(fragment, param->getType(), (cc.getNameManager()).getName(param), false);
}

string FunctionManager::getFunctionName(const CodeFragmentPtr& context, const core::LiteralPtr& external) {

	// get prototype definition
	CodeFragmentPtr protoType = resolve(external);

	// add dependency
	context->addDependency(protoType);

	// print function name (which then will be call-able)
	return external->getValue();
}

CodeFragmentPtr FunctionManager::resolve(const LiteralPtr& literal) {
	// lookup element
	auto pos = externalFunctions.find(literal);
	if (pos != externalFunctions.end()) {
		return pos->second;
	}

	// extract function type
	auto type = dynamic_pointer_cast<const FunctionType>(literal->getType());
	assert(type && "Literal is not a function!");

	const string& name = literal->getValue();
	CodeFragmentPtr protoType = CodeFragment::createNew("Prototype for external function: " + name + " ... type: " + literal->getType()->toString());
	TypeManager& typeManager = cc.getTypeManager();
	protoType << typeManager.getTypeInfo(protoType, type->getReturnType()).externName << " " << name << "(";
	//protoType << typeManager.getTypeName(protoType, type->getReturnType(), true) << " " << name << "(";
	protoType << join(", ", type->getParameterTypes(), [&, this](std::ostream& out, const TypePtr& cur) {
		out << typeManager.getTypeInfo(protoType, cur).externName;
	});
	protoType << ");\n";

	// insert into function map and return
	externalFunctions.insert(std::make_pair(literal, protoType));
	return protoType;
}


string FunctionManager::getFunctionName(const CodeFragmentPtr& context, const core::LambdaExprPtr& lambda) {

	// the pointer to the prototype
	CodeFragmentPtr prototype;

	// look up predefined prototypes
	auto pos = prototypes.find(lambda->getLambda());
	if (pos == prototypes.end()) {
		if (lambda->isRecursive()) {
			// resolve recursive functions
			prototype = resolve(lambda->getDefinition());
		} else {
			// resolve non-recursive functions directly (avoids prototype)
			LambdaPtr fun = lambda->getLambda();
			prototype = resolve(fun);
			prototypes.insert(std::make_pair(fun, prototype));
		}
	} else {
		prototype = pos->second;
	}

	// resolve lambda definitions and add dependency
	context->addDependency(prototype);

	// add function name to stream
	return cc.getNameManager().getName(lambda->getLambda());
}


CodeFragmentPtr FunctionManager::resolve(const LambdaDefinitionPtr& definition) {

	// check within register
	auto pos = functionGroup.find(definition);
	if (pos != functionGroup.end()) {
		return pos->second;
	}

	// TODO special handling of non-recursive functions

	// prepare some manager
	TypeManager& typeManager = cc.getTypeManager();
	NameManager& nameManager = cc.getNameManager();
	NodeManager& manager = cc.getNodeManager();

	// create dummy code group depending on all prototypes and functions
	CodeFragmentPtr group = CodeFragment::createNewDummy("Dummy fragment for recursive function group");


	// A) create prototypes for lambdas
	for_each(definition->getDefinitions(), [&, this](const std::pair<VariablePtr, LambdaPtr>& cur) {

		string name = nameManager.getName(cur.second);

		const FunctionTypePtr& funType = cur.second->getType();
		CodeFragmentPtr prototype = CodeFragment::createNew("Prototype of " + name + " ... type: " + funType->toString());
		prototype << getSignatureOf(prototype, funType, name, typeManager) << ";\n";

		this->prototypes.insert(std::make_pair(cur.second, prototype));

		group->addDependency(prototype);
	});

	// B) create function definitions for all lambdas
	for_each(definition->getDefinitions(), [&, this](const std::pair<VariablePtr, LambdaPtr>& cur) {

		// unroll recursive definition once
		LambdaPtr unrolled = definition->unrollOnce(manager, cur.first)->getLambda();

		// attack the same name to the unrolled version as to the original node
		string name = nameManager.getName(cur.second);
		nameManager.setName(unrolled, name);

		// resolve recursive this function body once
		CodeFragmentPtr function = resolve(unrolled);

		// add dependency
		group->addDependency(function);
	});

	// add dependency of context
	functionGroup.insert(std::make_pair(definition, group));
	return group;
}

CodeFragmentPtr FunctionManager::resolve(const LambdaPtr& lambda) {

	// lookup definition
	auto pos = functions.find(lambda);
	if (pos != functions.end()) {
		return pos->second;
	}

	// provide some manager
	TypeManager& typeManager = cc.getTypeManager();
	NameManager& nameManager = cc.getNameManager();

	// get name
	string name = nameManager.getName(lambda);
	FunctionTypePtr funType = lambda->getType();


	// create function code for lambda
	CodeFragmentPtr function = CodeFragment::createNew("Definition of " + name + " ... type: " + funType->toString());

	// allows derived function managers to insert a function prefix
	addFunctionPrefix(function, lambda);

	// write the function header
	function << typeManager.getTypeName(function, funType->getReturnType()) << " " << name << "(";

//	if (lambda->isCapturing()) {
//		function << "void* _capture";
//		if (!lambda->getParameterList().empty()) {
//			function << ", ";
//		}
//	}
	if (!lambda->getParameterList().empty()) {
		auto start = lambda->getParameterList().begin();
		auto end = lambda->getParameterList().end();
		appendFunctionParameter(function, *start);
		++start;
		while (start != end) {
			function << ", ";
			appendFunctionParameter(function, *start);
			++start;
		}
	}
	function << ")";


	// add function body
	function << " {" << CodeBuffer::indR << "\n";

//	if (lambda->isCapturing()) {
//		// extract capture list
//		function << "// --------- Captured Stuff - Begin -------------\n";
//
//		// get name of struct from type manager
//		TypeManager::FunctionTypeInfo functionTypeInfo = typeManager.getFunctionTypeInfo(funType);
//		function->addDependency(functionTypeInfo.definitions);
//		string structName = functionTypeInfo.closureName;
//
//		int i = 0;
//		for_each(lambda->getCaptureList(), [&](const VariablePtr& var) {
//			VariableManager::VariableInfo info;
//			info.location = VariableManager::HEAP;
//
//			varManager.addInfo(var, info);
//
//			// standard handling
//			function << typeManager.formatParamter(function, var->getType(), nameManager.getName(var), false);
//			function << " = ((" << structName << "*)_capture)->" << format("p%d", i++) << ";\n";
//
//		});
//
//		function << "// --------- Captured Stuff -  End  -------------\n";
//	}

	// generate the function body
	cc.getStmtConverter().convert(lambda->getBody(), function);

	function << CodeBuffer::indL << "\n}\n";
	function << "\n";

	// register and return result
	functions.insert(std::make_pair(lambda, function));
	return function;
}


} // end namespace simple_backend
} // end namespace insieme


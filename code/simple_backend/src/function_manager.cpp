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

#include <set>

#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/backend_convert.h"
#include "insieme/simple_backend/statement_converter.h"

#include "insieme/utils/set_utils.h"

namespace insieme {
namespace simple_backend {

namespace {
	string getSignatureOf(const CodeFragmentPtr& context, const FunctionTypePtr& funType, const string& name, TypeManager& typeManager) {
		std::stringstream ss;

		ss << typeManager.getTypeName(context, funType->getReturnType()) << " " << name << "(";

		auto parameter = funType->getParameterTypes();

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
	fragment << cc.getTypeManager().formatParamter(fragment, param->getType(), cc.getNameManager().getName(param), false);
}

void FunctionManager::appendFunctionParameters(const CodeFragmentPtr& fragment, const vector<VariablePtr>& params) {
	// early exit ...
	if (params.empty()) {
		return;
	}

	// process all parameters step by step
	auto start = params.begin();
	auto end = params.end();
	appendFunctionParameter(fragment, *start);
	++start;
	while (start != end) {
		fragment << ", ";
		appendFunctionParameter(fragment, *start);
		++start;
	}
}

string FunctionManager::getFunctionName(const CodeFragmentPtr& context, const core::LiteralPtr& external) {

	// get prototype definition
	CodeFragmentPtr protoType = resolve(external);

	// add dependency
	context->addDependency(protoType);

	// print function name (which then will be call-able)
	return external->getStringValue();
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

	TypePtr returnType = type->getReturnType();

	const string& name = literal->getStringValue();
	CodeFragmentPtr protoType = CodeFragment::createNew("Prototype for external function: " + name + " ... type: " + literal->getType()->toString());

	// register code fragment to be able to call recursively (within the function wrapper)
	externalFunctions.insert(std::make_pair(literal, protoType));

	TypeManager& typeManager = cc.getTypeManager();

	// create list of functions included via header (yes, it is a hack)
	static std::set<string> INCLUDED = utils::set::toSet<std::set<string>>(
			"atoi", "atof", "atol", "fprintf", "printf", "par_printf", "malloc", "alloca",
			"fopen", "fread", "fwrite", "fgetc", "fflush", "fclose", "fscanf", "sscanf",
			"sprintf", "__isnanl", "__isinfl" );

	if (INCLUDED.find(name) == INCLUDED.end()) {
		protoType << typeManager.getTypeInfo(protoType, returnType).externName << " " << name << "(";
		//protoType << typeManager.getTypeName(protoType, type->getReturnType(), true) << " " << name << "(";
		protoType << join(", ", type->getParameterTypes(), [&, this](std::ostream& out, const TypePtr& cur) {
			out << typeManager.getTypeInfo(protoType, cur).externName;
		});
		protoType << ");\n";
	}

	// do not create a wrapper if a variable argument list is included
	if (contains(type->getParameterTypes()->getElements(), cc.getNodeManager().getLangBasic().getVarList(), equal_target<TypePtr>())) {
		return protoType;
	}

	// add closure wrapper
	const string wrapperName = name + "_wrap";
	protoType << "static " << typeManager.getTypeName(protoType, returnType) << " " << wrapperName << "(";

	// TODO: test whether void* is fine or actual pointer is better
//	function << typeManager.getFunctionTypeInfo(funType).closureName << "* _closure";
	protoType << "void* _closure";

	// create a temporary-parameter list
	NodeManager& manager = cc.getNodeManager();
	vector<VariablePtr> params;
	::transform(type->getParameterTypes()->getElements(), std::back_inserter(params), [&](const TypePtr& type) {
		return Variable::get(manager, type);
	});

	if (!params.empty()) {
		protoType << ", ";
		appendFunctionParameters(protoType, params);
	}
	protoType << ") { " << ((cc.getLangBasic().isUnit(returnType))?"":"return ");
	vector<ExpressionPtr> args;
	::copy(params, std::back_inserter(args));
	cc.getStmtConverter().convert(CallExpr::get(manager, returnType, literal, args), protoType);
	protoType << "; }\n";

	// done => return prototype reference
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
	for_each(definition->getDefinitions(), [&, this](const LambdaBindingPtr& cur) {

		string name = nameManager.getName(cur->getLambda());

		const FunctionTypePtr& funType = cur->getLambda()->getType();
		CodeFragmentPtr prototype = CodeFragment::createNew("Prototype of " + name + " ... type: " + funType->toString());
		prototype << getSignatureOf(prototype, funType, name, typeManager) << ";\n";

		this->prototypes.insert(std::make_pair(cur->getLambda(), prototype));

		group->addDependency(prototype);
	});

	// B) create function definitions for all lambdas
	for_each(definition->getDefinitions(), [&, this](const LambdaBindingPtr& cur) {

		// unroll recursive definition once
		LambdaPtr unrolled = definition->unrollOnce(manager, cur->getVariable())->getLambda();

		// attack the same name to the unrolled version as to the original node
		string name = nameManager.getName(cur->getLambda());
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
	string wrapperName = name + "_wrap";
	FunctionTypePtr funType = lambda->getType();

	const vector<VariablePtr>& params = lambda->getParameterList();

	// create function code for lambda
	CodeFragmentPtr function = CodeFragment::createNew("Definition of " + name + " ... type: " + funType->toString());

	// allows derived function managers to insert a function prefix
	addFunctionPrefix(function, lambda);

	// write the function header
	TypePtr returnType = funType->getReturnType();
	function << typeManager.getTypeName(function, returnType) << " " << name << "(";
	appendFunctionParameters(function, params);
	function << ")";


	// add function body
	StatementPtr body = lambda->getBody();
	if (body->getNodeType() != NT_CompoundStmt) {
		body = CompoundStmt::get(cc.getNodeManager(), body);
	}

	// generate the function body
	cc.getStmtConverter().convert(body, function);

	function << "\n";
	if (createWrapper == true) {
		// add closure wrapper
		function << "static " << typeManager.getTypeName(function, returnType) << " " << wrapperName << "(";
		// TODO: test whether void* is fine or actual pointer is better
	//	function << typeManager.getFunctionTypeInfo(funType).closureName << "* _closure";
		function << "void* _closure";
		if (!params.empty()) {
			function << ", ";
			appendFunctionParameters(function, params);
		}
		function << ") { " << ((cc.getLangBasic().isUnit(returnType))?"":"return ") << name << "(";
		if (!params.empty()) {
			auto start = params.begin();
			auto end = params.end();
			function << cc.getNameManager().getName(*start);
			++start;
			while (start != end) {
				function << ", ";
				function << cc.getNameManager().getName(*start);
				++start;
			}
		}
		function << "); }\n";
	}

	// register and return result
	functions.insert(std::make_pair(lambda, function));
	return function;
}


void FunctionManager::createClosure(const CodeFragmentPtr& target, const core::BindExprPtr& bind) {

	// lookup bind
	CodeFragmentPtr definitions = resolve(bind);

	// add dependencies
	target->addDependency(definitions);

	// call constructor
	string name = cc.getNameManager().getName(bind);
	string ctrName = name + "_ctr";

	// filter all captured arguments
	vector<ExpressionPtr> captured = bind->getBoundExpressions();

	// obtain name of resulting function type and add cast
	string funTypeName = cc.getTypeManager().getTypeName(target, bind->getType());
	target << "(" << funTypeName << ")" << ctrName << "(";

	// allocate memory
	target << "(" << name << "*)alloca(sizeof(" + name + ")),";

	// add nested lambda
	cc.getStmtConverter().convert(bind->getCall()->getFunctionExpr());

	// append captured parameters
	for_each(captured, [&](const ExpressionPtr& cur) {
		target << ", ";
		cc.getStmtConverter().convert(cur, target);
	});

	target << ")";

}

CodeFragmentPtr FunctionManager::resolve(const BindExprPtr& bind) {

	// check cache
	auto pos = binds.find(bind);
	if (pos != binds.end()) {
		return pos->second;
	}

	// a short-cut to the type manager
	TypeManager& typeManager = cc.getTypeManager();

	// obtain some information
	const string& name = cc.getNameManager().getName(bind);
	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(bind->getType());
	FunctionTypePtr nestedFunType = static_pointer_cast<const FunctionType>(bind->getCall()->getFunctionExpr()->getType());

	// produce new definitions for this bind
	CodeFragmentPtr code = CodeFragment::createNew("Definition of " + name + " ... type: " + funType->toString());
	binds.insert(std::make_pair(bind, code));

	// add dependency to function type
	const TypeManager::FunctionTypeInfo& info = typeManager.getFunctionTypeInfo(funType);
	code->addDependency(info.definitions);

	// obtain list of captured variables + parameter map
	utils::map::PointerMap<ExpressionPtr, string> variableMap;

	// add parameters
	int paramCounter = 0;
	const vector<VariablePtr>& params = bind->getParameters()->getElements();
	for_each(params, [&](const VariablePtr& cur) {
		variableMap.insert(std::make_pair<ExpressionPtr, string>(cur, format("p%d", ++paramCounter)));
	});

	// add captured expressions
	int captureCounter = 0;
	const vector<ExpressionPtr>& args = bind->getCall()->getArguments();
	vector<ExpressionPtr> captured = bind->getBoundExpressions();
	for_each(args, [&](const ExpressionPtr& cur) {
		variableMap.insert(std::make_pair(cur, format("c%d", ++captureCounter)));
	});

	// 1) define struct
	code << "// -- Begin - Closure Constructs ------------------------------------------------------------\n";
	code << "// struct definition a closure of type " << *funType << "\n";
	code << "typedef struct _" << name << " {\n";
	code << "    " << typeManager.formatFunctionPointer(code, funType, "call") << ";\n";
	code << "    " << typeManager.formatParamter(code, nestedFunType, "nested") << ";\n";
	// add captured values
	for_each(captured, [&](const ExpressionPtr& cur) {
		code << "    " << typeManager.formatParamter(code, cur->getType(), variableMap.find(cur)->second, false) << ";\n";
	});
	code << "} " << name << ";\n\n";

	// 2) define mapping function (realizing the argument mapping)
	string mapperName = name + "_bind";
	string resultType = typeManager.getTypeName(code, funType->getReturnType());
	code << "static inline " << resultType << " " << mapperName << "(" << name << "* closure";
	if (!params.empty()) {
		code << ", " << join(", ", params, [&, this](std::ostream& out, const VariablePtr& cur) {
			out << typeManager.formatParamter(code, cur->getType(), variableMap.find(cur)->second, false);
		});
	}
	code << ") { ";
	if (resultType != "void") code << "return";
	code << " closure->nested->call(closure->nested";
	if (!args.empty()) {
		code << ", " << join(",", args, [&, this](std::ostream& out, const ExpressionPtr& cur) {
			out << (contains(captured, cur, equal_target<ExpressionPtr>())?"closure->":"") << variableMap.find(cur)->second;
		});
	}
	code << "); }\n\n";

	// 3) define a constructor
	string ctrName = name + "_ctr";

	code << "static inline " << name << "* " << ctrName << "(" << name << "* closure, ";
	code << typeManager.formatParamter(code, nestedFunType, "nested");
	if (!captured.empty()) {
		code << ", " << join(", ", captured, [&, this](std::ostream& out, const ExpressionPtr& cur) {
			out << typeManager.formatParamter(code, cur->getType(), variableMap.find(cur)->second, false);
		});
	}
	code << ") {\n";
	code << "    *closure = (" << name << "){&" << name << "_bind, nested";
	if (!captured.empty()) {
		code << ", " << join(", ", captured, [&, this](std::ostream& out, const ExpressionPtr& cur) {
			out << variableMap.find(cur)->second;
		});
	}
	code << "};\n";
	code << "    return closure;\n";
	code << "}\n";

	code << "// -- End - Closure Constructs --------------------------------------------------------------\n";

	// done
	return code;
}

} // end namespace simple_backend
} // end namespace insieme


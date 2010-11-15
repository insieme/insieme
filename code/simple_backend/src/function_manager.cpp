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

	std::ostream& printFunctionParamter(std::ostream& out, CodePtr& context, const VariablePtr& param, ConversionContext& cc) {

		// register ref-based variable within the variable manager
		if (param->getType()->getNodeType() == NT_RefType) {
			VariableManager::VariableInfo info;
			info.location = VariableManager::HEAP;
			cc.getVariableManager().addInfo(param, info);
		}

		// format parameter using type manager
		return out << cc.getTypeMan().formatParamter(context, param->getType(), cc.getNameGen().getVarName(param));
	}

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
	cs << cc.getTypeMan().getTypeName(cptr, funType->getReturnType()) << " " << ident << "(";
	// handle arguments
	cs << join(", ", lambda->getParams(), [&, this](std::ostream& os, const VariablePtr& param) {
		printFunctionParamter(os, cptr, param, this->cc);
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
		cs << cc.getTypeMan().getTypeName(surrounding, funType->getReturnType()) << " " << ident << "(";
		// handle arguments
		auto &cci = cc;
		cs << join(", ", cur.second->getParams(), [&](std::ostream& os, const VariablePtr& param) -> std::ostream& {
			return (os << cci.getTypeMan().getTypeName(cptr, param->getType()) << " " << cci.getNameGen().getVarName(param));
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
	cs << cc.getTypeMan().getTypeName(cptr, type->getReturnType()) << " " << name << "(";
	auto argType = type->getArgumentType();
	if(auto tupleArgType = dynamic_pointer_cast<const TupleType>(argType)) {
		cs << join(", ", tupleArgType->getElementTypes(), [&, this](std::ostream& o, const TypePtr& cur) -> std::ostream& {
			return (o << this->cc.getTypeMan().getTypeName(cptr, cur));
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

} // end namespace simple_backend
} // end namespace insieme


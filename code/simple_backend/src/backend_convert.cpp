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

#include "insieme/simple_backend/backend_convert.h"

#include "insieme/core/types.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/ast_builder.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;
using namespace utils::log;


void ConvertedCode::addFragment(const ExpressionPtr& source, CodePtr& fragment) {
	codeFragments.insert(std::make_pair(source, fragment));
}

std::ostream& ConvertedCode::printTo(std::ostream& out) const {

	// print some general header information ...
	out << "// --- Generated Inspire Code ---\n";

	// print headers
	for_each(headers, [&](const string& cur) {
		out << cur << std::endl;
	});

	// add code for entry points
	for_each(getSource()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "// --- Entry Point ---\n";
		assert(this->codeFragments.find(ep) != this->codeFragments.end());
		::operator<<(out, this->codeFragments.find(ep)->second);
	});
	return out;
}


void ConvertedCode::addHeaderLine(const string& line) {
	headers.push_back(line);
}

TargetCodePtr Converter::convert(const core::ProgramPtr& prog) {

	ConvertedCode* converted = new ConvertedCode(prog);

	// add headers
	stmtConverter->appendHeaders(converted);

	// convert the individual entry points
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {

		// create a fresh code fragment
		CodePtr fragment = std::make_shared<CodeFragment>();

		// convert code
		getStmtConverter().convert(ep, fragment);

		// register fragment
		fragment->setDummy(true);
		converted->addFragment(ep, fragment);
	});
	return TargetCodePtr(converted);
}

void StmtConverter::appendHeaders(ConvertedCode* converted) {
	// add basic includes
	converted->addHeaderLine("#include <alloca.h>");
	converted->addHeaderLine("#include <stddef.h>");
	converted->addHeaderLine("#include <stdlib.h>");

	// add runtime header
	converted->addHeaderLine("#include <runtime.h>");

	// add some macro definitions
	converted->addHeaderLine("#define bool int");
	converted->addHeaderLine("#define true 1");
	converted->addHeaderLine("#define false 0");
	converted->addHeaderLine("#define null 0");
}

void StmtConverter::visitJobExpr(const JobExprPtr& ptr) {
	// just use job manager
	cc.getJobManager().createJob(getCode(), ptr);
}


void StmtConverter::visitLambdaExpr(const LambdaExprPtr& ptr) {
	FunctionManager& funManager = cc.getFunctionManager();
	getCodeStream() << funManager.getFunctionName(defCodePtr, ptr);
}

void StmtConverter::visitCallExpr(const CallExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	//DLOG(INFO) << "CALLEXPR - " << ptr->getFunctionExpr() << ". prev cStr: \n" << cStr.getString();
	const std::vector<ExpressionPtr>& args = ptr->getArguments();
	auto funExp = ptr->getFunctionExpr();

	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(funExp->getType());
	assert(funType->getCaptureTypes().empty() && "Cannot call function exposing capture variables.");

	// special built in function handling -- TODO make more generic
	if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {

		// special handling for var-list handling
		if(cc.getLangBasic().isVarlistPack(funExp)) {
			//DLOG(INFO) << cStr.getString();
			// if the arguments are a tuple expression, use the expressions within the tuple ...
			if (args.size() == 1) { // should actually be implicit if all checks are satisfied
				if (TupleExprPtr arguments = dynamic_pointer_cast<const TupleExpr>(args[0])) {
					// print elements of the tuple directly ...
					functionalJoin([&]{ this->getCodeStream() << ", "; }, arguments->getExpressions(), [&](const ExpressionPtr& ep) { this->visit(ep); });

					// in case there is no argument => print 0
					if (arguments->getExpressions().empty()) {
						this->getCodeStream() << "0";
					}

					return;
				}
			}

			functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			//DLOG(INFO) << cStr.getString();
			//LOG(INFO) << "\n=========================== " << args.front() << " ---->> " << args.back() << "\n";
			return;
		}

		// try generic build-in C operator handling
		auto pos = formats.find(literalFun);
		if (pos != formats.end()) {
			pos->second->format(*this, cStr, ptr);
			return;
		}
	}

	// skip empty capture init expression
	if (funExp->getNodeType() == NT_CaptureInitExpr) {
		CaptureInitExprPtr cur = static_pointer_cast<const CaptureInitExpr>(funExp);
		if (cur->getValues().empty()) {
			// skip init expression
			funExp = cur->getLambda();
		}
	}

	// handle function based on the kind of function node
	switch(funExp->getNodeType()) {

		case NT_Literal: {
			cStr << cc.getFunctionManager().getFunctionName(defCodePtr, static_pointer_cast<const Literal>(funExp));
			cStr << "(";
			functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			cStr << ")";
			return;
		}

		case NT_Variable:
		{
			visit(funExp);
			cStr << "->fun";
			cStr << "(";
			visit(funExp);
			if (!args.empty()) {
				cStr << ", ";
				functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			}
			cStr << ")";
			return;
		}

		case NT_CallExpr:
		{

			TypeManager::FunctionTypeEntry details = cc.getTypeManager().getFunctionTypeDetails(funType);
			defCodePtr->addDependency(details.functorAndCaller);

			// use call wrapper
			cStr << details.callerName;
			cStr << "(";
			visit(funExp);
			if (!args.empty()) {
				cStr << ", ";
				functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			}
			cStr << ")";
			return;
		}

		case NT_CaptureInitExpr:
		{

			TypeManager::FunctionTypeEntry details = cc.getTypeManager().getFunctionTypeDetails(funType);
			defCodePtr->addDependency(details.functorAndCaller);

			// check whether it is a direct initialization / call situation
			bool directCall = false;
			CaptureInitExprPtr initExpr = static_pointer_cast<const CaptureInitExpr>(funExp);
			if (LambdaExprPtr lambda = dynamic_pointer_cast<const LambdaExpr>(initExpr->getLambda())) {
				// it is a direct call to a function => avoid using call wrapper
				cStr << cc.getFunctionManager().getFunctionName(defCodePtr, lambda);
				directCall = true;
			}

			if (!directCall) {
				// use call wrapper
				cStr << details.callerName;
			}

			cStr << "(";
			visitCaptureInitExprInternal(initExpr, directCall);
			if (!args.empty()) {
				cStr << ", ";
				functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			}
			cStr << ")";
			return;
		}

		case NT_LambdaExpr: {
			// function (without capture list) is directly provided => simply invoke
			cStr << cc.getFunctionManager().getFunctionName(defCodePtr, static_pointer_cast<const LambdaExpr>(funExp));
			cStr << "(";
			functionalJoin([&]{ this->getCodeStream() << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			cStr << ")";
			return;
		}

		default :
			cStr << "<?>Unhandled Type of Call Target</?>";
	}

}

void StmtConverter::visitCaptureInitExpr(const CaptureInitExprPtr& ptr) {
	visitCaptureInitExprInternal(ptr, false);
}

void StmtConverter::visitCaptureInitExprInternal(const CaptureInitExprPtr& ptr, bool directCall) {

	// resolve resulting type of expression
	FunctionTypePtr resType = static_pointer_cast<const FunctionType>(ptr->getType());
	TypeManager::FunctionTypeEntry resDetails = cc.getTypeManager().getFunctionTypeDetails(resType);
	defCodePtr->addDependency(resDetails.functorAndCaller);

	// resolve type of sub-expression
	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(ptr->getLambda()->getType());
	TypeManager::FunctionTypeEntry details = cc.getTypeManager().getFunctionTypeDetails(funType);
	defCodePtr->addDependency(details.functorAndCaller);

	// create surrounding cast
	CodeStream& cStr = getCodeStream();
	cStr << "((" << resDetails.functorName << "*)";

	// create struct including values
	cStr << "(&((" << details.functorName << ")";
	cStr << "{";

	// add function reference
	if (directCall) {
		cStr << "0";
	} else {
		cStr << "&";
		visit(ptr->getLambda());
	}

	 // TODO: add real size
	cStr << ", 0";

	// add captured parameters
	for_each(ptr->getValues(), [&, this](const ExpressionPtr& cur) {
		cStr << ", ";
		this->visit(cur);
	});

	cStr << "})))";
}

void StmtConverter::visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
	auto var = ptr->getVariable();

	// investigate initialization to determine whether variable is a pointer / skalar
	VariableManager& varManager = cc.getVariableManager();
	VariableManager::VariableInfo info;
	info.location = VariableManager::NONE;
	if (var->getType()->getNodeType() == NT_RefType) {

		const RefTypePtr& refType = static_pointer_cast<const RefType>(var->getType());
		if (refType->getElementType()->getNodeType() == NT_ArrayType) {
			// this is a "pointer" in C - and a pointer is on the stack (pointing to the HEAP)
			info.location = VariableManager::STACK;
		} else {
			ExpressionPtr initialization = ptr->getInitialization();
			switch (initialization->getNodeType()) {
			case NT_Variable:
				info = varManager.getInfo(static_pointer_cast<const Variable>(initialization));
				break;
			case NT_CallExpr:
				if (analysis::isCallOf(initialization, cc.getLangBasic().getRefNew())) {
					info.location = VariableManager::HEAP;
				} else {
					info.location = VariableManager::STACK;
				}
				break;
			default:
				// default is a stack variable
				info.location = VariableManager::STACK;
				break;
			}
		}
	}
	varManager.addInfo(var, info);


	// standard handling
	CodeStream& cStr = getCodeStream();
	string varName = cc.getNameManager().getVarName(var);
	cStr << cc.getTypeManager().formatParamter(defCodePtr, var->getType(), varName, info.location == VariableManager::STACK);

	// check whether there is an initialization
	const ExpressionPtr& init = ptr->getInitialization();
	if (core::analysis::isCallOf(init, cc.getLangBasic().getUndefined())) {
		return;
	}

	// test whether it is a variable initialization using an undefined value
	if (core::analysis::isCallOf(init, cc.getLangBasic().getRefVar()) &&
		core::analysis::isCallOf(static_pointer_cast<const CallExpr>(init)->getArguments()[0], cc.getLangBasic().getUndefined())) {
		return;
	}

	// start initialization
	cStr << " = ";

	// a special handling for initializing heap allocated structs (to avoid large stack allocated objects)
	const RefTypePtr& refType = (info.location == VariableManager::HEAP)?static_pointer_cast<const RefType>(var->getType()):RefTypePtr(NULL);
	if (refType && refType->getElementType()->getNodeType() == NT_StructType) {

		ASTBuilder builder(cc.getNodeManager());
		const lang::BasicGenerator& basic = cc.getNodeManager().basic;

		// start by allocating the required memory
		const StructTypePtr structType = static_pointer_cast<const StructType>(refType->getElementType());
		visit(builder.callExpr(basic.getRefNew(), builder.callExpr(basic.getUndefined(), basic.getTypeLiteral(structType))));

		// initialize all the members
		const ExpressionPtr& init = static_pointer_cast<const CallExpr>(ptr->getInitialization())->getArgument(0);
		if (analysis::isCallOf(init, basic.getUndefined())) {
			// that's it - no more work required
			return;
		}

		// ensure init value is a struct
		assert(init->getNodeType() == NT_StructExpr && "Initialization is not of proper type!");
		const StructExprPtr& structValue = static_pointer_cast<const StructExpr>(init);

		// init values, one after another
		for_each(structValue->getMembers(), [&, this](const StructExpr::Member& cur) {

			const Identifier& name = cur.first;
			ExpressionPtr value = cur.second;

			// remove leading var/new calls
			if (analysis::isCallOf(value, basic.getRefNew()) || analysis::isCallOf(value, basic.getRefVar())) {
				value = static_pointer_cast<const CallExpr>(value)->getArgument(0);
			}

			// skip vector initialization
			if (analysis::isCallOf(value, basic.getVectorInitUniform()) || analysis::isCallOf(value, basic.getVectorInitUndefined())) {
				// TODO: support init uniform
				return;
			}

			// start new line .. initialization of a member is required
			cStr << ";\n";

			// special treatement of vector initialization
			if (value->getNodeType() == NT_VectorExpr) {

				VectorTypePtr vectorType = static_pointer_cast<const VectorType>(static_pointer_cast<const VectorExpr>(value)->getType());
				string elementName = cc.getTypeManager().getTypeName(defCodePtr, vectorType->getElementType(), true);

				// init values using memcopy
				cStr << "memcpy(&((*" << varName << ")." << name << "),&((" << elementName << "[])";
				this->visit(value);
				cStr << "), sizeof(";
				cStr << elementName;
				cStr << ") * " << vectorType->getSize();
				cStr << ")";
				return;
			}

			// create assignment statement
			auto target = builder.callExpr(basic.getCompositeRefElem(), var,
					basic.getIdentifierLiteral(name), basic.getTypeLiteral(value->getType()));
			this->visit(builder.callExpr(basic.getRefAssign(), target, value));
		});

		// done - default handling is not necessary
		return;
	}

	// generate initializer expression
	if (core::analysis::isCallOf(ptr->getInitialization(), cc.getLangBasic().getRefVar())) {
		// in case it is allocated on a stack, skip ref.var
		CallExprPtr call = static_pointer_cast<const CallExpr>(ptr->getInitialization());
		visit(call->getArguments()[0]);
	} else {
		visit(ptr->getInitialization());
	}
}

void StmtConverter::visitLiteral(const LiteralPtr& ptr) {
	// just print literal
	getCodeStream() << ptr->getValue();
}

void StmtConverter::visitReturnStmt(const ReturnStmtPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "return ";
	visit(ptr->getReturnExpr());
	cStr << ";";
}

namespace {

	StatementPtr wrapBody(StatementPtr body) {
		if (body->getNodeCategory() == NC_Expression) {
			return CompoundStmt::get(body->getNodeManager(), toVector(body));
		}
		return body;
	}

}

void StmtConverter::visitForStmt(const ForStmtPtr& ptr) {
	auto decl = ptr->getDeclaration();
	auto var = decl->getVariable();

	CodeStream& cStr = getCodeStream();

	string ident = cc.getNameManager().getVarName(var);
	cStr << "for(";
	visit(decl);
	cStr << "; " << ident << " < ";
	visit(ptr->getEnd());
	cStr << "; " << ident << " += ";
	visit(ptr->getStep());
	cStr << ") ";
	visit(wrapBody(ptr->getBody()));
}

void StmtConverter::visitIfStmt(const IfStmtPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "if(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(wrapBody(ptr->getThenBody()));
	if (!cc.getLangBasic().isNoOp(ptr->getElseBody())) {
		cStr << " else ";
		visit(wrapBody(ptr->getElseBody()));
	}
}

void StmtConverter::visitWhileStmt(const WhileStmtPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "while(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(wrapBody(ptr->getBody()));
}

void StmtConverter::visitSwitchStmt(const SwitchStmtPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "switch(";
	visit(ptr->getSwitchExpr());
	cStr << ") {\n";
	for_each(ptr->getCases(), [&](const SwitchStmt::Case& curCase) { // GCC sucks
		cStr << "case ";
		this->visit(curCase.first);
		cStr << ":" << CodeStream::indR << "\n";
		this->visit(curCase.second);
		cStr << "; break;" << CodeStream::indL << "\n";
	});
	cStr << "}";
}

void StmtConverter::visitCompoundStmt(const CompoundStmtPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	if(ptr->getStatements().size() > 0) {
		cStr << "{" << CodeStream::indR << "\n";
		for_each(ptr->getChildList(), [&](const NodePtr& cur) {
			this->visit(cur);
			cStr << ";";
			if(cur != ptr->getChildList().back()) cStr << "\n";
		});
		cStr << CodeStream::indL << "\n}";
	} else {
		cStr << "{}";
	}
}

void StmtConverter::visitCastExpr(const CastExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "((" << cc.getTypeManager().getTypeName(defCodePtr, ptr->getType()) << ")(";
	visit(ptr->getSubExpression());
	cStr << "))";
}

void StmtConverter::visitVariable(const VariablePtr& ptr) {
	CodeStream& cStr = getCodeStream();

//	if (ptr->getType()->getNodeType() == NT_RefType
//			&& cc.getVariableManager().getInfo(ptr).location != VariableManager::HEAP) {
//		cStr << "&";
//	}

	bool deref = true;
	if (const RefTypePtr& refType = dynamic_pointer_cast<const RefType>(ptr->getType())) {
		TypePtr elementType = refType->getElementType();
		NodeType nodeType =elementType->getNodeType();
		if (nodeType == NT_ArrayType || nodeType == NT_VectorType) {
			deref = false;
		}

		// mainly for local capture variables => those are not on the stack
		if (deref && cc.getVariableManager().getInfo(ptr).location == VariableManager::HEAP) {
			deref = false;
		}
	}

	cStr << ((deref)?"&":"") << cc.getNameManager().getVarName(ptr);
}

void StmtConverter::visitMemberAccessExpr(const MemberAccessExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	TypePtr type = ptr->getType();
	if (type->getNodeType() == NT_RefType) {
		cStr << "&";
	}
	cStr << "(";
	visit(ptr->getSubExpression());
	cStr << "." << ptr->getMemberName() << ")";
}

void StmtConverter::visitStructExpr(const StructExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "((" << cc.getTypeManager().getTypeName(defCodePtr, ptr->getType(), true) <<"){";
	cStr << CodeStream::indR;
	for_each(ptr->getMembers(), [&](const StructExpr::Member& cur) {
		// skip ref.var if present
		if (core::analysis::isCallOf(cur.second, cc.getLangBasic().getRefVar())) {
			this->visit(static_pointer_cast<const CallExpr>(cur.second)->getArguments()[0]);
		} else {
			this->visit(cur.second);
		}
		if(cur != ptr->getMembers().back()) cStr << ",\n";
	});
	cStr << CodeStream::indL << "\n";
	cStr << "})";
}

void StmtConverter::visitUnionExpr(const UnionExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	cStr << "((" << cc.getTypeManager().getTypeName(defCodePtr, ptr->getType(), true) <<"){";
	visit(ptr->getMember());
	cStr << "})";
}

void StmtConverter::visitVectorExpr(const VectorExprPtr& ptr) {
	CodeStream& cStr = getCodeStream();

	// handle single-element initializations
	if (ptr->getExpressions().size() == 1) {
		this->visit(ptr->getExpressions()[0]);
		return;
	}

	// test whether all expressions are calls to ref.var ...
	cStr << "{";
	int i=0;
	for_each(ptr->getExpressions(), [&](const ExpressionPtr& cur) {
		if (!core::analysis::isCallOf(cur, cc.getLangBasic().getRefVar())) {
			LOG(FATAL) << "Unsupported vector initialization: " << toString(*cur);
			assert(false && "Vector initialization not supported for the given values!");
		}
		// print argument of ref.var
		this->visit(static_pointer_cast<const CallExpr>(cur)->getArguments()[0]);
		if((++i)!=ptr->getExpressions().size()) cStr << ", ";
	});
	cStr << "}";
}

void StmtConverter::visitMarkerExpr(const MarkerExprPtr& ptr) {
	// just ignore
	visit(ptr->getSubExpression());
}

void StmtConverter::visitMarkerStmt(const MarkerStmtPtr& ptr) {
	// just ignore
	visit(ptr->getSubStatement());
}



// -------------------------------- Variable Manager -----------------------------------------

const VariableManager::VariableInfo& VariableManager::getInfo(const VariablePtr& variable) const {
	auto pos = variableMap.find(variable);
	if(pos == variableMap.end()) {
		LOG(INFO) << "v" << variable->getId();
	}
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



namespace {

	ExpressionPtr evalLazy(const NodePtr& lazy) {

		NodeManager& manager = lazy->getNodeManager();

		ExpressionPtr exprPtr = dynamic_pointer_cast<const Expression>(lazy);
		assert(exprPtr && "Lazy is not an expression!");

		FunctionTypePtr funType = dynamic_pointer_cast<const FunctionType>(exprPtr->getType());
		assert(funType && "Illegal lazy type!");

		// form call expression
		CallExprPtr call = CallExpr::get(manager, funType->getReturnType(), exprPtr, toVector<ExpressionPtr>());
		return core::transform::tryInlineToExpr(manager, call);
	}

	void handleIncOperand(StmtConverter& converter, const NodePtr& target) {

		assert(dynamic_pointer_cast<const Expression>(target) && "Operator must be an expression.");

		// check whether a deref is required
		CodeStream& stream = converter.getCodeStream();
		stream << "(*";
		converter.convert(target);
		stream << ")";
	}

	void handleRefConstructor(StmtConverter& converter, CodeStream& cStr, const NodePtr& initValue, bool isNew) {

		// check input parameters
		assert(dynamic_pointer_cast<const Expression>(initValue) && "Init Value is not an expression!");

		// quick check for arrays => extra handling
		const core::lang::BasicGenerator& basic = converter.getConversionContext().getLangBasic();
		if (core::analysis::isCallOf(initValue, basic.getArrayCreate1D()) ||
			core::analysis::isCallOf(initValue, basic.getArrayCreateND())) {

			// vector creation is sufficient
			converter.convert(initValue);
			return;
		}


		// extract type
		TypePtr type = static_pointer_cast<const Expression>(initValue)->getType();
		string typeName = converter.getConversionContext().getTypeManager().getTypeName(converter.getCode(), type, true);

		// use stack or heap allocator
		string allocator = (isNew)?"malloc":"alloca";

		// special handling of some initialization values
		string stmt = toString(*initValue);

		// TODO: use pattern matching!

		// check for vector init undefined and undefined
		if (core::analysis::isCallOf(initValue, basic.getVectorInitUndefined()) || core::analysis::isCallOf(initValue, basic.getUndefined())) {
			cStr << allocator << "(sizeof(" << typeName << "))";
			return;
		}

		if (isNew && core::analysis::isCallOf(initValue, basic.getVectorInitUniform())) {
			NodePtr param = static_pointer_cast<const CallExpr>(initValue)->getArguments()[0];

			// iterate through multiple vector init uniform calls
			while (core::analysis::isCallOf(param, basic.getVectorInitUniform())) {
				param = static_pointer_cast<const CallExpr>(param)->getArguments()[0];
			}

			// innermost has to be a ref-var call with a literal 0
			if (core::analysis::isCallOf(param, basic.getRefVar()) || core::analysis::isCallOf(param, basic.getRefNew())) {
				const NodePtr& refVar = static_pointer_cast<const CallExpr>(param)->getArguments()[0];
				if (LiteralPtr literal = dynamic_pointer_cast<const Literal>(refVar)) {
					string value = literal->getValue();
					if (basic.isInitZero(literal) || value == "0" || value == "0.0" || value == "\0") {
						cStr << "calloc(sizeof(" << typeName << "), 1)";
						return;
					}
				}
				if (core::analysis::isCallOf(refVar, basic.getInitZero())) {
					cStr << "calloc(sizeof(" << typeName << "), 1)";
					return;
				}
			}
		}

		// TODO: use memset for other initializations => see memset!!

		cStr << "memcpy(";
		cStr << allocator << "(";
		cStr << "sizeof(";
		cStr << typeName;
		cStr << ")), &((";
		cStr << typeName;
		cStr << ")";
		converter.convert(initValue);
		cStr << "), sizeof(";
		cStr << typeName;
		cStr << "))";
	}
}


namespace formatting {


	/**
	 * A utility function to obtain the n-th argument within the given call expression.
	 *
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the requested argument
	 * @return the requested argument or a NULL pointer in case there is no such argument
	 */
	ExpressionPtr getArgument(const CallExprPtr& call, unsigned n) {
		auto arguments = call->getArguments();
		if (n < arguments.size()) {
			return arguments[n];
		}
		return ExpressionPtr();
	}

	/**
	 * A utility function visiting the n-th argument of a call expression.
	 *
	 * @param converter the converter to be used for the actual conversion
	 * @param call the expression from which the argument should be extracted
	 * @param n the index of the argument to be visited; in case there is no such argument, nothing will be visited
	 */
	void visitArgument(StmtConverter& converter, const CallExprPtr& call, unsigned n) {
		ExpressionPtr argument = getArgument(call, n);
		if (argument) {
			converter.convert(argument);
		}
	}



	FormatTable getBasicFormatTable(const lang::BasicGenerator& basic) {

		FormatTable res;

		#include "insieme/simple_backend/format_spec_start.mac"


		ADD_FORMATTER(basic.getRefDeref(), { OUT("*"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getRefAssign(), {
				NodeManager& manager = converter.getConversionContext().getNodeManager();
				ExpressionPtr target = static_pointer_cast<const Expression>(ARG(0));
				TypePtr valueType = static_pointer_cast<const RefType>(target->getType())->getElementType();
				converter.convert(CallExpr::get(manager, valueType, basic.getRefDeref(), toVector<ExpressionPtr>(target)));
				OUT(" = ");
				VISIT_ARG(1);
		});

		ADD_FORMATTER_DETAIL(basic.getRefVar(), false, { handleRefConstructor(converter, cStr, ARG(0), false); });
		ADD_FORMATTER_DETAIL(basic.getRefNew(), false, { handleRefConstructor(converter, cStr, ARG(0), true); });

		ADD_FORMATTER(basic.getRefDelete(), { OUT(" free("); VISIT_ARG(0); OUT(")"); });

		ADD_FORMATTER(basic.getScalarToVector(), { VISIT_ARG(0); });

		ADD_FORMATTER(basic.getArrayCreate1D(), {

				// test whether the size is fixed to 1
				if (ARG(1)->getNodeType() == NT_Literal && static_pointer_cast<const Literal>(ARG(1))->getValue() == "1") {
					// special handling of arrays with a single element
					ASTBuilder builder(call->getNodeManager());
					NodePtr init = ARG(0);
					if (core::analysis::isCallOf(init, basic.getRefVar())) {
						init = builder.refNew(static_pointer_cast<const CallExpr>(init)->getArguments()[0]);
						converter.convert(init);
					} else if (core::analysis::isCallOf(init, basic.getRefNew())) {
						converter.convert(init);
					} else {
						converter.convert(builder.refNew(static_pointer_cast<const Expression>(ARG(0))));
					}

				} else {

					// ensure array is randomly initialized
					ExpressionPtr initValue = ARG(0);
					assert(!core::analysis::isCallOf(initValue, basic.getRefVar()) && "Initialization of arrays based on ref-elements not supported yet!" );
					assert(core::analysis::isCallOf(initValue, basic.getUndefined()) && "Initializing arrays with concrete values not supported yet.");

					// all arrays are allocated on the HEAP
					OUT("malloc(");
					OUT("sizeof(");
					TypePtr type = static_pointer_cast<const Expression>(ARG(0))->getType();
					OUT(converter.getConversionContext().getTypeManager().getTypeName(converter.getCode(), type, true));
					OUT(")*");
					VISIT_ARG(1);
					OUT(")");
				}
		});

		ADD_FORMATTER_DETAIL(basic.getArraySubscript1D(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

		ADD_FORMATTER_DETAIL(basic.getArrayRefElem1D(), false, {
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(basic.getArrayRefProjection1D(), false, {
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		ADD_FORMATTER_DETAIL(basic.getVectorSubscript(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

		ADD_FORMATTER_DETAIL(basic.getVectorRefProjection(), false, {
				OUT("&("); VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); OUT(")");
		});

		//ADD_FORMATTER(basic.getVectorInitUniform(), { OUT("{"); VISIT_ARG(0); OUT("}"); });
		ADD_FORMATTER_DETAIL(basic.getVectorInitUniform(), false, { OUT("{}"); });
		ADD_FORMATTER_DETAIL(basic.getVectorInitUndefined(), false, { OUT("{}"); });


		// struct operations
		ADD_FORMATTER(basic.getCompositeRefElem(), { OUT("&((*"); VISIT_ARG(0); OUT(")."); VISIT_ARG(1); OUT(")"); });
		ADD_FORMATTER(basic.getCompositeMemberAccess(), { VISIT_ARG(0); OUT("."); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getRealAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntAnd(), { VISIT_ARG(0); OUT("&"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntOr(), { VISIT_ARG(0); OUT("|"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntXor(), { VISIT_ARG(0); OUT("^"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntNot(), { OUT("~"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getUnsignedIntLShift(), { VISIT_ARG(0); OUT("<<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntRShift(), { VISIT_ARG(0); OUT(">>"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntPreInc(), { OUT("++"); handleIncOperand(converter, ARG(0)); });
		ADD_FORMATTER(basic.getUnsignedIntPostInc(), { handleIncOperand(converter, ARG(0)); OUT("++"); });
		ADD_FORMATTER(basic.getUnsignedIntPreDec(), { OUT("--"); handleIncOperand(converter, ARG(0)); });
		ADD_FORMATTER(basic.getUnsignedIntPostDec(), { handleIncOperand(converter, ARG(0)); OUT("--"); });


		ADD_FORMATTER(basic.getSignedIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntLShift(), { VISIT_ARG(0); OUT("<<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntRShift(), { VISIT_ARG(0); OUT(">>"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntPreInc(), { OUT("++"); handleIncOperand(converter, ARG(0)); });
		ADD_FORMATTER(basic.getSignedIntPostInc(), { handleIncOperand(converter, ARG(0)); OUT("++"); });
		ADD_FORMATTER(basic.getSignedIntPreDec(), { OUT("--"); handleIncOperand(converter, ARG(0)); });
		ADD_FORMATTER(basic.getSignedIntPostDec(), { handleIncOperand(converter, ARG(0)); OUT("--"); });

		ADD_FORMATTER(basic.getSignedIntAnd(), { VISIT_ARG(0); OUT("&"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntOr(), { VISIT_ARG(0); OUT("|"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntXor(), { VISIT_ARG(0); OUT("^"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntNot(), { OUT("~"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getBoolLAnd(), { VISIT_ARG(0); OUT("&&"); converter.convert(evalLazy(ARG(1))); });
		ADD_FORMATTER(basic.getBoolLOr(), { VISIT_ARG(0); OUT("||"); converter.convert(evalLazy(ARG(1))); });
		ADD_FORMATTER(basic.getBoolNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolLNot(), { OUT("!"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getCharNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getCharEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getCharGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getCharGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getCharLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getCharLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getUnsignedIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUnsignedIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getRealEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealNe(), { VISIT_ARG(0); OUT("!="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });


		// string conversion
		ADD_FORMATTER_DETAIL(basic.getStringToCharPointer(), false, { VISIT_ARG(0); });


		ADD_FORMATTER(basic.getIfThenElse(), {
				OUT("("); VISIT_ARG(0); OUT(")?(");
				converter.convert(evalLazy(ARG(1)));
				OUT("):(");
				converter.convert(evalLazy(ARG(2)));
				OUT(")");
		});

		ADD_FORMATTER_DETAIL(basic.getSizeof(), false, {
				OUT("sizeof(");
				GenericTypePtr type = dynamic_pointer_cast<const GenericType>(
						static_pointer_cast<const Expression>(ARG(0))->getType()
				);
				assert(type && "Illegal argument to sizeof operator");
				TypePtr target = type->getTypeParameter()[0];
				OUT(converter.getConversionContext().getTypeManager().getTypeName(converter.getCode(), target, true));
				OUT(")");
		});


		// handle parallel operators
		ADD_FORMATTER_DETAIL(basic.getParallel(), false, { OUT("isbr_parallel("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(basic.getMerge(), false, { OUT("isbr_merge("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(basic.getBarrier(), false, { OUT("isbr_barrier("); VISIT_ARG(0); OUT(")"); });

		ADD_FORMATTER_DETAIL(basic.getGetThreadGroup(), false, { OUT("isbr_getThreadGroup("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(basic.getGetThreadId(), false, { OUT("isbr_getThreadId("); VISIT_ARG(0); OUT(")"); });
		ADD_FORMATTER_DETAIL(basic.getGetGroupSize(), false, { OUT("isbr_getGroupSize("); VISIT_ARG(0); OUT(")"); });


		ADD_FORMATTER_DETAIL(basic.getPFor(), false, {
				converter.getConversionContext().getJobManager().createPFor(converter.getCode(), call);
		});

		#include "insieme/simple_backend/format_spec_end.mac"

		return res;
	}
}

}
}


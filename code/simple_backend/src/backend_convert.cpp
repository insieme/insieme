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

#include "insieme/simple_backend/variable_manager.h"

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

	// including this header will result into problems on a 32 bit system
	//  - reason: memset / memcpy uses size_t, which is fixed to 64 bit within insieme
	//converted->addHeaderLine("#include <string.h>");

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

	// special built in function handling
	if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {

		// special handling for var-list handling
		if(cc.getLangBasic().isVarlistPack(funExp)) {
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

namespace {

	/**
	 * Determines whether the given type is a reference of an array or vector type.
	 * TODO: move this to a more general case, use it more often (especially within the backend_convert.cpp)
	 */
	const bool isVectorOrArrayRef(const TypePtr& type) {
		if (type->getNodeType() != NT_RefType) {
			return false;
		}
		const RefTypePtr& refType = static_pointer_cast<const RefType>(type);
		NodeType nodeType = refType->getElementType()->getNodeType();
		return nodeType == NT_VectorType || nodeType == NT_ArrayType;
	}
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

	// add size of struct
	cStr << ", sizeof(" << details.functorName << ")";

	// add captured parameters
	for_each(ptr->getValues(), [&, this](const ExpressionPtr& cur) {

		// TODO: handle capture variables uniformely
		bool addAddressOperator = isVectorOrArrayRef(cur->getType());
		if (addAddressOperator
				&& cur->getNodeType() == NT_Variable
				&& cc.getVariableManager().getInfo(static_pointer_cast<const Variable>(cur)).location == VariableManager::HEAP) {

			addAddressOperator = false;
		}
		cStr << (addAddressOperator?",&":",");

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
	bool isAllocatedOnHEAP = false;
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
					isAllocatedOnHEAP = true;
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
	cStr << cc.getTypeManager().formatParamter(defCodePtr, var->getType(), varName, !isAllocatedOnHEAP);

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

	const RefTypePtr& refType = (isAllocatedOnHEAP)?static_pointer_cast<const RefType>(var->getType()):RefTypePtr(NULL);
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

	bool deref = true;
	if (const RefTypePtr& refType = dynamic_pointer_cast<const RefType>(ptr->getType())) {
		TypePtr elementType = refType->getElementType();
		NodeType nodeType = elementType->getNodeType();
		if (nodeType == NT_VectorType || nodeType == NT_ArrayType) {
			deref = false;
		}

		// for local captured variables and HEAP data
		if (deref && cc.getVariableManager().getInfo(ptr).location == VariableManager::HEAP) {
			//no deref necessary in those cases - since a pointer is used to handle those
			// TODO: restructure location field, determining whether something is a pointer or scalar
			deref = false;
		}
	} else {
		// no de-referencing required at all - since no reference is represented by this variable
		deref = false;
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



}
}


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

#include <glog/logging.h>

#include "insieme/core/annotated_ptr.h"
#include "insieme/core/types.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;

ConvertedCode ConversionContext::convert(const core::ProgramPtr& prog) {
	ConvertedCode converted(prog);
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {
		ConvertVisitor convVisitor(*this);
		convVisitor.visit(ep);
		CodePtr ptr = convVisitor.getCode();
		if (ep->getNodeType() == NT_LambdaExpr || ep->getNodeType()==NT_RecLambdaExpr) {
			// remove root node content => not required
			CodePtr tmp = CodePtr(new CodeFragment(string("root-node")));
			for_each(ptr->getDependencies(), [&tmp](const CodePtr& cur) {
				tmp->addDependency(cur);
			});
			ptr = tmp;
		}
		converted.insert(std::make_pair(ep, ptr));
	});
	return converted;
}


void ConvertVisitor::visitLambdaExpr(const LambdaExprPtr& ptr) {

	// obtain a name for the function ...
	//string cFunName = cc.getNameGen().getName(ptr);


	// get name for function type
	string name = nameGen.getName(ptr, "lambda");
	string structName = "struct " + name + "_closure";

	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(ptr->getType());

	// define the empty lambda struct
	// A) create struct for lambda closure
	{
		CodePtr cptr(new CodeFragment(string("lambda_") + name));
		CodeStream& out = cptr->getCodeStream();
		defCodePtr->addDependency(cptr);
		out << structName << " { \n";

			// add function pointer
			out << "    ";
			out << cc.getTypeMan().getTypeName(cptr, funType->getReturnType());
			out << "(*fun)(" << "void*";
			auto arguments = funType->getArgumentType()->getElementTypes();
			if (!arguments.empty()) {
				out << "," << join(",", arguments, [&, this](std::ostream& out, const TypePtr& cur) {
					out << cc.getTypeMan().getTypeName(cptr, cur);
				});
			}
			out << ");\n";

			// add struct size
			out << "    const size_t size;\n";

			// add capture values
			for_each(ptr->getCaptureList(), [&](const DeclarationStmtPtr& cur) {
				VariablePtr var = cur->getVariable();
				out << "    " << cc.getTypeMan().formatParamter(cptr, var->getType(),nameGen.getVarName(var)) << ";\n";
			});
		out << "};\n";
	}

	// B) create function to be computed using function manager + add dependency
	defCodePtr->addDependency(cc.getFuncMan().getFunction(ptr));

	// C) allocate a struct
	cStr << "((struct " << nameGen.getName(ptr->getType(), "funType") << "*)&((" << structName << "){&" << cc.getNameGen().getName(ptr) << ", sizeof(" + structName + ")";
	if (!ptr->getCaptureList().empty()) {
		cStr << "," << join(",", ptr->getCaptureList(), [&, this](std::ostream& out, const DeclarationStmtPtr& cur) {
			this->visit(cur->getInitialization());
		});
	}
	cStr << "}))";



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
			case NT_MemberAccessExpr:
				return false;
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
		if(funName == "ref.deref") {

			// test whether a deref is required
			bool deref = requiresDeref(args.front(), cc);

			// add operation
			if (deref) cStr << "(*";
			if (ptr->getArguments()[0]->getNodeType() == NT_Variable) cStr << "*";
			visit(args.front());
			if (deref) cStr << ")";

			return;
		} if(funName == "ref.var") {
			// TODO handle case where not RHS of local var decl
			visit(args.front());
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

		// try generic build-in C operator handling
		auto pos = formats.find(literalFun);
		if (pos != formats.end()) {
			pos->second->format(*this, cStr, ptr);
			return;
		}
	}

	// TODO: gradually remove this -> literal handling should be sufficient
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

	// invoke external method ...
	if (funExp->getNodeType() == NT_Literal) {
		visit(funExp);
		cStr << "(";
		functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
		cStr << ")";
		return;
	}

	// TODO: add special handling if lambda is directly given (not a variable)

	// handle variables
	if (funExp->getNodeType() == NT_Variable) {
		visit(funExp);
		cStr << "->fun";
		cStr << "(";
		visit(funExp);
		if (!args.empty()) {
			cStr << ", ";
			functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
		}
		cStr << ")";
		return;
	}

	// make code section depending on lambda type definition (required for caller)
	cc.getTypeMan().getTypeName(defCodePtr, funExp->getType());

	// use type specific call routine ..
	cStr << "call_" << nameGen.getName(funExp->getType(), "funType") << "(";
	visit(funExp);
	if (!args.empty()) {
		cStr << ", ";
		functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
	}
	cStr << ")";

	//cStr << "<?>Unhandled Call Expression</?>";

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
		cStr << cc.getTypeMan().getTypeName(defCodePtr, innerType, true) << " " << nameGen.getVarName(var);
		for_each(vecLengths, [this](unsigned vl) { this->cStr << "[" << vl << "]"; });
		// TODO initialization
		return;
	}

	// standard handling
	cStr << cc.getTypeMan().getTypeName(defCodePtr, var->getType(), info.location == VariableManager::STACK) << " " << nameGen.getVarName(var) << " = ";

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

void ConvertVisitor::visitIfStmt(const IfStmtPtr& ptr) {
	cStr << "if(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(ptr->getThenBody());
	if (*(ptr->getElseBody()) != (*lang::STMT_NO_OP_PTR)) {
		cStr << " else ";
		visit(ptr->getElseBody());
	}
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
	} else {
		cStr << "{}";
	}
}

void ConvertVisitor::visitCastExpr(const CastExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType()) << ")(";
	visit(ptr->getSubExpression());
	cStr << "))";
}

void ConvertVisitor::visitVariable(const VariablePtr& ptr) {
	if (ptr->getType()->getNodeType() == NT_RefType) {
		cStr << "&";
	}
	cStr << nameGen.getVarName(ptr);
}

void ConvertVisitor::visitMemberAccessExpr(const MemberAccessExprPtr& ptr) {
	cStr << "(";
	visit(ptr->getSubExpression());
	cStr << "." << ptr->getMemberName() << ")";
}

void ConvertVisitor::visitStructExpr(const StructExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType()) <<"){";
	for_each(ptr->getMembers(), [&](const StructExpr::Member& cur) {
		this->visit(cur.second);
		cStr << ", ";
	});
	cStr << "})";
}

void ConvertVisitor::visitUnionExpr(const UnionExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType()) <<"){";
	visit(ptr->getMember());
	cStr << "})";
}


// -------------------------------- Variable Manager -----------------------------------------

const VariableManager::VariableInfo& VariableManager::getInfo(const VariablePtr& variable) const {
	auto pos = variableMap.find(variable);
	if(pos == variableMap.end())
		DLOG(INFO) << "v" << variable->getId();
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




namespace detail {

	namespace {

		/**
		 * A utility function to obtain the n-th argument within the given call expression.
		 *
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the requested argument
		 * @return the requested argument or a NULL pointer in case there is no such argument
		 */
		NodePtr getArgument(const CallExprPtr& call, unsigned n) {
			auto arguments = call->getArguments();
			if (n < arguments.size()) {
				return arguments[n];
			}
			return NodePtr();
		}

		/**
		 * A utility function visiting the n-th argument of a call expression.
		 *
		 * @param visitor the visitor to be used for the actual visiting
		 * @param call the expression from which the argument should be extracted
		 * @param n the index of the argument to be visited; in case there is no such argument, nothing will be visited
		 */
		void visitArgument(ConvertVisitor& visitor, const CallExprPtr& call, unsigned n) {
			NodePtr argument = getArgument(call, n);
			if (argument) {
				visitor.visit(argument);
			}
		}

	}


	FormatTable initFormatTable() {

		FormatTable res;

		#define OUT(Literal) cStr << Literal
		#define ARG(N) getArgument(call, N)
		#define VISIT_ARG(N) visitArgument(visitor, call, N)
		#define ADD_FORMATTER(Literal, FORMAT) \
					res.insert(std::make_pair(Literal, make_formatter([](ConvertVisitor& visitor, CodeStream& cStr, const CallExprPtr& call) FORMAT )))


		ADD_FORMATTER(lang::OP_REF_ASSIGN_PTR, {
				NodeManager& manager = visitor.getConversionContext().getNodeManager();
				ExpressionPtr target = static_pointer_cast<const Expression>(ARG(0));
				TypePtr valueType = static_pointer_cast<const RefType>(target->getType())->getElementType();
				visitor.visit(CallExpr::get(manager, valueType, lang::OP_REF_DEREF, toVector<ExpressionPtr>(target)));
				OUT(" = ");
				VISIT_ARG(1);
		});

		// TODO: integrate those as well ...
//		ADD_FORMATTER(lang::OP_REF_VAR_PTR, { OUT(" var("); VISIT_ARG(0); OUT(")"); });
//		ADD_FORMATTER(lang::OP_REF_NEW_PTR, { OUT(" new("); VISIT_ARG(0); OUT(")"); });
//		ADD_FORMATTER(lang::OP_REF_DELETE_PTR, { OUT(" del("); VISIT_ARG(0); OUT(")"); });

		//ADD_FORMATTER(lang::OP_SUBSCRIPT_PTR, { VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); });
		ADD_FORMATTER(lang::OP_SUBSCRIPT_SINGLE_PTR, { VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); });

		ADD_FORMATTER(lang::OP_REAL_ADD_PTR, { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_SUB_PTR, { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_MUL_PTR, { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_REAL_DIV_PTR, { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });

		ADD_FORMATTER(lang::OP_UINT_ADD_PTR, { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_SUB_PTR, { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_MUL_PTR, { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_DIV_PTR, { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_MOD_PTR, { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(lang::OP_INT_ADD_PTR, { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_SUB_PTR, { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_MUL_PTR, { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_DIV_PTR, { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_MOD_PTR, { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(lang::OP_BOOL_AND_PTR, { VISIT_ARG(0); OUT("&&"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_OR_PTR, { VISIT_ARG(0); OUT("||"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_EQ_PTR, { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_BOOL_NOT_PTR, { OUT("!"); VISIT_ARG(0); });

		ADD_FORMATTER(lang::OP_UINT_EQ_PTR, { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_GE_PTR, { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_GT_PTR, { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_LT_PTR, { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_UINT_LE_PTR, { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(lang::OP_INT_EQ_PTR, { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_GE_PTR, { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_GT_PTR, { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_LT_PTR, { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(lang::OP_INT_LE_PTR, { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(lang::OP_ITE_PTR, { OUT("(("); VISIT_ARG(0); OUT(")?("); VISIT_ARG(1); OUT("):("); VISIT_ARG(1); OUT("))"); });

		#undef ADD_FORMATTER
		#undef OUT
		#undef ARG
		#undef VISIT_ARG


		return res;


	}

}

}
}

std::ostream& operator<<(std::ostream& out, const insieme::simple_backend::ConvertedCode& code) {
	out << "// --- Generated Inspire Code ---\n";
	out << "#include <stddef.h>\n";
	out << "#define bool int\n";
	out << "#define true 1\n";
	out << "#define false 0\n";

	for_each(code.getProgram()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "// --- Entry Point ---\n";
		out << (*code.find(ep)).second;
	});
	return out;
}

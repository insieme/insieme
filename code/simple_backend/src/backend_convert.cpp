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
#include "insieme/core/transform/manipulation.h"

namespace insieme {
namespace simple_backend {
	
using namespace core;

ConversionContext::ConversionContext(const NodePtr& target)
	 : typeMan(nameGen), funcMan(*this), nodeManager(target->getNodeManager()), basic(nodeManager.basic) { }


ConvertedCode ConversionContext::convert(const core::ProgramPtr& prog) {
	ConvertedCode converted(prog);
	for_each(prog->getEntryPoints(), [&converted, this](const ExpressionPtr& ep) {
		ConvertVisitor convVisitor(*this);
		convVisitor.visit(ep);
		CodePtr ptr = convVisitor.getCode();
		ptr->setDummy(true);
		converted.insert(std::make_pair(ep, ptr));
	});
	return converted;
}


void ConvertVisitor::visitLambdaExpr(const LambdaExprPtr& ptr) {
	FunctionManager& funManager = cc.getFuncMan();
	cStr << funManager.getFunctionName(defCodePtr, ptr);
}

namespace {

	/**
	 * Determines whether using the given expression as a LHS expression within an assignment or within a
	 * RHS read requires a de-referencing within C.
	 */
	bool requiresDeref(const ExpressionPtr& target, ConversionContext& cc) {
		switch (target->getNodeType()) {
			case NT_CallExpr:
				// subscript operator should not be again dereferenced in the end
				return !(cc.basic.isSubscriptOperator(static_pointer_cast<const CallExpr>(target)->getFunctionExpr()));
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

	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(funExp->getType());
	assert(funType->getCaptureTypes().empty() && "Cannot call function exposing capture variables.");

	// special built in function handling -- TODO make more generic
	if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {
		//LOG(INFO) << "+++++++ visitCallExpr dyncastLit\n";
		auto funName = literalFun->getValue();
		//LOG(INFO) << "+++++++ val: " << funName << "\n";
		//if (cc.basic.isRefDeref(literalFun)) {
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
		} else if(cc.basic.isVarlistPack(funExp)) {
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
		auto pos = formats.find(literalFun->getValue());
		if (pos != formats.end()) {
			cStr << "(";
			pos->second->format(*this, cStr, ptr);
			cStr << ")";
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


	switch(funExp->getNodeType()) {

		case NT_Literal: {
			cStr << cc.getFuncMan().getFunctionName(defCodePtr, static_pointer_cast<const Literal>(funExp));
			functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
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
				functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			}
			cStr << ")";
			return;
		}

		case NT_CallExpr:
		case NT_CaptureInitExpr:
		{

			TypeManager::FunctionTypeEntry details = cc.getTypeMan().getFunctionTypeDetails(funType);
			defCodePtr->addDependency(details.functorAndCaller);

			cStr << details.callerName;
			cStr << "(";
			visit(funExp);
			if (!args.empty()) {
				cStr << ", ";
				functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			}
			cStr << ")";
			return;
		}

		case NT_LambdaExpr: {
			// function (without capture list) is directly provided => simply invoke
			cStr << cc.getFuncMan().getFunctionName(defCodePtr, static_pointer_cast<const LambdaExpr>(funExp));
			cStr << "(";
			functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
			cStr << ")";
			return;
		}

		default :
			cStr << "<?>Unhandled Type of Call Target</?>";
	}

}

void ConvertVisitor::visitCaptureInitExpr(const CaptureInitExprPtr& ptr) {

	// resolve resulting type of expression
	FunctionTypePtr resType = static_pointer_cast<const FunctionType>(ptr->getType());
	TypeManager::FunctionTypeEntry resDetails = cc.getTypeMan().getFunctionTypeDetails(resType);
	defCodePtr->addDependency(resDetails.functorAndCaller);

	// resolve type of sub-expression
	FunctionTypePtr funType = static_pointer_cast<const FunctionType>(ptr->getLambda()->getType());
	TypeManager::FunctionTypeEntry details = cc.getTypeMan().getFunctionTypeDetails(funType);
	defCodePtr->addDependency(details.functorAndCaller);

	// create surrounding cast
	cStr << "((" << resDetails.functorName << "*)";

	// create struct including values
	cStr << "(&((" << details.functorName << ")";
	cStr << "{";

	// add function reference
	cStr << "&";
	visit(ptr->getLambda());

	 // TODO: add real size
	cStr << ", 0";

	// add captured parameters
	for_each(ptr->getValues(), [&, this](const ExpressionPtr& cur) {
		cStr << ", ";
		this->visit(cur);
	});

	cStr << "})))";
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

			// mark as a stack variable only if created using var.new => otherwise always a pointer (conservative)
			info.location = cc.getNodeManager().basic.isRefVar(function)?VariableManager::STACK:VariableManager::HEAP;

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
	cStr << cc.getTypeMan().getTypeName(defCodePtr, var->getType(), info.location == VariableManager::STACK) << " " << nameGen.getVarName(var);

	// check whether there is an initialization
	const ExpressionPtr& init = ptr->getInitialization();
	if (cc.basic.isUndefined(init)) {
		return;
	}

	// test whether it is a variable initialization using an undefined value
	ExpressionPtr varInit = CallExpr::get(cc.getNodeManager(),cc.basic.getRefAlpha(),
			cc.basic.getRefVar(), toVector<ExpressionPtr>(cc.basic.getUndefined()));

	if (*init == *varInit) {
		return;
	}

	// generate initializer expression
	cStr << " = ";
	visit(ptr->getInitialization());
}

void ConvertVisitor::visitLiteral(const LiteralPtr& ptr) {
	// just print literal
	cStr << ptr->getValue();
}

void ConvertVisitor::visitReturnStmt(const ReturnStmtPtr& ptr)
{
	cStr << "return ";
	visit(ptr->getReturnExpr());
	cStr << ";";
}

void ConvertVisitor::visitIfStmt(const IfStmtPtr& ptr) {
	cStr << "if(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(ptr->getThenBody());
	if (cc.basic.isNoOp(ptr->getElseBody())) {
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
	if (ptr->getType()->getNodeType() == NT_RefType
			&& cc.getVariableManager().getInfo(ptr).location == VariableManager::STACK) {

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
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType(), true) <<"){";
	for_each(ptr->getMembers(), [&](const StructExpr::Member& cur) {
		this->visit(cur.second);
		if(cur != ptr->getMembers().back()) cStr << ", ";
	});
	cStr << "})";
}

void ConvertVisitor::visitUnionExpr(const UnionExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType(), true) <<"){";
	visit(ptr->getMember());
	cStr << "})";
}

void ConvertVisitor::visitVectorExpr(const VectorExprPtr& ptr) {

	// handle single-element initializations
	if (ptr->getExpressions().size() == 1) {
		this->visit(ptr->getExpressions()[0]);
		return;
	}

	// test whether all expressions are calls to ref.var ...
	cStr << "{";
	for_each(ptr->getExpressions(), [&](const ExpressionPtr& cur) {
		if (!(cur->getNodeType() == NT_CallExpr && cc.basic.isRefVar(static_pointer_cast<const CallExpr>(cur)->getFunctionExpr()))) {
			assert(false && "Vector initialization not supported for the given values!");
		}
		this->visit(cur);
		if(cur != ptr->getExpressions().back()) cStr << ", ";
	});
	cStr << "}";


//	cStr << "&{";
//	cStr << join(", ", ptr->getExpressions(), [&, this](std::ostream&, const ExpressionPtr& cur) {
//		this->visit(cur);
//	});
//	cStr << "}";
}

void ConvertVisitor::visitMarkerExpr(const MarkerExprPtr& ptr) {
	// just ignore
	visit(ptr->getSubExpression());
}

void ConvertVisitor::visitMarkerStmt(const MarkerStmtPtr& ptr) {
	// just ignore
	visit(ptr->getSubStatement());
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

	ExpressionPtr evalLazy(const NodePtr& lazy) {

		NodeManager& manager = lazy->getNodeManager();

		ExpressionPtr exprPtr = dynamic_pointer_cast<const Expression>(lazy);
		assert(exprPtr && "Lazy is not an expression!");

		FunctionTypePtr funType = dynamic_pointer_cast<const FunctionType>(exprPtr->getType());
		assert(funType && "Illegal lazy type!");

		// form call expression
		CallExprPtr call = CallExpr::get(manager, funType->getReturnType(), exprPtr, toVector<ExpressionPtr>());
		return core::transform::tryInline(manager, call);
	}


	FormatTable initFormatTable(const lang::BasicGenerator& basic) {

		FormatTable res;

		#define OUT(Literal) cStr << Literal
		#define ARG(N) getArgument(call, N)
		#define VISIT_ARG(N) visitArgument(visitor, call, N)
		#define ADD_FORMATTER_DETAIL(Literal, Brackets, FORMAT) \
					res.insert(std::make_pair(Literal->getValue(), make_formatter([&basic](ConvertVisitor& visitor, CodeStream& cStr, const CallExprPtr& call) { \
						if (Brackets) OUT("("); \
						FORMAT \
						if (Brackets) OUT(")"); \
					} )))

		#define ADD_FORMATTER(Literal, FORMAT) \
					ADD_FORMATTER_DETAIL(Literal, true, FORMAT)

		ADD_FORMATTER(basic.getRefAssign(), {
				NodeManager& manager = visitor.getConversionContext().getNodeManager();
				ExpressionPtr target = static_pointer_cast<const Expression>(ARG(0));
				TypePtr valueType = static_pointer_cast<const RefType>(target->getType())->getElementType();
				visitor.visit(CallExpr::get(manager, valueType, basic.getRefDeref(), toVector<ExpressionPtr>(target)));
				OUT(" = ");
				VISIT_ARG(1);
		});

		// TODO: integrate those as well ...
//		ADD_FORMATTER(lang::OP_REF_VAR_PTR, { OUT(" var("); VISIT_ARG(0); OUT(")"); });
//		ADD_FORMATTER(lang::OP_REF_NEW_PTR, { OUT(" new("); VISIT_ARG(0); OUT(")"); });
//		ADD_FORMATTER(lang::OP_REF_DELETE_PTR, { OUT(" del("); VISIT_ARG(0); OUT(")"); });

		//ADD_FORMATTER(lang::OP_SUBSCRIPT_PTR, { VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getArray1DSubscript(), { VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); });
		ADD_FORMATTER(basic.getVectorSubscript(), { VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]"); });

		ADD_FORMATTER(basic.getRealAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getUIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getBoolAnd(), { VISIT_ARG(0); OUT("&&"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolOr(), { VISIT_ARG(0); OUT("||"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolNot(), { OUT("!"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getUIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getUIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getIntEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getIntLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getRealEq(), { VISIT_ARG(0); OUT("=="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealGe(), { VISIT_ARG(0); OUT(">="); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealGt(), { VISIT_ARG(0); OUT(">"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealLt(), { VISIT_ARG(0); OUT("<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getRealLe(), { VISIT_ARG(0); OUT("<="); VISIT_ARG(1); });

		//ADD_FORMATTER(basic.getVectorInitUniform(), { OUT("{"); VISIT_ARG(0); OUT("}"); });
		ADD_FORMATTER_DETAIL(basic.getVectorInitUniform(), false, { OUT("{}"); });
		ADD_FORMATTER_DETAIL(basic.getVectorInitUndefined(), false, { OUT("{}"); });


		ADD_FORMATTER(basic.getIfThenElse(), {
				OUT("("); VISIT_ARG(0); OUT(")?(");
				visitor.visit(evalLazy(ARG(1)));
				OUT("):(");
				visitor.visit(evalLazy(ARG(2)));
				OUT(")");
		});


		#undef ADD_FORMATTER
		#undef ADD_FORMATTER_DETAIL
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
	out << "#define null 0\n";

	for_each(code.getProgram()->getEntryPoints(), [&](const insieme::core::ExpressionPtr& ep) {
		out << "// --- Entry Point ---\n";
		out << (*code.find(ep)).second;
	});
	return out;
}

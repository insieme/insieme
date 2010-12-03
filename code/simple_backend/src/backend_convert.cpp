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

#include "insieme/utils/logging.h"

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

			// add operation
			cStr << "(*";
			visit(args.front());
			cStr << ")";

			return;
		} if(cc.basic.isVarlistPack(funExp)) {
			//DLOG(INFO) << cStr.getString();
			// if the arguments are a tuple expression, use the expressions within the tuple ...
			if (args.size() == 1) { // should actually be implicit if all checks are satisfied
				if (TupleExprPtr arguments = dynamic_pointer_cast<const TupleExpr>(args[0])) {
					// print elements of the tuple directly ...
					functionalJoin([&]{ this->cStr << ", "; }, arguments->getExpressions(), [&](const ExpressionPtr& ep) { this->visit(ep); });

					// in case there is no argument => print 0
					if (arguments->getExpressions().empty()) {
						this->cStr << "0";
					}

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


	switch(funExp->getNodeType()) {

		case NT_Literal: {
			cStr << cc.getFuncMan().getFunctionName(defCodePtr, static_pointer_cast<const Literal>(funExp));
			cStr << "(";
			functionalJoin([&]{ this->cStr << ", "; }, args, [&](const ExpressionPtr& ep) { this->visit(ep); });
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
		default:
			// default is a stack variable
			info.location = VariableManager::STACK;
			break;
		}
	}
	varManager.addInfo(var, info);


	// standard handling
	cStr << cc.getTypeMan().formatParamter(defCodePtr, var->getType(), nameGen.getVarName(var), true);
	//cStr << cc.getTypeMan().getTypeName(defCodePtr, var->getType(), true) << " " << nameGen.getVarName(var);

	// check whether there is an initialization
	const ExpressionPtr& init = ptr->getInitialization();
	if (core::analysis::isCallOf(init, cc.basic.getUndefined())) {
		return;
	}

	// test whether it is a variable initialization using an undefined value
	if (core::analysis::isCallOf(init, cc.basic.getRefVar()) &&
		core::analysis::isCallOf(static_pointer_cast<const CallExpr>(init)->getArguments()[0], cc.basic.getUndefined())) {
		return;
	}

	// generate initializer expression
	cStr << " = ";
	if (core::analysis::isCallOf(ptr->getInitialization(), cc.basic.getRefVar())) {
		// in case it is allocated on a stack, skip ref.var
		CallExprPtr call = static_pointer_cast<const CallExpr>(ptr->getInitialization());
		visit(call->getArguments()[0]);
	} else {
		visit(ptr->getInitialization());
	}
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

namespace {

	StatementPtr wrapBody(StatementPtr body) {
		if (body->getNodeCategory() == NC_Expression) {
			return CompoundStmt::get(body->getNodeManager(), toVector(body));
		}
		return body;
	}

}

void ConvertVisitor::visitForStmt(const ForStmtPtr& ptr) {
	auto decl = ptr->getDeclaration();
	auto var = decl->getVariable();
	string ident = cc.getNameGen().getVarName(var);
	cStr << "for(";
	visit(decl);
	cStr << "; " << ident << " < ";
	visit(ptr->getEnd());
	cStr << "; " << ident << " += ";
	visit(ptr->getStep());
	cStr << ") ";
	visit(wrapBody(ptr->getBody()));
}

void ConvertVisitor::visitIfStmt(const IfStmtPtr& ptr) {
	cStr << "if(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(wrapBody(ptr->getThenBody()));
	if (!cc.basic.isNoOp(ptr->getElseBody())) {
		cStr << " else ";
		visit(wrapBody(ptr->getElseBody()));
	}
}

void ConvertVisitor::visitWhileStmt(const WhileStmtPtr& ptr) {
	cStr << "while(";
	visit(ptr->getCondition());
	cStr << ") ";
	visit(wrapBody(ptr->getBody()));
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
	TypePtr type = ptr->getType();
	if (type->getNodeType() == NT_RefType &&
		static_pointer_cast<const RefType>(type)->getElementType()->getNodeType() != NT_VectorType) {

		cStr << "&";
	}
	cStr << "(";
	visit(ptr->getSubExpression());
	cStr << "." << ptr->getMemberName() << ")";
}

void ConvertVisitor::visitStructExpr(const StructExprPtr& ptr) {
	cStr << "((" << cc.getTypeMan().getTypeName(defCodePtr, ptr->getType(), true) <<"){";
	cStr << CodeStream::indR;
	for_each(ptr->getMembers(), [&](const StructExpr::Member& cur) {
		// skip ref.var if present
		if (core::analysis::isCallOf(cur.second, cc.basic.getRefVar())) {
			this->visit(static_pointer_cast<const CallExpr>(cur.second)->getArguments()[0]);
		} else {
			this->visit(cur.second);
		}
		if(cur != ptr->getMembers().back()) cStr << ",\n";
	});
	cStr << CodeStream::indL << "\n";
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
		if (!core::analysis::isCallOf(cur, cc.basic.getRefVar())) {
			DLOG(FATAL) << "Unsupported vector initialization: " << toString(*cur);
			assert(false && "Vector initialization not supported for the given values!");
		}
		// print argument of ref.var
		this->visit(static_pointer_cast<const CallExpr>(cur)->getArguments()[0]);
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

		void handleIncOperand(ConvertVisitor& visitor, const NodePtr& target) {

			assert(dynamic_pointer_cast<const Expression>(target) && "Operator must be an expression.");

			// check whether a deref is required
			CodeStream& stream = visitor.getCode()->getCodeStream();
			stream << "(*";
			visitor.visit(target);
			stream << ")";
		}

		void handleRefConstructor(ConvertVisitor& visitor, CodeStream& cStr, const NodePtr& initValue, bool isNew) {

			// check input parameters
			assert(dynamic_pointer_cast<const Expression>(initValue) && "Init Value is not an expression!");

			// quick check for arrays => extra handling
			const core::lang::BasicGenerator& basic = visitor.getConversionContext().basic;
			if (core::analysis::isCallOf(initValue, basic.getArrayCreate1D()) ||
			    core::analysis::isCallOf(initValue, basic.getArrayCreateND())) {

				// vector creation is sufficient
				visitor.visit(initValue);
				return;
			}


			// extract type
			TypePtr type = static_pointer_cast<const Expression>(initValue)->getType();
			string typeName = visitor.getConversionContext().getTypeMan().getTypeName(visitor.getCode(), type, true);

			// use stack or heap allocator
			string allocator = (isNew)?"malloc":"alloca";

			// special handling of some initialization values
			string stmt = toString(*initValue);

			// TODO: use pattern matching!

			// check for vector init undefined and undefined
			if (core::analysis::isCallOf(initValue, basic.getVectorInitUndefined()) || basic.isUndefined(initValue)) {
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
				if (core::analysis::isCallOf(param, basic.getRefVar())) {
					const NodePtr& refVar = static_pointer_cast<const CallExpr>(param)->getArguments()[0];
					if (LiteralPtr literal = dynamic_pointer_cast<const Literal>(refVar)) {
						if (literal->getValueAs<double>() == 0.0) {
							cStr << "calloc(sizeof(" << typeName << "), 1)";
							return;
						}
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
			visitor.visit(initValue);
			cStr << "), sizeof(";
			cStr << typeName;
			cStr << "))";
		}

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


		ADD_FORMATTER_DETAIL(basic.getRefVar(), false, { handleRefConstructor(visitor, cStr, ARG(0), false); });
		ADD_FORMATTER_DETAIL(basic.getRefNew(), false, { handleRefConstructor(visitor, cStr, ARG(0), true); });

		ADD_FORMATTER(basic.getRefDelete(), { OUT(" free(*"); VISIT_ARG(0); OUT(")"); });

		ADD_FORMATTER(basic.getArrayCreate1D(), {

				// ensure array is randomly initialized
				assert(core::analysis::isCallOf(ARG(0), basic.getRefVar()) && "Non-ref initalization of arrays not supported yet!" );
				ExpressionPtr initValue = static_pointer_cast<const CallExpr>(ARG(0))->getArguments()[0];
				assert(core::analysis::isCallOf(initValue, basic.getUndefined()) && "Initializing arrays with concrete values not supported yet.");

				// all arrays are allocated on the HEAP
				OUT("malloc(");
				OUT("sizeof(");
				TypePtr type = static_pointer_cast<const Expression>(ARG(0))->getType();
				OUT(visitor.getConversionContext().getTypeMan().getTypeName(visitor.getCode(), type, true));
				OUT(")*");
				VISIT_ARG(1);
				OUT(")");

		});

		ADD_FORMATTER_DETAIL(basic.getArraySubscript1D(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});


		ADD_FORMATTER_DETAIL(basic.getVectorSubscript(), false, {
				bool isRef = call->getType()->getNodeType() == NT_RefType;
				if (isRef) OUT("&(");
				VISIT_ARG(0); OUT("["); VISIT_ARG(1); OUT("]");
				if (isRef) OUT(")");
		});

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

		ADD_FORMATTER(basic.getUnsignedIntPreInc(), { OUT("++"); handleIncOperand(visitor, ARG(0)); });
		ADD_FORMATTER(basic.getUnsignedIntPostInc(), { handleIncOperand(visitor, ARG(0)); OUT("++"); });
		ADD_FORMATTER(basic.getUnsignedIntPreDec(), { OUT("--"); handleIncOperand(visitor, ARG(0)); });
		ADD_FORMATTER(basic.getUnsignedIntPostDec(), { handleIncOperand(visitor, ARG(0)); OUT("--"); });


		ADD_FORMATTER(basic.getSignedIntAdd(), { VISIT_ARG(0); OUT("+"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntSub(), { VISIT_ARG(0); OUT("-"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMul(), { VISIT_ARG(0); OUT("*"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntDiv(), { VISIT_ARG(0); OUT("/"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntMod(), { VISIT_ARG(0); OUT("%"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntLShift(), { VISIT_ARG(0); OUT("<<"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntRShift(), { VISIT_ARG(0); OUT(">>"); VISIT_ARG(1); });

		ADD_FORMATTER(basic.getSignedIntPreInc(), { OUT("++"); handleIncOperand(visitor, ARG(0)); });
		ADD_FORMATTER(basic.getSignedIntPostInc(), { handleIncOperand(visitor, ARG(0)); OUT("++"); });
		ADD_FORMATTER(basic.getSignedIntPreDec(), { OUT("--"); handleIncOperand(visitor, ARG(0)); });
		ADD_FORMATTER(basic.getSignedIntPostDec(), { handleIncOperand(visitor, ARG(0)); OUT("--"); });

		ADD_FORMATTER(basic.getSignedIntAnd(), { VISIT_ARG(0); OUT("&"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntOr(), { VISIT_ARG(0); OUT("|"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntXor(), { VISIT_ARG(0); OUT("^"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getSignedIntNot(), { OUT("~"); VISIT_ARG(0); });

		ADD_FORMATTER(basic.getBoolLAnd(), { VISIT_ARG(0); OUT("&&"); VISIT_ARG(1); });
		ADD_FORMATTER(basic.getBoolLOr(), { VISIT_ARG(0); OUT("||"); VISIT_ARG(1); });
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

		ADD_FORMATTER_DETAIL(basic.getSizeof(), false, {
				OUT("sizeof(");
				GenericTypePtr type = dynamic_pointer_cast<const GenericType>(
						static_pointer_cast<const Expression>(ARG(0))->getType()
				);
				assert(type && "Illegal argument to sizeof operator");
				TypePtr target = type->getTypeParameter()[0];
				OUT(visitor.getConversionContext().getTypeMan().getTypeName(visitor.getCode(), target, true));
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
	out << "#include <alloca.h>\n";
	out << "#include <stddef.h>\n";
	out << "#include <stdlib.h>\n";
	//out << "#include <string.h>\n";
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

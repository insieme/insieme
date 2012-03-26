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

#include "insieme/simple_backend/statement_converter.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/attributes.h"

#include "insieme/simple_backend/variable_manager.h"
#include "insieme/simple_backend/name_manager.h"
#include "insieme/simple_backend/type_manager.h"
#include "insieme/simple_backend/job_manager.h"
#include "insieme/simple_backend/function_manager.h"
#include "insieme/simple_backend/ir_extensions.h"

#include "insieme/simple_backend/utils/simple_backend_utils.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace simple_backend {

	using namespace core;

	void StmtConverter::convert(const NodePtr& node) {
		assert(currentCodeFragment);

		// process node
		visit(node);
	}


	void StmtConverter::convert(const NodePtr& node, const CodeFragmentPtr& fragment) {
		// replace target-code fragment
		CodeFragmentPtr current = currentCodeFragment;
		currentCodeFragment = fragment;

		// safe current local variable scope
		VariableManager::VarInfoMap backup = cc.getVariableManager().getVarInfoMap();

		// process subtree
		visit(node);

		// restore local scope
		cc.getVariableManager().setVarInfoMap(backup);

		// reset to old code fragment
		currentCodeFragment = current;
	}


	void StmtConverter::convertAsParameterToExternal(const core::ExpressionPtr& ep) {

		const CodeFragmentPtr& code = getCurrentCodeFragment();

		// obtain externalizing pattern
		string pattern = cc.getTypeManager().getTypeInfo(code, ep->getType()).externalizingPattern;

		// special handling for any ref-casts
		if (core::analysis::isCallOf(ep, ep->getNodeManager().getLangBasic().getAnyRefToRef())) {
			pattern = "%s";
		}

		// check pattern - is there some change necessary?
		if (pattern == "%s") {
			// no extra treatment required
			this->visit(ep);
			return;
		}

		// use a dummy code fragment to dump code
		CodeFragmentPtr fragment = CodeFragment::createNew("");
		this->convert(ep, fragment);
		code << format(pattern.c_str(), fragment->getCodeBuffer().toString().c_str());

		// propagate dependencies
		code->addDependencies(fragment->getDependencies());
	}

	void StmtConverter::convertAsParameterToExternal(const core::ExpressionPtr& expression, const CodeFragmentPtr& fragment) {
		// replace target-code fragment
		CodeFragmentPtr current = currentCodeFragment;
		currentCodeFragment = fragment;

		// process subtree
		convertAsParameterToExternal(expression);

		// reset to old code fragment
		currentCodeFragment = current;
	}

	vector<string> StmtConverter::getHeaderDefinitions() {
		vector<string> res;

		// add basic includes
		res.push_back("#include <alloca.h>");
		res.push_back("#include <stddef.h>");
		res.push_back("#include <stdio.h>");
		res.push_back("#include <stdlib.h>");
		res.push_back("#include <math.h>");

		// including this header will result into problems on a 32 bit system
		//  - reason: memset / memcpy uses size_t, which is fixed to 64 bit within insieme
		//  - conflicting type for strcpy operation
		//res.push_back("#include <string.h>");

		// add runtime header
		res.push_back("#include <runtime.h>");

		// add some macro definitions
		res.push_back("#define bool int");
		res.push_back("#define true 1");
		res.push_back("#define false 0");
		res.push_back("#define null 0");

		return res;
	}


	void StmtConverter::initStruct(const core::ExpressionPtr& target, const core::ExpressionPtr& init) {

		// obtain reference to lang basic
		const core::lang::BasicGenerator& basic = cc.getLangBasic();
		core::IRBuilder builder(cc.getNodeManager());

		// check whether an initializing is required
		if (analysis::isCallOf(init, basic.getUndefined())) {
			// that's it - no more work required
			return;
		}

		core::ExpressionPtr structInit = init;

		// ignore ref new / ref var
		if (target->getType()->getNodeType() == NT_RefType) {
			if (core::analysis::isCallOf(structInit, basic.getRefNew()) || core::analysis::isCallOf(structInit, basic.getRefVar())) {
				structInit = static_pointer_cast<const CallExpr>(structInit)->getArgument(0);
			}
		}

		// ensure init value is a struct
		assert(structInit->getNodeType() == NT_StructExpr && "Initialization is not of proper type!");
		const StructExprPtr& structValue = static_pointer_cast<const StructExpr>(structInit);

		// init values, one after another
		for_each(structValue->getMembers()->getElements(), [&, this](const NamedValuePtr& cur) {

			const StringValuePtr& name = cur->getName();
			const ExpressionPtr& value = cur->getValue();

			// remove leading var calls
//			if (core::analysis::isCallOf(value, cc.getLangBasic().getRefVar())) {
//				value = static_pointer_cast<const CallExpr>(value)->getArgument(0);
//				if (core::analysis::isCallOf(value, cc.getLangBasic().getRefDeref())) {
//					value = static_pointer_cast<const CallExpr>(value)->getArgument(0);
//				}
//			}

			// remove leading var/new calls
//			if (analysis::isCallOf(value, basic.getRefNew()) || analysis::isCallOf(value, basic.getRefVar())) {
//				value = static_pointer_cast<const CallExpr>(value)->getArgument(0);
//			}

			// skip vector initialization
			if (analysis::isCallOf(value, basic.getVectorInitUniform()) || analysis::isCallOf(value, basic.getVectorInitUndefined())) {
				// TODO: support init uniform
				return;
			}

			// start new line .. initialization of a member is required
			currentCodeFragment << ";\n";

			// create assignment statement
			auto assignmentTarget = builder.callExpr(basic.getCompositeRefElem(), target,
					builder.getIdentifierLiteral(name), builder.getTypeLiteral(value->getType()));
			this->visit(builder.callExpr(basic.getRefAssign(), assignmentTarget, value));
		});

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

		/**
		 * This function is wrapping the given statement into a compound statement in case
		 * it is a single expression.
		 *
		 * @param body the statement to be wrapped
		 * @return the given statement or a compound statement in case the statement is an expression
		 */
		StatementPtr wrapBody(StatementPtr body) {
			if (body->getNodeType() != NT_CompoundStmt) {
				return CompoundStmt::get(body->getNodeManager(), toVector(body));
			}
			return body;
		}

	}

	// --------------------------------------------------------------------------------
	// -- node processing -------------------------------------------------------------
	// --------------------------------------------------------------------------------

	void StmtConverter::visitNode(const NodePtr& node) {
		getCurrentCodeFragment() << "<?>" << toString(*node) << "</?>";
	}


	void StmtConverter::visitProgram(const ProgramPtr& program) {

		// TODO: remove second clause when frontend is fixed ...

		// check whether program is a main program
//		if (program->isMain() || utils::isMainProgram(program)) {
//
//			// create main program + argument wrapper (if necessary)
//			CodeFragmentPtr code = CodeFragment::createNew("main function");
//
//			// add argument conversion
//			LambdaExprPtr main = static_pointer_cast<const LambdaExpr>(program->getEntryPoints()[0]);
//			if (main->getParameterList().empty()) {
//
//				// handle main with no arguments
//				code << "int main() {" << CodeBuffer::indR << "\n";
//				convert(main->getBody(), code);
//				code << CodeBuffer::indL << ";\n}\n";
//
//				// make current code fragment depending on the main function
//				getCurrentCodeFragment()->addDependency(code);
//				return;
//			}
//
//			// create procedure header
//			code << "int main(int __argc, char** __argv) {" << CodeBuffer::indR << "\n";
//			code << "\n// encapsulating arguments within Insieme Types ...\n";
//
//			// declare parameter
//			const VariablePtr& argc = main->getParameterList()[0];
//			const VariablePtr& argv = main->getParameterList()[1];
//
//			VariableManager::VariableInfo info;
//			info.location = VariableManager::STACK;
//			cc.getVariableManager().addInfo(argc, info);
//			cc.getVariableManager().addInfo(argv, info);
//
//			cc.getNameManager().setName(argc, "argc");
//			cc.getNameManager().setName(argv, "argv");
//
//			const RefTypePtr argvType = static_pointer_cast<const RefType>(main->getParameterList()[1]->getType());
//			const ArrayTypePtr aaCharType = static_pointer_cast<const ArrayType>(argvType->getElementType());
//			const ArrayTypePtr aCharType = static_pointer_cast<const ArrayType>(aaCharType->getElementType());
//
//			string charArrayArrayName = cc.getTypeManager().getTypeName(code, aaCharType, true);
//			string charArrayName = cc.getTypeManager().getTypeName(code, aCharType, true);
//
//			// check whether the arrays length should be supported
//			bool useSize = cc.isSupportArrayLength();
//
//			code << "int argc = __argc;\n";
//			code << charArrayArrayName << " argv = (" << charArrayArrayName << "){alloca(sizeof(" << charArrayName << ") * argc)" << ((useSize)?", {argc}":"") << "};\n";
//
//			// create a literal for the strlen function
//			IRBuilder builder(cc.getNodeManager());
//			FunctionTypePtr strLenType = builder.functionType(
//					toVector<TypePtr>(builder.refType(aCharType)),
//					builder.getLangBasic().getUInt8());
//
//			if (useSize) {
//				LiteralPtr strlen = builder.literal(strLenType, "strlen");
//				string strlenName = cc.getFunctionManager().getFunctionName(code, strlen);
//
//				// initialize argument vector data structure
//				code << "for(int i=0; i<argc; ++i) {" << CodeBuffer::indR << "\n";
//				code << "argv.data[i] = (" << charArrayName << "){__argv[i],{" << strlenName << "(__argv[i])+1}};" << CodeBuffer::indL;
//				code << "\n}\n";
//			} else {
//				// initialize argument vector data structure
//				code << "for(int i=0; i<argc; ++i) {" << CodeBuffer::indR << "\n";
//				code << "argv.data[i] = (" << charArrayName << "){__argv[i]};" << CodeBuffer::indL;
//				code << "\n}\n";
//			}
//
//
//			// add body
//			code << "\n// ---- begin of actual code body ----\n";
//
//			convert(main->getBody(), code);
//
//			code << "\n// ----  end of actual code body  ----\n";
//
//			// complete procedure
//			code << CodeBuffer::indL << "\n";
//			code << "}\n\n";
//
//			// make current code fragment depending on the main function
//			getCurrentCodeFragment()->addDependency(code);
//
//		} else {

			// TODO: add wrapper for external access

			// handle individual entry points
			for_each(program->getEntryPoints(), [&](const ExpressionPtr& cur) {
				// add entry point
				this->convert(cur);
			});
//		}
	}

	////////////////////////////////////////////////////////////////////////// Statements

	void StmtConverter::visitBreakStmt(const BreakStmtPtr&) {
		getCurrentCodeFragment() << "break";
	}

	void StmtConverter::visitContinueStmt(const ContinueStmtPtr&) {
		getCurrentCodeFragment() << "continue";
	}

	void StmtConverter::visitDeclarationStmt(const DeclarationStmtPtr& ptr) {
		auto var = ptr->getVariable();

		// investigate initialization to determine whether variable is a pointer / scalar
		VariableManager& varManager = cc.getVariableManager();
		VariableManager::VariableInfo info;
		info.location = VariableManager::NONE;
		bool isAllocatedOnHEAP = false;
		if (var->getType()->getNodeType() == NT_RefType) {
			ExpressionPtr initialization = ptr->getInitialization();
			switch (initialization->getNodeType()) {
			case NT_Variable:
				info = varManager.getInfo(static_pointer_cast<const Variable>(initialization));
				isAllocatedOnHEAP = (info.location == VariableManager::HEAP);
				break;
			case NT_CallExpr:
				if (analysis::isCallOf(initialization, cc.getLangBasic().getRefVar())) {
					info.location = VariableManager::STACK;
				} else {
					info.location = VariableManager::HEAP;
					isAllocatedOnHEAP = true;
				}

				break;
			default:
				// default is a stack variable
				info.location = VariableManager::STACK;
				break;
			}
		}
		varManager.addInfo(var, info);


		// standard handling
		const CodeFragmentPtr& code = currentCodeFragment;
		string varName = cc.getNameManager().getName(var);
		code << cc.getTypeManager().formatParamter(code, var->getType(), varName, !isAllocatedOnHEAP);

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
		code << " = ";

		const RefTypePtr& refType = (isAllocatedOnHEAP)?static_pointer_cast<const RefType>(var->getType()):RefTypePtr(NULL);
		if (refType && refType->getElementType()->getNodeType() == NT_StructType) {

			IRBuilder builder(cc.getNodeManager());
			const lang::BasicGenerator& basic = cc.getNodeManager().getLangBasic();

			// start by allocating the required memory
			const StructTypePtr structType = static_pointer_cast<const StructType>(refType->getElementType());
			visit(builder.callExpr(basic.getRefNew(), builder.callExpr(basic.getUndefined(), builder.getTypeLiteral(structType))));

			// initialize all the members
			initStruct(var, ptr->getInitialization());

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

	void StmtConverter::visitForStmt(const ForStmtPtr& ptr) {
		IRBuilder builder(cc.getNodeManager());
		auto var = ptr->getIterator();
		auto decl = builder.declarationStmt(var, ptr->getStart());

		const CodeFragmentPtr& code = currentCodeFragment;

		string ident = cc.getNameManager().getName(var);
		code << "for(";
		visit(decl);
		code << "; " << ident << " < ";
		visit(ptr->getEnd());
		code << "; " << ident << " += ";
		visit(ptr->getStep());
		code << ") ";
		visit(wrapBody(ptr->getBody()));
	}

	void StmtConverter::visitIfStmt(const IfStmtPtr& ptr) {
		IRBuilder builder(cc.getNodeManager());
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "if(";
		visit(ptr->getCondition());
		code << ")";
		visit(wrapBody(ptr->getThenBody()));
		if (!builder.isNoOp(ptr->getElseBody())) {
			code << " else ";
			visit(wrapBody(ptr->getElseBody()));
		}
	}

	void StmtConverter::visitWhileStmt(const WhileStmtPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "while(";
		visit(ptr->getCondition());
		code << ") ";
		visit(wrapBody(ptr->getBody()));
	}

	void StmtConverter::visitSwitchStmt(const SwitchStmtPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "switch(";
		visit(ptr->getSwitchExpr());
		code << ") {\n";
		for_each(ptr->getCases()->getElements(), [&](const SwitchCasePtr& curCase) {
			code << "case ";
			this->visit(curCase->getGuard());
			code << ":" << CodeBuffer::indR << "\n";
			this->visit(curCase->getBody());
			code << "; break;" << CodeBuffer::indL << "\n";
		});
		code << "}";
	}

	void StmtConverter::visitCompoundStmt(const CompoundStmtPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		if (ptr->getStatements().size() ==1) {
			code << " { ";
			this->visit(ptr->getStatements()[0]);
			code << "; }";
		} else if(ptr->getStatements().size() > 1) {
			code << "{" << CodeBuffer::indR << "\n";
			for_each(ptr->getChildList(), [&](const NodePtr& cur) {
				this->visit(cur);
				code << ";";
				if(cur != ptr->getChildList().back()) code << "\n";
			});
			code << CodeBuffer::indL << "\n}";
		} else {
			code << "{}";
		}
	}

	////////////////////////////////////////////////////////////////////////// Expressions


	void StmtConverter::visitJobExpr(const JobExprPtr& ptr) {
		// just use job manager
		cc.getJobManager().createJob(getCurrentCodeFragment(), ptr);
	}


	void StmtConverter::visitLambdaExpr(const LambdaExprPtr& ptr) {
		FunctionManager& funManager = cc.getFunctionManager();

		CodeFragmentPtr& code = currentCodeFragment;

		// obtain name of resulting function type and add cast
		FunctionTypePtr funType = static_pointer_cast<const FunctionType>(ptr->getType());
		const TypeManager::FunctionTypeInfo& info = cc.getTypeManager().getFunctionTypeInfo(funType);
		const string& name = info.closureName;
		code << "(" << name << "*)" << name << "_ctr(";

		// allocate memory
		code << "(" << name << "*)alloca(sizeof(" + name + ")),";
		code << "&" << funManager.getFunctionName(currentCodeFragment, ptr);
		code << "_wrap)";
	}

	namespace {

		void addArgumentList(StmtConverter& converter, const CodeFragmentPtr& code,
				const core::TypeList& parameterTypes, const core::ExpressionList& arguments, bool externCall) {

			// check same number of arguments - TODO: re-enable when frontend is fixed
			// assert(parameterTypes.size() == arguments.size() && "Invalid parameter/argument combination!");

			// TODO: remove this
			if (parameterTypes.size() != arguments.size()) {
				// default handling
				functionalJoin([&]{ code << ", "; }, arguments, [&](const ExpressionPtr& ep) { converter.convert(ep, code); });
				return;
			}

			// TODO: implicit conversion between vector / array (also within references)
			// TODO: externalizing when calling external function

			// OLD VERSION:
			functionalJoin([&]{ code << ", "; }, arguments, [&](const ExpressionPtr& ep) { converter.convert(ep, code); });
		}

	}


	void StmtConverter::visitCallExpr(const CallExprPtr& ptr) {

		// shorter name for member variable
		const CodeFragmentPtr& code = currentCodeFragment;

		const std::vector<ExpressionPtr>& args = ptr->getArguments();
		auto funExp = core::analysis::stripAttributes(ptr->getFunctionExpr());

		FunctionTypePtr funType = static_pointer_cast<const FunctionType>(funExp->getType());
		const TypeList& params = funType->getParameterTypes()->getElements();

		// create a lambda capable of externalizing arguments
		auto parameterExternalizer = [&](const ExpressionPtr& ep) {
			// use member function
			convertAsParameterToExternal(ep);
		};

		// special built in function handling
		if(auto literalFun = dynamic_pointer_cast<const Literal>(funExp)) {

			// special handling for var-list handling
			if(cc.getLangBasic().isVarlistPack(funExp)) {
				// if the arguments are a tuple expression, use the expressions within the tuple ...
				if (args.size() == 1) { // should actually be implicit if all checks are satisfied
					if (TupleExprPtr arguments = dynamic_pointer_cast<const TupleExpr>(args[0])) {
						// print elements of the tuple directly ...
						functionalJoin([&]{ code << ", "; }, arguments->getExpressions()->getElements(), parameterExternalizer);

						// in case there is no argument => print 0
						if (arguments->getExpressions().empty()) {
							code << "0";
						}

						return;
					}
				}

				functionalJoin([&]{ code << ", "; }, args, parameterExternalizer);
				return;
			}

			// try generic build-in C operator handling
			auto pos = formats.find(literalFun);
			if (pos != formats.end()) {
				pos->second->format(*this, ptr);
				return;
			}
		}

		// handle function based on the kind of function node
		switch(funExp->getNodeType()) {

			case NT_Literal: {

				// TODO: internalize results using general mechanism
//				bool internalize = false;
//				TypePtr returnType = funType->getReturnType();
//				if (returnType->getNodeType() == NT_RefType) {
//					TypePtr elementType = static_pointer_cast<const RefType>(returnType)->getElementType();
//					if (elementType->getNodeType() == NT_ArrayType) {
//						internalize = true;
//						// add conversion
//						code << "&((" << cc.getTypeManager().getTypeName(code, elementType) << "){";
//					}
//				}

				code << cc.getFunctionManager().getFunctionName(code, static_pointer_cast<const Literal>(funExp));
				code << "(";

				// externalize (convert to C equivalents) arguments before passing them to the call
				//addArgumentList(*this, code, params, args, true);
				functionalJoin([&]{ code << ", "; }, args, parameterExternalizer);
				code << ")";

//				if (internalize) {
//					code << "})";
//				}
				return;
			}

			case NT_Variable:
			{
				visit(funExp);
				code << "->fun";
				code << "(";
				visit(funExp);
				if (!args.empty()) {
					code << ", ";
					addArgumentList(*this, code, params, args, false);
				}
				code << ")";
				return;
			}

			case NT_CallExpr:
			{

				TypeManager::FunctionTypeInfo details = cc.getTypeManager().getFunctionTypeInfo(funType);
				code->addDependency(details.definitions);

				// use call wrapper
				code << details.callerName;
				code << "(";
				visit(funExp);
				if (!args.empty()) {
					code << ", ";
					addArgumentList(*this, code, params, args, false);
				}
				code << ")";
				return;
			}

			case NT_LambdaExpr: {

				// function (without capture list) is directly provided => simply invoke
				code << cc.getFunctionManager().getFunctionName(code, static_pointer_cast<const LambdaExpr>(funExp));
				code << "(";
				addArgumentList(*this, code, params, args, false);
				code << ")";
				return;
			}

			case NT_BindExpr: {


				// resolve function type => should add caller instructions
				const TypeManager::FunctionTypeInfo& info = cc.getTypeManager().getFunctionTypeInfo(funType);
				code->addDependency(info.definitions);

				// use caller to evaluate bind expression
				code << info.callerName << "(";
				visit(funExp);
				if (!args.empty()) {
					code << ", ";
					addArgumentList(*this, code, params, args, false);
				}
				code << ")";
				return;
			}

			default :
				code << "<?>Unhandled Type of Call Target</?>";
		}

	}


	void StmtConverter::visitBindExpr(const core::BindExprPtr& ptr) {
		// use function manager to add bind construction
		cc.getFunctionManager().createClosure(currentCodeFragment, ptr);
	}


	void StmtConverter::visitLiteral(const LiteralPtr& ptr) {

		// special handling for the global struct
		if (ptr->getStringValue() == IRExtensions::GLOBAL_ID) {
			if (ptr->getType()->getNodeType() == NT_RefType) {
				currentCodeFragment << "&";
			}

			// add code dependency to global struct
			CodeFragmentPtr globals = cc.getVariableManager().getGlobalVarFragment();
			currentCodeFragment->addDependency(globals);
		}

		// special handling for type literals
		if (core::analysis::isTypeLiteralType(ptr->getType())) {
			currentCodeFragment << "0";
			return;
		}

		// just print the value represented by the literal
		currentCodeFragment << ptr->getStringValue();
	}

	void StmtConverter::visitReturnStmt(const ReturnStmtPtr& ptr) {
		if (cc.getNodeManager().getLangBasic().isUnit(ptr->getReturnExpr()->getType())) {
			currentCodeFragment << "return";
			return;
		}

		currentCodeFragment << "return ";
		visit(ptr->getReturnExpr());
	}


	void StmtConverter::visitCastExpr(const CastExprPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "((" << cc.getTypeManager().getTypeName(code, ptr->getType(),true) << ")(";
		visit(ptr->getSubExpression());
		code << "))";
	}

	void StmtConverter::visitVariable(const VariablePtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		bool deref = true;
		if (dynamic_pointer_cast<const RefType>(ptr->getType())) {
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

		code << ((deref)?"&":"") << cc.getNameManager().getName(ptr);
	}

	void StmtConverter::visitStructExpr(const StructExprPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "((" << cc.getTypeManager().getTypeName(code, ptr->getType(), true) <<"){";
		code << CodeBuffer::indR;
		for_each(ptr->getMembers()->getElements(), [&](const NamedValuePtr& cur) {
			// skip ref.var if present
			if (core::analysis::isCallOf(cur->getValue(), cc.getLangBasic().getRefVar())) {
				core::ExpressionPtr arg = static_pointer_cast<const CallExpr>(cur->getValue())->getArgument(0);
				if (core::analysis::isCallOf(arg, cc.getLangBasic().getRefDeref())) {
					arg = static_pointer_cast<const CallExpr>(arg)->getArgument(0);
				}
				this->visit(arg);
			} else {
				this->visit(cur->getValue());
			}
			if(cur != ptr->getMembers()->getElements().back()) code << ",\n";
		});
		code << CodeBuffer::indL << "\n";
		code << "})";
	}

	void StmtConverter::visitUnionExpr(const UnionExprPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		code << "((" << cc.getTypeManager().getTypeName(code, ptr->getType(), true) <<"){";
		visit(ptr->getMember());
		code << "})";
	}

	void StmtConverter::visitVectorExpr(const VectorExprPtr& ptr) {
		const CodeFragmentPtr& code = currentCodeFragment;

		// handle single-element initializations
		if (ptr->getExpressions().size() == 1) {
			this->visit(ptr->getExpressions()[0]);
			return;
		}

		// get the type of the resulting vector
		string typeName = getConversionContext().getTypeManager().getTypeName(code, ptr->getType());

		// test whether all expressions are calls to ref.var ...
		code << "((" << typeName << "){{";
		int i=0;
		for_each(ptr->getExpressions()->getElements(), [&](const ExpressionPtr& cur) {
//			if (!core::analysis::isCallOf(cur, cc.getLangBasic().getRefVar())) {
//				LOG(FATAL) << "Unsupported vector initialization: " << toString(*cur);
//				assert(false && "Vector initialization not supported for the given values!");
//			}
			// print argument of ref.var
//			this->visit(static_pointer_cast<const CallExpr>(cur)->getArguments()[0]);
			this->visit(cur);
			if((++i)!=ptr->getExpressions().size()) code << ", ";
		});
		code << "}})";
	}

	void StmtConverter::visitMarkerExpr(const MarkerExprPtr& ptr) {
		// just ignore
		visit(ptr->getSubExpression());
	}

	void StmtConverter::visitMarkerStmt(const MarkerStmtPtr& ptr) {
		// just ignore
		visit(ptr->getSubStatement());
	}


} // end namespace simple_backend
} // end namespace insieme

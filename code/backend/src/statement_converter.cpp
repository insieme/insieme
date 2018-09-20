/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include "insieme/backend/statement_converter.h"

#include <boost/algorithm/string.hpp>

#include "insieme/backend/function_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"

#include "insieme/backend/backend_config.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/variable_manager.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"

#include "insieme/annotations/c/extern.h"
#include "insieme/annotations/c/include.h"

#include "insieme/utils/logging.h"

namespace insieme {
namespace backend {

	// --------------- Conversion Context struct ---------------

	namespace {

		c_ast::CCodeFragmentPtr toCodeFragment(const ConversionContext& context, c_ast::NodePtr code) {
			c_ast::CCodeFragmentPtr fragment = c_ast::CCodeFragment::createNew(context.getConverter().getFragmentManager(), code);
			fragment->addDependencies(context.getDependencies());
			return fragment;
		}

		c_ast::ExpressionPtr visitInitExprInternal(const Converter& converter, const core::InitExprPtr& ptr, ConversionContext& context) {
			// to be created: an initialization of the corresponding struct
			//     (<type>){<list of members>}

			auto addRef = [&](const c_ast::ExpressionPtr& res) {
				return (core::lang::isPlainReference(ptr)) ? c_ast::ref(res) : res;
			};

			auto innerType = core::analysis::getReferencedType(ptr->getType());
			auto typeInfo = converter.getTypeManager().getTypeInfo(context, innerType);
			context.addDependency(typeInfo.definition);

			auto& rExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

			// get type
			c_ast::TypePtr type = typeInfo.rValueType;
			// omit type if we are initializing memory allocated by ref_decl or from a global (it is implicit, and we need to prevent temporary construction)
			if(rExt.isCallOfRefDecl(ptr->getMemoryExpr()) || ptr->getMemoryExpr().isa<core::LiteralPtr>()) {
				type = nullptr;
			}

			// create init expression
			c_ast::InitializerPtr init = converter.getCNodeManager()->create<c_ast::Initializer>(type);

			// append initialization values
			::transform(ptr->getInitDecls()->getDeclarations(), std::back_inserter(init->values), [&](const core::DeclarationPtr& decl) {

				auto conv = converter.getStmtConverter().convertExpression(context, decl->getInitialization());
				// disable explicit types for nested init values since those are inferred automatically
				if(auto nestedInit = conv.isa<c_ast::InitializerPtr>()) {
					nestedInit->type = nullptr;
				}
				// if we are initializing with a declaration, which references part of our own memory here, we add a deref
				if (core::analysis::getRefDeclCall(decl)) {
					conv = c_ast::deref(conv);
				}
				return conv;
			});

			// support empty initialization
			if(init->values.empty()) { return addRef(init); }

			// remove last element if it is a variable sized struct
			if(core::lang::isUnknownSizedArray(ptr->getInitExprList().back())) {
				assert_false(init->values.empty());
				init->values.pop_back();
			}

			// if array, initialize inner data member
			if(core::lang::isFixedSizedArray(innerType)) {
				init = converter.getCNodeManager()->create<c_ast::Initializer>(type, toVector<c_ast::NodePtr>(c_ast::init(init->values)));
			}

			// on the other hand, if this is an intercepted type and we initialize it with a single fixed size array, we must not initialize the inner data member
			if(insieme::annotations::c::hasIncludeAttached(innerType) && ptr->getInitExprList().size() == 1) {
				auto inner = ptr->getInitExprList()[0].isa<core::InitExprPtr>();
				if(inner && core::lang::isFixedSizedArray(core::analysis::getReferencedType(inner))) {
					auto innerInit = converter.getStmtConverter().convertExpression(context, inner).as<c_ast::UnaryOperationPtr>()->operand.as<c_ast::InitializerPtr>();
					init->values[0] = innerInit->values[0];
				}
			}

			// return completed
			return addRef(init);
		}

		void convertGlobalInit(ConversionContext& context, const core::LiteralPtr& lit, const core::ExpressionPtr& initExpr) {
			// generate fragment for global if it doesn't exist yet
			context.getConverter().getStmtConverter().convertExpression(context, lit);

			auto& converter = context.getConverter();
			auto fragmentManager = converter.getFragmentManager();
			string fragmentName = "global:" + lit->getStringValue();
			auto fragment = fragmentManager->getFragment(fragmentName);
			assert_true(fragment) << "Global Literal fragment not generated";
			auto cFragment = fragment.as<c_ast::CCodeFragmentPtr>();
			auto decl = cFragment->getCode()[1].as<c_ast::GlobalVarDeclPtr>();
			assert_true(decl) << "Global Literal has no decl";

			ConversionContext innerContext(converter, context.getEntryPoint());
			auto irInitExpr = initExpr.isa<core::InitExprPtr>();
			if(irInitExpr) { // it's either an init expr, or...
				decl->init = c_ast::deref(visitInitExprInternal(innerContext.getConverter(), irInitExpr, innerContext));
			} else { // it has to be a constructor call
				assert_true(core::analysis::isConstructorCall(initExpr));
				// change constructor mem loc to ref temp and translate normally
				auto constrCall = initExpr.as<core::CallExprPtr>();
				auto args = constrCall.getArgumentList();
				args[0] = core::lang::buildRefTemp(args[0].getType());
				core::IRBuilder builder(converter.getNodeManager());
				auto adjustedCall = builder.callExpr(constrCall.getType(), constrCall.getFunctionExpr(), args);
				decl->init = c_ast::deref(converter.getStmtConverter().convertExpression(innerContext, adjustedCall));
			}

			// move dependencies to global var-decl fragment
			fragment->addDependencies(innerContext.getDependencies());
			fragment->addRequirements(innerContext.getRequirements());
			fragment->addIncludes(innerContext.getIncludes());

			context.addDependency(fragment);
		}
	}


	// --------------- conversion operations -------------------

	c_ast::NodePtr StmtConverter::convert(ConversionContext& context, const core::NodePtr& node) {
		// create a context for the conversion and conduct procedure
		return visit(node, context);
	}


	////////////////////////////////////////////////////////////////////////// Core Visitor

	c_ast::NodePtr StmtConverter::visit(const core::NodePtr& node, ConversionContext& context) {

		// first ask the handlers
		for(auto cur : stmtHandler) {
			c_ast::NodePtr res = cur(context, node);
			if(res) { return res; }
		}

		// use default conversion
		return IRVisitor::visit(node, context);
	}


	////////////////////////////////////////////////////////////////////////// Basic Nodes

	c_ast::NodePtr StmtConverter::visitNode(const core::NodePtr& node, ConversionContext& context) {
		// default handling of unsupported nodes => produce comment
		return converter.getCNodeManager()->create<c_ast::Comment>("Unsupported: " + toString(node));
	}

	c_ast::NodePtr StmtConverter::visitType(const core::TypePtr& type, ConversionContext& context) {
		// obtain type information, add dependency and return type name
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(context, type);
		context.getDependencies().insert(info.definition);
		return info.rValueType;
	}


	c_ast::NodePtr StmtConverter::visitProgram(const core::ProgramPtr& node, ConversionContext& context) {
		// get shared C Node Manager reference
		const c_ast::SharedCNodeManager& manager = converter.getCNodeManager();
		// program is not producing any C code => just dependencies
		for_each(node->getEntryPoints(), [&](const core::ExpressionPtr& entryPoint) {

			// fix name of main entry
			if(node->getEntryPoints().size() == static_cast<std::size_t>(1)) {
				context.getConverter().getNameManager().setName(entryPoint, converter.getBackendConfig().mainFunctionName);
			}

			// create a new context
			ConversionContext entryContext(converter, core::LambdaPtr());

			c_ast::CodeFragmentPtr fragment;
			if(entryPoint->getNodeType() == core::NT_LambdaExpr) {
				// handle function-entry point specially
				core::LambdaExprPtr lambda = static_pointer_cast<const core::LambdaExpr>(entryPoint);
				fragment = converter.getFunctionManager().getInfo(context, lambda).definition;
			} else {
				// use default conversion
				fragment = toCodeFragment(entryContext, this->visit(entryPoint, entryContext));
			}

			// add converted fragment to dependency list
			context.getDependencies().insert(fragment);

		});

		// create empty node (program does not represent any code)
		return manager->create<c_ast::OpaqueCode>("");
	}


	////////////////////////////////////////////////////////////////////////// Expressions

	c_ast::NodePtr StmtConverter::visitCallExpr(const core::CallExprPtr& ptr, ConversionContext& context) {
		// special handling for global variable initialization
		if(core::analysis::isConstructorCall(ptr) && core::analysis::getArgument(ptr, 0).isa<core::LiteralPtr>()) {
			convertGlobalInit(context, core::analysis::getArgument(ptr, 0).as<core::LiteralPtr>(), ptr);
			return {};
		}
		// everything else is handled by the function manager
		auto cCall = converter.getFunctionManager().getCall(context, ptr);
		// if materializing, transition to lvalue
		if(core::analysis::isMaterializingCall(ptr)) {
			return c_ast::ref(cCall.as<c_ast::ExpressionPtr>());
		}
		return cCall;
	}

	c_ast::NodePtr StmtConverter::visitBindExpr(const core::BindExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(context, ptr);
	}

	c_ast::NodePtr StmtConverter::visitCastExpr(const core::CastExprPtr& ptr, ConversionContext& context) {
		const auto& info = converter.getTypeManager().getTypeInfo(context, ptr->getType());
		context.addDependency(info.definition);
		return c_ast::cast(info.rValueType, visit(ptr->getSubExpression(), context));
	}

	c_ast::NodePtr StmtConverter::visitJobExpr(const core::JobExprPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Literal>("JOB-UNSUPPORTED");
	}

	c_ast::NodePtr StmtConverter::visitLambdaExpr(const core::LambdaExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(context, ptr);
	}

	c_ast::NodePtr StmtConverter::visitLiteral(const core::LiteralPtr& ptr, ConversionContext& context) {
		// Function literals are handled by function manager
		if(ptr->getType()->getNodeType() == core::NT_FunctionType) { return converter.getFunctionManager().getValue(context, ptr); }

		// special handling for unit literal
		if(converter.getNodeManager().getLangBasic().isUnitConstant(ptr)) {
			return c_ast::cast(converter.getCNodeManager()->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Void),
			                   converter.getCNodeManager()->create<c_ast::Literal>("0"));
		}

		// convert literal
		auto toLiteral = [&](const string& value) { return converter.getCNodeManager()->create<c_ast::Literal>(value); };
		std::string name = ptr->getStringValue();
		if(core::annotations::hasAttachedName(ptr)) name = core::annotations::getAttachedName(ptr);
		c_ast::ExpressionPtr res = toLiteral(name);

		// handle primitive types
		auto type = ptr->getType();
		auto& basic = ptr->getNodeManager().getLangBasic();
		if(basic.isPrimitive(type)) {
			// handle special cases
			const string& value = ptr->getStringValue();

			// things that need not be extra-casted (default values)
			if(basic.isInt4(type) || basic.isIntInf(type) || basic.isUIntInf(type)) { return res; }

			// add an 'f' suffix in case it is a float literal and it is missing, fix integral float literals
			if(basic.isReal4(type)) {
				string literalValue = value;
				if(!boost::contains(literalValue, ".")) literalValue += ".0";
				if(*literalValue.rbegin() != 'f') literalValue += 'f';
				return toLiteral(literalValue);

				// fix integral double literals
			} else if(basic.isReal8(type)) {
				string literalValue = value;
				if(!boost::contains(literalValue, ".")) literalValue += ".0";
				return toLiteral(literalValue);
			}

			// add the correct suffixes if they are missing
			if(basic.isUInt4(type) && *value.rbegin() != 'u') return toLiteral(value + "u");
			if(basic.isInt8(type) && *value.rbegin() != 'l') return toLiteral(value + "l");
			if(basic.isUInt8(type) && !boost::ends_with(value, "ul")) return toLiteral(value + "ul");
			if(basic.isInt16(type) && !boost::ends_with(value, "ll")) return toLiteral(value + "ll");
			if(basic.isUInt16(type) && !boost::ends_with(value, "ull")) return toLiteral(value + "ull");

			// fall-back solution: use an explicit cast
			auto info = converter.getTypeManager().getTypeInfo(context, type);
			context.addDependency(info.definition);
			return c_ast::cast(info.rValueType, res);
		}

		// special handling for type literals (fall-back solution)
		if(core::analysis::isTypeLiteralType(type)) {
			const TypeInfo& info = converter.getTypeManager().getTypeInfo(context, type);
			context.addDependency(info.declaration);
			return c_ast::lit(info.rValueType, "type_token");
		}

		// special handling for boolean literals
		if(ptr.getNodeManager().getLangBasic().isBool(type)) { context.getIncludes().insert("stdbool.h"); }

		// handle null pointer
		if(converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>().isCallOfRefNull(ptr)) {
			return converter.getCNodeManager()->create<c_ast::Literal>("0");
		}

		// handle pre-defined C identifiers (standard - 6.4.2.2)
		const static vector<string> predefined = {"__func__", "__FUNCTION__", "__PRETTY_FUNCTION__"};
		if(contains(predefined, ptr->getStringValue())) {
			return res; // just print literal as it is
		}

		// handle C string literals
		// TODO: move this to an extension since it is a reference
		if(ptr->getStringValue()[0] == '"') {
			core::TypePtr type = core::lang::ReferenceType(ptr).getElementType();
			if(core::lang::isArray(type) && basic.isWChar(core::lang::ArrayType(type).getElementType())) {
				// reproduce the longstring signature for widechars, this is 16 in windows and 32 in unix
				res = toLiteral("L" + ptr->getStringValue());
			}
			return res;
		}

		// handle literals declared within other header files
		if(annotations::c::hasIncludeAttached(ptr)) {
			// add header file
			context.getIncludes().insert(annotations::c::getAttachedInclude(ptr));

			// and use it (as a pointer if it is a reference type)
			return core::lang::isReference(ptr) ? c_ast::ref(res) : res;
		}

		// handle literals referencing external data elements
		if(core::analysis::isRefType(type)) {
			// look up external variable declaration
			auto fragmentManager = converter.getFragmentManager();
			string fragmentName = "global:" + ptr->getStringValue();
			auto fragment = fragmentManager->getFragment(fragmentName);

			// check fragment
			if(!fragment) {
				// create new declaration
				c_ast::CCodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(fragmentManager);
				// register fragment
				fragmentManager->bindFragment(fragmentName, declaration);

				// get type info
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(context, type);


				// add external declaration
				auto& cManager = converter.getCNodeManager();
				declaration->getCode().push_back(cManager->create<c_ast::Comment>("------- Global Variable Declaration ----------"));
				declaration->getCode().push_back(cManager->create<c_ast::GlobalVarDecl>(info.lValueType, ptr->getStringValue(), annotations::c::isExtern(ptr)));

				// add dependency to element type definition
				const TypeInfo& elementInfo = context.getConverter().getTypeManager().getTypeInfo(context, core::analysis::getReferencedType(type));
				declaration->addDependency(elementInfo.definition);

				fragment = declaration;
			}

			// add dependency
			context.getDependencies().insert(fragment);

			// also, result has to be referenced
			return c_ast::ref(res);
		}

		// done
		return res;
	}

	c_ast::NodePtr StmtConverter::visitInitExpr(const core::InitExprPtr& ptr, ConversionContext& context) {
		// check if it is a global initialization
		auto memExp = ptr->getMemoryExpr();
		if(auto lit = memExp.isa<core::LiteralPtr>()) {
			convertGlobalInit(context, lit, ptr);
			return {};
		}

		// otherwise it's the default case
		return visitInitExprInternal(converter, ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitTupleExpr(const core::TupleExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

		auto typeInfo = converter.getTypeManager().getTypeInfo(context, ptr->getType());
		context.addDependency(typeInfo.definition);

		// get type and create init expression
		c_ast::TypePtr type = typeInfo.rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// append initialization values
		::transform(ptr->getExpressions()->getElements(), std::back_inserter(init->values),
		            [&](const core::ExpressionPtr& cur) { return convert(context, cur); });

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitVariable(const core::VariablePtr& ptr, ConversionContext& context) {
		// just look up variable within variable manager and return variable token ...
		const VariableInfo& info = context.getVariableManager().getInfo(ptr);
		return (info.location == VariableInfo::DIRECT) ? c_ast::ref(info.var) : info.var;
	}

	c_ast::NodePtr StmtConverter::visitMarkerExpr(const core::MarkerExprPtr& ptr, ConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubExpression(), context);
	}


	////////////////////////////////////////////////////////////////////////// Statements

	c_ast::NodePtr StmtConverter::visitBreakStmt(const core::BreakStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Break>();
	}

	c_ast::NodePtr StmtConverter::visitCompoundStmt(const core::CompoundStmtPtr& ptr, ConversionContext& context) {
		c_ast::CompoundPtr res = converter.getCNodeManager()->create<c_ast::Compound>();
		for_each(ptr->getStatements(), [&](const core::StatementPtr& cur) {
			c_ast::NodePtr stmt = this->visit(cur, context);
			if(stmt) {
				// strip no-op ref
				auto unOp = stmt.isa<c_ast::UnaryOperationPtr>();
				if(unOp && unOp->operation == c_ast::UnaryOperation::Reference) stmt = unOp->operand;
				res->statements.push_back(stmt);
			}
		});
		return res;
	}

	c_ast::NodePtr StmtConverter::visitContinueStmt(const core::ContinueStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Continue>();
	}

	namespace {

		bool toBeAllocatedOnStack(const core::ExpressionPtr& initValue) {
			auto& refExt = initValue->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

			// if it is a call to a ref.new => put it on the heap
			if(refExt.isCallOfRefNew(initValue) || refExt.isCallOfRefNewInit(initValue)) return false;
			// if it is a constructor call on memory allocated on the heap, put it on the heap
			if(core::analysis::isConstructorCall(initValue)) {
				return toBeAllocatedOnStack(core::analysis::getArgument(initValue, 0));
			}

			// everything else is stack based
			return true;
		}
	}

	c_ast::NodePtr StmtConverter::visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, ConversionContext& context) {

		// goal: create a variable declaration and register new variable within variable manager
		auto manager = converter.getCNodeManager();
		core::IRBuilder builder(ptr->getNodeManager());
		auto& refExt = ptr->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();

		core::VariablePtr var = ptr->getVariable();

		core::ExpressionPtr init = ptr->getInitialization();
		bool undefinedInit = refExt.isCallOfRefTemp(init) || refExt.isCallOfRefDecl(init);

		core::TypePtr plainType = var->getType();

		// decide storage location of variable
		VariableInfo::MemoryLocation location = VariableInfo::NONE;
		// assigning from the same type (not uninitialized) doesn't regard the location
		if(plainType == init->getType() && !undefinedInit && !core::analysis::isConstructorCall(init) && !init.isa<core::InitExprPtr>()) {
			location = VariableInfo::INDIRECT;
		} else if(core::lang::isReference(plainType)) {
			if(toBeAllocatedOnStack(init)) {
				location = VariableInfo::DIRECT;
			} else {
				location = VariableInfo::INDIRECT;
			}
		}

		// register variable information
		const VariableInfo& info = context.getVariableManager().addInfo(context, var, location);

		// add code dependency
		context.getDependencies().insert(info.typeInfo->definition);

		// if a reference variable is put on the stack, the element type definition is also required
		if(location == VariableInfo::DIRECT) {
			auto elementType = core::analysis::getReferencedType(plainType);
			context.getDependencies().insert(context.getConverter().getTypeManager().getTypeInfo(context, elementType).definition);
		}

		c_ast::ExpressionPtr initValue = convertInitExpression(context, plainType, init);

		// deref materialized initial value if explicitly initialized
		if (location == VariableInfo::DIRECT && core::analysis::getRefDeclCall(ptr->getDeclaration())) {
			initValue = c_ast::deref(initValue);
		}

		// if the declared variable is undefined, we don't create an initialization value
		if(undefinedInit) {
			initValue = c_ast::ExpressionPtr();
		}

		return manager->create<c_ast::VarDecl>(info.var, initValue);
	}

	c_ast::ExpressionPtr StmtConverter::convertInitExpression(ConversionContext& context, const core::TypePtr& targetType, const core::ExpressionPtr& init) {

		core::ExpressionPtr initValue = init;

		// implement implicit constructor call semantics
		if(core::lang::isPlainReference(targetType)) { // only values get constructed
			auto tagTy = core::analysis::getReferencedType(targetType).isa<core::TagTypePtr>();
			if(tagTy) { // only records have constructors
				if(auto constructorOpt = core::analysis::hasConstructorAccepting(tagTy, init->getType())) {
					// build a temporary, unused call to the constructor in order to accomplish side effects
					// (e.g. declaration/definition, dependencies)
					auto constructor = tagTy.peel(constructorOpt.get());
					core::IRBuilder builder(converter.getNodeManager());
					auto temp = core::lang::buildRefTemp(targetType);
					auto call = builder.callExpr(constructor->getType().as<core::FunctionTypePtr>().getReturnType(), constructor, temp, init);
					converter.getFunctionManager().getCall(context, call);
				}
			}
		}

		auto coreInitExpr = initValue.isa<core::InitExprPtr>();
		if(coreInitExpr) return visitInitExprInternal(converter, coreInitExpr, context);
		return convertExpression(context, initValue);
	}

	namespace {
		bool isSimple(const core::ExpressionPtr& exp) {
			try {
				core::arithmetic::Formula form = core::arithmetic::toFormula(exp);
				if(form.isConstant() || form.isValue()) { return true; }
			} catch(const core::arithmetic::NotAFormulaException& e) {}
			return false;
		}
	}


	c_ast::NodePtr StmtConverter::visitForStmt(const core::ForStmtPtr& ptr, ConversionContext& context) {
		converter.getNameManager().pushVarScope(false);

		auto manager = converter.getCNodeManager();

		VariableManager& varManager = context.getVariableManager();
		core::IRBuilder builder(ptr->getNodeManager());
		auto var_iter = ptr->getIterator();

		// get induction variable info
		const VariableInfo info_iter = varManager.addInfo(context, var_iter, VariableInfo::NONE);
		// add dependency to iterator type definition
		context.getDependencies().insert(info_iter.typeInfo->definition);

		auto initVector = toVector(std::make_pair(info_iter.var, convertExpression(context, ptr->getStart())));

		// Process:
		// For both "end" and "step" expressions:
		// - if simple: use directly
		// - if not: build variables to store results

		// handle step
		core::ExpressionPtr step = ptr->getStep();
		auto cStepExpr = convertExpression(context, step);
		c_ast::ExpressionPtr cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, cStepExpr);
		core::arithmetic::Formula stepForm = core::arithmetic::toFormula(step);
		if(!isSimple(step)) {
			// create variable storing step
			c_ast::VariablePtr var_step = manager->create<c_ast::Variable>(info_iter.var->type, manager->create("_step"));
			initVector.push_back(std::make_pair(var_step, convertExpression(context, step)));
			cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, var_step);
		} else { // use pre(inc/dec) if abs(step) == 1
			if(stepForm.isConstant()) {
				if(stepForm.isOne()) { cStep = c_ast::preInc(info_iter.var); }
				if((-stepForm).isOne()) { cStep = c_ast::preDec(info_iter.var); }
			}
		}

		// handle end
		core::ExpressionPtr end = ptr->getEnd();
		c_ast::ExpressionPtr cEnd = convertExpression(context, end);
		if(!isSimple(end)) {
			// create variable storing end
			c_ast::VariablePtr var_end = manager->create<c_ast::Variable>(info_iter.var->type, manager->create("_end"));
			initVector.push_back(std::make_pair(var_end, cEnd));
			cEnd = var_end;
		}

		// create init and body
		c_ast::VarDeclPtr cInit = manager->create<c_ast::VarDecl>(initVector);
		converter.getNameManager().pushVarScope(false);
		c_ast::StatementPtr cBody = convertStmt(context, ptr->getBody());
		converter.getNameManager().popVarScope();

		// remove variable info since no longer in scope
		varManager.remInfo(var_iter);
		converter.getNameManager().popVarScope();

		// combine all into a for
		// Note: the IR for loop always assumes "<" as the operator used to compare the end value
		return manager->create<c_ast::For>(cInit, c_ast::lt(info_iter.var, cEnd), cStep, cBody);
	}

	c_ast::NodePtr StmtConverter::visitIfStmt(const core::IfStmtPtr& ptr, ConversionContext& context) {
		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr thenBranch = convertStmt(context, ptr->getThenBody());
		c_ast::StatementPtr elseBranch = (core::analysis::isNoOp(ptr->getElseBody())) ? c_ast::StatementPtr() : convertStmt(context, ptr->getElseBody());

		return manager->create<c_ast::If>(condition, thenBranch, elseBranch);
	}

	c_ast::NodePtr StmtConverter::visitWhileStmt(const core::WhileStmtPtr& ptr, ConversionContext& context) {
		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr body = convertStmt(context, ptr->getBody());

		return manager->create<c_ast::While>(condition, body);
	}

	c_ast::NodePtr StmtConverter::visitTryCatchStmt(const core::TryCatchStmtPtr& ptr, ConversionContext& context) {

		// convert body
		c_ast::StatementPtr body = convertStmt(context, ptr->getBody());

		// convert clauses
		vector<c_ast::TryCatch::Clause> clauses;
		for(const core::CatchClausePtr& clause : ptr->getClauses()) {

			// TODO: provide a fix for the catch-all case
//			// handle catch-all case
//			if(basic.isAny(clause->getVariable()->getType())) {
//				clauses.push_back(c_ast::TryCatch::Clause(convertStmt(context, clause->getBody())));
//				break;
//			}

			// register variable information
			const VariableInfo& info = context.getVariableManager().addInfo(context, clause->getVariable(), VariableInfo::MemoryLocation::NONE);

			// convert body of catch clause with registered variable
			auto body = convertStmt(context, clause->getBody());

			// add clause
			clauses.push_back(c_ast::TryCatch::Clause(info.var, body));
		}

		// finally create a try-catch node and be done
		return converter.getCNodeManager()->create<c_ast::TryCatch>(body, clauses);
	}

	c_ast::NodePtr StmtConverter::visitReturnStmt(const core::ReturnStmtPtr& ptr, ConversionContext& context) {
		// special handling for unit-return
		if(converter.getNodeManager().getLangBasic().isUnitConstant(ptr->getReturnExpr())) {
			return converter.getCNodeManager()->create<c_ast::Return>();
		}

		// special handling for in-place construction
		if(auto directInit = checkDirectConstruction(context, ptr->getReturnDeclaration())) {
			return converter.getCNodeManager()->create<c_ast::Return>(directInit);
		}

		// test for a materialization, which is implicit in C++
		if(auto implicit = checkPassingTemporaryMaterializedToReference(context, ptr->getReturnExpr())) {
			return converter.getCNodeManager()->create<c_ast::Return>(implicit);
		}

		// convert the result expression
		auto result = convertExpression(context, ptr->getReturnExpr());

		// deref plain references before returning the resulting value
		if (*ptr->getReturnType() == *ptr->getReturnExpr()->getType() && core::lang::isPlainReference(ptr->getReturnExpr())) {
			result = c_ast::deref(result);
		}

		// return value of return expression
		return converter.getCNodeManager()->create<c_ast::Return>(result);
	}

	c_ast::NodePtr StmtConverter::visitThrowStmt(const core::ThrowStmtPtr& ptr, ConversionContext& context) {
		core::ExpressionPtr expr = ptr->getThrowExpr();
		core::IRBuilder builder(expr.getNodeManager());

		core::ExpressionPtr rethrow = builder.literal("__insieme__rethrow", builder.getLangBasic().getUnit());
		if(expr == rethrow) {
			return converter.getCNodeManager()->create<c_ast::Throw>();
		} else {
			return converter.getCNodeManager()->create<c_ast::Throw>(convertExpression(context, ptr->getThrowExpr()));
		}
	}

	c_ast::NodePtr StmtConverter::visitGotoStmt(const core::GotoStmtPtr& ptr, ConversionContext& context) {
		// just create a goto stmt within the c_ast
		return converter.getCNodeManager()->create<c_ast::Goto>(ptr->getLabel()->getValue());
	}

	c_ast::NodePtr StmtConverter::visitLabelStmt(const core::LabelStmtPtr& ptr, ConversionContext& context) {
		// just create a goto stmt within the c_ast
		return converter.getCNodeManager()->create<c_ast::Label>(ptr->getLabel()->getValue());
	}

	c_ast::NodePtr StmtConverter::visitSwitchStmt(const core::SwitchStmtPtr& ptr, ConversionContext& context) {
		auto manager = converter.getCNodeManager();

		// create empty switch ...
		c_ast::SwitchPtr res = manager->create<c_ast::Switch>(convertExpression(context, ptr->getSwitchExpr()));

		// add cases ..
		::transform(ptr->getCases()->getElements(), std::back_inserter(res->cases), [&](const core::SwitchCasePtr& cur) {
			return std::make_pair(convertExpression(context, cur->getGuard()), convertStmt(context, cur->getBody()));
		});

		// add default ..
		res->defaultBranch = convertStmt(context, ptr->getDefaultCase());

		// .. and done
		return res;
	}

	c_ast::NodePtr StmtConverter::visitMarkerStmt(const core::MarkerStmtPtr& ptr, ConversionContext& context) {
		// markers are just ignored
		return visit(ptr->getSubStatement(), context);
	}

	insieme::backend::c_ast::ExpressionPtr checkDirectConstruction(ConversionContext& context, const core::DeclarationPtr& decl) {
		auto targetT = decl->getType();
		auto arg = decl->getInitialization();
		if(!targetT) return {};
		if(!(core::lang::isPlainReference(targetT) && core::lang::isPlainReference(arg))) return {};
		if(!core::analysis::isConstructorCall(arg)) return {};
		auto thisArg = core::analysis::getArgument(arg, 0);
		if(!targetT->getNodeManager().getLangExtension<core::lang::ReferenceExtension>().isCallOfRefDecl(thisArg)) return {};
		// build init expression -- shortcut by building constructor call and taking its arguments
		StmtConverter& stmtConverter = context.getConverter().getStmtConverter();
		auto conCall = stmtConverter.convert(context, arg);
		if(!conCall.isa<c_ast::ConstructorCallPtr>()) conCall = conCall.as<c_ast::UnaryOperationPtr>()->operand;
		auto constructorCall = conCall.as<c_ast::ConstructorCallPtr>();
		return c_ast::init(context.getConverter().getCNodeManager().get(), constructorCall->arguments);
	}

	c_ast::ExpressionPtr checkPassingTemporaryMaterializedToReference(ConversionContext& context, const core::ExpressionPtr& arg) {
		const auto& refExt = arg->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
		if(refExt.isCallOfRefKindCast(arg) || refExt.isCallOfRefCast(arg)) {
			if(core::lang::isCppReference(arg) || core::lang::isCppRValueReference(arg)) {
				const auto& inner = core::analysis::getArgument(arg, 0);
				if(refExt.isCallOfRefTempInit(inner)) {
					const auto& target = core::analysis::getArgument(inner, 0);
					return context.getConverter().getStmtConverter().convert(context, target).as<c_ast::ExpressionPtr>();
				}
			}
		}
		return {};
	}

} // end namespace backend
} // end namespace insieme

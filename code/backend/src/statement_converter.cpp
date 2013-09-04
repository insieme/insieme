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

#include "insieme/backend/statement_converter.h"

#include "insieme/backend/function_manager.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"

#include "insieme/backend/c_ast/c_ast.h"
#include "insieme/backend/c_ast/c_ast_utils.h"
#include "insieme/backend/c_ast/c_code.h"

#include "insieme/backend/ir_extensions.h"
#include "insieme/backend/variable_manager.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/types/variable_sized_struct_utils.h"
#include "insieme/core/lang/ir++_extension.h"

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
			if (res) {
				return res;
			}
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
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(type);
		context.getDependencies().insert(info.definition);
		return info.rValueType;
	}


	c_ast::NodePtr StmtConverter::visitProgram(const core::ProgramPtr& node, ConversionContext& context) {

		// get shared C Node Manager reference
		const c_ast::SharedCNodeManager& manager = converter.getCNodeManager();
		// program is not producing any C code => just dependencies
		for_each(node->getEntryPoints(), [&](const core::ExpressionPtr& entryPoint) {

			// fix name of main entry
			if (node->getEntryPoints().size() == static_cast<std::size_t>(1)) {
				context.getConverter().getNameManager().setName(entryPoint, "main");
			}

			// create a new context
			ConversionContext entryContext(converter);

			c_ast::CodeFragmentPtr fragment;
			if (entryPoint->getNodeType() == core::NT_LambdaExpr) {
				// handle function-entry point specially
				core::LambdaExprPtr lambda = static_pointer_cast<const core::LambdaExpr>(entryPoint);
				fragment = converter.getFunctionManager().getInfo(lambda).definition;
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
		// handled by the function manager
		return converter.getFunctionManager().getCall(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitBindExpr(const core::BindExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitCastExpr(const core::CastExprPtr& ptr, ConversionContext& context) {
		const auto& info = converter.getTypeManager().getTypeInfo(ptr->getType());
		context.addDependency(info.definition);
		return c_ast::cast(info.rValueType, visit(ptr->getSubExpression(), context));
	}

	c_ast::NodePtr StmtConverter::visitJobExpr(const core::JobExprPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Literal>("JOB-UNSUPPORTED");
	}

	c_ast::NodePtr StmtConverter::visitLambdaExpr(const core::LambdaExprPtr& ptr, ConversionContext& context) {
		// handled by the function manager
		return converter.getFunctionManager().getValue(ptr, context);
	}

	c_ast::NodePtr StmtConverter::visitLiteral(const core::LiteralPtr& ptr, ConversionContext& context) {
		// Function literals are handled by function manager
		if (ptr->getType()->getNodeType() == core::NT_FunctionType) {
			return converter.getFunctionManager().getValue(ptr, context);
		}

		// special handling for unit literal
		if (converter.getNodeManager().getLangBasic().isUnitConstant(ptr)) {
			return c_ast::cast(
				converter.getCNodeManager()->create<c_ast::PrimitiveType>(c_ast::PrimitiveType::Void),
				converter.getCNodeManager()->create<c_ast::Literal>("0")
			);
		}

		// special handling for int-type-parameter literal
		if (core::analysis::isIntTypeParamLiteral(ptr)) {
			core::IntTypeParamPtr value = core::analysis::getRepresentedTypeParam(ptr);
			assert(value.isa<core::ConcreteIntTypeParamPtr>() && "Uninstantiated int-type-parameter literal encountered!");
			return converter.getCNodeManager()->create<c_ast::Literal>(toString(*value) + "u");
		}

		// convert literal
		auto toLiteral = [&](const string& value) { return converter.getCNodeManager()->create<c_ast::Literal>(value); };
		c_ast::ExpressionPtr res = toLiteral(ptr->getStringValue());

		// handle primitive types
		auto& basic = ptr->getNodeManager().getLangBasic();
		if (basic.isPrimitive(ptr->getType())) {

			// handle special cases
			const string& value = ptr->getStringValue();

			// things that need not be extra-casted (default values)
			if (basic.isInt4(ptr->getType()) || basic.isReal4(ptr->getType()) || basic.isReal8(ptr->getType())) {
				return res;
			}

			// add a u in case it is a signed literal and it is missing
			if (basic.isUInt4(ptr->getType())) {
				if (*value.rbegin() != 'u') {
					res = toLiteral(value + "u");
				}
				return res;
			}

			// fall-back solution: use an explicit cast
			auto info = converter.getTypeManager().getTypeInfo(ptr->getType());
			context.addDependency(info.definition);
			return c_ast::cast(info.rValueType ,res);
		}

		// special handling for the global struct
		if (!ptr->getStringValue().compare(0, IRExtensions::GLOBAL_ID.size(), IRExtensions::GLOBAL_ID)) {
			if (ptr->getType()->getNodeType() == core::NT_RefType) {
				res = c_ast::ref(res);
			}

			// add code dependency to global struct
			auto fragment = converter.getFragmentManager()->getFragment(IRExtensions::GLOBAL_ID);
			assert(fragment && "Global Fragment not yet initialized!");
			context.getDependencies().insert(fragment);
			return res;
		}

		// special handling for type literals (fall-back solution)
		if (core::analysis::isTypeLiteralType(ptr->getType())) {
			const TypeInfo& info = converter.getTypeManager().getTypeInfo(ptr->getType());
			context.addDependency(info.declaration);
			return c_ast::lit(info.rValueType, "type_token");
		}

		// special handling for boolean literals
		if (ptr.getNodeManager().getLangBasic().isBool(ptr->getType())) {
			context.getIncludes().insert("stdbool.h");
		}

		// handle null pointer
		if (converter.getNodeManager().getLangBasic().isRefNull(ptr)) {
			return converter.getCNodeManager()->create<c_ast::Literal>("0");
		}

		// handle pre-defined C identifiers (standard - 6.4.2.2)
		const static vector<string> predefined = { "__func__", "__FUNCTION__", "__PRETTY_FUNCTION__" };
		if (contains(predefined, ptr->getStringValue())) {
			return res;		// just print literal as it is
		}

		// handle C string literals (which are of type ref<vector<...>>)
		if (ptr->getStringValue()[0] == '"') {
			return res;		// just print it and be done
		}

		// handle literals declared within other header files
		if (annotations::c::hasIncludeAttached(ptr)) {
			// add header file
			context.getIncludes().insert(annotations::c::getAttachedInclude(ptr));

			// and use it (as a pointer if it is a reference type)
			return (ptr->getType().isa<core::RefTypePtr>())?c_ast::ref(res):res;
		}

		// handle literals referencing external data elements
		if (core::analysis::isRefType(ptr->getType())) {
			// look up external variable declaration
			auto fragmentManager = converter.getFragmentManager();
			string fragmentName = "extLitDecl:" + ptr->getStringValue();
			auto fragment = fragmentManager->getFragment(fragmentName);

			// check fragment
			if (!fragment) {

				// create new declaration
				c_ast::CCodeFragmentPtr declaration = c_ast::CCodeFragment::createNew(fragmentManager);

				// get type info
				const TypeInfo& info = context.getConverter().getTypeManager().getTypeInfo(core::analysis::getReferencedType(ptr->getType()));

				// add external declaration
				auto& cManager = converter.getCNodeManager();
				declaration->getCode().push_back(cManager->create<c_ast::Comment>("------- Variable Declaration ----------"));
				declaration->getCode().push_back(cManager->create<c_ast::GlobalVarDecl>(info.lValueType, ptr->getStringValue(), annotations::c::isExtern(ptr)));

				// add dependency to type declaration
				declaration->addDependency(info.definition);

				// register fragment
				fragmentManager->bindFragment(fragmentName, declaration);

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

	c_ast::NodePtr StmtConverter::visitStructExpr(const core::StructExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

        auto typeInfo = converter.getTypeManager().getTypeInfo(ptr->getType());
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// obtain some helper
		auto& basic = converter.getNodeManager().getLangBasic();

		// append initialization values
		::transform(ptr->getMembers()->getElements(), std::back_inserter(init->values),
				[&](const core::NamedValuePtr& cur) {
					core::ExpressionPtr arg = cur->getValue();
					// skip ref.var if present
					if (core::analysis::isCallOf(cur->getValue(), basic.getRefVar())) {
						arg = static_pointer_cast<const core::CallExpr>(cur->getValue())->getArgument(0);
						if (core::analysis::isCallOf(arg, basic.getRefDeref())) {
							arg = static_pointer_cast<const core::CallExpr>(arg)->getArgument(0);
						}
					}
					return convert(context, arg);
		});

		// remove last element if it is a variable sized struct
		if (core::types::isVariableSized(ptr->getType())) {
			assert(!init->values.empty());
			init->values.pop_back();
		}

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitUnionExpr(const core::UnionExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding union
		//     (<type>){<single member>}

		core::TypePtr unionType = ptr->getType();
        auto typeInfo = converter.getTypeManager().getTypeInfo(unionType);
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;

        std::cout << "\nMember is of type: " << ptr->getMember()->getNodeType() << "\n" << ptr->getMember() << "\n";

        auto cmgr = context.getConverter().getCNodeManager();

        // special handling for vector initialization (should not be turned into a struct)
        auto value = convertExpression(context, ptr->getMember());
        if (ptr->getMember()->getNodeType() == core::NT_VectorExpr) {
        	assert(dynamic_pointer_cast<c_ast::Initializer>(value));
        	std::cout << "Number of elements: " << static_pointer_cast<c_ast::Initializer>(value)->values.size() << "\n";
        	value = cmgr->create<c_ast::VectorInit>(
        			static_pointer_cast<c_ast::VectorInit>(
        					static_pointer_cast<c_ast::Initializer>(value)->values[0]
        			)->values
        	);
        }

        return c_ast::init(
        		type,
        		cmgr->create(ptr->getMemberName()->getValue()),
        		value
        );
//		return c_ast::init(type, convert(context, ptr->getMember()));
	}

	c_ast::NodePtr StmtConverter::visitTupleExpr(const core::TupleExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct
		//     (<type>){<list of members>}

        auto typeInfo = converter.getTypeManager().getTypeInfo(ptr->getType());
        context.addDependency(typeInfo.definition);

        // get type and create init expression
        c_ast::TypePtr type = typeInfo.rValueType;
		c_ast::InitializerPtr init = c_ast::init(type);

		// append initialization values
		::transform(ptr->getExpressions()->getElements(), std::back_inserter(init->values),
				[&](const core::ExpressionPtr& cur) {
					return convert(context, cur);
		});

		// return completed
		return init;
	}

	c_ast::NodePtr StmtConverter::visitVariable(const core::VariablePtr& ptr, ConversionContext& context) {
		// just look up variable within variable manager and return variable token ...
		const VariableInfo& info = context.getVariableManager().getInfo(ptr);
		return (info.location == VariableInfo::DIRECT && !core::analysis::isRefOf(ptr->getType(), core::NT_ArrayType))?c_ast::ref(info.var):info.var;
	}

	c_ast::NodePtr StmtConverter::visitVectorExpr(const core::VectorExprPtr& ptr, ConversionContext& context) {
		// to be created: an initialization of the corresponding struct - where one value is a vector
		//     (<type>){{<list of members>}}

		// get type and create empty init expression
		const TypeInfo& info = converter.getTypeManager().getTypeInfo(ptr->getType());
		c_ast::TypePtr type = info.rValueType;
		context.getDependencies().insert(info.definition);

		// create inner vector init and append initialization values
		c_ast::VectorInitPtr vectorInit = context.getConverter().getCNodeManager()->create<c_ast::VectorInit>();
		::transform(ptr->getExpressions()->getElements(), std::back_inserter(vectorInit->values),
				[&](const core::ExpressionPtr& cur) {
					return convert(context, cur);
		});

		// create and return out initializer
		return c_ast::init(type, vectorInit);
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
			c_ast::NodePtr stmt = this->visit(cur,context);
			if (stmt) { res->statements.push_back(stmt); }
		});
		return res;
	}

	c_ast::NodePtr StmtConverter::visitContinueStmt(const core::ContinueStmtPtr& ptr, ConversionContext& context) {
		return converter.getCNodeManager()->create<c_ast::Continue>();
	}

	namespace {

		bool toBeAllocatedOnStack(const core::ExpressionPtr& initValue) {
			auto& basic = initValue->getNodeManager().getLangBasic();
			auto& ext = initValue->getNodeManager().getLangExtension<core::lang::IRppExtensions>();

			// if it is a call to a ref.var => put it on the stack
			if (core::analysis::isCallOf(initValue, basic.getRefVar())) {
				return true;
			}

			// if is materialize is stack
			if (core::analysis::isCallOf(initValue, ext.getMaterialize())) {
				return true;
			}

			// if it is a constructor call ..
			if (core::CallExprPtr call = initValue.isa<core::CallExprPtr>()) {
				return core::analysis::isCallOf(call[0], basic.getRefVar());
			}

			// everything else is heap based
			return false;
		}

		bool isStackBasedCArray(const core::ExpressionPtr& initValue) {
			const auto& basic = initValue->getNodeManager().getLangBasic();

			// it has to be stack based ..
			if (!core::analysis::isCallOf(initValue, basic.getRefVar())) {
				return false;
			}

			// .. and a newly created array
			auto value = initValue.as<core::CallExprPtr>()->getArgument(0);
			return core::analysis::isCallOf(value, basic.getArrayCreate1D());
		}

		bool isStackBasedCppArray(const core::ExpressionPtr& initValue) {
			const auto& basic = initValue->getNodeManager().getLangBasic();
			const auto& ext = initValue->getNodeManager().getLangExtension<core::lang::IRppExtensions>();

			// check whether it is a array ctor call
			if (!core::analysis::isCallOf(initValue, ext.getArrayCtor()) && !core::analysis::isCallOf(initValue, ext.getVectorCtor())) {
				return false;
			}

			// first argument needs to be a ref.var
			return basic.isRefVar(initValue.as<core::CallExprPtr>()->getArgument(0));
		}

		bool isStackBasedArray(const core::ExpressionPtr& initValue) {
			return isStackBasedCArray(initValue) || isStackBasedCppArray(initValue);
		}

		c_ast::NodePtr resolveStackBasedArrayInternal(ConversionContext& context, const core::VariablePtr& var, const core::ExpressionPtr& sizeExpr) {

			// obtain c-ast node manager
			const auto& converter = context.getConverter();
			auto manager = converter.getCNodeManager();

			// resolve type (needs to be explicitly handled here)
			auto size = converter.getStmtConverter().convertExpression(context, sizeExpr);
			auto elementType = var->getType().as<core::RefTypePtr>()->getElementType().as<core::SingleElementTypePtr>()->getElementType();
			const TypeInfo& typeInfo = converter.getTypeManager().getCVectorTypeInfo(elementType, size);

			// although it is on the stack, it is to be treated as it would be indirect (implicit pointer!)
			const VariableInfo& info = context.getVariableManager().addInfo(converter, var, VariableInfo::INDIRECT, typeInfo);

			// add dependency to type definition of variable
			context.getDependencies().insert(info.typeInfo->definition);

			// create a variable declaration without init value
			return manager->create<c_ast::VarDecl>(info.var);
		}

		c_ast::NodePtr resolveStackBasedCArray(ConversionContext& context, const core::VariablePtr& var, const core::ExpressionPtr& init) {
			assert(isStackBasedCArray(init) && "Invalid input parameter!");
			return resolveStackBasedArrayInternal(context, var, init.as<core::CallExprPtr>()->getArgument(0).as<core::CallExprPtr>()->getArgument(1));
		}

		c_ast::NodePtr resolveStackBasedCppArray(ConversionContext& context, const core::VariablePtr& var, const core::ExpressionPtr& init) {
			assert(isStackBasedCppArray(init) && "Invalid input parameter!");
			return resolveStackBasedArrayInternal(context, var, init.as<core::CallExprPtr>()->getArgument(2));
		}

		c_ast::NodePtr resolveStackBasedArray(ConversionContext& context, const core::VariablePtr& var, const core::ExpressionPtr& init) {
			if (isStackBasedCArray(init)) return resolveStackBasedCArray(context, var, init);
			if (isStackBasedCppArray(init)) return resolveStackBasedCppArray(context, var, init);
			assert(false && "Init value is not a stack based array!");
			return 0;
		}

	}

	c_ast::NodePtr StmtConverter::visitDeclarationStmt(const core::DeclarationStmtPtr& ptr, ConversionContext& context) {

		// goal: create a variable declaration and register new variable within variable manager
		auto manager = converter.getCNodeManager();

		core::VariablePtr var = ptr->getVariable();
		core::ExpressionPtr init = ptr->getInitialization();

		// special handling for stack based arrays (special case in C)
		if (isStackBasedArray(init)) {
			// delegate processing
			return resolveStackBasedArray(context, var, init);
		}

		// decide storage location of variable
		VariableInfo::MemoryLocation location = VariableInfo::NONE;
		if (core::analysis::hasRefType(var)) {

			if (toBeAllocatedOnStack(init)) {
				location = VariableInfo::DIRECT;
			} else {
				location = VariableInfo::INDIRECT;
			}
		}

		// register variable information
		const VariableInfo& info = context.getVariableManager().addInfo(converter, var, location);

		// add code dependency
		context.getDependencies().insert(info.typeInfo->definition);

		// if a reference variable is put on the stack, the element type definition is also required
		if (location == VariableInfo::DIRECT) {
			auto elementType = core::analysis::getReferencedType(var->getType());
			context.getDependencies().insert(context.getConverter().getTypeManager().getTypeInfo(elementType).definition);
		}

		// create declaration statement
		c_ast::ExpressionPtr initValue = convertInitExpression(context, init);

		// get rid of & operator in front of stack-based constructor calls
		if( core::analysis::isConstructorCall(init) && location == VariableInfo::DIRECT ) {
			initValue = c_ast::deref(initValue);
		}

		return manager->create<c_ast::VarDecl>(info.var, initValue);
	}

	c_ast::ExpressionPtr StmtConverter::convertInitExpression(ConversionContext& context, const core::ExpressionPtr& init) {
		auto& basic = converter.getNodeManager().getLangBasic();
		auto manager = converter.getCNodeManager();

		// test whether initialization is required ...
		if (core::analysis::isCallOf(init, basic.getRefVar())) {
			core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(init);
			core::ExpressionPtr init = call->getArgument(0);
			if (core::analysis::isCallOf(init, basic.getUndefined())) {
				// => undefined initialization, hence no initialization!
				return c_ast::ExpressionPtr();
			}

			if (init->getNodeType() == core::NT_StructExpr) {
				auto isUndefined = [&](const core::NamedValuePtr& cur) { return core::analysis::isCallOf(cur->getValue(), basic.getUndefined()); };
				if (all(init.as<core::StructExprPtr>()->getMembers(), isUndefined)) {
					// no init required
					return c_ast::ExpressionPtr();
				}

				// this is not supported yet - TODO: update struct expression to use names and initialize members individually
				assert(!any(init.as<core::StructExprPtr>()->getMembers(), isUndefined) && "Unsupported combination of defined and undefined values!");
			}

		}

		// TODO: handle initUndefine and init struct cases

		// drop ref.var ...
		core::ExpressionPtr initValue = init;
		if (core::analysis::isCallOf(initValue, basic.getRefVar())) {
			initValue = core::analysis::getArgument(initValue, 0);
		}

		return convertExpression(context, initValue);
	}

	namespace {
		bool isSimple(const core::ExpressionPtr& exp) {
			try {
				core::arithmetic::Formula form = core::arithmetic::toFormula(exp);
				if(form.isConstant() || form.isValue()) return true;
			} catch(core::arithmetic::NotAFormulaException e) { }
			return false;
		}
	}


	c_ast::NodePtr StmtConverter::visitForStmt(const core::ForStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		VariableManager& varManager = context.getVariableManager();
		core::IRBuilder builder(ptr->getNodeManager());
		auto var_iter = ptr->getIterator();

		// get induction variable info
		const VariableInfo& info_iter = varManager.addInfo(converter, var_iter, VariableInfo::NONE);
		// add dependency to iterator type definition
		context.getDependencies().insert(info_iter.typeInfo->definition);

		auto initVector = toVector(std::make_pair(info_iter.var, convertExpression(context, ptr->getStart())));

		// Process:
		// For both "end" and "step" expressions:
		// - if simple: use directly
		// - if not: build variables to store results

		// handle step
		core::ExpressionPtr step = ptr->getStep();
		c_ast::ExpressionPtr cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, convertExpression(context, step));
		if(!isSimple(step)) {
			// create variable storing step
			c_ast::VariablePtr var_step = manager->create<c_ast::Variable>(info_iter.var->type, manager->create("_step"));
			initVector.push_back(std::make_pair(var_step, convertExpression(context, step)));
			cStep = c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, info_iter.var, var_step);
		}
		else { // use pre(inc/dec) if abs(step) == 1
			core::arithmetic::Formula form = core::arithmetic::toFormula(step);
			if(form.isConstant()) {
				if(form.isOne()) cStep = c_ast::preInc(info_iter.var);
				if((-form).isOne()) cStep = c_ast::preDec(info_iter.var);
			}
		}

		// handle end
		core::ExpressionPtr end = ptr->getEnd();
		c_ast::ExpressionPtr cCheck = c_ast::lt(info_iter.var, convertExpression(context, end));
		if(!isSimple(end)) {
			// create variable storing end
			c_ast::VariablePtr var_end = manager->create<c_ast::Variable>(info_iter.var->type, manager->create("_end"));
			initVector.push_back(std::make_pair(var_end, convertExpression(context, end)));
			cCheck = c_ast::lt(info_iter.var, var_end);
		}

		// create init and body
		c_ast::VarDeclPtr cInit = manager->create<c_ast::VarDecl>(initVector);
		c_ast::StatementPtr cBody = convertStmt(context, ptr->getBody());

		// remove variable info since no longer in scope
		varManager.remInfo(var_iter);

		// combine all into a for
		return manager->create<c_ast::For>(cInit, cCheck, cStep, cBody);
	}

	c_ast::NodePtr StmtConverter::visitIfStmt(const core::IfStmtPtr& ptr, ConversionContext& context) {

		auto manager = converter.getCNodeManager();

		// create condition, then and else branch
		c_ast::ExpressionPtr condition = convertExpression(context, ptr->getCondition());
		c_ast::StatementPtr thenBranch = convertStmt(context, ptr->getThenBody());
		c_ast::StatementPtr elseBranch = (core::analysis::isNoOp(ptr->getElseBody()))
				?c_ast::StatementPtr():convertStmt(context, ptr->getElseBody());

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
		const auto& basic = converter.getNodeManager().getLangBasic();

		// convert body
		c_ast::StatementPtr body = convertStmt(context, ptr->getBody());

		// convert clauses
		vector<c_ast::TryCatch::Clause> clauses;
		for(const core::CatchClausePtr& clause : ptr->getClauses()) {

			// handle catch-all case
			if (basic.isAny(clause->getVariable()->getType())) {
				clauses.push_back(c_ast::TryCatch::Clause(convertStmt(context, clause->getBody())));
				break;
			}

			// register variable information
			const VariableInfo& info = context.getVariableManager().addInfo(converter, clause->getVariable(), VariableInfo::MemoryLocation::NONE);

			// convert body of catch clause with registered variable
			auto body = convertStmt(context, clause->getBody());

			// add clause
			clauses.push_back(c_ast::TryCatch::Clause(info.var, body));
		}

		// finally create a try-catch node and be done
		return converter.getCNodeManager()->create<c_ast::TryCatch>(body, clauses);
	}

	c_ast::NodePtr StmtConverter::visitReturnStmt(const core::ReturnStmtPtr& ptr, ConversionContext& context) {
		// wrap sub-expression into return expression
		if (context.getConverter().getNodeManager().getLangBasic().isUnit(ptr->getReturnExpr()->getType())) {
			// special handling for unit-return
			return converter.getCNodeManager()->create<c_ast::Return>();
		}
		return converter.getCNodeManager()->create<c_ast::Return>(convertExpression(context, ptr->getReturnExpr()));
	}

	c_ast::NodePtr StmtConverter::visitThrowStmt(const core::ThrowStmtPtr& ptr, ConversionContext& context) {
		// just create a throw stmt within the c_ast
		return converter.getCNodeManager()->create<c_ast::Throw>(convertExpression(context, ptr->getThrowExpr()));
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


} // end namespace backend
} // end namespace insieme

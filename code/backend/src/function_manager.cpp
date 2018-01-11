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

#include "insieme/backend/function_manager.h"

#include <set>
#include <tuple>
#include <functional>

#include <boost/algorithm/string/predicate.hpp>

#include "insieme/backend/converter.h"
#include "insieme/backend/type_manager.h"
#include "insieme/backend/statement_converter.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/variable_manager.h"
#include "insieme/backend/c_ast/c_ast_utils.h"

#include "insieme/core/analysis/attributes.h"
#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/normalize.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_cached_visitor.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/varargs_extension.h"
#include "insieme/core/transform/instantiate.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/manipulation_utils.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/extern_c.h"
#include "insieme/annotations/c/implicit.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/assert.h"
#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace backend {


	namespace detail {

		template <typename Type>
		struct info_trait;
		template <>
		struct info_trait<core::Literal> {
			typedef FunctionInfo type;
		};
		template<>
		struct info_trait<core::PureVirtualMemberFunction>
			: public info_trait<core::Literal> {};

		template <>
		struct info_trait<core::LambdaExpr> {
			typedef LambdaInfo type;
		};

		template<>
		struct info_trait<core::MemberFunction>
			: public info_trait<core::LambdaExpr> {};

		template <>
		struct info_trait<core::BindExpr> {
			typedef BindInfo type;
		};

		struct FunctionCodeInfo {
			c_ast::FunctionPtr function;
			c_ast::DefinitionPtr definition;
			c_ast::FragmentSet prototypeDependencies;
			c_ast::FragmentSet definitionDependencies;
			std::set<string> includes;
		};

		class FunctionInfoStore {
			const Converter& converter;

			utils::map::PointerMap<core::NodePtr, ElementInfo*> funInfos;

		  public:

			FunctionInfoStore(const Converter& converter) : converter(converter), funInfos() {}

			~FunctionInfoStore() {
				// free all stored type information instances (may contain duplicates)
				std::set<ElementInfo*> deleted;
				for (const auto& cur : funInfos)
					if (deleted.insert(cur.second).second)
						delete cur.second;
			}

			template <
				typename T,
				typename result_type = typename info_trait<typename std::remove_const<typename T::element_type>::type>::type*
			>
			result_type resolve(ConversionContext& context, const T& node) {
				// lookup information using internal mechanism and cast statically
				ElementInfo* info = resolveInternal(context, node);
				assert(dynamic_cast<result_type>(info));
				return static_cast<result_type>(info);
			}

			template <
				typename T,
				typename result_type = typename info_trait<typename std::remove_const<typename T::element_type>::type>::type*
			>
			result_type resolve(ConversionContext& context, const core::TagTypePtr& tagType, const T& node) {
				// lookup information using internal mechanism and cast statically
				ElementInfo* info = resolveInternal(context, tagType, node);
				assert(dynamic_cast<result_type>(info));
				return static_cast<result_type>(info);
			}

		  protected:

			ElementInfo* resolveInternal(ConversionContext& context, const core::NodePtr& expression);
			ElementInfo* resolveInternal(ConversionContext& context, const core::TagTypePtr& tagType, const core::LambdaExprPtr& member);
			ElementInfo* resolveInternal(ConversionContext& context, const core::TagTypePtr& tagType, const core::MemberFunctionPtr& member);

			FunctionInfo* resolveLiteral(ConversionContext& context, const core::LiteralPtr& literal);
			FunctionInfo* resolvePureVirtualMember(ConversionContext& context, const core::PureVirtualMemberFunctionPtr&);
			LambdaInfo* resolveLambda(ConversionContext& context, const core::LambdaExprPtr& lambda);
			LambdaInfo* resolveMemberFunction(ConversionContext& context, const core::MemberFunctionPtr&);
			BindInfo* resolveBind(ConversionContext& context, const core::BindExprPtr& bind);

			void resolveLambdaDefinition(ConversionContext& context, const core::LambdaDefinitionPtr& lambdaDefinition);

			// -------- utilities -----------

			FunctionCodeInfo resolveFunction(ConversionContext& context, const c_ast::IdentifierPtr name, const core::FunctionTypePtr& funType,
			                                 const core::LambdaPtr& lambda);

			std::pair<c_ast::IdentifierPtr, c_ast::CodeFragmentPtr> resolveLambdaWrapper(ConversionContext& context, const c_ast::FunctionPtr& function,
			                                                                             const core::FunctionTypePtr& funType);
		};
	}

	FunctionManager::FunctionManager(const Converter& converter)
	    : converter(converter), store(new detail::FunctionInfoStore(converter)), operatorTable(getBasicOperatorTable(converter.getNodeManager())),
	      includeTable(getBasicFunctionIncludeTable()) {}

	FunctionManager::FunctionManager(const Converter& converter, const OperatorConverterTable& operatorTable, const FunctionIncludeTable& includeTable)
	    : converter(converter), store(new detail::FunctionInfoStore(converter)), operatorTable(operatorTable), includeTable(includeTable) {}

	FunctionManager::~FunctionManager() {
		delete store;
	}

	const FunctionInfo& FunctionManager::getInfo(ConversionContext& context, const core::LiteralPtr& literal) {
		return *(store->resolve(context, literal));
	}

	const FunctionInfo& FunctionManager::getInfo(ConversionContext& context, const core::PureVirtualMemberFunctionPtr& fun) {
		return *(store->resolve(context, fun));
	}

	const LambdaInfo& FunctionManager::getInfo(ConversionContext& context, const core::LambdaExprPtr& lambda) {
		return *(store->resolve(context, lambda));
	}

	const LambdaInfo& FunctionManager::getInfo(ConversionContext& context, const core::TagTypePtr& tagType, const core::LambdaExprPtr& fun) {
		return *(store->resolve(context, tagType,fun));
	}

	const LambdaInfo& FunctionManager::getInfo(ConversionContext& context, const core::TagTypePtr& tagType, const core::MemberFunctionPtr& memberFun) {
		return *(store->resolve(context, tagType, memberFun));
	}

	const BindInfo& FunctionManager::getInfo(ConversionContext& context, const core::BindExprPtr& bind) {
		return *(store->resolve(context, bind));
	}

	bool FunctionManager::isBuiltIn(const core::NodePtr& op) const {
		if(op->getNodeCategory() != core::NC_Expression) { return false; }
		return operatorTable.find(op.as<core::ExpressionPtr>()) != operatorTable.end() || core::lang::isBuiltIn(op);
	}

	namespace {

		void appendAsArguments(ConversionContext& context, c_ast::CallPtr& call, const core::TypeList& targetTypes, const core::ExpressionList& arguments) {
			// collect some manager references
			const Converter& converter = context.getConverter();
			StmtConverter& stmtConverter = converter.getStmtConverter();

			auto varlistPack = converter.getNodeManager().getLangExtension<core::lang::VarArgsExtension>().getVarlistPack();

			// create a recursive lambda appending arguments to the caller (descent into varlist-pack calls)
			std::function<void(const core::TypePtr& targetType, const core::ExpressionPtr& argument)> append;
			auto recLambda = [&](const core::TypePtr& targetType, const core::ExpressionPtr& cur) {

				// test if current argument is a variable argument list
				if(core::analysis::isCallOf(cur, varlistPack)) {
					// inline arguments of varlist-pack call => append arguments directly
					const auto& packed = core::transform::extractArgExprsFromCall(cur.as<core::CallExprPtr>());

					for_each(static_pointer_cast<const core::TupleExpr>(packed[0])->getExpressions()->getElements(),
					         [&](const core::ExpressionPtr& cur) { append(nullptr, cur); });
					return;
				}

				// test if the current argument is a type literal
				if(core::analysis::isTypeLiteralType(cur->getType())) {
					return; // skip those parameters
				}

				// test for direct construction
				if(auto direct = checkDirectConstruction(context, targetType, cur)) {
					call->arguments.push_back(direct);
					return;
				}

				// test for a materialization, which is implicit in C++
				if(auto implicit = checkPassingTemporaryMaterializedToReference(context, cur)) {
					call->arguments.push_back(implicit);
					return;
				}

				// convert the argument
				c_ast::ExpressionPtr res =
				    targetType ? stmtConverter.convertInitExpression(context, targetType, cur) : stmtConverter.convertExpression(context, cur);
				call->arguments.push_back(res);

			};
			append = recLambda;

			// invoke append for all arguments
			for(const auto& pair : make_paired_range(targetTypes, arguments)) {
				append(pair.first, pair.second);
			}
		}

		c_ast::NodePtr handleMemberCall(ConversionContext& context, const core::CallExprPtr& call, c_ast::CallPtr c_call) {
			// by default, do nothing
			c_ast::ExpressionPtr res = c_call;

			// extract type of target function
			const core::FunctionTypePtr funType = call->getFunctionExpr()->getType().as<core::FunctionTypePtr>();

			auto irCallArgs = call->getArgumentList();

			// ----------------- constructor call ---------------

			// re-structure call into a constructor call
			if(funType->isConstructor()) {
				vector<c_ast::NodePtr> args = c_call->arguments;
				assert_false(args.empty());

				const auto& refExt = call->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
				auto location = args[0];
				args.erase(args.begin());

				// extract class type
				auto classType = context.getConverter().getTypeManager().getTypeInfo(context, core::analysis::getObjectType(funType)).lValueType;

				// distinguish memory location to be utilized
				// case a) create object on stack => default

				auto thisArg = core::lang::removeSurroundingRefCasts(irCallArgs[0]);

				// case b) create object on heap
				bool isOnHeap = refExt.isCallOfRefNew(thisArg);

				// case c) create object in-place (placement new)
				c_ast::ExpressionPtr loc = (refExt.isCallOfRefTemp(thisArg) || refExt.isCallOfRefTempInit(thisArg)
				                            || refExt.isCallOfRefNew(thisArg) || refExt.isCallOfRefDecl(thisArg))
				                               ? c_ast::ExpressionPtr()
				                               : location.as<c_ast::ExpressionPtr>();

				// to get support for the placement new the new header is required
				if(loc) { context.addInclude("<new>"); }

				// create constructor call
				res = c_ast::ctorCall(classType, args, loc);

				// add new call if required
				if(isOnHeap) {
					res = c_ast::newCall(res);
				} else if(!loc) {
					// if it is not an in-place construction => add a & operation
					res = c_ast::ref(res);
				}
			}

			// ---------------- destructor call -----------------

			if(funType->isDestructor()) {
				// obtain object
				vector<c_ast::NodePtr> args = c_call->arguments;
				assert_eq(args.size(), 1u);
				auto obj = args[0].as<c_ast::ExpressionPtr>();
				if(!irCallArgs[0].isa<core::InitExprPtr>() && core::lang::isPlainReference(irCallArgs[0])) obj = c_ast::deref(obj);

				// extract class type
				auto classType = context.getConverter().getTypeManager().getTypeInfo(context, core::analysis::getObjectType(funType)).lValueType;

				// create resulting call
				res = c_ast::dtorCall(classType, obj, false); // it is not a virtual destructor if it is explicitly mentioned
			}

			// --------------- member function call -------------

			// re-structure call in case it is a member function call
			if(funType->isMemberFunction()) {
				vector<c_ast::NodePtr> args = c_call->arguments;
				assert_false(args.empty());

				auto obj = args[0].as<c_ast::ExpressionPtr>();
				if(!irCallArgs[0].isa<core::InitExprPtr>() && core::lang::isPlainReference(irCallArgs[0])) obj = c_ast::deref(obj);
				args.erase(args.begin());

				res = c_ast::memberCall(obj, c_call->function, args);
			}

			// --------------- virtual member function call -------------

			// TODO
			if(funType->isVirtualMemberFunction()) {
				assert_not_implemented();
			}

			// use result
			return res;
		}

		core::CallExprPtr wrapPlainFunctionArguments(const core::CallExprPtr& call) {
			// extract node manager
			auto& manager = call->getNodeManager();
			core::IRBuilder builder(manager);

			// check whether there is a argument which is a vector but the parameter is not
			const core::TypePtr& type = call->getFunctionExpr()->getType();
			assert_eq(type->getNodeType(), core::NT_FunctionType) << "Function should be of a function type!";
			const core::FunctionTypePtr& funType = core::static_pointer_cast<const core::FunctionType>(type);

			const core::TypeList& paramTypes = funType->getParameterTypes()->getElements();
			const core::ExpressionList& args = call->getArgumentList();

			// check number of arguments
			if(paramTypes.size() != args.size()) {
				// => invalid call, don't touch this
				return call;
			}

			// generate new argument list
			bool changed = false;
			core::ExpressionList newArgs = args;
			for(unsigned i = 0; i < newArgs.size(); i++) {
				// get pair of types
				core::FunctionTypePtr paramType = paramTypes[i].isa<core::FunctionTypePtr>();
				core::FunctionTypePtr argType = newArgs[i]->getType().isa<core::FunctionTypePtr>();

				// ignore identical types or non-function types
				if(!paramType || !argType || *paramType == *argType) { continue; }

				// only interested if param is a bind and arg a function
				if(!(paramType->isClosure() && argType->isPlain())) { continue; }

				// create a bind wrapping the targeted function
				core::VariableList bindParams;
				core::ExpressionList argList;
				for(const core::TypePtr& type : argType->getParameterTypes()) {
					auto var = builder.variable(type);
					bindParams.push_back(var);
					argList.push_back(var);
				}

				// the argument needs to be wrapped into a bind
				const core::TypePtr& retType = argType->getReturnType();
				core::FunctionTypePtr newType = builder.functionType(argType->getParameterTypes(), retType, core::FK_CLOSURE);

				newArgs[i] = builder.bindExpr(newType, bindParams, builder.callExpr(retType, newArgs[i], argList));

				// note the change
				changed = true;
			}
			if(!changed) {
				// return original call
				return call;
			}

			// exchange arguments and done
			return builder.callExpr(call->getType(), call->getFunctionExpr(), newArgs);
		}

		// helpers for determining target types for parameter expression conversion
		core::TypeList materializeTypeList(const core::TypeList& types) {
			return ::transform(types, [](const core::TypePtr& typ) { return core::transform::materialize(typ); });
		};
		core::TypeList extractCallTypeList(const core::CallExprPtr& call) {
			return call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getParameterTypeList();
		};

		c_ast::NodePtr instantiateCall(const c_ast::NodePtr& innerCall, const std::vector<c_ast::TypePtr>& instantiationTypes) {
			auto mgr = innerCall->getManager();
			if(instantiationTypes.empty()) return innerCall;
			if(auto funCall = innerCall.isa<c_ast::CallPtr>()) {
				return c_ast::call(mgr->create<c_ast::ExplicitInstantiation>(funCall->function, instantiationTypes), funCall->arguments);
			}
			if(auto memCall = innerCall.isa<c_ast::MemberCallPtr>()) {
				return c_ast::memberCall(memCall->object, mgr->create<c_ast::ExplicitInstantiation>(memCall->memberFun, instantiationTypes),
				                         memCall->arguments);
			}
			assert_not_implemented();
			return {};
		}

		core::ExpressionPtr generateDummyLambda(const core::FunctionTypePtr funType, const core::VariableList params) {
			core::IRBuilder builder(funType.getNodeManager());
			const auto& basic = builder.getLangBasic();

			//auto body = builder.compoundStmt(builder.stringLit("DUMMY"));
			auto unit = basic.getUnit();
			auto callee = builder.literal("assert", builder.functionType(toVector<core::TypePtr>(basic.getInt4()), unit));
			annotations::c::attachInclude(callee, "assert.h");
			auto arg = builder.callExpr(basic.getBoolLAnd(), basic.getFalse(),
			                            builder.wrapLazy(builder.castExpr(basic.getBool(),builder.stringLit("This is an Insieme generated dummy function which should never be called"))));
			auto call = builder.callExpr(unit, callee, arg);

			return builder.lambdaExpr(funType, params, builder.compoundStmt(call));
		}

		// generate intercepted operator calls as operators in output code
		// also omit implicit conversion operator calls
		c_ast::NodePtr postprocessOperatorCalls(const c_ast::NodePtr& potentialOpCall, bool isImplicit) {
			// first, grab the information we need (name and arguments)
			// regardless of whether we are dealing with a function or a method
			string name;
			c_ast::ExpressionPtr left = nullptr;
			c_ast::ExpressionPtr right = nullptr;
			if(auto funCall = potentialOpCall.isa<c_ast::CallPtr>()) {
				if(auto ident = funCall->function.isa<c_ast::IdentifierPtr>()) {
					name = ident->name;
					if(funCall->arguments.size() > 0) left = funCall->arguments[0].isa<c_ast::ExpressionPtr>();
					if(funCall->arguments.size() > 1) right = funCall->arguments[1].isa<c_ast::ExpressionPtr>();
				}
			}
			if(auto memCall = potentialOpCall.isa<c_ast::MemberCallPtr>()) {
				if(auto ident = memCall->memberFun.isa<c_ast::IdentifierPtr>()) {
					name = ident->name;
					left = memCall->object.isa<c_ast::ExpressionPtr>();
					if(memCall->arguments.size() > 0) right = memCall->arguments[0].isa<c_ast::ExpressionPtr>();
				}
			}
			// gathered required data
			if(!left) return potentialOpCall; // early exit if we don't have any argument value
			// now, we handle the operators
			// first binary
			if(right) {
				if(boost::ends_with(name, "operator<<=")) return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseLeftShiftAssign, left, right);
				if(boost::ends_with(name, "operator>>=")) return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseRightShiftAssign, left, right);
				if(boost::ends_with(name, "operator->*")) return c_ast::binaryOp(c_ast::BinaryOperation::PointerToMember, left, right);
				if(boost::ends_with(name, "operator+="))  return c_ast::binaryOp(c_ast::BinaryOperation::AdditionAssign, left, right);
				if(boost::ends_with(name, "operator-="))  return c_ast::binaryOp(c_ast::BinaryOperation::SubtractionAssign, left, right);
				if(boost::ends_with(name, "operator*="))  return c_ast::binaryOp(c_ast::BinaryOperation::MultiplicationAssign, left, right);
				if(boost::ends_with(name, "operator/="))  return c_ast::binaryOp(c_ast::BinaryOperation::DivisionAssign, left, right);
				if(boost::ends_with(name, "operator%="))  return c_ast::binaryOp(c_ast::BinaryOperation::ModuloAssign, left, right);
				if(boost::ends_with(name, "operator^="))  return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseXOrAssign, left, right);
				if(boost::ends_with(name, "operator&="))  return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseAndAssign, left, right);
				if(boost::ends_with(name, "operator|="))  return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseOrAssign, left, right);
				if(boost::ends_with(name, "operator<<"))  return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseLeftShift, left, right);
				if(boost::ends_with(name, "operator>>"))  return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseRightShift, left, right);
				if(boost::ends_with(name, "operator=="))  return c_ast::binaryOp(c_ast::BinaryOperation::Equal, left, right);
				if(boost::ends_with(name, "operator!="))  return c_ast::binaryOp(c_ast::BinaryOperation::NotEqual, left, right);
				if(boost::ends_with(name, "operator<="))  return c_ast::binaryOp(c_ast::BinaryOperation::LessOrEqual, left, right);
				if(boost::ends_with(name, "operator>="))  return c_ast::binaryOp(c_ast::BinaryOperation::GreaterOrEqual, left, right);
				if(boost::ends_with(name, "operator&&"))  return c_ast::binaryOp(c_ast::BinaryOperation::LogicAnd, left, right);
				if(boost::ends_with(name, "operator||"))  return c_ast::binaryOp(c_ast::BinaryOperation::LogicOr, left, right);
				if(boost::ends_with(name, "operator+"))   return c_ast::binaryOp(c_ast::BinaryOperation::Additon, left, right);
				if(boost::ends_with(name, "operator-"))   return c_ast::binaryOp(c_ast::BinaryOperation::Subtraction, left, right);
				if(boost::ends_with(name, "operator*"))   return c_ast::binaryOp(c_ast::BinaryOperation::Multiplication, left, right);
				if(boost::ends_with(name, "operator/"))   return c_ast::binaryOp(c_ast::BinaryOperation::Division, left, right);
				if(boost::ends_with(name, "operator%"))   return c_ast::binaryOp(c_ast::BinaryOperation::Modulo, left, right);
				if(boost::ends_with(name, "operator^"))   return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseXOr, left, right);
				if(boost::ends_with(name, "operator&"))   return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseAnd, left, right);
				if(boost::ends_with(name, "operator|"))   return c_ast::binaryOp(c_ast::BinaryOperation::BitwiseOr, left, right);
				if(boost::ends_with(name, "operator="))   return c_ast::binaryOp(c_ast::BinaryOperation::Assignment, left, right);
				if(boost::ends_with(name, "operator<"))   return c_ast::binaryOp(c_ast::BinaryOperation::LessThan, left, right);
				if(boost::ends_with(name, "operator>"))   return c_ast::binaryOp(c_ast::BinaryOperation::GreaterThan, left, right);
				if(boost::ends_with(name, "operator,"))   return c_ast::binaryOp(c_ast::BinaryOperation::Comma, left, right);
			}
			// then, unary
			if(boost::ends_with(name, "operator!")) return c_ast::logicNot(left);
			if(boost::ends_with(name, "operator~")) return c_ast::bitwiseNot(left);

			// finally conversion
			if(isImplicit && boost::contains(name, "operator ")) return left;

			return potentialOpCall;
		}
	}

	const c_ast::NodePtr FunctionManager::getCall(ConversionContext& context, const core::CallExprPtr& in) {
		core::IRBuilder builder(context.getConverter().getNodeManager());
		bool isImplicit = insieme::annotations::c::isMarkedAsImplicit(in);

		// conduct some cleanup (argument wrapping)
		core::CallExprPtr call = wrapPlainFunctionArguments(in);

		// handle template calls
		if(builder.getLangBasic().isCallOfInstantiate(call->getFunctionExpr())) {
			auto typeInstCall = call->getFunctionExpr();
			auto targetType = core::analysis::getArgument(typeInstCall, 0)->getType();
			auto innerLit = core::analysis::getArgument(typeInstCall, 1).isa<core::LiteralPtr>();
			assert_true(innerLit) << "Non-intercepted template calls not implemented";
			auto replacementLit = builder.literal(innerLit->getValue(), targetType);
			core::transform::utils::migrateAnnotations(innerLit, replacementLit);
			call = builder.callExpr(typeInstCall->getType().as<core::FunctionTypePtr>()->getReturnType(), replacementLit, call->getArgumentList());
			isImplicit = isImplicit || insieme::annotations::c::isMarkedAsImplicit(call);
		}

		// extract target function
		core::ExpressionPtr fun = core::analysis::stripAttributes(call->getFunctionExpr());

		fun = builder.normalize(fun);

		// 1) see whether call is call to a known operator
		auto pos = operatorTable.find(fun);
		if(pos != operatorTable.end()) {
			// use operator converter
			return pos->second(context, call);
		}

		// 2) test whether target is a literal => external function, direct call
		if(fun->getNodeType() == core::NT_Literal) {
			// obtain literal information
			const FunctionInfo& info = getInfo(context, static_pointer_cast<const core::Literal>(fun));

			// produce call to external literal
			c_ast::CallPtr res = c_ast::call(info.function->name);
			appendAsArguments(context, res, materializeTypeList(extractCallTypeList(call)), call->getArgumentList());

			// add dependencies
			context.getDependencies().insert(info.prototype);

			// return external function call
			auto ret = handleMemberCall(context, call, res);
			ret = instantiateCall(ret, info.instantiationTypes);
			ret = postprocessOperatorCalls(ret, isImplicit);
			return ret;
		}

		// 3) test whether target is a lambda => call lambda directly, without creating a closure
		if(fun->getNodeType() == core::NT_LambdaExpr) {
			// obtain lambda information
			const LambdaInfo& info = getInfo(context, static_pointer_cast<const core::LambdaExpr>(fun));

			// add dependencies and requirements
			context.getDependencies().insert(info.prototype);
			context.getDependencies().insert(context.getConverter().getTypeManager().getTypeInfo(context, call->getType()).definition);
			context.getRequirements().insert(info.definition);

			// deal with different call mechanisms
			auto funType = fun.as<core::LambdaExprPtr>()->getFunctionType();

			// -------------- standard function call ------------

			// produce call to internal lambda
			c_ast::CallPtr c_call = c_ast::call(info.function->name);
			appendAsArguments(context, c_call, materializeTypeList(extractCallTypeList(call)), call->getArgumentList());

			// handle potential member calls
			auto ret = handleMemberCall(context, call, c_call);
			return ret;
		}

		core::FunctionTypePtr funType = static_pointer_cast<const core::FunctionType>(fun->getType());

		// 4) test whether target is a plain function pointer => call function pointer, no closure
		if(funType->isPlain()) {
			// add call to function pointer (which is the value)
			c_ast::CallPtr res = c_ast::call(c_ast::parentheses(getValue(context, call->getFunctionExpr())));
			appendAsArguments(context, res, materializeTypeList(extractCallTypeList(call)), call->getArgumentList());
			return res;
		}

		// 5) if is a member function pointer
		if(funType->isMemberFunction()) {
			// add call to function pointer (which is the value)

			// extract first parameter of the function, it is the target object
			c_ast::ExpressionPtr trgObj = converter.getStmtConverter().convertExpression(context, core::transform::extractInitExprFromDecl(call[0]));

			// make a call to the member pointer executor binary operator
			c_ast::ExpressionPtr funcExpr = c_ast::parentheses(c_ast::pointerToMember(trgObj, getValue(context, call->getFunctionExpr())));

			// the call is a call to the member function with the n-1 tail arguments
			c_ast::CallPtr res = c_ast::call(funcExpr);
			core::TypeList types = extractCallTypeList(call);
			types.erase(types.begin());
			core::ExpressionList args = call->getArgumentList();
			args.erase(args.begin());
			appendAsArguments(context, res, materializeTypeList(types), args);
			return res;
		}

		// TODO
		if(funType->isVirtualMemberFunction()) {
			assert_not_implemented();
		}

		// Finally: the generic fall-back solution:
		//		get function as a value and call it using the function-type's caller function

		c_ast::ExpressionPtr value = getValue(context, call->getFunctionExpr());

		const FunctionTypeInfo& typeInfo = converter.getTypeManager().getTypeInfo(context, funType);
		c_ast::CallPtr res = c_ast::call(typeInfo.callerName, c_ast::cast(typeInfo.rValueType, value));
		appendAsArguments(context, res, materializeTypeList(extractCallTypeList(call)), call->getArgumentList());

		// add dependencies
		context.getDependencies().insert(typeInfo.caller);

		return res;
	}


	const c_ast::ExpressionPtr FunctionManager::getValue(ConversionContext& context, const core::ExpressionPtr& fun) {
		auto manager = converter.getCNodeManager();

		// handle according to node type
		switch(fun->getNodeType()) {
		case core::NT_BindExpr: {
			return getValue(context, static_pointer_cast<const core::BindExpr>(fun));
		}
		case core::NT_Literal: {
			const FunctionInfo& info = getInfo(context, static_pointer_cast<const core::Literal>(fun));
			if(static_pointer_cast<const core::FunctionType>(fun->getType())->isPlain()) {
				context.getDependencies().insert(info.prototype);
				return c_ast::ref(info.function->name);
			}
			context.getDependencies().insert(info.lambdaWrapper);
			return c_ast::ref(info.lambdaWrapperName);
		}
		case core::NT_LambdaExpr: {
			const LambdaInfo& info = getInfo(context, static_pointer_cast<const core::LambdaExpr>(fun));
			context.getDependencies().insert(info.prototype);

			// FIXME: hack to support member function pointers intialization
			c_ast::NodePtr thing = static_pointer_cast<c_ast::CCodeFragment>(info.definition)->getCode()[1];
			if(c_ast::MemberFunctionPtr mem = thing.isa<c_ast::MemberFunctionPtr>()) { return c_ast::ref(c_ast::scope(mem->className, info.function->name)); }

			return c_ast::ref(info.function->name);
		}
		case core::NT_Variable:
		case core::NT_CallExpr: {
			// variable is already representing a value
			return converter.getStmtConverter().convertExpression(context, fun);
		}
		case core::NT_CastExpr: {
			// function pointer casted to a different type of function pointer
			return converter.getStmtConverter().convertExpression(context, fun.as<core::CastExprPtr>());
		}
		default:
			LOG(FATAL) << "Encountered unsupported node: " << *fun;
			assert_fail() << "Unexpected Node Type!";
			return c_ast::ExpressionPtr();
		}
	}

	const c_ast::ExpressionPtr FunctionManager::getValue(ConversionContext& context, const core::BindExprPtr& bind) {
		auto manager = converter.getCNodeManager();

		// create a value instance by initializing the bind closure using its constructor

		// collect some information
		const BindInfo& info = getInfo(context, bind);
		const FunctionTypeInfo& typeInfo = converter.getTypeManager().getTypeInfo(context, static_pointer_cast<const core::FunctionType>(bind->getType()));

		// add dependencies
		c_ast::FragmentSet& dependencies = context.getDependencies();
		dependencies.insert(typeInfo.definition);
		dependencies.insert(typeInfo.constructor);
		dependencies.insert(info.definitions);


		// allocate memory for the bind expression
		context.getIncludes().insert("alloca.h");
		c_ast::ExpressionPtr alloc =
		    c_ast::cast(c_ast::ptr(info.closureType), c_ast::call(manager->create("alloca"), c_ast::unaryOp(c_ast::UnaryOperation::SizeOf, info.closureType)));

		// pre-process target function
		auto fun = bind->getCall()->getFunctionExpr();

		// instantiate generic lambdas if necessary
		if(fun.isa<core::LambdaExprPtr>()) {
			// extract node manager
			auto& mgr = bind->getNodeManager();

			// get type variable substitution for call
			core::types::SubstitutionOpt&& map = core::types::getTypeVariableInstantiation(mgr, bind->getCall());

			// instantiate function expression
			fun = core::transform::instantiate(mgr, fun.as<core::LambdaExprPtr>(), map);
		}

		// create nested closure
		c_ast::ExpressionPtr nested = getValue(context, fun);

		// create constructor call
		c_ast::CallPtr res = c_ast::call(info.constructorName, alloc, nested);

		// add captured expressions
		auto boundExpressions = bind->getBoundExpressions();
		auto emptyTypes = ::transform(boundExpressions, [](const core::NodePtr&) { return core::TypePtr(); });
		// TODO do we need correct target types for bind expression arguments?
		appendAsArguments(context, res, emptyTypes, boundExpressions);

		// done
		return res;
	}

	const boost::optional<string> FunctionManager::getHeaderFor(const string& function) const {
		// try looking up function within the include table
		auto pos = includeTable.find(function);
		if(pos != includeTable.end()) { return pos->second; }
		// not found => return empty optional
		return boost::optional<string>();
	}

	namespace {

		const boost::optional<string> internalGetHeaderFor(const core::ExpressionPtr& function) {

			// check whether there is a annotated header
			if(annotations::c::hasIncludeAttached(function)) { return annotations::c::getAttachedInclude(function); }

			// otherwise there is no header ..
			return {};
		}

	}

	const boost::optional<string> FunctionManager::getHeaderFor(const core::LiteralPtr& function) const {
		// include table has priority
		auto res = getHeaderFor(function->getStringValue());
		if(res) { return res; }

		// use annotation
		return internalGetHeaderFor(function);
	}

	const boost::optional<string> FunctionManager::getHeaderFor(const core::LambdaExprPtr& function) const {
		return internalGetHeaderFor(function);
	}

	namespace detail {

		core::FunctionTypePtr getFunctionType(ConversionContext& context, const core::NodePtr& node) {

			// for expressions get the the type and interpret as a function type
			if (auto expr = node.isa<core::ExpressionPtr>()) {
				return expr->getType().as<core::FunctionTypePtr>();
			}

			// extract type of a member function node
			if (auto memFun = node.isa<core::MemberFunctionPtr>()) {
				return memFun->getImplementation()->getType().as<core::FunctionTypePtr>();
			}

			// extract type from a pure virtual function
			if (auto pureVirtual = node.isa<core::PureVirtualMemberFunctionPtr>()) {
				return pureVirtual->getType();
			}

			// unsupported case
			assert_fail() << "Unsupported function type: " << node->getNodeType() << "\n";
			return core::FunctionTypePtr();
		}


		ElementInfo* FunctionInfoStore::resolveInternal(ConversionContext& context, const core::NodePtr& fun) {

			// normalize all functions to avoid duplicates
			core::NodePtr function = core::analysis::normalize(fun);

			// lookup information within store
			auto pos = funInfos.find(function);
			if (pos != funInfos.end()) { return pos->second; }

			// copy name for normalized version
			auto& nameManager = converter.getNameManager();
			nameManager.setName(function, nameManager.getName(fun));

			// resolve all parameter types and the return type
			auto funType = getFunctionType(context, function);
			for(const auto& param : funType->getParameterTypes()) {
				converter.getTypeManager().getTypeInfo(context, param);
			}
			converter.getTypeManager().getTypeInfo(context, funType->getReturnType());

			// check whether the function has been resolved as a side-effect of resolving the types
			pos = funInfos.find(function);
			if (pos != funInfos.end()) { return pos->second; }

			// obtain function information
			ElementInfo* info;

			// not known yet => requires some resolution
			switch(function->getNodeType()) {
			case core::NT_Literal: info = resolveLiteral(context, function.as<core::LiteralPtr>()); break;
			case core::NT_LambdaExpr: info = resolveLambda(context, function.as<core::LambdaExprPtr>()); break;
			case core::NT_BindExpr: info = resolveBind(context, function.as<core::BindExprPtr>()); break;
			case core::NT_MemberFunction: info = resolveMemberFunction(context, function.as<core::MemberFunctionPtr>()); break;
			case core::NT_PureVirtualMemberFunction: info = resolvePureVirtualMember(context, function.as<core::PureVirtualMemberFunctionPtr>()); break;
			default:
				// this should not happen ...
				assert_fail() << "Unsupported node type encountered!";
				return new ElementInfo();
			}

			// store information
			funInfos.insert(std::make_pair(function, info));

			// return pointer to obtained information
			return info;
		}

		ElementInfo* FunctionInfoStore::resolveInternal(ConversionContext& context, const core::TagTypePtr& tagType, const core::LambdaExprPtr& fun) {

			// resolve element
			ElementInfo* info = resolveInternal(context, fun);

			// attach result also to peeled version for this function
			auto peeled = tagType->peel(fun);
			funInfos.insert({core::analysis::normalize(peeled),info});

			// done
			return info;
		}

		ElementInfo* FunctionInfoStore::resolveInternal(ConversionContext& context, const core::TagTypePtr& tagType, const core::MemberFunctionPtr& member) {

			// resolve element
			ElementInfo* info = resolveInternal(context, member);

			// attach result also to peeled version for this function
			auto peeled = tagType->peel(member->getImplementation());
			funInfos.insert({core::analysis::normalize(peeled),info});

			// done
			return info;
		}


		FunctionInfo* FunctionInfoStore::resolveLiteral(ConversionContext& context, const core::LiteralPtr& literal) {
			assert_eq(literal->getType()->getNodeType(), core::NT_FunctionType) << "Only supporting literals with a function type!";

			// some preparation
			auto manager = converter.getCNodeManager();
			core::FunctionTypePtr funType = static_pointer_cast<const core::FunctionType>(literal->getType());
			FunctionInfo* res = new FunctionInfo();

			TypeManager& typeManager = converter.getTypeManager();

			// ------------------------ resolve function ---------------------

			std::string name = insieme::utils::demangleToIdentifier(literal->getStringValue());
			if(core::annotations::hasAttachedName(literal)) name = core::annotations::getAttachedName(literal);
			FunctionCodeInfo fun = resolveFunction(context, manager->create(name), funType, core::LambdaPtr());
			res->function = fun.function;

			// ------------------------ add prototype -------------------------

			auto header = converter.getFunctionManager().getHeaderFor(literal);
			if(header) {
				// => use prototype of include file
				res->prototype = c_ast::IncludeFragment::createNew(converter.getFragmentManager(), *header);
				res->declaration = nullptr;

			} else if(funType->isMemberFunction()) {

				// add pure-virtual member function to class declaration
				const auto& typeInfo = typeManager.getTypeInfo(context, core::analysis::getObjectType(funType));
				res->prototype = typeInfo.definition;
				res->prototype->addDependencies(fun.prototypeDependencies);

				// parse reference type
				core::lang::ReferenceType refType(funType->getParameterType(0));

				// add declaration of pure-virtual function
				c_ast::StructTypePtr classDecl = typeInfo.lValueType.as<c_ast::StructTypePtr>();
				auto mFun = manager->create<c_ast::MemberFunction>(classDecl->name, fun.function, refType.isConst(), refType.isVolatile());
				auto decl = manager->create<c_ast::MemberFunctionPrototype>(mFun, true, true);
				res->declaration = decl;
				classDecl->members.push_back(decl);

				//TODO
			} else if(funType->isVirtualMemberFunction()) {
				assert_not_implemented();

			} else if(literal->getStringValue().substr(0, 6) == "__sync") {
				// => ignore built-in atomic operations

			} else {
				// => add prototype for this literal
				c_ast::TopLevelElementPtr code = manager->create<c_ast::FunctionPrototype>(fun.function);
				if(annotations::c::isExternC(literal)) { code = manager->create<c_ast::ExternC>(code); }
				res->prototype = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), code);
				res->prototype->addDependencies(fun.prototypeDependencies);
				res->declaration = code;
			}

			// -------------------------- add lambda wrapper ---------------------------

			auto wrapper = resolveLambdaWrapper(context, fun.function, funType);
			res->lambdaWrapperName = wrapper.first;
			res->lambdaWrapper = wrapper.second;
			res->lambdaWrapper->addDependencies(fun.prototypeDependencies);
			res->lambdaWrapper->addDependency(res->prototype);

			// -------------------------- add instantiation types if they are there --------------------------

			for(auto instantiationType : funType->getInstantiationTypes()) {
				auto typeArg = typeManager.getTemplateArgumentType(context, instantiationType);
				res->instantiationTypes.push_back(typeArg);
				// if argument type is not intercepted, add a dependency on its definition
				auto tempParamTypeDef = typeManager.getDefinitionOf(typeArg);
				if(tempParamTypeDef) res->prototype->addDependency(tempParamTypeDef);
			}

			// done
			return res;
		}

		FunctionInfo* FunctionInfoStore::resolvePureVirtualMember(ConversionContext& context, const core::PureVirtualMemberFunctionPtr&  pureVirtualMemberFun) {

			// create a literal of the proper type and resolve the literal
			core::IRBuilder builder(pureVirtualMemberFun->getNodeManager());
			return resolve(context, builder.literal(pureVirtualMemberFun->getName(), pureVirtualMemberFun->getType()));

		}

		LambdaInfo* FunctionInfoStore::resolveLambda(ConversionContext& context, const core::LambdaExprPtr& lambda) {

			// test whether the lambda is an intercepted lambda, modeled by a derived operator
			auto header = converter.getFunctionManager().getHeaderFor(lambda);
			if(header) {
				LambdaInfo* res = new LambdaInfo();

				auto manager = converter.getCNodeManager();
				auto name = context.getConverter().getNameManager().getName(lambda);
				auto funType = lambda->getType();
				FunctionCodeInfo fun = resolveFunction(context, manager->create(name), funType, core::LambdaPtr());
				res->function = fun.function;

				// => use prototype of include file
				res->prototype = c_ast::IncludeFragment::createNew(converter.getFragmentManager(), *header);
				res->declaration = nullptr;
				return res;
			}

			// resolve lambda definitions
			resolveLambdaDefinition(context, lambda->getDefinition());

			// look up lambda again
			return resolve(context, lambda);
		}

		LambdaInfo* FunctionInfoStore::resolveMemberFunction(ConversionContext& context, const core::MemberFunctionPtr& memberFun) {

			auto impl = memberFun->getImplementation();

			// generate dummy lambda in cases of members without implementation (literals)
			if(auto lit = impl.isa<core::LiteralPtr>()) {
				core::IRBuilder builder(memberFun->getNodeManager());
				auto funType = impl->getType().as<core::FunctionTypePtr>();
				core::VariableList params = ::transform(funType->getParameterTypeList(), [&](const core::TypePtr t) {
					return builder.variable(core::transform::materialize(t));
				});
				impl = generateDummyLambda(funType, params);
			}

			// fix name
			c_ast::CodeFragmentPtr nameDependency;
			auto name = utils::demangleToIdentifier(memberFun->getNameAsString());
			// for user-defined conversion operators, we need to generate their type representation from the actual return type,
			// because function pointer types need to be typedef'd
			if(boost::starts_with(name, "operator ")) {
				auto retTypeInfo = converter.getTypeManager().getTypeInfo(context, impl->getType().as<core::FunctionTypePtr>()->getReturnType());
				name = format("operator %s", *retTypeInfo.rValueType);
				nameDependency = retTypeInfo.declaration;
			}
			converter.getNameManager().setName(impl, name);

			// convert implementation
			auto res = resolve(context, impl.as<core::LambdaExprPtr>());

			// correct virtual flag
			c_ast::MemberFunctionPrototypePtr decl = res->declaration.as<c_ast::MemberFunctionPrototypePtr>();
			decl->isVirtual = memberFun->isVirtual();

			// check for default members
			if(core::analysis::isaDefaultAssignment(memberFun)) {
				// set declaration to default
				decl->flag = c_ast::BodyFlag::Default;

				// delete definition by removing the prototypes requirement towards the definition
				res->prototype->remRequirement(res->definition);
			}

			// add potentially required dependency for the name
			if(nameDependency) res->prototype->addDependency(nameDependency);

			// done
			return res;
		}

		BindInfo* FunctionInfoStore::resolveBind(ConversionContext& context, const core::BindExprPtr& bind) {
			// prepare some manager
			NameManager& nameManager = converter.getNameManager();
			TypeManager& typeManager = converter.getTypeManager();
			auto manager = converter.getCNodeManager();

			// create resulting data container
			BindInfo* res = new BindInfo();

			// set up names
			string name = nameManager.getName(bind, "bind");
			res->closureName = manager->create(name + "_closure");
			res->mapperName = manager->create(name + "_mapper");
			res->constructorName = manager->create(name + "_ctr");

			// instantiate nested call
			auto call = bind->getCall();

			// instantiate generic lambdas if necessary
			if(auto fun = call->getFunctionExpr().isa<core::LambdaExprPtr>()) {
				// extract node manager
				auto& mgr = bind->getNodeManager();

				// get type variable substitution for call
				core::types::SubstitutionOpt&& map = core::types::getTypeVariableInstantiation(mgr, bind->getCall());

				// instantiate function expression
				fun = core::transform::instantiate(mgr, fun.as<core::LambdaExprPtr>(), map);

				// replace call with call to instantiated function
				call = core::IRBuilder(call->getNodeManager()).callExpr(call->getType(), fun, call->getArgumentList());
			}

			// create a map between expressions in the IR and parameter / captured variable names in C
			utils::map::PointerMap<core::ExpressionPtr, c_ast::VariablePtr> variableMap;

			// add parameters
			int paramCounter = 0;
			const vector<core::VariablePtr>& parameter = bind->getParameters()->getElements();
			for_each(parameter, [&](const core::VariablePtr& cur) {
				variableMap[cur] = var(typeManager.getTypeInfo(context, cur->getType()).rValueType, format("p%d", ++paramCounter));
			});

			// add arguments of call
			int argumentCounter = 0;
			const vector<core::ExpressionPtr>& args = call->getArgumentList();
			for_each(args, [&](const core::ExpressionPtr& cur) {
				variableMap[cur] = var(typeManager.getTypeInfo(context, cur->getType()).rValueType, format("c%d", ++argumentCounter));
			});

			// extract captured variables
			vector<core::ExpressionPtr> captured = bind->getBoundExpressions();

			vector<c_ast::VariablePtr> varsCaptured;
			::transform(captured, std::back_inserter(varsCaptured), [&](const core::ExpressionPtr& cur) { return variableMap[cur]; });


			// ----------- define closure type ---------------

			// create closure struct
			c_ast::StructTypePtr closureStruct = manager->create<c_ast::StructType>(res->closureName);

			// get function type of mapper
			core::FunctionTypePtr funType = static_pointer_cast<const core::FunctionType>(bind->getType());
			const FunctionTypeInfo& funInfo = typeManager.getTypeInfo(context, funType);

			// construct variable / struct entry pointing to the function to be called when processing the closure
			c_ast::FunctionTypePtr mapperType = manager->create<c_ast::FunctionType>(typeManager.getTypeInfo(context, funType->getReturnType()).rValueType);
			mapperType->parameterTypes.push_back(manager->create<c_ast::PointerType>(closureStruct));
			for_each(parameter,
			         [&](const core::VariablePtr& var) { mapperType->parameterTypes.push_back(typeManager.getTypeInfo(context, var->getType()).rValueType); });
			c_ast::VariablePtr varCall = c_ast::var(manager->create<c_ast::PointerType>(mapperType), "call");

			// get generic type of nested closure
			core::FunctionTypePtr nestedFunType = static_pointer_cast<const core::FunctionType>(call->getFunctionExpr()->getType());
			const FunctionTypeInfo& nestedClosureInfo = typeManager.getTypeInfo(context, nestedFunType);

			// define variable / struct entry pointing to the nested closure variable
			c_ast::TypePtr varNestedType = nestedClosureInfo.rValueType;
			if (nestedFunType.isPlain()) varNestedType = c_ast::ptr(varNestedType);
			c_ast::VariablePtr varNested = c_ast::var(varNestedType, "nested");

			// finally, add fields to struct
			closureStruct->elements.push_back(varCall);
			closureStruct->elements.push_back(varNested);
			addAll(closureStruct->elements, varsCaptured);

			c_ast::NodePtr closureDecl = manager->create<c_ast::TypeDeclaration>(closureStruct);
			c_ast::NodePtr closureDef = manager->create<c_ast::TypeDefinition>(closureStruct);
			res->closureType = manager->create<c_ast::NamedType>(res->closureName);


			// --------------------------------- define mapper -------------------------------------
			c_ast::VariablePtr varClosure = var(manager->create<c_ast::PointerType>(res->closureType), "closure");

			c_ast::FunctionPtr mapper;
			c_ast::FunctionPrototypePtr mapperDecl;
			{
				bool plain = nestedFunType->isPlain();
				c_ast::TypePtr returnType = mapperType->returnType;

				vector<c_ast::VariablePtr> params;
				params.push_back(varClosure);
				::transform(bind->getParameters()->getElements(), std::back_inserter(params), [&](const core::VariablePtr& cur) { return variableMap[cur]; });

				c_ast::ExpressionPtr fun = indirectAccess(varClosure, "nested");
				if(!plain) { fun = indirectAccess(fun, "call"); }

				c_ast::CallPtr call = manager->create<c_ast::Call>(fun);
				if(!plain) { call->arguments.push_back(indirectAccess(varClosure, "nested")); }

				for_each(args, [&](const core::ExpressionPtr& cur) {
					c_ast::VariablePtr var = variableMap[cur];
					c_ast::ExpressionPtr param = var;
					if(contains(captured, cur, equal_target<core::ExpressionPtr>())) { param = indirectAccess(varClosure, var->name); }
					call->arguments.push_back(param);
				});

				c_ast::StatementPtr body = call;
				if(!isVoid(returnType)) { body = manager->create<c_ast::Return>(call); }

				mapper = manager->create<c_ast::Function>(returnType, res->mapperName, params, body);
				mapperDecl = manager->create<c_ast::FunctionPrototype>(mapper);
			}

			// --------------------------------- define constructor -------------------------------------

			c_ast::NodePtr constructor;
			{
				// the constructor collects captured variables and a pointer to a pre-allocated closure struct
				// and initializes all the closure's fields.

				// create return type
				c_ast::TypePtr returnType = funInfo.rValueType;

				// assemble parameters
				vector<c_ast::VariablePtr> params;
				params.push_back(varClosure);
				params.push_back(varNested);
				addAll(params, varsCaptured);

				// create the body
				c_ast::InitializerPtr init = c_ast::init(res->closureType, c_ast::ref(res->mapperName), varNested);
				addAll(init->values, varsCaptured);
				init->target = varClosure;
				c_ast::StatementPtr body = compound(init, c_ast::ret(c_ast::cast(returnType, varClosure)));

				// assemble constructor
				constructor =
				    manager->create<c_ast::Function>(c_ast::Function::STATIC | c_ast::Function::INLINE, returnType, res->constructorName, params, body);
			}

			// attach definitions of closure, mapper declaration and constructor
			res->definitions = c_ast::CCodeFragment::createNew(
			    converter.getFragmentManager(),
			    manager->create<c_ast::Comment>("-- Begin - Bind Constructs ------------------------------------------------------------"), closureDecl,
			    closureDef, mapperDecl, constructor,
			    manager->create<c_ast::Comment>("--  End  - Bind Constructs ------------------------------------------------------------")
			);

			res->definitions->addDependency(funInfo.declaration);
			res->definitions->addDependency(nestedClosureInfo.definition);
			res->definitions->addDependency(nestedClosureInfo.caller);

			// create code fragment for mapper implementation
			auto mapperDef = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), mapper);
			mapperDef->addDependency(res->definitions);
			res->definitions->addRequirement(mapperDef);

			// make the mapper definition dependent on the parameter type definitions
			for(const auto& param : parameter) {
				mapperDef->addDependency(typeManager.getTypeInfo(context, param->getType()).definition);
			}

			// finally - add a dependency to the return type definition since it is returned by value
			res->definitions->addDependency(typeManager.getTypeInfo(context, call->getType()).definition);

			// done
			return res;
		}

		void FunctionInfoStore::resolveLambdaDefinition(ConversionContext& context, const core::LambdaDefinitionPtr& lambdaDefinition) {
			// prepare some manager
			NameManager& nameManager = converter.getNameManager();
			core::NodeManager& manager = converter.getNodeManager();
			auto& cManager = converter.getCNodeManager();
			auto& fragmentManager = converter.getFragmentManager();
			auto& typeManager = converter.getTypeManager();

			// create definition and declaration block
			c_ast::CCodeFragmentPtr declarations = c_ast::CCodeFragment::createNew(fragmentManager);
			c_ast::CCodeFragmentPtr definitions = c_ast::CCodeFragment::createNew(fragmentManager);

			// add requirement for definition once been declared
			declarations->addRequirement(definitions);

			declarations->appendCode(cManager->create<c_ast::Comment>("------- Function Prototypes ----------"));
			definitions->appendCode(cManager->create<c_ast::Comment>("------- Function Definitions ---------"));
			definitions->appendCode(converter.convertToComment(lambdaDefinition));

			// A) get list of all lambdas within this recursive group
			vector<std::pair<c_ast::IdentifierPtr, core::LambdaExprPtr>> lambdas;

			::transform(lambdaDefinition->getDefinitions(), std::back_inserter(lambdas),
			            [&](const core::LambdaBindingPtr& cur) -> std::pair<c_ast::IdentifierPtr, core::LambdaExprPtr> {
				            auto lambda = core::LambdaExpr::get(manager, cur->getReference(), lambdaDefinition);
				            return std::make_pair(cManager->create(nameManager.getName(lambda)), lambda);
				        });

			// B) create an entries within info table containing code fragments, wrappers and prototypes
			for(const std::pair<c_ast::IdentifierPtr, core::LambdaExprPtr>& pair: lambdas) {

				const c_ast::IdentifierPtr& name = pair.first;
				const core::LambdaExprPtr& lambda = pair.second;

				auto funType = lambda->getFunctionType();
				bool isMember = funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction() || funType->isVirtualMemberFunction();

				// create information
				LambdaInfo* info = new LambdaInfo();
				info->prototype = declarations;
				info->definition = definitions;

				// member functions are declared within object definition
				core::TypePtr classType;
				c_ast::NamedCompositeTypePtr classDecl;
				if(isMember) {
					classType = core::analysis::getObjectType(funType);
					const auto& typeInfo = typeManager.getTypeInfo(context, classType);
					info->prototype = typeInfo.definition;
					classDecl = typeInfo.lValueType.as<c_ast::NamedCompositeTypePtr>();
					// add requirement of implementation
					info->prototype->addRequirement(info->definition);
				}

				// if not member and not recursive, skip prototype
				if(!isMember && !lambda->isRecursive()) {
					info->prototype = definitions;
				} else {
					definitions->addDependency(info->prototype);
				}

				// create dummy function ... no body
				core::LambdaPtr body;
				FunctionCodeInfo codeInfo = resolveFunction(context, name, funType, body);
				info->function = codeInfo.function;

				auto wrapper = resolveLambdaWrapper(context, codeInfo.function, funType);
				info->lambdaWrapperName = wrapper.first;
				info->lambdaWrapper = wrapper.second;
				info->lambdaWrapper->addDependency(info->prototype);
				info->lambdaWrapper->addRequirement(info->definition);

				// obtain current lambda and add lambda info
				auto res = funInfos.insert(std::make_pair(lambda, info));

				// if this info is new the same function has been handled while resolving the body
				if(!res.second) {
					// fun info was already there - delete local copy and be done
					delete info;
					return;
				}

				// add prototype ...
				if(isMember) {
					// add declaration
					if(funType.isConstructor()) {
						// add constructor
						auto ctor = cManager->create<c_ast::Constructor>(classDecl->name, info->function);
						c_ast::BodyFlag flag = (core::analysis::isaDefaultConstructor(lambda) ? c_ast::BodyFlag::Default : c_ast::BodyFlag::None);
						auto decl = cManager->create<c_ast::ConstructorPrototype>(ctor, flag);
						classDecl->ctors.push_back(decl);
						info->declaration = decl;
					} else if(funType.isDestructor()) {
						// add destructor
						assert_false(classDecl->dtor) << "Destructor already defined!";
						bool defaultDtor = core::analysis::isaDefaultDestructor(lambda);
						c_ast::BodyFlag flag = defaultDtor ? c_ast::BodyFlag::Default : c_ast::BodyFlag::None;
						auto dtor = cManager->create<c_ast::Destructor>(classDecl->name, info->function);
						auto decl = cManager->create<c_ast::DestructorPrototype>(dtor, flag);
						classDecl->dtor = decl;
						info->declaration = decl;
					} else if(funType.isVirtualMemberFunction()) {
						//TODO
						assert_not_implemented();
					} else {
						// add member function
						assert_true(funType.isMemberFunction());
						auto mfun = cManager->create<c_ast::MemberFunction>(classDecl->name, info->function);
						c_ast::BodyFlag flag = c_ast::BodyFlag::None;
						if (core::analysis::isaDefaultMember(lambda)) {
							flag = c_ast::BodyFlag::Default;
						}
						auto decl = cManager->create<c_ast::MemberFunctionPrototype>(mfun, flag);

						// add cv modifier
						auto thisRef = core::lang::ReferenceType(funType->getParameterType(0));
						mfun->isConstant = thisRef.isConst();
						mfun->isVolatile = thisRef.isVolatile();

						classDecl->members.push_back(decl);
						info->declaration = decl;
					}

					// remove dependencies from others to this class (causes cyclic dependencies)
					for(const auto& dep : codeInfo.prototypeDependencies) {
						if(dep.isa<c_ast::IncludeFragmentPtr>()) { dep->remDependency(info->prototype); }
					}

					// add dependencies to class declaration
					info->prototype->addDependencies(codeInfo.prototypeDependencies);

					// add includes
					info->prototype->addIncludes(codeInfo.includes);

				} else {
					// ... to prototype block
					declarations->getCode().push_back(cManager->create<c_ast::FunctionPrototype>(codeInfo.function));
				}

				// import dependency from resolved code fragment
				declarations->addDependencies(codeInfo.prototypeDependencies);

				// add includes
				declarations->addIncludes(codeInfo.includes);
			}

			// C) create function definitions
			for(const std::pair<c_ast::IdentifierPtr, core::LambdaExprPtr>& pair : lambdas) {

				const c_ast::IdentifierPtr& name = pair.first;
				const core::LambdaExprPtr& lambda = pair.second;

				// skip default implementations
				auto funType = lambda->getFunctionType();
				if(core::analysis::isaDefaultMember(lambda)) {
					return;
				}

				// peel function and create function definition
				core::LambdaExprPtr unrolled = lambdaDefinition->peel(manager, lambda->getReference());
				assert_false(unrolled->isRecursive()) << "Peeled function must not be recursive!";

				// resolve function ... now with body
				FunctionCodeInfo codeInfo = resolveFunction(context, name, funType, unrolled->getLambda());

				// add function
				LambdaInfo* info = static_cast<LambdaInfo*>(funInfos[lambda]);
				info->function = codeInfo.function;

				// add definition to definition block
				definitions->getCode().push_back(codeInfo.definition);

				// add code dependencies
				definitions->addDependencies(codeInfo.definitionDependencies);

				// add includes
				definitions->addIncludes(codeInfo.includes);
			}
		}

		namespace {

			core::NodePtr getAccessedField(const core::VariablePtr& thisVar, const core::ExpressionPtr& candidate) {
				static const core::NodePtr NO_ACCESS;

				// check whether it is accessing an element
				if(candidate->getNodeType() != core::NT_CallExpr) { return NO_ACCESS; }
				core::CallExprPtr call = candidate.as<core::CallExprPtr>();

				// check whether it is a field access
				const auto& refs = thisVar->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
				if(core::analysis::isCallOf(call, refs.getRefAssign())) {
					core::ExpressionPtr target = call->getArgument(0);
					if(auto refAccess = refs.isCallOfRefMemberAccess(target)) {
						// check whether it is accessing this
						if(auto deref = refs.isCallOfRefDeref(refAccess[0])) {
							if(deref[0] != thisVar) { return NO_ACCESS; }

							// extract identifier name
							return refAccess[1];
						}
					}
				}

				// check whether it is a super-constructor call
				auto funType = call->getFunctionExpr()->getType().as<core::FunctionTypePtr>();
				if(funType->isConstructor()) {
					auto target = call->getArgument(0);

					// test whether argument is this (super-constructor call)
					if(auto deref = refs.isCallOfRefDeref(target)) {
						if(deref[0] == thisVar) { return core::analysis::getObjectType(funType); }
					}

					// test whether argument is a member (member initializer)
					if(auto refAccess = refs.isCallOfRefMemberAccess(target)) {
						// check whether it is accessing this
						if(auto deref = refs.isCallOfRefDeref(refAccess[0])) {
							if(deref[0] != thisVar) { return NO_ACCESS; }

							// extract identifier name
							return refAccess[1];
						}
					}
				}

				return NO_ACCESS;
			}

			bool valuesDerivedFromParametersOnly(const core::VariablePtr& thisVar, const core::VariableList& params, const core::CallExprPtr& call) {
				assert_true(getAccessedField(thisVar, call)) << "not an access!";

				// collect values
				core::ExpressionList values;
				{
					// in case it is an assignment
					const auto& refExt = call->getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
					if(core::analysis::isCallOf(call, refExt.getRefAssign())) {
						values.push_back(call->getArgument(1)); // that is the value
					} else {
						// it is a constructor call => collect all arguments but the first
						auto args = call->getArgumentList();
						values.insert(values.end(), args.begin() + 1, args.end());
					}
				}

				// check variables within values
				bool parametersOnly = true;

				// build up a checker
				auto check = core::makeCachedLambdaVisitor([&](const core::NodePtr& cur) -> bool {
					static const bool PRUNE = true;
					static const bool CONTINUE = false;

					// see whether a problem has been found before
					if(!parametersOnly) return PRUNE;

					// do not enter nested scopes
					if(cur.getNodeType() == core::NT_LambdaExpr) return PRUNE;

					// only interested in variables
					if(cur.getNodeType() != core::NT_Variable) return CONTINUE;

					// check the variable
					auto curVar = cur.as<core::VariablePtr>();
					// we want only parameters, but make an exception if the thisVar is used
					if(!contains(params, curVar) && (curVar != thisVar)) {
						parametersOnly = false;
						return PRUNE;
					}

					// no problem, continue search
					return CONTINUE;
				}, false);

				// check all the values
				for(const auto& cur : values) {
					if(parametersOnly) { core::visitDepthFirstOncePrunable(cur, check); }
				}

				return parametersOnly;
			}


			struct FirstWriteCollector : public core::IRVisitor<void, core::Address, const core::VariablePtr&, const core::VariableList&, core::NodeSet&,
			                                                    std::vector<core::StatementAddress>&, bool> {
				std::vector<core::StatementAddress> collect(const core::VariablePtr& thisVar, const core::VariableList& params,
				                                            const core::CompoundStmtAddress& body) {
					// prepare context information
					core::NodeSet touched;
					std::vector<core::StatementAddress> res;

					// use visitor infrastructure
					visit(body, thisVar, params, touched, res, false);

					// return result list
					return res;
				}

				void visitCompoundStmt(const core::CompoundStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                       core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, iterating);
				}

				void visitIfStmt(const core::IfStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params, core::NodeSet& touched,
				                 std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, iterating);
				}

				void visitSwitchStmt(const core::SwitchStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                     core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, iterating);
				}

				void visitSwitchCases(const core::SwitchCasesAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                      core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, iterating);
				}

				void visitSwitchCase(const core::SwitchCaseAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                     core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, iterating);
				}

				void visitForStmt(const core::ForStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params, core::NodeSet& touched,
				                  std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, true);
				}

				void visitWhileStmt(const core::WhileStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                    core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// iterate through sub-statements
					visitAll(cur->getChildList(), thisVar, params, touched, res, true);
				}

				void visitDeclarationStmt(const core::DeclarationStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                          core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// we can stop here
				}

				void visitTryCatchStmt(const core::TryCatchStmtAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                       core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// we can stop here
				}

				void visitCallExpr(const core::CallExprAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params, core::NodeSet& touched,
				                   std::vector<core::StatementAddress>& res, bool iterating) {
					// extract field
					auto field = getAccessedField(thisVar, cur);
					if(!field) {
						return; // not accessing a field
					}

					// check whether field has been touched before
					if(touched.contains(field)) { return; }

					// mark field as being touched
					touched.insert(field);

					// we must not be inside a loop
					if(iterating) { return; }

					// check whether value is only depending on input parameters
					if(!valuesDerivedFromParametersOnly(thisVar, params, cur)) { return; }

					// we have found a first assign
					res.push_back(cur);
				}

				void visitStatement(const core::StatementAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params,
				                    core::NodeSet& touched, std::vector<core::StatementAddress>& res, bool iterating) {
					// terminate decent here!
				}

				void visitNode(const core::NodeAddress& cur, const core::VariablePtr& thisVar, const core::VariableList& params, core::NodeSet& touched,
				               std::vector<core::StatementAddress>& res, bool iterating) {
					std::cerr << "\n\n --------------------- ASSERTION ERROR -------------------\n";
					std::cerr << "Node of type " << cur->getNodeType() << " should not be reachable!\n";
					assert_fail() << "Must not be reached!";
				}
			};

			std::pair<c_ast::Constructor::InitializationList, core::CompoundStmtPtr> extractInitializer(const Converter& converter, const core::LambdaPtr& ctor,
			                                                                                            ConversionContext& context) {
				auto& mgr = converter.getNodeManager();
				core::IRBuilder builder(mgr);
				auto cmgr = converter.getCNodeManager();
				c_ast::Constructor::InitializationList initializers;
				auto& rExt = mgr.getLangExtension<core::lang::ReferenceExtension>();

				auto body = ctor->getBody();

				// determine "this" parameter for ref member
				auto thisVar = ctor->getParameterList()[0];
				auto thisExp = builder.deref(thisVar);

				// set of identifiers already initialized
				core::NodeSet initedFields;

				auto isFieldInit = [&](core::ExpressionPtr memLoc) {
					if(memLoc == thisExp) {
						// delegating constructors
						return cmgr->create<c_ast::Identifier>(core::analysis::getTypeName(core::analysis::getReferencedType(thisExp)));
					}
					if(rExt.isCallOfRefParentCast(memLoc)) {
						// base constructors
						return cmgr->create<c_ast::Identifier>(
						    core::analysis::getTypeName(core::analysis::getRepresentedType(core::analysis::getArgument(memLoc, 1))));
					}
					if(rExt.isCallOfRefDeref(memLoc)) memLoc = core::analysis::getArgument(memLoc, 0);
					if(rExt.isCallOfRefMemberAccess(memLoc)) {
						// field initializers
						auto structExp = core::analysis::getArgument(memLoc, 0);
						if(structExp == thisExp) {
							auto field = core::analysis::getArgument(memLoc, 1).as<core::LiteralPtr>();
							if(!::contains(initedFields, field)) {
								initedFields.insert(field);
								return cmgr->create<c_ast::Identifier>(field->getStringValue());
							}
						}
					}
					return c_ast::IdentifierPtr();
				};

				core::StatementList newBody;
				for(const auto& stmt : body) {
					if(auto initExpr = stmt.isa<core::InitExprPtr>()) {
						auto init = initExpr->getInitExprList();
						assert_eq(init.size(), 1) << "Unexpected init expression list in constructor, expected exactly one expression\n" << init;
						auto memLoc = initExpr->getMemoryExpr();
						if(auto fieldId = isFieldInit(initExpr->getMemoryExpr())) {
							auto cInitExpr = converter.getStmtConverter().convertExpression(context, init[0]);
							initializers.push_back( { fieldId, toVector<c_ast::NodePtr>(cInitExpr) } );
							continue;
						}
					}
					if(auto memCtor = stmt.isa<core::CallExprPtr>()) {
						if(core::analysis::isConstructorCall(memCtor)) {
							if(auto fieldId = isFieldInit(core::analysis::getArgument(memCtor, 0))) {
								// build a replacement constructor to easily translate and migrate the translated arguments to the init list
								auto replacementArgs = memCtor->getArgumentList();
								replacementArgs[0] = core::lang::buildRefTemp(replacementArgs[0]->getType());
								auto replacementCall = builder.callExpr(memCtor->getType(), memCtor->getFunctionExpr(), replacementArgs);
								auto cCall = converter.getStmtConverter().convertExpression(context, replacementCall);
								c_ast::ConstructorCallPtr cCtor;
								if(cCall->getNodeType() == c_ast::NT_UnaryOperation) {
									cCtor = cCall.as<c_ast::UnaryOperationPtr>()->operand.as<c_ast::ConstructorCallPtr>();
								} else {
									cCtor = cCall.as<c_ast::ConstructorCallPtr>();
								}
								initializers.push_back({fieldId, cCtor->arguments});
								continue;
							}
						}
					}
					newBody.push_back(stmt);
				}

				// return result
				return std::make_pair(initializers, builder.compoundStmt(newBody));
			}


		} // end namespace


		FunctionCodeInfo FunctionInfoStore::resolveFunction(ConversionContext& context, const c_ast::IdentifierPtr name, const core::FunctionTypePtr& funType,
		                                                    const core::LambdaPtr& lambda) {
			FunctionCodeInfo res;

			// get C node manager
			auto manager = converter.getCNodeManager();

			// get other managers
			TypeManager& typeManager = converter.getTypeManager();
			NameManager& nameManager = converter.getNameManager();

			// check whether this is a member function
			bool isMember = funType->isConstructor() || funType->isDestructor() || funType->isMemberFunction() || funType->isVirtualMemberFunction();

			// resolve return type
			const TypeInfo& returnTypeInfo = typeManager.getTypeInfo(context, funType->getReturnType());
			res.prototypeDependencies.insert(returnTypeInfo.declaration);
			res.definitionDependencies.insert(returnTypeInfo.definition);
			c_ast::TypePtr returnType = returnTypeInfo.rValueType;

			// create a new variable scope for the resolution of the body
			nameManager.pushVarScope(true);

			// resolve parameters
			unsigned counter = 0;
			vector<c_ast::VariablePtr> parameter;
			for_each(funType->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {

				// skip type literals passed as arguments
				if(core::analysis::isTypeLiteralType(cur)) {
					counter++;
					return;
				}

				// resolve parameter type
				const TypeInfo& paramTypeInfo = typeManager.getTypeInfo(context, cur);
				res.prototypeDependencies.insert(paramTypeInfo.declaration);
				res.definitionDependencies.insert(paramTypeInfo.definition);

				c_ast::TypePtr paramType = paramTypeInfo.rValueType;

				string paramName;
				if(lambda) {
					if(isMember && counter == 0) {
						// first parameter of member functions is this!
						paramName = "this";
						nameManager.setName(lambda->getParameterList()[counter], paramName);
					} else {
						if(!converter.getNodeManager().getLangExtension<core::lang::VarArgsExtension>().isVarList(cur) || counter < lambda->getParameterList().size()) {
							paramName = nameManager.getName(lambda->getParameterList()[counter]);
						}
					}
				} else {
					paramName = format("p%d", counter + 1);
				}
				parameter.push_back(c_ast::var(paramType, manager->create(paramName)));

				counter++;
			});

			// resolve body
			c_ast::StatementPtr cBody;
			c_ast::Constructor::InitializationList initializer;

			if(lambda) {
				// set up variable manager
				ConversionContext context(converter, lambda);
				for_each(lambda->getParameterList(), [&](const core::VariablePtr& cur) {
					context.getVariableManager().addInfo(context, cur, VariableInfo::DIRECT);
				});

				core::CompoundStmtPtr body = lambda->getBody();

				// extract initializer list
				if(funType->isConstructor()) {
					// collect initializer values + remove from body
					std::tie(initializer, body) = extractInitializer(converter, lambda, context);
				}

				// replace returns in constructors and destructors
				if(lambda->getType()->isConstructor() || lambda->getType()->isDestructor()) {
					body = core::transform::transformBottomUpGen(body, [](const core::ReturnStmtPtr& retStmt){
						return core::IRBuilder(retStmt->getNodeManager()).returnStmt();
					});
				}

				// convert the body code fragment and collect dependencies
				c_ast::NodePtr code = converter.getStmtConverter().convert(context, body);
				cBody = static_pointer_cast<c_ast::Statement>(code);
				res.definitionDependencies.insert(context.getDependencies().begin(), context.getDependencies().end());

				// also attach includes
				res.includes = context.getIncludes();
			}

			// drop nested variable scope
			nameManager.popVarScope();

			// create function
			res.function = manager->create<c_ast::Function>(returnType, name, parameter, cBody);
			res.definition = res.function;

			// a lazy-evaluated utility to obtain the name of a class a member function is associated to
			auto getClassName = [&]() -> c_ast::IdentifierPtr {

				const auto& type = typeManager.getTypeInfo(context, core::analysis::getObjectType(funType)).lValueType;

				if(const auto& tagType = type.isa<c_ast::NamedCompositeTypePtr>()) { return tagType->name; }

				if(const auto& namedType = type.isa<c_ast::NamedTypePtr>()) { return namedType->name; }
				std::cerr << "Unable to determine class-name for member function: " << funType << "\n";
				assert_fail() << "Unsupported case!";
				return c_ast::IdentifierPtr();
			};

			// modify function if required
			if(funType->isMemberFunction()) {
				// update definition to define a member function
				auto thisType = core::lang::ReferenceType(funType->getParameterType(0));
				bool isConst = thisType.isConst();
				bool isVolatile = thisType.isVolatile();
				res.definition = manager->create<c_ast::MemberFunction>(getClassName(), res.function, isConst, isVolatile);

			} else if(funType->isConstructor()) {
				// update definition to define a member function
				res.definition = manager->create<c_ast::Constructor>(getClassName(), res.function, initializer);

			} else if(funType->isDestructor()) {
				// update definition to define a member function
				res.definition = manager->create<c_ast::Destructor>(getClassName(), res.function);

				//TODO
			} else if(funType->isVirtualMemberFunction()) {
				assert_not_implemented();
			}
			return res;
		}

		std::pair<c_ast::IdentifierPtr, c_ast::CodeFragmentPtr> FunctionInfoStore::resolveLambdaWrapper(ConversionContext& context,
		                                                                                                const c_ast::FunctionPtr& function,
		                                                                                                const core::FunctionTypePtr& funType) {
			// get C node manager
			auto manager = converter.getCNodeManager();

			// obtain function type information
			core::FunctionTypePtr closureType =
			    core::FunctionType::get(funType->getNodeManager(), funType->getParameterTypes(), funType->getReturnType(), core::FK_CLOSURE);
			TypeManager& typeManager = converter.getTypeManager();
			const FunctionTypeInfo& funTypeInfo = typeManager.getTypeInfo(context, closureType);

			// create a new function representing the wrapper

			// create list of parameters for wrapper
			vector<c_ast::VariablePtr> parameter;

			// first parameter is the closure
			parameter.push_back(c_ast::var(funTypeInfo.rValueType, manager->create("closure")));

			// resolve parameters
			int counter = 1;
			::transform(funType->getParameterTypes()->getElements(), std::back_inserter(parameter), [&](const core::TypePtr& cur) {
				const TypeInfo& paramTypeInfo = typeManager.getTypeInfo(context, cur);
				return c_ast::var(paramTypeInfo.rValueType, manager->create(format("p%d", counter++)));
			});

			// pick a name for the wrapper
			c_ast::IdentifierPtr name = manager->create(function->name->name + "_wrap");

			// create a function body (call to the function including wrappers)
			c_ast::CallPtr call = manager->create<c_ast::Call>(function->name);

			// filter out type literal parameters
			vector<core::TypePtr> paramTypes;
			for_each(funType->getParameterTypes()->getElements(), [&](const core::TypePtr& cur) {
				if(!core::analysis::isTypeLiteralType(cur)) { paramTypes.push_back(cur); }
			});

			// add parameters for wrapper
			for(const auto& cur : make_paired_range(paramTypes, function->parameter)) {
				call->arguments.push_back(cur.second);
			}

			c_ast::StatementPtr body = call;
			if(!c_ast::isVoid(function->returnType)) { body = manager->create<c_ast::Return>(call); }

			c_ast::FunctionPtr wrapper = manager->create<c_ast::Function>(function->returnType, name, parameter, body);

			c_ast::CodeFragmentPtr res = c_ast::CCodeFragment::createNew(converter.getFragmentManager(), wrapper);
			res->addDependency(funTypeInfo.definition);

			return std::make_pair(name, res);
		}
	}

	FunctionIncludeTable getBasicFunctionIncludeTable() {
		// the basic include table is empty
		return FunctionIncludeTable();
	}

} // end namespace backend
} // end namespace insieme

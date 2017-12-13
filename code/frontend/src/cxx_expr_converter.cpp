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

#include "insieme/frontend/expr_converter.h"

#include "insieme/frontend/clang.h"
#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/conversion/init_lists.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/debug.h"
#include "insieme/frontend/utils/expr_to_bool.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/name_mangling.h"

#include "insieme/core/analysis/default_members.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/type_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/compound_operators.h"
#include "insieme/core/lang/varargs_extension.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"


using namespace clang;
using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//										CXX EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::Visit(const clang::Expr* expr) {
		VLOG(2) << "CXXStmtConverter";
		return BaseVisit(expr, [&](const clang::Expr* param) { return ConstStmtVisitor<Converter::CXXExprConverter, core::ExpressionPtr>::Visit(param); });
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
		core::ExpressionPtr retIr = ExprConverter::VisitImplicitCastExpr(castExpr);
		LOG_EXPR_CONVERSION(castExpr, retIr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
		core::ExpressionPtr retIr = ExprConverter::VisitExplicitCastExpr(castExpr);
		LOG_EXPR_CONVERSION(castExpr, retIr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCallExpr(const clang::CallExpr* callExpr) {
		core::CallExprPtr irCall = ExprConverter::VisitCallExpr(callExpr).as<core::CallExprPtr>();
		LOG_EXPR_CONVERSION(callExpr, irCall);

		auto funExp = irCall->getFunctionExpr();
		auto funType = funExp->getType().as<core::FunctionTypePtr>();
		auto paramTypeList = funType->getParameterTypeList();
		unsigned clangArgCount = callExpr->arg_end() - callExpr->arg_begin();

		// fix variadic argument type list
		auto& vaExt = converter.getNodeManager().getLangExtension<core::lang::VarArgsExtension>();
		if(!paramTypeList.empty() && paramTypeList.back() == vaExt.getVarList()) {
			paramTypeList.pop_back();
			while (paramTypeList.size() < clangArgCount) {
				paramTypeList.push_back(core::TypePtr());
			}
		}

		assert_eq(paramTypeList.size(), clangArgCount) << "Argument counts don't match for funType "
				<< *funType << " and clang call expr " << dumpClang(callExpr, converter.getCompiler().getSourceManager());

		// in Inspire 2.0, copy and move constructor calls are implicit on function calls
		core::ExpressionList newArgs;
		size_t i = 0;
		std::transform(callExpr->arg_begin(), callExpr->arg_end(), std::back_inserter(newArgs), [&](const clang::Expr* clangArgExpr) {
			return convertCxxArgExpr(clangArgExpr, paramTypeList[i++]);
		});

		// Implicit materialization of this argument is not performed in clang AST
		if(funType->isMember()) {
			newArgs[0] = frontend::utils::prepareThisExpr(converter, newArgs[0]);
		}

		return builder.callExpr(convertExprType(callExpr), funExp, newArgs);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  MEMBER EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(membExpr, retIr);

		// handle calls to static methods like function calls would be handled
		auto memberDecl = membExpr->getMemberDecl();
		if(auto methDecl = llvm::dyn_cast<clang::CXXMethodDecl>(memberDecl)) {
			if(!converter.getFunMan()->contains(methDecl->getCanonicalDecl())) {
				converter.getDeclConverter()->Visit(methDecl);
			}
			retIr = converter.getFunMan()->lookup(methDecl->getCanonicalDecl());
			return retIr;
		}

		// handle normal data members
		retIr = ExprConverter::VisitMemberExpr(membExpr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(declRef, retIr);

		if(const clang::FieldDecl* field = llvm::dyn_cast<clang::FieldDecl>(declRef->getDecl())) {
			field->dump(); // prevent unused warning
			assert_not_implemented();
			// this is the direct access to a member field in a generic way: something like Obj::a
		}

		return retIr = Converter::ExprConverter::VisitDeclRefExpr(declRef);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULLPTR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXNullPtrLiteralExpr(const clang::CXXNullPtrLiteralExpr* nptrExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(nptrExpr, retIr);
		retIr = core::lang::buildPtrNull(core::lang::buildPtrType(converter.getIRBuilder().getLangBasic().getUnit()));
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CXX PSEUDO DESTRUCTOR
	//
	// Destructor call generated in a template instantiation for a base type
	// Nothing happens here, Clang probably has it only to regenerate the original code
	// We replace it by a no-op function defined in the FE extension and get rid of the entire call in cleanup later
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXPseudoDestructorExpr(const clang::CXXPseudoDestructorExpr* pseudo) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(pseudo, retIr);
		auto& feExt = converter.getNodeManager().getLangExtension<frontend::utils::FrontendInspireModule>();
		// rest off FE expects a pointer here
		retIr = core::lang::buildPtrOfFunction(feExt.getCxxPseudoDestructorCall());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBoolLiteralExpr(const clang::CXXBoolLiteralExpr* boolLit) {
		core::ExpressionPtr retExpr = converter.builder.boolLit(boolLit->getValue());
		LOG_EXPR_CONVERSION(boolLit, retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXMemberCallExpr(const clang::CXXMemberCallExpr* callExpr) {
		core::ExpressionPtr ret;
		LOG_EXPR_CONVERSION(callExpr, ret);

		const CXXMethodDecl* methodDecl = callExpr->getMethodDecl();

		// first, translate class decl if that hasn't happened yet
		auto record = callExpr->getRecordDecl();
		if(record) converter.getDeclConverter()->VisitRecordDecl(record);

		if(!methodDecl) {
			// member function pointer call
			frontend_assert(false) << "Member function pointer call not implemented";
		} else {
			// get method lambda
			auto methodLambda = converter.getFunMan()->lookup(methodDecl);

			// get the "this" object and add to arguments
			core::ExpressionPtr thisObj = Visit(callExpr->getImplicitObjectArgument());
			// Implicit materialization of this argument is not performed in clang AST
			thisObj = frontend::utils::prepareThisExpr(converter, thisObj);

			// build call and we are done
			auto retType = convertExprType(callExpr);

			// special handling for return type for dtor calls
			if(llvm::dyn_cast<clang::CXXDestructorDecl>(methodDecl)) {
				retType = thisObj->getType();
			}

			ret = utils::buildCxxMethodCall(converter, retType, methodLambda, thisObj, callExpr->arguments());
		}

		return ret;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX OPERATOR CALL EXPRESSION
	//
	//  A call to an overloaded operator written using operator syntax.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXOperatorCallExpr(const clang::CXXOperatorCallExpr* callExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(callExpr, retIr);

		// first, translate class decl if that hasn't happened yet
		if(auto calleeDecl = callExpr->getCalleeDecl()) {
			if(auto memDecl = llvm::dyn_cast<clang::CXXMethodDecl>(calleeDecl)) {
				auto thisType = memDecl->getThisType(converter.getCompiler().getASTContext()).getTypePtr();
				auto recType = llvm::dyn_cast<clang::RecordType>(llvm::dyn_cast<clang::PointerType>(thisType)->getPointeeType().getTypePtr());
				auto recordDecl = recType->getDecl();
				converter.getDeclConverter()->VisitRecordDecl(recordDecl);
			}
		}

		retIr = VisitCallExpr(callExpr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	namespace {
		/// Convert a ConstructExpr to an IR constructor call, allocating the required memory either on the stack (default) or on the heap
		core::ExpressionPtr convertConstructExprInternal(Converter& converter, const clang::CXXConstructExpr* constructExpr, bool onStack) {
			core::TypePtr resType = converter.convertType(constructExpr->getType());
			// first constructor argument is the object memory location -- we need to build this either on the stack or heap
			auto irMemLoc = onStack ? core::lang::buildRefTemp(resType) : converter.getIRBuilder().undefinedNew(resType);
			return utils::convertConstructExpr(converter, constructExpr, irMemLoc);
		}
	}

	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXConstructExpr(const clang::CXXConstructExpr* callExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(callExpr, retIr);

		retIr = convertConstructExprInternal(converter, callExpr, true);

		frontend_assert(retIr) << "ConstructExpr could not be translated\n";
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX NEW CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	namespace {
		/// Resize the array created by an array create call (required due to mismatched size reported for initializer expressions in new[])
		core::ExpressionPtr resizeArrayCreate(const Converter& converter, core::ExpressionPtr createExpr, const core::ExpressionPtr& newSize) {
			auto& nodeMan = createExpr->getNodeManager();
			auto& builder = converter.getIRBuilder();
			auto& refExt = nodeMan.getLangExtension<core::lang::ReferenceExtension>();
			if(refExt.isCallOfRefDeref(createExpr)) createExpr = core::analysis::getArgument(createExpr, 0);

			auto initExpr = createExpr.isa<core::InitExprPtr>();
			frontend_assert(initExpr) << "Trying to resize array creation, but expression is not init";

			auto arrGt = initExpr.getType();
			frontend_assert(core::lang::isReference(arrGt)) << "Inited type must be reference";
			auto subTy = core::analysis::getReferencedType(arrGt);
			frontend_assert(core::lang::isArray(subTy)) << "Expected array type, got " << *subTy;
			auto arrTy = core::lang::ArrayType(subTy);

			auto form = core::arithmetic::toFormula(newSize);
			frontend_assert(form.isConstant()) << "Non const-sized new[] not yet implemented";
			arrTy.setSize(form.getIntegerValue());

			core::ExpressionPtr retIr = builder.initExpr(builder.undefinedNew((core::GenericTypePtr)arrTy), initExpr.getInitExprList());
			return retIr;
		}

		core::ExpressionPtr buildArrayNew(Converter& converter, const clang::CXXNewExpr* newExpr, const core::TypePtr& elemType) {
			auto& builder = converter.getIRBuilder();
			auto& basic = converter.getNodeManager().getLangBasic();
			auto arrayLenBase = converter.convertExpr(newExpr->getArraySize());
			auto arrayLenExpr = builder.numericCast(arrayLenBase, basic.getUIntInf());

			// allocate constant sized arrays types more simply
			auto constArrayLen = core::arithmetic::toConstantInt(arrayLenExpr);
			if(constArrayLen) {
				auto irNewExp = builder.undefinedNew(core::lang::ArrayType::create(elemType, builder.uintLit(constArrayLen.get())));
				// for class types, we need to build an init expr even if no initializer is supplied, to call constructors implicitly
				// also if we don't have any initializer, we build an empty initexpr to correctly identify this in the backend again
				if(newExpr->getConstructExpr() || !newExpr->hasInitializer()) {
					irNewExp = builder.initExpr(irNewExp);
				}
				// otherwise, build initexpr for the initializer
				else if(newExpr->hasInitializer()) {
					core::ExpressionPtr initializerExpr = converter.convertInitExpr(newExpr->getInitializer());
					// make sure we allocate the correct amount of array elements with partial initialization
					irNewExp = resizeArrayCreate(converter, initializerExpr, arrayLenBase);
				}
				return core::lang::buildPtrFromArray(irNewExp);
			}

			auto arrLenVarParam = builder.variable(builder.refType(basic.getUIntInf()));
			auto arrLenVar = builder.variable(basic.getUIntInf());
			auto arrType = core::lang::ArrayType::create(elemType, arrLenVar);
			auto arrLenVarDecl = builder.declarationStmt(arrLenVar, builder.deref(arrLenVarParam));
			// generate memory location
			auto memloc = builder.undefinedNew(arrType);
			auto arr = memloc;
			// build our init function
			core::TypeList initFunParamTypes { basic.getUIntInf() };
			core::VariableList initFunParams { arrLenVarParam };
			core::ExpressionList initFunArguments { arrayLenExpr };
			// for class types, we need to build an init expr even if no initializer is supplied, to call constructors implicitly
			// also if we don't have any initializer, we build an empty initexpr to correctly identify this in the backend again
			if(newExpr->getConstructExpr() || !newExpr->hasInitializer()) {
				arr = builder.initExpr(memloc);
			}
			else if(newExpr->hasInitializer()) {
				// convert the initializer to a temporary init expression and then use its init expressions
				auto tempInits = converter.convertInitExpr(newExpr->getInitializer());
				if(tempInits->getNodeType() != core::NT_InitExpr) tempInits = core::analysis::getArgument(tempInits, 0);
				for(auto decl : tempInits.as<core::InitExprPtr>()->getInitDecls()) {
					auto expr = decl->getInitialization();
					auto exprType = expr->getType();
					if(core::analysis::isRefType(exprType)) exprType = core::analysis::getReferencedType(exprType);
					auto paramType = builder.refType(exprType, true, false, core::lang::ReferenceType::Kind::CppReference);
					initFunParamTypes.push_back(paramType);
					initFunParams.push_back(builder.variable(paramType));
					initFunArguments.push_back(expr);
				}
				core::ExpressionList initList;
				for(auto i = initFunParams.cbegin()+1; i != initFunParams.cend(); ++i) {
					initList.push_back(*i);
				}
				arr = builder.initExpr(memloc, initList);
			}
			auto initFunRetType = core::lang::buildPtrType(elemType);
			auto initFunType = builder.functionType(initFunParamTypes, initFunRetType);
			auto initFunBody = builder.compoundStmt(arrLenVarDecl, builder.returnStmt(core::lang::buildPtrFromArray(arr), builder.refType(initFunRetType)));
			auto initFun = builder.lambdaExpr(initFunType, initFunParams, initFunBody, "new_arr_fun");
			// call the init function
			return builder.callExpr(initFunRetType, initFun, initFunArguments);
		}
	}

	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXNewExpr(const clang::CXXNewExpr* newExpr) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(newExpr, retExpr);

		frontend_assert(newExpr->getNumPlacementArgs() < 2) << "Custom placement new not yet supported";

		core::TypePtr type = converter.convertType(newExpr->getAllocatedType());

		if(newExpr->isArray()) {
			frontend_assert(newExpr->getNumPlacementArgs() == 0) << "Placement new for arrays not yet supported";
			retExpr = buildArrayNew(converter, newExpr, type);
		} else {
			// if no constructor is found, it is a new over a non-class type
			if(!newExpr->getConstructExpr()) {
				if(newExpr->getNumPlacementArgs() == 0) {
					// build new expression depending on whether or not we have an initializer expression
					core::ExpressionPtr newExp;
					if(newExpr->hasInitializer()) {
						newExp = builder.refNew(Visit(newExpr->getInitializer()));
					}
					else {
						newExp = builder.undefinedNew(type);
					}
					retExpr = core::lang::buildPtrFromRef(newExp);
				}
				else {
					auto location = Visit(newExpr->getPlacementArg(0));
					if(newExpr->hasInitializer()) {
						retExpr = utils::buildCxxPlacementNew(location, Visit(newExpr->getInitializer()));
					}
					else {
						retExpr = core::lang::buildPtrReinterpret(location, type);
					}
				}
			}
			// we have a constructor, so we are building a class
			else {
				if(newExpr->getNumPlacementArgs() == 0) {
					retExpr = convertConstructExprInternal(converter, newExpr->getConstructExpr(), false);
				}
				else {
					auto location = Visit(newExpr->getPlacementArg(0));
					retExpr = utils::convertConstructExpr(converter, newExpr->getConstructExpr(),
					                                      core::lang::buildPtrToRef(core::lang::buildPtrReinterpret(location, type)));
				}
				retExpr = core::lang::buildPtrFromRef(retExpr);
			}
		}

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX DELETE CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDeleteExpr(const clang::CXXDeleteExpr* deleteExpr) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(deleteExpr, retExpr);
		auto& builder = converter.getIRBuilder();

		// convert the target of our delete expr
		core::ExpressionPtr exprToDelete = Visit(deleteExpr->getArgument());
		frontend_assert(core::lang::isPointer(exprToDelete)) << "\"delete\" called on non-pointer, not supported.";

		auto toDelete = core::lang::buildPtrToRef(exprToDelete);
		// make sure we delete a ref array if we have an array delete
		if(deleteExpr->isArrayForm()) {
			toDelete = core::lang::buildPtrToArray(exprToDelete);
		} else {
			// build destructor call if required
			// 1) check in TU to see if the type needs a destructor call
			// 2) if so, build call directly so that it works after resolving regardless of calling context
			auto irType = converter.convertType(deleteExpr->getDestroyedType());
			if(auto genType = irType.isa<core::GenericTypePtr>()) {
				if(::containsKey(converter.getIRTranslationUnit().getTypes(), genType)) {
					toDelete = builder.callExpr(builder.getLiteralForDestructor(core::analysis::buildDefaultDestructorType(builder.refType(irType))), toDelete);
				}
			}
		}

		// destructor calls are implicit in inspire 2.0, just like C++
		retExpr = builder.refDelete(toDelete);

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThisExpr(const clang::CXXThisExpr* thisExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(thisExpr, retIr);

		// obtain current "this" from variable manager
		auto thisRef = converter.getVarMan()->getThis();
		// this is a pointer, not a reference
		retIr = core::lang::buildPtrFromRef(thisRef);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					EXCEPTION CXX THROW EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXThrowExpr(const clang::CXXThrowExpr* throwExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(throwExpr, retIr);

		//core::ExpressionPtr subExpr;
		//core::TypePtr targetTy;
		//if(throwExpr->getSubExpr()) {
		//	subExpr = Visit(throwExpr->getSubExpr());
		//	targetTy = converter.convertType(throwExpr->getSubExpr()->getType());
		//	core::TypePtr srcTy = subExpr->getType();
		//	if(targetTy != srcTy) { subExpr = converter.tryDeref(subExpr); }
		//} else {
		//	// a throw without expression rethrows the expression captured in the container catch.
		//	// if no expressesion, calls terminate(). but we delegate this to the output compiler
		//	targetTy = gen.getUnit();
		//	subExpr = builder.literal("__insieme__rethrow", targetTy);
		//}
		//retIr = converter.createCallExprFromBody(builder.throwStmt(subExpr), gen.getUnit());

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX DEFAULT ARG EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDefaultArgExpr(const clang::CXXDefaultArgExpr* defaultArgExpr) {
		auto retIr = Visit(defaultArgExpr->getExpr());
		LOG_EXPR_CONVERSION(defaultArgExpr, retIr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Bind Temporary expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXBindTemporaryExpr(const clang::CXXBindTemporaryExpr* bindTempExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(bindTempExpr, retIr);

		// temporary creation/destruction is implicit
		retIr = converter.convertExpr(bindTempExpr->getSubExpr());

		retIr = frontend::utils::convertMaterializingExpr(converter, retIr);

		return retIr;

	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					CXX Expression with cleanups
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitExprWithCleanups(const clang::ExprWithCleanups* cleanupExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(cleanupExpr, retIr);

		// temporary creation/destruction is implicit
		retIr = converter.convertExpr(cleanupExpr->getSubExpr());

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					ScalarValueInitExpr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXScalarValueInitExpr(const clang::CXXScalarValueInitExpr* scalarValueInit) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(scalarValueInit, retIr);

		retIr = builder.getZero(converter.convertType(scalarValueInit->getType()));

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Materialize temporary expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitMaterializeTemporaryExpr(const clang::MaterializeTemporaryExpr* materTempExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(materTempExpr, retIr);
		retIr = Visit(materTempExpr->GetTemporaryExpr());

		retIr = frontend::utils::convertMaterializingExpr(converter, retIr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Typeid expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXTypeidExpr(const clang::CXXTypeidExpr* typeidExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(typeidExpr, retIr);

		//core::ExpressionPtr expr;
		//if(typeidExpr->isTypeOperand()) {
		//	expr = builder.getTypeLiteral(converter.convertType(typeidExpr->getTypeOperand(converter.getCompiler().getASTContext())));
		//} else {
		//	expr = Visit(typeidExpr->getExprOperand());
		//}
		//retIr = builder.callExpr(mgr.getLangExtension<core::lang::IRppExtensions>().getTypeid(), expr);

		assert_not_implemented();

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					Default init expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXDefaultInitExpr(const clang::CXXDefaultInitExpr* initExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initExpr, retIr);
		retIr = converter.convertExpr(initExpr->getExpr());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Substituted non type template parameter expression
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitSubstNonTypeTemplateParmExpr(const clang::SubstNonTypeTemplateParmExpr* substExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(substExpr, retIr);
		frontend_assert(substExpr->getReplacement()) << "template parameter cannot be substituted by nothing\n";
		retIr = Visit(substExpr->getReplacement());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Binary member pointer Direct
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemD(const clang::BinaryOperator* exprD) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(exprD, retIr);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//			Binary member pointer indirect
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinPtrMemI(const clang::BinaryOperator* exprI) {
		// indirect, ->*
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(exprI, retIr);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND ASSINGMENT OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCompoundAssignOperator(const clang::CompoundAssignOperator* compOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(compOp, retIr);

		retIr = ExprConverter::VisitCompoundAssignOperator(compOp);

		// Remove surrounding deref that we need in C code: CompOps are lvalues in C++
		retIr = core::analysis::getArgument(retIr, 0);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  UNARY OPERATOR EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitUnaryOperator(const clang::UnaryOperator* unOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(unOp, retIr);

		auto subExpr = Visit(unOp->getSubExpr());
		auto exprType = convertExprType(unOp);

		if(core::lang::isCppReference(subExpr->getType())) {
			core::lang::ReferenceType plainRef(subExpr->getType());
			plainRef.setKind(core::lang::ReferenceType::Kind::Plain);
			subExpr = core::lang::buildRefCast(subExpr, plainRef.toType());
		}

		auto &compExt = converter.getNodeManager().getLangExtension<core::lang::CompoundOpsExtension>();
		auto &pExt = converter.getNodeManager().getLangExtension<core::lang::PointerExtension>();

		// prefix increment/decrement need to be handled differently in C++ (lvalue semantics)
		// ... but first we have to make sure to correctly translate ops on pointers
		// ... which also needs special C++ semantics

		if(core::lang::isReference(subExpr) && core::lang::isPointer(core::analysis::getReferencedType(subExpr->getType()))) {
			if(unOp->getOpcode() == clang::UO_PreInc) {
				return builder.callExpr(subExpr->getType(), pExt.getCxxStylePtrPreInc(), subExpr);
			}
			else if(unOp->getOpcode() == clang::UO_PreDec) {
				return builder.callExpr(subExpr->getType(), pExt.getCxxStylePtrPreDec(), subExpr);
			}
			auto opIt = unOpMap.find(unOp->getOpcode());
			if(opIt != unOpMap.end()) {
				auto op = opIt->second;
				return core::lang::buildPtrOperation(op, subExpr);
			}
		}

		if(unOp->getOpcode() == clang::UO_PreInc) {
			return builder.callExpr(compExt.getCompPrefixInc(), subExpr);
		}
		else if(unOp->getOpcode() == clang::UO_PreDec) {
			return builder.callExpr(compExt.getCompPrefixDec(), subExpr);
		}

		return retIr = createUnaryExpression(exprType, subExpr, unOp->getOpcode());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		core::ExpressionPtr lhs = Visit(binOp->getLHS());
		core::ExpressionPtr rhs = Visit(binOp->getRHS());
		core::TypePtr exprTy = converter.convertType(binOp->getType());

		// we need to translate the semantics of Cxx-style assignments to a function call --------------------------------------------------------- ASSIGNMENT -
		if(binOp->getOpcode() == clang::BO_Assign) {
			retIr = utils::buildCxxStyleAssignment(lhs, rhs);
		}
		else {
			retIr = createBinaryExpression(exprTy, lhs, rhs, binOp);
		}

        return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		    Binary trait
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitTypeTraitExpr(const clang::TypeTraitExpr* typeTraitExpr) {
		// this is found when using __base_of operator, clang gives already the boolean expression evaluated,
		// is an static type resolutions, we just forward this result
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(typeTraitExpr, retIr);
		retIr = converter.builder.boolLit(typeTraitExpr->getValue());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		SizeOfPack expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitSizeOfPackExpr(const clang::SizeOfPackExpr* sizeOfPackExpr) {
		// sizeOf... returns size_t --> use unsigned int
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(sizeOfPackExpr, retIr);
		retIr = builder.uintLit(sizeOfPackExpr->getPackLength());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		CXXStdInitializerListExpr expr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitCXXStdInitializerListExpr(const clang::CXXStdInitializerListExpr* stdInitListExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(stdInitListExpr, retIr);

		retIr = conversion::convertCxxStdInitializerListExpr(converter, stdInitListExpr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//		LambdaExpr ( [](){} )
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CXXExprConverter::VisitLambdaExpr(const clang::LambdaExpr* lExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(lExpr, retIr);

		// translate implicitly created class
		auto genTy = converter.convertType(clang::QualType(lExpr->getLambdaClass()->getTypeForDecl(), 0)).as<core::GenericTypePtr>();

		// look up struct type in order to use member types for argument translation
		auto tagTyIt = converter.getIRTranslationUnit().getTypes().find(genTy);
		frontend_assert(tagTyIt != converter.getIRTranslationUnit().getTypes().end());
		auto fields = tagTyIt->second.getFields();

		// gather init expressions and translate as if they were arguments
		core::ExpressionList initExprs;
		size_t fieldIndex = 0;
		for(auto capture: lExpr->capture_inits()) {
			frontend_assert(fields.size() > fieldIndex)
				<< "Mismatch between number of captures in generated struct and number of initializers while translating LambdaExpr";
			initExprs.push_back(converter.convertCxxArgExpr(capture, fields[fieldIndex++]->getType()));
		}


		// generate init expr of the lambda type
		return builder.initExprTemp(genTy, initExprs);
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

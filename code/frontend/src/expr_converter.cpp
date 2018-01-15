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

#include <iconv.h>

#include "insieme/frontend/expr_converter.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/conversion_utils.h"
#include "insieme/frontend/utils/expr_to_bool.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/functional_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/lang/array.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/compound_operators.h"
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/include.h"
#include "insieme/annotations/c/implicit.h"

using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {

	//---------------------------------------------------------------------------------------------------------------------
	//										BASE EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	core::ExpressionPtr Converter::ExprConverter::BaseVisit(const clang::Expr* expr, std::function<core::ExpressionPtr(const clang::Expr*)> self) {

		auto retIr = converter.applyExtensions<core::ExpressionPtr>(expr, [&](const clang::Expr* param) {
			converter.trackSourceLocation(param);
			auto retIr = self(param);
			converter.untrackSourceLocation();
			return retIr;
		});

		// print diagnosis messages
		converter.printDiagnosis(expr->getLocStart());

		// attach location annotation
		if(expr->getLocStart().isValid()) {
			auto presStart = converter.getSourceManager().getPresumedLoc(expr->getLocStart());
			auto presEnd = converter.getSourceManager().getPresumedLoc(expr->getLocEnd());
			core::annotations::attachLocation(retIr, std::string(presStart.getFilename()), presStart.getLine(), presStart.getColumn(), presEnd.getLine(),
				                              presEnd.getColumn());
		}

		return retIr;
	}

	core::TypePtr Converter::ExprConverter::convertExprType(const clang::Expr* expr) {
		auto qType = expr->getType();
		auto irType = converter.convertType(qType);
		if(expr->getValueKind() == clang::VK_LValue) {
			irType = builder.refType(irType, qType.isConstQualified(), qType.isVolatileQualified());
		} else if(expr->getValueKind() == clang::VK_XValue) {
			irType = builder.refType(irType, qType.isConstQualified(), qType.isVolatileQualified(), core::lang::ReferenceType::Kind::CppRValueReference);
		}
		return irType;
	}

	// translate expression as usual, but convert translated string literals to r-values rather than l-values
	core::ExpressionPtr Converter::ExprConverter::convertInitExpr(const clang::Expr* original) {
		auto expr = converter.convertCxxArgExpr(original);
		if(original->isLValue()) {
			auto origType = llvm::dyn_cast_or_null<clang::ConstantArrayType>(original->getType()->getAsArrayTypeUnsafe());
			auto lit = expr.isa<core::LiteralPtr>();
			if(!origType || !lit) return expr;
			string s = insieme::utils::unescapeString(lit->getValue()->getValue());
			core::NodeManager& mgr = lit->getNodeManager();
			core::IRBuilder builder(mgr);
			core::ExpressionList initExps;
			for(char c : s) {
				initExps.push_back(builder.literal(format("'%s'", toString(insieme::utils::escapeChar(c))), mgr.getLangBasic().getChar()));
			}
			initExps.push_back(builder.literal("'\\0'", mgr.getLangBasic().getChar()));
			expr = builder.deref(builder.initExprTemp(converter.convertType(original->getType()).as<core::GenericTypePtr>(), initExps));
			VLOG(2) << "convertInitExpr: translated string literal\n" << dumpClang(original, converter.getSourceManager()) << " - to - \n" << dumpColor(expr);
		}
		return expr;
	}

	// translate expression, but skip outer construct expr and cast references to correct kind
	core::ExpressionPtr Converter::ExprConverter::convertCxxArgExpr(const clang::Expr* clangArgExprInput, const core::TypePtr& targetType) {
		core::ExpressionPtr ret = converter.convertExpr(clangArgExprInput);
		const clang::Expr* clangArgExpr = clangArgExprInput;

		// skip over potential CXXDefaultArgExpr
		auto defaultArg = llvm::dyn_cast<clang::CXXDefaultArgExpr>(clangArgExpr);
		if(defaultArg) clangArgExpr = defaultArg->getExpr();
		// skip over potential ExprWithCleanups
		auto conWithCleanups = llvm::dyn_cast<clang::ExprWithCleanups>(clangArgExpr);
		if(conWithCleanups) clangArgExpr = conWithCleanups->getSubExpr();
		// skip over potential CXXBindTemporaryExpr
		auto conBindTemporary = llvm::dyn_cast<clang::CXXBindTemporaryExpr>(clangArgExpr);
		if(conBindTemporary) clangArgExpr = conBindTemporary->getSubExpr();

		// detect copy/move constructor calls
		VLOG(2) << "---\nCXX call checking arg: " << dumpClang(clangArgExpr, converter.getCompiler().getSourceManager());
		auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(clangArgExpr);
		if(constructExpr) {
			auto constructor = constructExpr->getConstructor();
			if(constructor->isCopyOrMoveConstructor()) {
				// we only create the refCast here in case the original translated IR's functionExpr even has 2 parameters.
				// this might not be the case if the translation has been intercepted somewhere, and thus we must not access the second parameter in this case
				auto prefFunType = ret.as<core::CallExprPtr>()->getFunctionExpr()->getType().as<core::FunctionTypePtr>();
				if(prefFunType->getParameterTypeList().size() == 2) {
					VLOG(2) << "CopyOrMove in param list at " << utils::location(clangArgExpr->getLocStart(), converter.getSourceManager()) << "\n";
					ret = converter.convertExpr(constructExpr->getArg(0));
					// cast ref as required by copy/move constructor
					auto paramType = prefFunType->getParameterType(1);
					// Note that we only cast here, if also the paramType is a ref-type. This might not be the case if we are translating some code, where some
					// extensions are not using the type system in the same way we expect here in the frontend
					if(core::lang::isReference(ret) && core::lang::isReference(paramType)) {
						ret = core::lang::buildRefCast(ret, paramType);
					}
					VLOG(2) << "CXX call converted newArg: " << dumpPretty(ret);
				}
			}
		}
		// if a targetType was provided, and we are working on a ref, cast the ref as required
		if(targetType && core::analysis::isRefType(targetType) && core::analysis::isRefType(ret->getType())) {
			auto targetRef = core::lang::ReferenceType(targetType);
			auto retRef = core::lang::ReferenceType(ret->getType());
			ret = core::lang::buildRefKindCast(ret, targetRef.getKind());
		}
		// if we are directly using an init list as an argument, the generated Inspire code has a superfluous deref
		// (we are initializing the value of the function argument directly without any copies!)
		if(llvm::isa<clang::InitListExpr>(clangArgExprInput) && ret.isa<core::CallExprPtr>()) ret = core::analysis::getArgument(ret, 0);
		// if a target type is given, and it is a value type, replace plain ref initializers to initialize that memory location
		if(targetType && !core::analysis::isRefType(targetType) && core::transform::materialize(targetType) == ret->getType()) {
			ret = utils::fixTempMemoryInInitExpression(core::lang::buildRefDecl(builder.refType(targetType)), ret);
		}
		return ret;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CExprConverter::Visit(const clang::Expr* expr) {
		VLOG(2) << "CStmtConverter";
		return BaseVisit(expr, [&](const clang::Expr* param) { return ConstStmtVisitor<CExprConverter, core::ExpressionPtr>::Visit(param); });
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitIntegerLiteral(const clang::IntegerLiteral* intLit) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(intLit, retExpr);

		std::string value;
		if(intLit->getType().getTypePtr()->isUnsignedIntegerOrEnumerationType() || !intLit->getValue().isNegative()) {
			value = toString(intLit->getValue().getLimitedValue());
		} else {
			value = toString(intLit->getValue().getSExtValue());
		}

		core::TypePtr type = convertExprType(intLit);

		frontend_assert(!value.empty()) << "literal cannot be an empty string";
		retExpr = builder.literal(type, value);

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FLOATING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitFloatingLiteral(const clang::FloatingLiteral* floatLit) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(floatLit, retExpr);

		const llvm::fltSemantics& sema = floatLit->getValue().getSemantics();

		llvm::SmallVector<char, 24> buff;
		floatLit->getValue().toString(buff, 0, 0);
		std::string strVal(buff.begin(), buff.end());

		if(llvm::APFloat::semanticsPrecision(sema) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEsingle)) {
			retExpr = builder.literal(strVal, basic.getReal4());
		} else if(llvm::APFloat::semanticsPrecision(sema) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEdouble)) {
			retExpr = builder.literal(strVal, basic.getReal8());
		} else if(llvm::APFloat::semanticsPrecision(sema) == llvm::APFloat::semanticsPrecision(llvm::APFloat::x87DoubleExtended)) {
			retExpr = builder.literal(strVal, basic.getReal16());
		} else {
			frontend_assert(false) << "Unsupported floating point literal type encountered.";
		}

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCharacterLiteral(const clang::CharacterLiteral* charLit) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(charLit, retExpr);

		std::string value;
		if(charLit->getKind() == clang::CharacterLiteral::Ascii) {
			value = "\'" + insieme::utils::escapeChar(charLit->getValue()) + "\'";
		} else {
			frontend_assert(false) << "Non-ASCII character literals unsupported";
		}

		return retExpr = builder.literal(value, converter.convertType(charLit->getType()));
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitStringLiteral(const clang::StringLiteral* stringLit) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(stringLit, retExpr);

		// retrieve data itself
		std::string strValue = stringLit->getBytes().str();
		int vectorLength = strValue.length();
		core::TypePtr elemType;
		switch(stringLit->getKind()) {
		case clang::StringLiteral::Ascii:
		case clang::StringLiteral::UTF8: elemType = basic.getChar(); break;
		case clang::StringLiteral::UTF16:
			elemType = basic.getWChar16();
			vectorLength /= 2;
			converter.warnings.insert("Insieme widechar support is experimental, check on windows");
			break;
		case clang::StringLiteral::UTF32:
		case clang::StringLiteral::Wide: {
			// some literature about encodings transformation
			// http://www.joelonsoftware.com/articles/Unicode.html
			vectorLength = stringLit->getBytes().size() / 4;
			elemType = basic.getWChar32();

			size_t size = stringLit->getBytes().size();
			size_t outSize = size / 4;
			char buff[size];
			char out[outSize];
			char* rptr = buff;
			char* wptr = out;
			memcpy(buff, stringLit->getBytes().data(), size);
			iconv_t cd = iconv_open("UTF-8", "UTF-32");
			iconv(cd, &rptr, &size, &wptr, &outSize);
			frontend_assert(size == 0) << "encoding modification failed.... \n";
			strValue = std::string(out, vectorLength);
			converter.warnings.insert("Insieme widechar support is experimental");
			break;
		}
		}
		frontend_assert(elemType);
		vectorLength += 1; // add the null char

		if(stringLit->getKind() == clang::StringLiteral::Ascii) {
			strValue = insieme::utils::escapeString(strValue);
		}

		retExpr = builder.literal(convertExprType(stringLit), "\"" + strValue + "\"");
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTHESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitParenExpr(const clang::ParenExpr* parExpr) {
		core::ExpressionPtr retExpr = converter.convertExpr(parExpr->getSubExpr());
		LOG_EXPR_CONVERSION(parExpr, retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					      GNU NULL EXPR EXPRESSION
	//
	// GNUNullExpr - Implements the GNU __null extension, which is a name for a
	// null pointer constant that has integral type (e.g., int or long) and is
	// the same size and alignment as a pointer. The __null extension is
	// typically only used by system headers, which define NULL as __null in
	// C++ rather than using 0 (which is an integer that may not match the size
	// of a pointer).
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitGNUNullExpr(const clang::GNUNullExpr* nullExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(nullExpr, retIr);
		retIr = builder.getZero(converter.convertType(nullExpr->getType()));
		return retIr;
	}

	core::ExpressionPtr Converter::ExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
		auto retIr = VisitCastExpr(castExpr);
		insieme::annotations::c::markAsImplicit(retIr);
		return retIr;
	}
	core::ExpressionPtr Converter::ExprConverter::VisitExplicitCastExpr(const clang::ExplicitCastExpr* castExpr) {
		return VisitCastExpr(castExpr);
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCastExpr(const clang::CastExpr* castExpr) {
		core::ExpressionPtr retIr = utils::performClangCastOnIR(converter, castExpr);
		LOG_EXPR_CONVERSION(castExpr, retIr);
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCallExpr(const clang::CallExpr* callExpr) {
		// return converted node
		core::ExpressionPtr irExpr;
		LOG_EXPR_CONVERSION(callExpr, irExpr);

		core::ExpressionPtr irCallee = converter.convertExpr(callExpr->getCallee());
		frontend_assert_expr(core::lang::isPointer, irCallee) << "Expecting callee to be of function pointer type, is: " << *irCallee;

		core::ExpressionList irArguments;
		for(auto arg : callExpr->arguments()) {
			irArguments.push_back(converter.convertExpr(arg));
		}

		// simplify generated call if direct call of function
		auto& pExt = irCallee->getNodeManager().getLangExtension<core::lang::PointerExtension>();
		if(core::analysis::isCallOf(irCallee, pExt.getPtrOfFunction())) {
			irCallee = core::analysis::getArgument(irCallee, 0);
		} else {
			irCallee = core::lang::buildPtrDeref(irCallee);
		}

		irExpr = builder.callExpr(converter.convertType(callExpr->getCallReturnType()), irCallee, irArguments);
		return irExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	// The identifier __func__ shall be implicitly declared by the translator as if, immediately following
	// the opening brace of each function definition, the declaration
	// static const char __func__[] = "function-name";
	// appeared, where function-name is the name of the lexically-enclosing function.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitPredefinedExpr(const clang::PredefinedExpr* preExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(preExpr, retIr);

		retIr = converter.convertExpr(preExpr->getFunctionName());

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						UnaryExprOrTypeTraitExpr
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// UnaryExprOrTypeTraitExpr - expression with either a type or (unevaluated)
	// expression operand. Used for sizeof/alignof (C99 6.5.3.4) and vec_step
	// (OpenCL 1.1 6.11.12).
	core::ExpressionPtr Converter::ExprConverter::VisitUnaryExprOrTypeTraitExpr(const clang::UnaryExprOrTypeTraitExpr* expr) {
		core::ExpressionPtr irNode;
		LOG_EXPR_CONVERSION(expr, irNode);

		core::TypePtr&& type =
				expr->isArgumentType() ? converter.convertType(expr->getArgumentType()) : converter.convertType(expr->getArgumentExpr()->getType());

		switch(expr->getKind()) {
		case clang::UETT_SizeOf:
			irNode = builder.callExpr(basic.getSizeof(), builder.getTypeLiteral(type));
			break;

		case clang::UETT_AlignOf:
			frontend_assert(false) << "allign of is not supported";

		case clang::UETT_VecStep:
			frontend_assert(false) << "vect step no suported in C (this is ocl feature).";

		default: frontend_assert(false) << "UnaryExprOrTypeTraitExpr not handled in C.";
		}

		return irNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							MEMBER EXPRESSION
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(membExpr, retIr);
		core::ExpressionPtr base = converter.convertExpr(membExpr->getBase());

		// deal with pointer accesses by conversion to ref
		if(membExpr->isArrow()) {
			frontend_assert(core::lang::isPointer(base)) << "Arrow member access on non-pointer " << dumpColor(base);
			base = core::lang::buildPtrToRef(base);
		}

		auto memberDecl = membExpr->getMemberDecl();
		frontend_assert(llvm::isa<clang::FieldDecl>(memberDecl)) << "non-field member in C";
		string memName = frontend::utils::getNameForField(llvm::dyn_cast<clang::FieldDecl>(memberDecl), converter.getSourceManager());
		auto retType = converter.convertType(memberDecl->getType());
 		auto retTypeLit = builder.getTypeLiteral(retType);
		frontend_assert(retType);

		core::ExpressionPtr access = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
		frontend_assert(access);

		// track whether we need to unwrap an outer ref generated by ref_member_access
		bool unwrap = core::lang::isReference(retType);

		// if base isn't a ref, we have to materialize it here and unwrap it afterwards in any case
		if(!core::lang::isReference(base)) {
			base = frontend::utils::convertMaterializingExpr(converter, base);
			unwrap = true;
		}

		// we have to take qualifiers from either the object or the entire expression, depending on whether we are dealing with a ref member
		auto qualifiedType = membExpr->getType();
		if(unwrap) qualifiedType = membExpr->getBase()->getType();
		retType = builder.refType(retType, qualifiedType.isConstQualified(), qualifiedType.isVolatileQualified());

		retIr = builder.callExpr(retType, access, core::lang::toPlainReference(base), builder.getIdentifierLiteral(memName), retTypeLit);

		// member expression node can have implicit r-value semantic
		if(unwrap || membExpr->isRValue()) retIr = builder.deref(retIr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	core::ExpressionPtr Converter::ExprConverter::createUnaryExpression(const core::TypePtr& exprTy, const core::ExpressionPtr& subExpr,
	                                                                    clang::UnaryOperator::Opcode cop) {

		// A unary AddressOf operator translates to a ptr-from-ref in IR ----------------------------------------------------------------------- ADDRESSOF -
		if(cop == clang::UO_AddrOf) {
			// if function type, handle with special operator
			if(subExpr->getType().isa<core::FunctionTypePtr>()) {
				return core::lang::buildPtrOfFunction(subExpr);
			} else {
				return core::lang::buildPtrFromRef(subExpr);
			}
		}

		// A unary Deref operator translates to a ptr-to-ref operation in IR ----------------------------------------------------------------------- DEREF -
		if(cop == clang::UO_Deref) {
			frontend_assert(core::lang::isPointer(subExpr)) << "Deref operation only possible on pointers, got:\n" << dumpColor(subExpr->getType());
			// if function pointer type, access function directly
			if(core::lang::PointerType(subExpr->getType()).getElementType().isa<core::FunctionTypePtr>()) {
				return core::lang::buildPtrDeref(subExpr);
			} else {
				return core::lang::buildPtrToRef(subExpr);
			}
		}

		// "__extension__" operator ignored in IR ---------------------------------------------------------------------------------------------- EXTENSION -
		if(cop == clang::UO_Extension) {
			return subExpr;
		}

		auto opIt = unOpMap.find(cop);
		frontend_assert(opIt != unOpMap.end()) << "Unary Operator not implemented (" << clang::UnaryOperator::getOpcodeStr(cop).str() << ")";
		auto op = opIt->second;

		if(core::lang::isReference(subExpr) && core::lang::isPointer(core::analysis::getReferencedType(subExpr->getType()))) {
			return core::lang::buildPtrOperation(op, subExpr);
		}

		switch(op) {
		case core::lang::BasicGenerator::Operator::PostInc: return builder.postInc(subExpr);
		case core::lang::BasicGenerator::Operator::PostDec: return builder.postDec(subExpr);
		case core::lang::BasicGenerator::Operator::PreInc: return builder.preInc(subExpr);
		case core::lang::BasicGenerator::Operator::PreDec: return builder.preDec(subExpr);

		case core::lang::BasicGenerator::Operator::Plus: return builder.plus(subExpr);
		case core::lang::BasicGenerator::Operator::Minus: return builder.minus(subExpr);
		case core::lang::BasicGenerator::Operator::LNot:  return builder.logicNeg(utils::exprToBool(subExpr));

		default: break;
		}

		return builder.unaryOp(basic.getOperator(exprTy, op), subExpr);
	}

	core::ExpressionPtr Converter::ExprConverter::createBinaryExpression(core::TypePtr exprTy, core::ExpressionPtr left, core::ExpressionPtr right,
	                                                                     const clang::BinaryOperator* clangBinOp) {
		auto cop = clangBinOp->getOpcode();

		// if the binary operator is a comma separated expression, we convert it into a function call which returns the value of the last expression --- COMMA -
		if(cop == clang::BO_Comma) {
			return utils::buildCommaOperator(builder.wrapLazy(left), builder.wrapLazy(right));
		}

		// we need to translate the semantics of C-style assignments to a function call ----------------------------------------------------------- ASSIGNMENT -
		if(cop == clang::BO_Assign) {
			return utils::buildCStyleAssignment(left, right);
		}

		// the logical operators && and || need to eval their arguments lazily ------------------------------------------------------------------ LAZY LOGICAL -
		if(cop == clang::BO_LAnd || cop == clang::BO_LOr) {
			exprTy = basic.getBool();
			left = utils::exprToBool(left);
			right = builder.wrapLazy(utils::exprToBool(right));
		}

		auto opIt = binOpMap.find(cop);
		frontend_assert(opIt != binOpMap.end()) << "Binary Operator not implemented (" << clang::BinaryOperator::getOpcodeStr(cop).str() << ")";
		auto binOp = opIt->second;

		if((core::lang::isReference(left) && core::lang::isPointer(core::analysis::getReferencedType(left->getType())))
				|| (core::lang::isReference(right) && core::lang::isPointer(core::analysis::getReferencedType(right->getType())))
				|| (core::lang::isPointer(left->getType())) || (core::lang::isPointer(right->getType()))) {
			return core::lang::buildPtrOperation(binOp, left, right);
		}

		// the operator type needs to be deduced from operands (sometimes)
		core::ExpressionPtr irOp;

		// Comparisons will return a boolean (integer) but the operator requires different parameter types
		if(clangBinOp->isComparisonOp()) {
			// find first super type common to both types,
			//      int4   uint4
			//       |   /   |
			//      int8 <- unit8    => (int4, uint8) should be int8
			irOp = basic.getOperator(core::types::getBiggestCommonSubType(left->getType(), right->getType()), binOp);
		}
		else {
			irOp = basic.getOperator(exprTy, binOp);
		}

		return builder.binaryOp(irOp, left, right);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND ASSINGMENT OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCompoundAssignOperator(const clang::CompoundAssignOperator* compOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(compOp, retIr);

		core::ExpressionPtr lhs = converter.convertExpr(compOp->getLHS());
		core::ExpressionPtr rhs = converter.convertExpr(compOp->getRHS());

		// make lhs a plain reference
		lhs = core::lang::toPlainReference(lhs);

		core::ExpressionPtr compAssignFunc;
		auto& compOpExt = converter.getNodeManager().getLangExtension<core::lang::CompoundOpsExtension>();

		assert_true(core::lang::isReference(lhs->getType())) << "left side must be assignable";

		// Get a function pointer to the appropriate compound operator
		switch(compOp->getOpcode()) {
		case clang::BO_AddAssign: compAssignFunc = compOpExt.getCompAssignAdd();        break;
		case clang::BO_SubAssign: compAssignFunc = compOpExt.getCompAssignSubtract();   break;
		case clang::BO_MulAssign: compAssignFunc = compOpExt.getCompAssignMultiply();   break;
		case clang::BO_DivAssign: compAssignFunc = compOpExt.getCompAssignDivide();     break;
		case clang::BO_RemAssign: compAssignFunc = compOpExt.getCompAssignModulo();     break;
		case clang::BO_AndAssign: compAssignFunc = compOpExt.getCompAssignBitwiseAnd(); break;
		case clang::BO_OrAssign : compAssignFunc = compOpExt.getCompAssignBitwiseOr();  break;
		case clang::BO_XorAssign: compAssignFunc = compOpExt.getCompAssignBitwiseXor(); break;
		case clang::BO_ShlAssign: compAssignFunc = compOpExt.getCompAssignLeftShift();  break;
		case clang::BO_ShrAssign: compAssignFunc = compOpExt.getCompAssignRightShift(); break;
		default: assert_fail() << "Found unknown compound operator (Opcode: " << compOp->getOpcode() << ")";
		}

		// Special case where LHS is a char is handled in compound operators extension.
		retIr = builder.deref(builder.callExpr(compAssignFunc, lhs, rhs));

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		core::ExpressionPtr lhs = converter.convertExpr(binOp->getLHS());
		core::ExpressionPtr rhs = converter.convertExpr(binOp->getRHS());
		core::TypePtr exprTy = converter.convertType(binOp->getType());

		retIr = createBinaryExpression(exprTy, lhs, rhs, binOp);

		// in C, bool operations return an int
		if(retIr->getType() == basic.getBool()) {
			retIr = utils::buildBoolToInt(retIr);
		}

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitUnaryOperator(const clang::UnaryOperator* unOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(unOp, retIr);

		core::ExpressionPtr subExpr = converter.convertExpr(unOp->getSubExpr());
		core::TypePtr exprTy = converter.convertType(unOp->getType());

		retIr = createUnaryExpression(exprTy, subExpr, unOp->getOpcode());

		// in C, bool operations return an int
		if(retIr->getType() == basic.getBool()) {
			retIr = utils::buildBoolToInt(retIr);
		}

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitConditionalOperator(const clang::ConditionalOperator* condOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(condOp, retIr);

		core::ExpressionPtr condExpr = converter.convertExpr(condOp->getCond());
		core::ExpressionPtr trueExpr = converter.convertExpr(condOp->getTrueExpr());
		core::ExpressionPtr falseExpr = converter.convertExpr(condOp->getFalseExpr());

		// before we pack the true and false expressions into lazies we need to check whether they have the same reference properties
		if(core::lang::isReference(trueExpr) && core::lang::isReference(falseExpr)) {
			core::lang::ReferenceType trueRefType(trueExpr);
			core::lang::ReferenceType falseRefType(falseExpr);

			if(trueRefType.getElementType() == falseRefType.getElementType()) {
				core::lang::ReferenceType targetTrueRefType(trueRefType);
				core::lang::ReferenceType targetFalseRefType(falseRefType);

				// if one of them is const, cast them both to const
				if(trueRefType.isConst() || falseRefType.isConst()) {
					targetTrueRefType.setConst(true);
					targetFalseRefType.setConst(true);
				}

				// if their kinds differ and one of them is plain, cast them both to plain
				if(trueRefType.getKind() != falseRefType.getKind()) {
					if(trueRefType.getKind() == core::lang::ReferenceType::Kind::Plain) {
						targetFalseRefType.setKind(core::lang::ReferenceType::Kind::Plain);
					} else if(falseRefType.getKind() == core::lang::ReferenceType::Kind::Plain) {
						targetTrueRefType.setKind(core::lang::ReferenceType::Kind::Plain);
					}
				}
				// now build the ref casts. In case the type is already correct, this is a no-op
				trueExpr = core::lang::buildRefCast(trueExpr, targetTrueRefType.toType());
				falseExpr = core::lang::buildRefCast(falseExpr, targetFalseRefType.toType());
			}
		}
		// the same goes for pointer types
		if(core::lang::isPointer(trueExpr) && core::lang::isPointer(falseExpr)) {
			core::lang::PointerType truePtrType(trueExpr);
			core::lang::PointerType falsePtrType(falseExpr);

			// if one of them is const, cast them both to const
			if(truePtrType.isConst() || falsePtrType.isConst()) {
				trueExpr = core::lang::buildPtrCast(trueExpr, true, truePtrType.isVolatile());
				falseExpr = core::lang::buildPtrCast(falseExpr, true, falsePtrType.isVolatile());
			}
		}

		condExpr = utils::exprToBool(condExpr);
		trueExpr = builder.wrapLazy(trueExpr);
		falseExpr = builder.wrapLazy(falseExpr);

		retIr = builder.ite(condExpr, trueExpr, falseExpr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitArraySubscriptExpr(const clang::ArraySubscriptExpr* arraySubExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(arraySubExpr, retIr);

		core::ExpressionPtr ptrExpr = converter.convertExpr(arraySubExpr->getBase());
		core::ExpressionPtr idxExpr = converter.convertExpr(arraySubExpr->getIdx());

		retIr = core::lang::buildPtrSubscript(ptrExpr, idxExpr);

		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(declRef, retIr);

		if(const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(declRef->getDecl())) {
			// fall back to literals for extern variables
			if(varDecl->hasExternalStorage()) {
				retIr = builder.literal(declRef->getDecl()->getNameAsString(), converter.convertVarType(declRef->getType()));
			} else {
				retIr = converter.getVarMan()->lookup(varDecl);
			}
			return retIr;
		}

		if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(declRef->getDecl())) {
			// fall back to literals for builtin functions
			if(declRef->getType()->isBuiltinType()) {
				auto vd = llvm::dyn_cast<clang::ValueDecl>(declRef->getDecl());
				auto clangTy = vd ? vd->getType() : declRef->getType();
				retIr = builder.literal(declRef->getDecl()->getNameAsString(), converter.convertType(clangTy));
			} else {
				if(!converter.getFunMan()->contains(funcDecl->getCanonicalDecl())) {
					converter.getDeclConverter()->Visit(const_cast<clang::FunctionDecl*>(funcDecl));
				}
				retIr = converter.getFunMan()->lookup(funcDecl->getCanonicalDecl());
			}
			return retIr;
		}

		if(const clang::EnumConstantDecl* decl = llvm::dyn_cast<clang::EnumConstantDecl>(declRef->getDecl())) {
			retIr = utils::buildEnumConstantExpression(converter, decl);
			auto irT = converter.convertType(declRef->getType());
			// in C, the target type of the decl ref expression is an int, so we cast here
			// in C++, the target type is an enum type, so nothing will happen
			return builder.numericCast(retIr, irT);
		}

		frontend_assert(false) << "DeclRefExpr not handled: " << dumpClang(declRef, converter.getSourceManager());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  VECTOR/STRUCT INITALIZATION EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	namespace {
		std::pair<core::TagTypePtr, core::GenericTypePtr> lookupRecordTypes(Converter& converter, const clang::InitListExpr* initList) {
			auto clangType = initList->getType();
			auto clangRecordType = llvm::dyn_cast<clang::RecordType>(clangType->getCanonicalTypeUnqualified());
			frontend_assert(clangRecordType);
			auto irGenericType = converter.getRecordMan()->lookup(clangRecordType->getDecl());
			auto irRecordTypeIt = converter.getIRTranslationUnit().getTypes().find(irGenericType);
			frontend_assert(irRecordTypeIt != converter.getIRTranslationUnit().getTypes().end()) << "Initializing unregistered record type";
			auto irRecordType = irRecordTypeIt->second.as<core::TagTypePtr>();
			return std::make_pair(irRecordType, irGenericType);
		}

		core::ExpressionList buildExprListForStructInit(Converter& converter, const clang::InitListExpr* initList) {
			core::TagTypePtr irRecordType = lookupRecordTypes(converter, initList).first;
			core::ExpressionList values;
			core::FieldList fields = irRecordType->getFields();
			unsigned i = 0;
			for(auto entry : fields) {
				core::ExpressionPtr initExp = converter.getIRBuilder().getZero(entry->getType());
				if(i < initList->getNumInits()) initExp = converter.convertInitExpr(initList->getInit(i));
				values.push_back(initExp);
				++i;
			}
			return values;
		}
	}

	core::ExpressionPtr Converter::ExprConverter::VisitInitListExpr(const clang::InitListExpr* initList) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initList, retIr);

		// check on clang type, at this point in the IR no struct types exist
		auto clangType = initList->getType();
		// convert the type if this is the first time we encounter it
		auto initListType = converter.convertType(clangType);
		auto convertedExprType = convertExprType(initList);
		if(!core::lang::isReference(convertedExprType)) convertedExprType = builder.refType(convertedExprType);
		auto genType = convertedExprType.as<core::GenericTypePtr>();

		if(clangType->isStructureType() || clangType->isClassType()) {
			auto types = lookupRecordTypes(converter, initList);
			auto values = buildExprListForStructInit(converter, initList);
			core::DeclarationList decls;

			// now we might have to add casts to those expressions, as they are effectively initializations
			const auto& genInitListType = initListType.isa<core::GenericTypePtr>();
			assert_true(genInitListType);
			assert_true(converter.getIRTranslationUnit().getTypes().find(genInitListType) != converter.getIRTranslationUnit().getTypes().end());
			const auto& structType = converter.getIRTranslationUnit().getTypes().find(genInitListType)->second;
			assert_true(structType->isStruct());
			// we use the field type to get the desired decl type and we may also need to cast the init expression accordingly
			for(unsigned index = 0; index < values.size(); ++index) {
				auto declType = core::transform::materialize(structType->getStruct()->getFields()->getElement(index)->getType());
				auto init = utils::castInitializationIfNotMaterializing(declType, values[index]);
				decls.push_back(builder.declaration(declType, init));
			}

			retIr = builder.initExpr(core::lang::buildRefTemp(genType), decls);
			retIr = utils::fixTempMemoryInInitExpressionInits(retIr);
		}
		else if(clangType->isUnionType()) {
			auto types = lookupRecordTypes(converter, initList);
			auto initExp = converter.convertInitExpr(initList->getInit(0));
			// note that we don't need to convert the init expression for unions, as unions must not contain reference members in C++
			retIr = builder.initExprTemp(genType, initExp);
			retIr = utils::fixTempMemoryInInitExpressionInits(retIr);
		}
		else if(clangType->isArrayType()) {
			auto gT = converter.convertType(clangType).as<core::GenericTypePtr>();
			frontend_assert(core::lang::isArray(gT)) << "Clang InitListExpr of unexpected generic type (should be array):\n" << dumpColor(gT);
			core::ExpressionList initExps;
			// handle special case if init list holds a single string literal
			if(initList->getNumInits() == 1 && llvm::dyn_cast<clang::StringLiteral>(initList->getInit(0))) {
				retIr = convertInitExpr(initList->getInit(0));
				return retIr;
			} else {
				for(unsigned i = 0; i < initList->getNumInits(); ++i) { // yes, that is really the best way to do this in clang 3.6
					initExps.push_back(convertInitExpr(initList->getInit(i)));
				}
				retIr = builder.initExprTemp(genType, initExps);
			}
			retIr = utils::fixTempMemoryInInitExpressionInits(retIr);
		}
		else if(clangType->isScalarType()) {
			retIr = convertInitExpr(initList->getInit(0));
			return retIr;
		} else {
			std::cout << "Clang InitListExpr of unexpected type:\n";
			clangType->dump();
			frontend_assert(false);
		}

		// if used as r-value, deref
		if(initList->isRValue()) {
			retIr = builder.deref(retIr);
		}

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  	COMPOUND LITERAL EXPRESSION
	// Introduced in C99 6.5.2.5, used to initialize structures or arrays with
	// the { } expression, example:
	// 		strcut A a;
	// 		a = (struct A) { 10, 20, 30 };
	//
	//	or:
	//		((int [3]){1,2,3})[2]  -> 2
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCompoundLiteralExpr(const clang::CompoundLiteralExpr* compLitExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(compLitExpr, retIr);


		if(const clang::InitListExpr* initList = llvm::dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer())) {
			retIr = VisitInitListExpr(initList);
			if(compLitExpr->isLValue()) {
				auto& refExt = converter.getNodeManager().getLangExtension<core::lang::ReferenceExtension>();
				if(refExt.isCallOfRefDeref(retIr)) {
					retIr = core::analysis::getArgument(retIr, 0);
					if(auto initExpr = retIr.as<core::InitExprPtr>()) {
						retIr = builder.initExpr(utils::buildFERefTemp(core::analysis::getReferencedType(retIr->getType())), initExpr->getInitExprList());
					}
				}
				else {
					retIr = builder.refTemp(retIr);
				}
			}
		}

		if(!retIr) frontend_assert(false) << "Unimplemented type of CompoundLiteralExpr encountered";
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  StmtExpr EXPRESSION
	// Statement Expressions (a GNU C extension) allow statements to appear as
	// an expression by enclosing them in parentheses, e.g.:
	//		({ int x = 5; x+3; })
	// The last entry should be an expression terminated by a semicolon and
	// represents the return type and value, otherwise (last entry == statement)
	// the entire construct has type void
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitStmtExpr(const clang::StmtExpr* stmtExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(stmtExpr, retIr);

		frontend_assert(stmtExpr->getSubStmt()->size() > 0) << "Empty Statement Expression encountered";

		// get compound stmt and convert to IR
		core::StatementPtr subStmtIr = converter.convertStmt(stmtExpr->getSubStmt());
		core::CompoundStmtPtr bodyIr = (subStmtIr.isa<core::CompoundStmtPtr>()) ? subStmtIr.as<core::CompoundStmtPtr>() : builder.compoundStmt(subStmtIr);

		// create new body with <returnStmt <expr>> instead of <expr> as last stmt
		core::StatementList newBody(bodyIr->getStatements());

		auto& lastEntry = newBody.back();
		
		if(lastEntry.isa<core::ExpressionPtr>()) {
			// the last entry is an expression, use its return type and value
			lastEntry = converter.builder.returnStmt(lastEntry.as<core::ExpressionPtr>());
		} else {
			// the last entry is a statement, return type is void
			lastEntry = converter.builder.returnStmt();
		}

		// create call to lambda with new body
		retIr = core::transform::outline(bodyIr->getNodeManager(), converter.builder.compoundStmt(newBody), true);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  IMPLICIT VALUE INIT EXPRESSION
	// generated for skipped struct members in initializations such as
	//      struct { int a; float b; char c; } sifc = { .a = 1, .c = 'a' };
	// ("b" would be initialized by a ImplicitValueInitExpr)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitImplicitValueInitExpr(const clang::ImplicitValueInitExpr* initExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initExpr, retIr);
		core::TypePtr elementType = converter.convertType(initExpr->getType());
		retIr = builder.getZero(elementType);
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  Atomic EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitAtomicExpr(const clang::AtomicExpr* atom) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(atom, retIr);
		//clang::AtomicExpr::AtomicOp operation = atom->getOp();

		//// only c11 atomic operations are handled here (not std::atomic stuff)

		//core::ExpressionPtr ptr = converter.convertExpr(atom->getPtr());
		//// if ptr is ref array -> array ref elem auf richtiges elem
		//// accept pure array type
		//if(ptr->getType()->getNodeType() == insieme::core::NT_ArrayType) { ptr = builder.arrayRefElem(ptr, builder.uintLit(0)); }
		//// also accept ref/array combination
		//if((ptr->getType()->getNodeType() == insieme::core::NT_RefType
		//    && static_pointer_cast<const insieme::core::RefType>(ptr->getType())->getElementType()->getNodeType() == insieme::core::NT_ArrayType)) {
		//	ptr = builder.arrayRefElem(ptr, builder.uintLit(0));
		//}

		//core::ExpressionPtr val = converter.convertExpr(atom->getVal1());
		//// dumpDetail(val);

		//switch(operation) {
		//// c11 atomic operations <stdatomic.h>
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_init: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_load: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_store: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_exchange: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_compare_exchange_strong: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_compare_exchange_weak: assert_not_implemented();

		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_fetch_add:
		//	return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndAdd(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_fetch_sub:
		//	return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndSub(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_fetch_and:
		//	return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndAnd(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_fetch_or: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndOr(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__c11_atomic_fetch_xor:
		//	return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndXor(), ptr, val);

		//// c++11 atomic operations <atomic>
		//case clang::AtomicExpr::AtomicOp::AO__atomic_load: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_load_n: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_store: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_store_n: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_exchange: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_exchange_n: assert_not_implemented();

		//case clang::AtomicExpr::AtomicOp::AO__atomic_compare_exchange: assert_not_implemented();
		//case clang::AtomicExpr::AtomicOp::AO__atomic_compare_exchange_n: assert_not_implemented();

		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_add: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndAdd(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_sub: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndSub(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_and: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndAnd(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_or: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndOr(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_xor: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicFetchAndXor(), ptr, val);

		//case clang::AtomicExpr::AtomicOp::AO__atomic_fetch_nand: assert_not_implemented();

		//case clang::AtomicExpr::AtomicOp::AO__atomic_add_fetch: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicAddAndFetch(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_sub_fetch: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicSubAndFetch(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_and_fetch: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicAndAndFetch(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_or_fetch: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicOrAndFetch(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_xor_fetch: return builder.callExpr(val->getType(), mgr.getLangBasic().getAtomicXorAndFetch(), ptr, val);
		//case clang::AtomicExpr::AtomicOp::AO__atomic_nand_fetch: assert_not_implemented();
		//}

		//assert_fail();
		//return core::ExpressionPtr();
		frontend_assert(false) << "Atomic expressions not implemented.";
		return retIr;
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <iconv.h>

#include "insieme/frontend/expr_converter.h"

#include "insieme/frontend/decl_converter.h"
#include "insieme/frontend/state/function_manager.h"
#include "insieme/frontend/state/record_manager.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/expr_to_bool.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

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
#include "insieme/core/lang/enum.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/annotations/c/include.h"

using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {
		
	//---------------------------------------------------------------------------------------------------------------------
	//										BASE EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------
	
	core::TypePtr Converter::ExprConverter::convertExprType(const clang::Expr* expr) {
		auto qType = expr->getType();
		auto irType = converter.convertType(qType);
		if(expr->getValueKind() == clang::VK_LValue || expr->getValueKind() == clang::VK_XValue) {
			irType = builder.refType(irType, qType.isConstQualified(), qType.isVolatileQualified());
		}
		return irType;
	}

	// translate expression as usual, but convert translated string literals to r-values rather than l-values
	core::ExpressionPtr Converter::ExprConverter::convertInitExpr(const clang::Expr* original) {
		auto expr = converter.convertExpr(original);
		if(original->isLValue()) {
			auto origType = llvm::dyn_cast_or_null<clang::ConstantArrayType>(original->getType()->getAsArrayTypeUnsafe());
			auto lit = expr.isa<core::LiteralPtr>();
			if(!origType || !lit) return expr;
			string s = insieme::utils::unescapeString(lit->getValue()->getValue());
			core::NodeManager& mgr = lit->getNodeManager();
			core::IRBuilder builder(mgr);
			ExpressionList initExps;
			for(char c : s) {
				initExps.push_back(builder.literal(format("'%s'",toString(insieme::utils::escapeChar(c))), mgr.getLangBasic().getChar()));
			}
			initExps.push_back(builder.literal("'\\0'", mgr.getLangBasic().getChar()));
			expr = core::lang::buildArrayCreate(lit->getNodeManager(), origType->getSize().getLimitedValue(), initExps);
			VLOG(2) << "convertInitExpr: translated string literal\n" << dumpClang(original, converter.getSourceManager()) << " - to - \n" << dumpColor(expr);
		}
		return expr;
	}

	// translate expression, but skip outer construct expr and cast references to correct kind
	core::ExpressionPtr Converter::ExprConverter::convertCxxArgExpr(const clang::Expr* clangArgExprInput, const core::TypePtr& targetType) {
		core::ExpressionPtr ret = converter.convertExpr(clangArgExprInput);
		const clang::Expr* clangArgExpr = clangArgExprInput;
		// skip over potential ExprWithCleanups
		auto conWithCleanups = llvm::dyn_cast<clang::ExprWithCleanups>(clangArgExpr);
		if(conWithCleanups) clangArgExpr = conWithCleanups->getSubExpr();
		// detect copy/move constructor calls
		VLOG(2) << "---\nCXX call checking arg: " << dumpClang(clangArgExpr, converter.getCompiler().getSourceManager());
		auto constructExpr = llvm::dyn_cast<clang::CXXConstructExpr>(clangArgExpr);
		if(constructExpr) {
			auto constructor = constructExpr->getConstructor();
			if(constructor->isCopyOrMoveConstructor()) {
				VLOG(2) << "CopyOrMove in param list at " << utils::location(clangArgExpr->getLocStart(), converter.getSourceManager()) << "\n";
				auto prevArg = ret;
				ret = Visit(constructExpr->getArg(0));
				// cast ref as required by copy constructor
				if(core::lang::isReference(ret)) {
					ret = core::lang::buildRefCast(
						ret, prevArg.as<core::CallExprPtr>()->getFunctionExpr()->getType().as<core::FunctionTypePtr>()->getParameterType(1));
				}
				VLOG(2) << "CXX call converted newArg: " << dumpPretty(ret);
			}
		}
		// if a targetType was provided, and we are working on a ref, cast the ref as required
		if(targetType && core::analysis::isRefType(targetType) && core::analysis::isRefType(ret->getType())) {
			auto targetRef = core::lang::ReferenceType(targetType);
			auto retRef = core::lang::ReferenceType(ret->getType());
			ret = core::lang::buildRefKindCast(ret, targetRef.getKind());
		}
		return ret;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CExprConverter::Visit(const clang::Expr* expr) {
		// iterate clang handler list and check if a handler wants to convert the expr
		core::ExpressionPtr retIr;
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->Visit(expr, converter);
			if(retIr) { break; }
		}

		if(!retIr) {
			converter.trackSourceLocation(expr);
			retIr = ConstStmtVisitor<CExprConverter, core::ExpressionPtr>::Visit(expr);
			converter.untrackSourceLocation();
		} else {
			VLOG(2) << "CExprConverter::Visit handled by plugin";
		}

		// print diagnosis messages
		converter.printDiagnosis(expr->getLocStart());

		// call frontend extension post visitors
		for(auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->PostVisit(expr, retIr, converter);
		}

		// attach location annotation
		if(expr->getLocStart().isValid()) {
			auto presStart = converter.getSourceManager().getPresumedLoc(expr->getLocStart());
			auto presEnd = converter.getSourceManager().getPresumedLoc(expr->getLocEnd());
			core::annotations::attachLocation(retIr, std::string(presStart.getFilename()), presStart.getLine(), presStart.getColumn(), presEnd.getLine(),
				                              presEnd.getColumn());
		}

        assert_true(retIr) << " no null return allowed";
		return retIr;
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
			frontend_assert(false) << "Unsupported floating point literal type encountered, possibly a \"long double\".";
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
		core::ExpressionPtr retExpr = Visit(parExpr->getSubExpr());
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
		//core::TypePtr type = converter.convertType(nullExpr->getType());

		//frontend_assert(core::analysis::isArrayType(type->getNodeType())) << "C pointer type must of type array<'a,1>\n";
		//return (retIr = builder.refReinterpret(mgr.getLangBasic().getRefNull(), type));
		frontend_assert(false) << "GNU NUllExpr not implemented.";
		return retIr;
	}

	core::ExpressionPtr Converter::ExprConverter::VisitImplicitCastExpr(const clang::ImplicitCastExpr* castExpr) {
		return VisitCastExpr(castExpr);
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
		frontend_assert_expr(core::lang::isPointer, irCallee) << "Expecting callee to be of function pointer type";

		core::ExpressionList irArguments;
		for(auto arg : callExpr->arguments()) {
			irArguments.push_back(Visit(arg));
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

		retIr = Visit(preExpr->getFunctionName());

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
		core::ExpressionPtr base = Visit(membExpr->getBase());
		
		// deal with pointer accesses by conversion to ref
		if(membExpr->isArrow()) {
			frontend_assert(core::lang::isPointer(base)) << "Arrow member access on non-pointer " << dumpColor(base);
			base = core::lang::buildPtrToRef(base);
		}

		auto memberDecl = membExpr->getMemberDecl();
		frontend_assert(llvm::isa<clang::FieldDecl>(memberDecl)) << "non-field member in C";
		string memName = frontend::utils::getNameForField(llvm::dyn_cast<clang::FieldDecl>(memberDecl), converter.getSourceManager());
		auto retType = converter.convertType(membExpr->getType());
		auto retTypeLit = builder.getTypeLiteral(retType);
		frontend_assert(retType);

		core::ExpressionPtr access = mgr.getLangBasic().getCompositeMemberAccess();
		frontend_assert(access);

		// if a ref struct is accessed, we get a ref to its member
		if(core::lang::isReference(base)) {
			access = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
			retType = builder.refType(retType);
		}

		retIr = builder.callExpr(retType, access, base, builder.getIdentifierLiteral(memName), retTypeLit);
		frontend_assert(retIr);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
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
            // find first super type commom to both types,
            //      int4   uint4
            //       |   /   |
            //      int8 <- unit8    => (int4, uint8) should be int8
            irOp = basic.getOperator(core::types::getBiggestCommonSubType( left->getType(), right->getType()), binOp);
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

		core::ExpressionPtr lhs = Visit(compOp->getLHS());
		core::ExpressionPtr rhs = Visit(compOp->getRHS());
		core::TypePtr exprTy = converter.convertType(compOp->getType());

		assert_true(core::lang::isReference(lhs->getType())) << "left side must be assignable";

		retIr = createBinaryExpression(exprTy, builder.deref(lhs), rhs, compOp);
		retIr = utils::buildCStyleAssignment(lhs, retIr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		core::ExpressionPtr lhs = Visit(binOp->getLHS());
		core::ExpressionPtr rhs = Visit(binOp->getRHS());
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

		core::ExpressionPtr subExpr = Visit(unOp->getSubExpr());
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

		core::TypePtr retTy = converter.convertType(condOp->getType());
		core::ExpressionPtr trueExpr = Visit(condOp->getTrueExpr());
		core::ExpressionPtr falseExpr = Visit(condOp->getFalseExpr());
		core::ExpressionPtr condExpr = Visit(condOp->getCond());
		
		trueExpr = builder.wrapLazy(trueExpr);
		falseExpr = builder.wrapLazy(falseExpr);
		condExpr = utils::exprToBool(condExpr);

		retIr = builder.callExpr(basic.getIfThenElse(), condExpr, trueExpr, falseExpr);

		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitArraySubscriptExpr(const clang::ArraySubscriptExpr* arraySubExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(arraySubExpr, retIr);
		
		core::ExpressionPtr ptrExpr = Visit(arraySubExpr->getBase());
		core::ExpressionPtr idxExpr = Visit(arraySubExpr->getIdx());

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
			const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(decl->getDeclContext())->getTypeForDecl());
			auto enumDecl = enumType->getDecl();
			auto enumClassType = converter.convertType(enumDecl->getIntegerType());
			//get the init val of the enum constant decl
			core::ExpressionPtr val;
			std::string value = decl->getInitVal().toString(10);
			val = builder.literal(enumClassType, value);
			if(val->getType() != enumClassType) {
				val = builder.numericCast(val, enumClassType);
			}

			//convert and return the struct init expr
			return builder.numericCast(val, builder.getLangBasic().getInt4());
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

		core::NamedValueList buildNamedValuesForStructInit(Converter& converter, const clang::InitListExpr* initList) {
			core::TagTypePtr irRecordType = lookupRecordTypes(converter, initList).first;
			core::NamedValueList values;
			core::FieldList fields = irRecordType->getFields();
			unsigned i = 0;
			for(auto entry : fields) {
				core::ExpressionPtr initExp = converter.getIRBuilder().getZero(entry->getType());
				if(i < initList->getNumInits()) initExp = converter.convertInitExpr(initList->getInit(i));
				values.push_back(converter.getIRBuilder().namedValue(entry->getName(), initExp));
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
		converter.convertType(clangType);
		
		if(clangType->isStructureType()) {
			auto types = lookupRecordTypes(converter, initList);
			auto values = buildNamedValuesForStructInit(converter, initList);
			retIr = builder.structExpr(types.first, values);
		}
		else if(clangType->isUnionType()) {
			auto types = lookupRecordTypes(converter, initList);
			auto member = builder.stringValue(initList->getInitializedFieldInUnion()->getNameAsString());
			auto initExp = Visit(initList->getInit(0));
			retIr = builder.unionExpr(types.first, member, initExp);
		}
		else if(clangType->isArrayType()) {
			auto gT = converter.convertType(clangType).as<core::GenericTypePtr>();
			frontend_assert(core::lang::isArray(gT)) << "Clang InitListExpr of unexpected generic type (should be array):\n" << dumpColor(gT);
			ExpressionList initExps;
			for(unsigned i = 0; i < initList->getNumInits(); ++i) { // yes, that is really the best way to do this in clang 3.6
				initExps.push_back(convertInitExpr(initList->getInit(i)));
			}
			retIr = core::lang::buildArrayCreate(core::lang::getArraySize(gT), initExps);
		} 
		else if(clangType->isScalarType()) {
			retIr = convertInitExpr(initList->getInit(0));
		} else {
			frontend_assert(false) << "Clang InitListExpr of unexpected type";
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
			// for some reason, this is an lvalue
			retIr = builder.refTemp(Visit(initList));
		}
		
		if(!retIr) frontend_assert(false) << "Unimplemented type of CompoundLiteralExpr encountered";
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  StmtExpr EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitStmtExpr(const clang::StmtExpr* stmtExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(stmtExpr, retIr);

		//// get compound stmt and convert to ir
		//const clang::CompoundStmt* inner = stmtExpr->getSubStmt();
		//core::StatementPtr subStmtIr = converter.convertStmt(inner);

		//// FIXME: aggregateStmts in stmt_wrapper _removes_ compoundStmt if compoundStmt contains only one stmt
		//core::CompoundStmtPtr innerIr = (subStmtIr.isa<core::CompoundStmtPtr>()) ? subStmtIr.as<core::CompoundStmtPtr>() : builder.compoundStmt(subStmtIr);

		//// create new body with <returnStmt <expr>> instead of <expr> as last stmt
		//core::StatementList newBody;
		//for(auto it = innerIr->getStatements().begin(); it != innerIr->getStatements().end() - 1; ++it) {
		//	newBody.push_back(*it);
		//}

		//core::TypePtr lambdaRetType = converter.convertType(stmtExpr->getType());
		//core::ExpressionPtr exprToReturn = (innerIr->getStatements().end() - 1)->as<core::ExpressionPtr>();

		//// fix type
		//if(exprToReturn->getType() != lambdaRetType) {
		//	if(auto refty = exprToReturn->getType().isa<core::RefTypePtr>()) {
		//		if(converter.lookupTypeDetails(refty->getElementType()) == converter.lookupTypeDetails(lambdaRetType)) {
		//			exprToReturn = builder.deref(exprToReturn);
		//		}
		//	} else if(converter.lookupTypeDetails(exprToReturn->getType()) != converter.lookupTypeDetails(lambdaRetType)) {
		//		exprToReturn = core::types::smartCast(exprToReturn, lambdaRetType);
		//	}
		//}
		//core::StatementPtr retExpr = converter.builder.returnStmt(exprToReturn);
		//newBody.push_back(retExpr);

		//// build the lambda and its parameters
		//core::StatementPtr&& lambdaBody = converter.builder.compoundStmt(newBody);
		//vector<core::VariablePtr> params = core::analysis::getFreeVariables(lambdaBody);
		//core::LambdaExprPtr lambda = converter.builder.lambdaExpr(lambdaRetType, lambdaBody, params);

		//// build the lambda call and its arguments
		//vector<core::ExpressionPtr> packedArgs;
		//std::for_each(params.begin(), params.end(), [&packedArgs](core::VariablePtr varPtr) { packedArgs.push_back(varPtr); });

		//return retIr = builder.callExpr(lambdaRetType, lambda, packedArgs);

		frontend_assert(false) << "GNU C statement expressions not implemented.";
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

		//core::ExpressionPtr ptr = Visit(atom->getPtr());
		//// if ptr is ref array -> array ref elem auf richtiges elem
		//// accept pure array type
		//if(ptr->getType()->getNodeType() == insieme::core::NT_ArrayType) { ptr = builder.arrayRefElem(ptr, builder.uintLit(0)); }
		//// also accept ref/array combination
		//if((ptr->getType()->getNodeType() == insieme::core::NT_RefType
		//    && static_pointer_cast<const insieme::core::RefType>(ptr->getType())->getElementType()->getNodeType() == insieme::core::NT_ArrayType)) {
		//	ptr = builder.arrayRefElem(ptr, builder.uintLit(0));
		//}

		//core::ExpressionPtr val = Visit(atom->getVal1());
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

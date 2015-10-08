/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum_extension.h"
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
		if(expr->getValueKind() == clang::VK_LValue) irType = builder.refType(irType, qType.isConstQualified(), qType.isVolatileQualified());
		return irType;
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
		unsigned int v = charLit->getValue();
		
		if(charLit->getKind() == clang::CharacterLiteral::Ascii) {
			value.append("\'");
			if(v == '\\') {
				value.append("\\\\");
			} else if(v == '\n') {
				value.append("\\n");
			} else if(v == '\t') {
				value.append("\\t");
			} else if(v == '\b') {
				value.append("\\b");
			} else if(v == '\a') {
				value.append("\\a");
			} else if(v == '\v') {
				value.append("\\v");
			} else if(v == '\r') {
				value.append("\\r");
			} else if(v == '\f') {
				value.append("\\f");
			} else if(v == '\?') {
				value.append("\\\?");
			} else if(v == '\'') {
				value.append("\\\'");
			} else if(v == '\"')
				value.append("\\\"");
			else if(v == '\0') {
				value.append("\\0");
			} else {
				char cad[2];
				cad[0] = v;
				cad[1] = '\0';
				value.append(cad);
			}
			value.append("\'");
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
		int vectorLenght = strValue.length();
		core::TypePtr elemType;
		switch(stringLit->getKind()) {
		case clang::StringLiteral::Ascii:
		case clang::StringLiteral::UTF8: elemType = basic.getChar(); break;
		case clang::StringLiteral::UTF16:
			elemType = basic.getWChar16();
			vectorLenght /= 2;
			converter.warnings.insert("Insieme widechar support is experimental, check on windows");
			break;
		case clang::StringLiteral::UTF32:
		case clang::StringLiteral::Wide: {
			// some literature about encodings transformation
			// http://www.joelonsoftware.com/articles/Unicode.html
			vectorLenght = stringLit->getBytes().size() / 4;
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
			strValue = std::string(out, vectorLenght);
			converter.warnings.insert("Insieme widechar support is experimental");
			break;
		}
		}
		frontend_assert(elemType);
		vectorLenght += 1; // add the null char

		auto expand = [&](char lookup, const char* replacement) {
			unsigned last = 0;
			unsigned it;
			string rep = replacement;
			while((it = strValue.find(lookup, last)) < strValue.length()) {
				last = it + rep.length();
				strValue.replace(it, 1, rep);
			}
		};

		if(stringLit->getKind() == clang::StringLiteral::Ascii) {
			expand('\\', "\\\\");
			expand('\n', "\\n");
			expand('\t', "\\t");
			expand('\b', "\\b");
			expand('\a', "\\a");
			expand('\v', "\\v");
			expand('\r', "\\r");
			expand('\f', "\\f");
			expand('\?', "\\\?");
			expand('\'', "\\\'");
			expand('\"', "\\\"");
			expand('\0', "\\0");
		}

		retExpr = builder.stringLit(strValue, stringLit->getType().isConstQualified());
		frontend_assert(retExpr->getType() == convertExprType(stringLit));
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
		default: frontend_assert(false) << "UnaryExprOrTypeTraitExpr not handled.";
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

		string memName(membExpr->getMemberDecl()->getName());
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

	namespace {
		core::ExpressionPtr buildBinaryOpExpression(Converter& converter, const core::TypePtr& exprTy, const core::ExpressionPtr& lhs,
			                                        const core::ExpressionPtr& rhs, core::lang::BasicGenerator::Operator op,
			                                        const clang::BinaryOperator* binOp) {
			auto& basic = converter.getNodeManager().getLangBasic();
			auto& builder = converter.getIRBuilder();

			if(core::lang::isPointer(lhs->getType()) || core::lang::isPointer(rhs->getType())) { return core::lang::buildPtrOperation(op, lhs, rhs); }

			auto irOp = basic.getOperator(exprTy, op);
			// for comparisons, use type of operand
			if(binOp->isComparisonOp()) irOp = basic.getOperator(lhs->getType(), op);

			return builder.binaryOp(irOp, lhs, rhs);
		}

		core::ExpressionPtr buildUnaryOpExpression(Converter& converter, const core::TypePtr& exprTy, const core::ExpressionPtr& expr,
			                                       core::lang::BasicGenerator::Operator op) {
			auto& basic = converter.getNodeManager().getLangBasic();
			auto& builder = converter.getIRBuilder();

			if(core::lang::isReference(expr) && core::lang::isPointer(core::analysis::getReferencedType(expr->getType()))) {
				return core::lang::buildPtrOperation(op, expr);
			}

			switch(op) {
			case core::lang::BasicGenerator::Operator::PostInc: return builder.postInc(expr);
			case core::lang::BasicGenerator::Operator::PostDec: return builder.postDec(expr);
			case core::lang::BasicGenerator::Operator::PreInc: return builder.preInc(expr);
			case core::lang::BasicGenerator::Operator::PreDec: return builder.preDec(expr);
			default: break;
			}

			return builder.unaryOp(basic.getOperator(exprTy, op), expr);
		}
	}

	core::ExpressionPtr Converter::ExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		core::ExpressionPtr lhs = Visit(binOp->getLHS());
		core::ExpressionPtr rhs = Visit(binOp->getRHS());
		core::TypePtr exprTy = converter.convertType(binOp->getType());

		// if the binary operator is a comma separated expression, we convert it into a function call which returns the value of the last expression --- COMMA -
		if(binOp->getOpcode() == clang::BO_Comma) {
			retIr = utils::buildCommaOperator(builder.wrapLazy(lhs), builder.wrapLazy(rhs));
			return retIr;
		}

		// we need to translate the semantics of C-style assignments to a function call ----------------------------------------------------------- ASSIGNMENT -
		if(binOp->getOpcode() == clang::BO_Assign) {
			retIr = utils::buildCStyleAssignment(lhs, rhs);
			return retIr;
		}

		std::map<clang::BinaryOperator::Opcode, core::lang::BasicGenerator::Operator> opMap = {
			{clang::BO_MulAssign, core::lang::BasicGenerator::Mul},    // a *= b
			{clang::BO_DivAssign, core::lang::BasicGenerator::Div},    // a /= b
			{clang::BO_RemAssign, core::lang::BasicGenerator::Mod},    // a %= b
			{clang::BO_AddAssign, core::lang::BasicGenerator::Add},    // a += b
			{clang::BO_SubAssign, core::lang::BasicGenerator::Sub},    // a -= b
			{clang::BO_ShlAssign, core::lang::BasicGenerator::LShift}, // a <<= b
			{clang::BO_ShrAssign, core::lang::BasicGenerator::RShift}, // a >>= b
			{clang::BO_AndAssign, core::lang::BasicGenerator::And},    // a &= b
			{clang::BO_OrAssign, core::lang::BasicGenerator::Or},      // a |= b
			{clang::BO_XorAssign, core::lang::BasicGenerator::Xor},    // a ^= b

			{clang::BO_Add, core::lang::BasicGenerator::Add},    // a + b
			{clang::BO_Sub, core::lang::BasicGenerator::Sub},    // a - b
			{clang::BO_Mul, core::lang::BasicGenerator::Mul},    // a * b
			{clang::BO_Div, core::lang::BasicGenerator::Div},    // a / b
			{clang::BO_Rem, core::lang::BasicGenerator::Mod},    // a % b
			{clang::BO_Shl, core::lang::BasicGenerator::LShift}, // a << b
			{clang::BO_Shr, core::lang::BasicGenerator::RShift}, // a >> b
			{clang::BO_And, core::lang::BasicGenerator::And},    // a & b
			{clang::BO_Xor, core::lang::BasicGenerator::Xor},    // a ^ b
			{clang::BO_Or, core::lang::BasicGenerator::Or},      // a | b

			{clang::BO_LAnd, core::lang::BasicGenerator::LAnd}, // a && b
			{clang::BO_LOr, core::lang::BasicGenerator::LOr},   // a || b

			{clang::BO_LT, core::lang::BasicGenerator::Lt}, // a < b
			{clang::BO_GT, core::lang::BasicGenerator::Gt}, // a > b
			{clang::BO_LE, core::lang::BasicGenerator::Le}, // a <= b
			{clang::BO_GE, core::lang::BasicGenerator::Ge}, // a >= b
			{clang::BO_EQ, core::lang::BasicGenerator::Eq}, // a == b
			{clang::BO_NE, core::lang::BasicGenerator::Ne}, // a != b
		};

		auto opIt = opMap.find(binOp->getOpcode());
		frontend_assert(opIt != opMap.end()) << "Binary Operator " << binOp->getOpcodeStr().str() << " not implemented.";

		if(binOp->isCompoundAssignmentOp()) { // ---------------------------------------------------------------------------------------------------- COMPOUND -
			// we need to deref the LHS, as it is assigned to the clang AST does not contain an LtoR cast
			auto op = buildBinaryOpExpression(converter, exprTy, builder.deref(lhs), rhs, opIt->second, binOp);
			// for compound operators, we need to build a CallExpr returning the LHS after the operation
			retIr = utils::buildCStyleAssignment(lhs, op);
			return retIr;
		}

		// the logical operators && and || need to eval their arguments lazily ------------------------------------------------------------------ LAZY LOGICAL -
		if(binOp->getOpcode() == clang::BO_LAnd || binOp->getOpcode() == clang::BO_LOr) {
			exprTy = basic.getBool();
			lhs = utils::exprToBool(lhs);
			rhs = builder.wrapLazy(utils::exprToBool(rhs));
		}

		retIr = buildBinaryOpExpression(converter, exprTy, lhs, rhs, opIt->second, binOp);

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

		// A unary + is an integral upcast in some situations, but then clang generates the implicit cast in the subexpr -------------------------------- PLUS -
		if(unOp->getOpcode() == clang::UO_Plus) {
			retIr = subExpr;
			return retIr;
		}
		
		// A unary - is equivalent to 0-x -------------------------------------------------------------------------------------------------------------- MINUS -
		if(unOp->getOpcode() == clang::UO_Minus) {
			retIr = builder.minus(subExpr);
			return retIr;
		}
				
		// A unary ! implies a conversion to bool in IR ------------------------------------------------------------------------------------------ LOGICAL NOT -
		if(unOp->getOpcode() == clang::UO_LNot) {
			retIr = builder.logicNeg(utils::exprToBool(subExpr));
			return utils::buildBoolToInt(retIr);
		}

		// A unary AddressOf operator translates to a ptr-from-ref in IR --------------------------------------------------------------------------- ADDRESSOF -
		if(unOp->getOpcode() == clang::UO_AddrOf) {
			// if function type, handle with special operator
			if(subExpr->getType().isa<core::FunctionTypePtr>()) {
				retIr = core::lang::buildPtrOfFunction(subExpr);
			} else {
				retIr = core::lang::buildPtrFromRef(subExpr);
			}
			return retIr;
		}

		// A unary Deref operator translates to a ptr-to-ref operation in IR --------------------------------------------------------------------------- DEREF -
		if(unOp->getOpcode() == clang::UO_Deref) {
			frontend_assert(core::lang::isPointer(subExpr)) << "Deref operation only possible on pointers, got:\n" << dumpColor(subExpr->getType());
			// if function pointer type, access function directly
			if(core::lang::PointerType(subExpr->getType()).getElementType().isa<core::FunctionTypePtr>()) {
				retIr = core::lang::buildPtrDeref(subExpr);
			} else {
				retIr = core::lang::buildPtrToRef(subExpr);
			}
			return retIr;
		}
		
		std::map<clang::UnaryOperator::Opcode, core::lang::BasicGenerator::Operator> opMap = {
			{clang::UO_Not, core::lang::BasicGenerator::Not},         // ~a
			{clang::UO_PreDec, core::lang::BasicGenerator::PreDec},   // --a
			{clang::UO_PreInc, core::lang::BasicGenerator::PreInc},   // ++a
			{clang::UO_PostDec, core::lang::BasicGenerator::PostDec}, // a--
			{clang::UO_PostInc, core::lang::BasicGenerator::PostInc}, // a++
		};

		auto opIt = opMap.find(unOp->getOpcode());
		frontend_assert(opIt != opMap.end()) << "Unary Operator " << clang::UnaryOperator::getOpcodeStr(unOp->getOpcode()).str() << " not implemented.";

		retIr = buildUnaryOpExpression(converter, exprTy, subExpr, opIt->second);
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
				retIr = converter.getFunMan()->lookup(funcDecl->getCanonicalDecl());
			}
			return retIr;
		}

		if(const clang::EnumConstantDecl* decl = llvm::dyn_cast<clang::EnumConstantDecl>(declRef->getDecl())) {
			string enumConstantname = decl->getNameAsString();
			const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(decl->getDeclContext())->getTypeForDecl());
			auto expType = converter.convertType(enumType->getCanonicalTypeInternal());
			return builder.literal(enumConstantname, expType);
		}

		frontend_assert(false) << "DeclRefExpr not handled: " << dumpClang(declRef, converter.getSourceManager());
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  VECTOR/STRUCT INITALIZATION EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

	namespace {
		// translate expression as usual, but convert translated string literals to r-values rather than l-values
		core::ExpressionPtr translateInitExpr(Converter& converter, const clang::Expr* original) {
			auto expr = converter.convertExpr(original);
			if(original->isLValue()) {
				auto lit = expr.isa<core::LiteralPtr>();
				if(!lit) return expr;
				string s = lit->getValue()->getValue();
				core::NodeManager& mgr = lit->getNodeManager();
				core::IRBuilder builder(mgr);
				ExpressionList initExps;
				for(char c : s.substr(1, s.size()-2)) {
					initExps.push_back(builder.literal(format("'%s'",toString(c)), mgr.getLangBasic().getChar()));
				}
				initExps.push_back(builder.literal("'\\0'", mgr.getLangBasic().getChar()));
				expr = core::lang::buildArrayCreate(lit->getNodeManager(), s.size()-1, initExps);
			}
			return expr;
		}

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
				if(i < initList->getNumInits()) initExp = translateInitExpr(converter, initList->getInit(i));
				frontend_assert(initExp->getType() == entry->getType()) << "Type mismatch in record initialization expression:\n"
					<< "expected: " << dumpColor(entry->getType()) << "got: " << dumpColor(initExp->getType());
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
			retIr = utils::buildRecordTypeFixup(builder.structExpr(types.first, values), types.second);
		}
		else if(clangType->isUnionType()) {
			auto types = lookupRecordTypes(converter, initList);
			auto member = builder.stringValue(initList->getInitializedFieldInUnion()->getNameAsString());
			auto initExp = Visit(initList->getInit(0));
			retIr = utils::buildRecordTypeFixup(builder.unionExpr(types.first, member, initExp), types.second);
		}
		else if(clangType->isArrayType()) {
			auto gT = converter.convertType(clangType).as<core::GenericTypePtr>();
			frontend_assert(core::lang::isArray(gT)) << "Clang InitListExpr of unexpected generic type (should be array):\n" << dumpColor(gT);
			ExpressionList initExps;
			for(unsigned i = 0; i < initList->getNumInits(); ++i) { // yes, that is really the best way to do this in clang 3.6
				initExps.push_back(translateInitExpr(converter, initList->getInit(i)));
			}
			retIr = core::lang::buildArrayCreate(core::lang::getArraySize(gT), initExps);
		} 
		else if(clangType->isScalarType()) {
			retIr = translateInitExpr(converter, initList->getInit(0));
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
			retIr = builder.refVar(Visit(initList));
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

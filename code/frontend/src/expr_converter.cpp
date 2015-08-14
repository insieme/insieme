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

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/name_manager.h"
#include "insieme/frontend/utils/clang_cast.h"
#include "insieme/frontend/utils/macros.h"
#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/memalloc.h"
#include "insieme/frontend/utils/stmt_wrapper.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"
#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/lang/enum_extension.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/annotations/naming.h"
#include "insieme/core/annotations/source_location.h"

#include "insieme/annotations/c/include.h"

using namespace insieme;

namespace insieme {
namespace frontend {
namespace conversion {
	
	//---------------------------------------------------------------------------------------------------------------------
	//										BASE EXPRESSION CONVERTER
	//---------------------------------------------------------------------------------------------------------------------

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Overwrite the basic visit method for expression in order to automatically
	// and transparently attach annotations to node which are annotated
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::CExprConverter::Visit(const clang::Expr* expr) {
		// iterate clang handler list and check if a handler wants to convert the expr
		core::ExpressionPtr retIr;
		for (auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->Visit(expr, converter);
			if (retIr) { break; }
		}

		if (!retIr) {
			converter.trackSourceLocation(expr);
			retIr = ConstStmtVisitor<CExprConverter, core::ExpressionPtr>::Visit(expr);
			converter.untrackSourceLocation();
		}
		else {
			VLOG(2) << "CExprConverter::Visit handled by plugin";
		}

		// print diagnosis messages
		converter.printDiagnosis(expr->getLocStart());

		// call frontend extension post visitors
		for (auto extension : converter.getConversionSetup().getExtensions()) {
			retIr = extension->PostVisit(expr, retIr, converter);
		}

		// attach location annotation
		if (expr->getLocStart().isValid()) {
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

		core::TypePtr type = converter.convertType(intLit->getType());
		if(intLit->getType().getTypePtr()->isUnsignedIntegerOrEnumerationType()) { value.append("u"); }
		frontend_assert(!value.empty()) << "literal con not be an empty string";
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
		} else {
			frontend_assert(false) << "no idea how you got here, but only single/double precission literals are allowed in insieme\n";
		}

		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitCharacterLiteral(const clang::CharacterLiteral* charLit) {
		core::ExpressionPtr retExpr;
		LOG_EXPR_CONVERSION(charLit, retExpr);

		string value;
		unsigned int v = charLit->getValue();

		core::TypePtr elemType;
		switch(charLit->getKind()) {
		case clang::CharacterLiteral::Ascii: elemType = basic.getChar(); break;
		case clang::CharacterLiteral::UTF16:
			elemType = basic.getWChar16();
			converter.warnings.insert("Insieme widechar support is experimental");
			break;
		case clang::CharacterLiteral::UTF32:
		case clang::CharacterLiteral::Wide:
			elemType = basic.getWChar32();
			converter.warnings.insert("Insieme widechar support is experimental");
			break;
		}
		frontend_assert(elemType);

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
		}

		return retExpr = builder.literal(value, elemType);
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

		retExpr = builder.stringLit("\"" + strValue + "\"");
		VLOG(2) << retExpr;
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitParenExpr(const clang::ParenExpr* parExpr) {
		core::ExpressionPtr retExpr;

		LOG_EXPR_CONVERSION(parExpr, retExpr);
		return (retExpr = Visit(parExpr->getSubExpr()));
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
		//core::TypePtr type = converter.convertType(nullExpr->getType());

		//core::ExpressionPtr retIr;
		//LOG_EXPR_CONVERSION(nullExpr, retIr);
		//frontend_assert(core::analysis::isArrayType(type->getNodeType())) << "C pointer type must of type array<'a,1>\n";
		//return (retIr = builder.refReinterpret(mgr.getLangBasic().getRefNull(), type));
		assert_not_implemented();
		return core::ExpressionPtr();
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
		core::ExpressionPtr irNode;
		LOG_EXPR_CONVERSION(callExpr, irNode);

		//core::ExpressionPtr func = converter.convertExpr(callExpr->getCallee());

		//// check if we have a call to a static function
		//bool isStatic = false;
		//if(callExpr->getDirectCallee()) {
		//	if(auto mdecl = llvm::dyn_cast<clang::CXXMethodDecl>(callExpr->getDirectCallee())) { isStatic = mdecl->isStatic(); }
		//}

		//core::CallExprPtr call = func.isa<core::CallExprPtr>();
		//core::FunctionTypePtr funcTy;

		//// if we have a nested function call to a static function
		//// try to retrieve the the function type from the inner call expression.
		//assert_true(func->getType().isa<core::FunctionTypePtr>() || call->getFunctionExpr()->getType().as<core::FunctionTypePtr>())
		//    << "Cannot find a function type in the converted callee expression";
		//(call && isStatic) ? (funcTy = call->getFunctionExpr()->getType().as<core::FunctionTypePtr>()) : (funcTy = func->getType().as<core::FunctionTypePtr>());

		//// FIXME if we have a call to "free" we get a refDelete back which expects ref<'a'>
		//// this results in a cast ot "'a" --> use the type we get from the funcDecl
		//if(callExpr->getDirectCallee()) {
		//	const clang::FunctionDecl* funcDecl = llvm::cast<clang::FunctionDecl>(callExpr->getDirectCallee());
		//	// FIXME changing type to fit "free" -- with refDelete
		//	funcTy = converter.convertFunctionType(funcDecl);
		//}

		//ExpressionList&& args = getFunctionArguments(callExpr, funcTy);

		//// In case we have a K&R C-style declaration (without argument types)
		//// and a different number of arguments in the call we need to adjust the function type
		//vector<core::TypePtr> typeList = funcTy.getParameterTypeList();
		//if(typeList.size() == 0 && args.size() > 0) {
		//	typeList = ::transform(args, [](const core::ExpressionPtr& exprPtr) { return exprPtr->getType(); });
		//	core::FunctionTypePtr newFuncTy = builder.functionType(typeList, funcTy->getReturnType());
		//	func = core::transform::replaceAddress(mgr, core::ExpressionAddress(func)->getType(), newFuncTy).getRootNode().as<core::ExpressionPtr>();
		//}

		//// in case that there is a nested call to static functions try to
		//// rebuild the intended behavior (e.g., fun() { base_call(); return nested_call; } (args) )
		//if(call && isStatic) {
		//	auto originalFunType = call->getFunctionExpr()->getType().as<core::FunctionTypePtr>();
		//	irNode = builder.callExpr(originalFunType->getReturnType(), call->getFunctionExpr());
		//	assert_true(originalFunType->getReturnType().isa<core::FunctionTypePtr>());
		//	irNode = builder.callExpr(originalFunType->getReturnType().as<core::FunctionTypePtr>()->getReturnType(), irNode, args);
		//	return irNode;
		//}

		//irNode = builder.callExpr(funcTy->getReturnType(), func, args

		assert_not_implemented();
		return irNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitPredefinedExpr(const clang::PredefinedExpr* preExpr) {
		string lit;
		//switch(preExpr->getIdentType()) {
		//case clang::PredefinedExpr::Func: lit = "__func__"; break;
		//case clang::PredefinedExpr::Function: lit = "__FUNCTION__"; break;
		//case clang::PredefinedExpr::PrettyFunction: lit = "__PRETTY_FUNCTION__"; break;
		//case clang::PredefinedExpr::PrettyFunctionNoVirtual:
		//default: frontend_assert(false) << "Handle for predefined function not defined\n";
		//}

		//core::TypePtr type = converter.convertType(preExpr->getType());
		//frontend_assert(type->getNodeType() == core::NT_VectorType);
		//core::TypePtr elemType = type.as<core::VectorTypePtr>()->getElementType();
		//return builder.literal(lit, builder.refType(builder.arrayType(elemType)));
		assert_not_implemented();
		return core::ExpressionPtr();
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
		case clang::UETT_SizeOf: {
			return (irNode = frontend::utils::getSizeOfType(builder, type));
		}
		case clang::UETT_AlignOf: {
			return (irNode = frontend::utils::getAlignOfType(builder, type));
		}
		case clang::UETT_VecStep: {
			frontend_assert(false) << "vecStep Kind of expressions not handled\n";
		}
		}

		return core::ExpressionPtr();
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							MEMBER EXPRESSION
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitMemberExpr(const clang::MemberExpr* membExpr) {
		core::ExpressionPtr&& base = Visit(membExpr->getBase());
		core::ExpressionPtr retIr;// = exprutils::getMemberAccessExpr(converter, builder, base, membExpr);
		LOG_EXPR_CONVERSION(membExpr, retIr);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitBinaryOperator(const clang::BinaryOperator* binOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(binOp, retIr);

		//core::ExpressionPtr&& lhs = Visit(binOp->getLHS());
		//core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
		//core::TypePtr exprTy = converter.convertType(binOp->getType());

		//frontend_assert(lhs) << "no left side could be translated";
		//frontend_assert(rhs) << "no right side could be translated";
		//frontend_assert(exprTy) << "no type for expression";

		//// if the binary operator is a comma separated expression, we convert it into a function call which returns the
		//// value of the last expression
		//if(binOp->getOpcode() == clang::BO_Comma) {
		//	core::TypePtr retType;
		//	// the return type of this lambda is the type of the last expression (according to the C standard)
		//	stmtutils::StmtWrapper stmts;
		//	stmts.push_back(lhs);
		//	stmts.push_back(basic.isUnit(rhs->getType()) ? static_cast<core::StatementPtr>(rhs) : builder.returnStmt(rhs));
		//	retType = rhs->getType();


		//	// core::StatementPtr body =  builder.compoundStmt(stmts);
		//	return (retIr = converter.createCallExprFromBody(stmts, retType));
		//}


		///*
		// * we take care of compound operators first, we rewrite the RHS expression in a normal form, i.e.:
		// * 		->		a op= b  ---->  a = a op b
		// */
		//clang::BinaryOperatorKind baseOp;
		//core::lang::BasicGenerator::Operator op;
		//bool isCompound = true;

		//switch(binOp->getOpcode()) {
		//// a *= b
		//case clang::BO_MulAssign:
		//	op = core::lang::BasicGenerator::Mul;
		//	baseOp = clang::BO_Mul;
		//	break;
		//// a /= b
		//case clang::BO_DivAssign:
		//	op = core::lang::BasicGenerator::Div;
		//	baseOp = clang::BO_Div;
		//	break;
		//// a %= b
		//case clang::BO_RemAssign:
		//	op = core::lang::BasicGenerator::Mod;
		//	baseOp = clang::BO_Rem;
		//	break;
		//// a += b
		//case clang::BO_AddAssign:
		//	op = core::lang::BasicGenerator::Add;
		//	baseOp = clang::BO_Add;
		//	break;
		//// a -= b
		//case clang::BO_SubAssign:
		//	op = core::lang::BasicGenerator::Sub;
		//	baseOp = clang::BO_Sub;
		//	break;
		//// a <<= b
		//case clang::BO_ShlAssign:
		//	op = core::lang::BasicGenerator::LShift;
		//	baseOp = clang::BO_Shl;
		//	break;
		//// a >>= b
		//case clang::BO_ShrAssign:
		//	op = core::lang::BasicGenerator::RShift;
		//	baseOp = clang::BO_Shr;
		//	break;
		//// a &= b
		//case clang::BO_AndAssign:
		//	op = core::lang::BasicGenerator::And;
		//	baseOp = clang::BO_And;
		//	break;
		//// a |= b
		//case clang::BO_OrAssign:
		//	op = core::lang::BasicGenerator::Or;
		//	baseOp = clang::BO_Or;
		//	break;
		//// a ^= b
		//case clang::BO_XorAssign:
		//	op = core::lang::BasicGenerator::Xor;
		//	baseOp = clang::BO_Xor;
		//	break;
		//default: isCompound = false;
		//}

		//// perform any pointer arithmetic needed
		//auto doPointerArithmetic = [&]() -> core::ExpressionPtr {
		//	// LHS must be a ref<array<'a>>
		//	core::TypePtr subRefTy = GET_REF_ELEM_TYPE(lhs->getType());

		//	frontend_assert(core::analysis::isRefType(lhs->getType()));
		//	if(subRefTy->getNodeType() == core::NT_VectorType) {
		//		lhs = builder.callExpr(basic.getRefVectorToRefArray(), lhs);
		//	} else if(!isCompound && subRefTy->getNodeType() != core::NT_ArrayType) {
		//		lhs = builder.callExpr(basic.getScalarToArray(), lhs);
		//	}

		//	// Capture pointer arithmetics
		//	// 	Base op must be either a + or a -
		//	frontend_assert((baseOp == clang::BO_Add || baseOp == clang::BO_Sub)) << "Operators allowed in pointer arithmetic are + and - only\n"
		//	                                                                      << "baseOp used: " << binOp->getOpcodeStr().str() << "\n";

		//	// LOG(INFO) << rhs->getType();
		//	frontend_assert(basic.isInt(rhs->getType())) << "Array view displacement must be an integer type\nGiven: " << *rhs->getType();
		//	if(basic.isUnsignedInt(rhs->getType())) { rhs = builder.castExpr(basic.getInt8(), rhs); }
		//	if(basic.isInt16(rhs->getType())) { rhs = builder.castExpr(basic.getInt4(), rhs); }

		//	// compound operator do not deref the target var (no implicit LtoR cast)
		//	// we need a right side in the operation call
		//	core::ExpressionPtr arg = lhs;
		//	if(isCompound) { arg = builder.deref(lhs); }

		//	// we build the pointer arithmetic expression,
		//	// if is not addition, must be substration, therefore is a negative increment
		//	return builder.callExpr(basic.getArrayView(), arg, baseOp == clang::BO_Add ? rhs : builder.invertSign(rhs));
		//};

		//// compound operators are op + assignation. we need to express this in IR in a different way.
		//if(isCompound) {
		//	// we check if the RHS is a ref, in that case we use the deref operator
		//	// rhs = converter.tryDeref(rhs);

		//	// We have a compound operation applied to a ref<array<'a>> type which is probably one of
		//	// the function parameters, We have to wrap this variable in a way it becomes a
		//	// ref<ref<array<'a>>>
		//	if(core::analysis::isRefType(lhs->getType()) && !core::analysis::isRefType(rhs->getType())
		//	   && core::analysis::getReferencedType(lhs->getType())->getNodeType() == core::NT_ArrayType) {
		//		// FIXME: if this does not create errors..  delete it
		//		frontend_assert(false) << "who uses this?\n";
		//		lhs = wrapVariable(binOp->getLHS());
		//	}

		//	// capture the case when pointer arithmetic is performed
		//	if(core::analysis::isRefType(lhs->getType()) && !core::analysis::isRefType(rhs->getType())
		//	   && core::analysis::isRefType(core::analysis::getReferencedType(lhs->getType()))
		//	   && core::analysis::getReferencedType(core::analysis::getReferencedType(lhs->getType()))->getNodeType() == core::NT_ArrayType) {
		//		// do pointer arithmetic
		//		rhs = doPointerArithmetic();
		//	} else {
		//		// get basic element type
		//		core::ExpressionPtr&& subExprLHS = converter.tryDeref(lhs);
		//		// beware of cpp refs, to operate, we need to deref the value in the left side
		//		if(core::analysis::isAnyCppRef(subExprLHS->getType())) {
		//			subExprLHS = builder.toIRRef(subExprLHS);
		//			subExprLHS = converter.tryDeref(subExprLHS);
		//		}
		//		// rightside will become the current operation
		//		//  a += 1   =>    a = a + 1

		//		auto compOp = llvm::cast<clang::CompoundAssignOperator>(binOp);

		//		if(compOp->getComputationLHSType() != binOp->getType()) {
		//			exprTy = converter.convertType(compOp->getComputationLHSType());
		//			subExprLHS = core::types::castScalar(exprTy, subExprLHS);
		//		}

		//		core::ExpressionPtr opFunc;
		//		if(binOp->isShiftAssignOp() || binOp->getOpcode() == clang::BO_XorAssign || binOp->getOpcode() == clang::BO_OrAssign
		//		   || binOp->getOpcode() == clang::BO_AndAssign) {
		//			opFunc = basic.getOperator(builder.typeVariable("a"), op);
		//		} else {
		//			opFunc = basic.getOperator(exprTy, op);
		//		}

		//		rhs = builder.callExpr(exprTy, opFunc, subExprLHS, rhs);

		//		if(compOp->getComputationResultType() != binOp->getType()) { rhs = core::types::castScalar(converter.convertType(binOp->getType()), rhs); }
		//	}
		//}

		//bool isAssignment = false;
		//bool isLogical = false;

		//baseOp = binOp->getOpcode();

		//core::ExpressionPtr opFunc;
		//switch(binOp->getOpcode()) {
		//case clang::BO_PtrMemD:
		//case clang::BO_PtrMemI:
		//	frontend_assert(false) << "Operator not yet supported!\n";

		//// a * b
		//case clang::BO_Mul:
		//	op = core::lang::BasicGenerator::Mul;
		//	break;
		//// a / b
		//case clang::BO_Div:
		//	op = core::lang::BasicGenerator::Div;
		//	break;
		//// a % b
		//case clang::BO_Rem:
		//	op = core::lang::BasicGenerator::Mod;
		//	break;
		//// a + b
		//case clang::BO_Add:
		//	op = core::lang::BasicGenerator::Add;
		//	break;
		//// a - b
		//case clang::BO_Sub:
		//	op = core::lang::BasicGenerator::Sub;
		//	break;
		//// a << b
		//case clang::BO_Shl:
		//	op = core::lang::BasicGenerator::LShift;
		//	break;
		//// a >> b
		//case clang::BO_Shr:
		//	op = core::lang::BasicGenerator::RShift;
		//	break;
		//// a & b
		//case clang::BO_And:
		//	op = core::lang::BasicGenerator::And;
		//	break;
		//// a ^ b
		//case clang::BO_Xor:
		//	op = core::lang::BasicGenerator::Xor;
		//	break;
		//// a | b
		//case clang::BO_Or:
		//	op = core::lang::BasicGenerator::Or;
		//	break;

		//// Logic operators

		//// a && b
		//case clang::BO_LAnd:
		//	op = core::lang::BasicGenerator::LAnd;
		//	isLogical = true;
		//	break;
		//// a || b
		//case clang::BO_LOr:
		//	op = core::lang::BasicGenerator::LOr;
		//	isLogical = true;
		//	break;
		//// a < b
		//case clang::BO_LT:
		//	op = core::lang::BasicGenerator::Lt;
		//	isLogical = true;
		//	break;
		//// a > b
		//case clang::BO_GT:
		//	op = core::lang::BasicGenerator::Gt;
		//	isLogical = true;
		//	break;
		//// a <= b
		//case clang::BO_LE:
		//	op = core::lang::BasicGenerator::Le;
		//	isLogical = true;
		//	break;
		//// a >= b
		//case clang::BO_GE:
		//	op = core::lang::BasicGenerator::Ge;
		//	isLogical = true;
		//	break;
		//// a == b
		//case clang::BO_EQ:
		//	op = core::lang::BasicGenerator::Eq;
		//	isLogical = true;
		//	break;
		//// a != b
		//case clang::BO_NE:
		//	op = core::lang::BasicGenerator::Ne;
		//	isLogical = true;
		//	break;

		//case clang::BO_MulAssign:
		//case clang::BO_DivAssign:
		//case clang::BO_RemAssign:
		//case clang::BO_AddAssign:
		//case clang::BO_SubAssign:
		//case clang::BO_ShlAssign:
		//case clang::BO_ShrAssign:
		//case clang::BO_AndAssign:
		//case clang::BO_XorAssign:
		//case clang::BO_OrAssign:
		//case clang::BO_Assign: {
		//	baseOp = clang::BO_Assign;

		//	// make sure the lhs is a L-Value
		//	lhs = asLValue(lhs);

		//	if(core::types::isRefArray(lhs->getType().as<core::RefTypePtr>()->getElementType()) && core::types::isRefVector(rhs->getType())) {
		//		rhs = builder.callExpr(mgr.getLangBasic().getRefVectorToRefArray(), rhs);
		//	}

		//	// OK!! here there is a problem,
		//	//	let fun000 = fun(ref<array<int<4>,1>> v5) -> unit {
		//	//		    decl ref<ref<array<int<4>,1>>> v6 =  var(v5);
		//	//	};
		//	// check ~/myTest/ptr.cpp

		//	// why to cast?
		//	// some casts are not pressent in IR
		//	if(basic.isPrimitive(rhs->getType())) {
		//		if(core::analysis::isVolatileType(GET_REF_ELEM_TYPE(lhs->getType()))) {
		//			rhs = builder.makeVolatile(core::types::castScalar(core::analysis::getVolatileType(GET_REF_ELEM_TYPE(lhs->getType())), rhs));
		//		} else {
		//			rhs = core::types::castScalar(GET_REF_ELEM_TYPE(lhs->getType()), rhs);
		//		}
		//	}

		//	isAssignment = true;
		//	opFunc = converter.getFrontendIR().getRefAssign();
		//	if(converter.getCompiler().isCXX()) {
		//		exprTy = lhs.getType();
		//	} else {
		//		exprTy = lhs.getType().as<core::RefTypePtr>()->getElementType();
		//	}
		//	break;
		//}
		//default: frontend_assert(false) << "Operator not supported\n";
		//}

		//// Operators && and || introduce short circuit operations, this has to be directly supported in the IR.
		//// Vector operations are handled in basic.cpp
		//if((baseOp == clang::BO_LAnd || baseOp == clang::BO_LOr) && exprTy->getNodeType() != core::NT_VectorType) {
		//	lhs = core::types::castToBool(lhs);
		//	rhs = core::types::castToBool(rhs);

		//	// lazy evaluation of RHS
		//	// generate a bind call
		//	exprTy = basic.getBool();
		//	stmtutils::StmtWrapper body;
		//	body.push_back(builder.returnStmt(rhs));
		//	rhs = converter.createCallExprFromBody(body, basic.getBool(), true);
		//}

		//core::TypePtr&& lhsTy = lhs->getType();
		//core::TypePtr&& rhsTy = rhs->getType();

		//if(!isAssignment) {
		//	// handling for ocl-vector operations
		//	if((binOp->getLHS()->getType().getUnqualifiedType()->isExtVectorType() || binOp->getRHS()->getType().getUnqualifiedType()->isExtVectorType())) {
		//		//			lhs = core::types::smartCast(lhs, exprTy);
		//		//			rhs = core::types::smartCast(rhs, exprTy);

		//		// generate a ocl_vector - scalar operation
		//		opFunc = basic.getOperator(lhs->getType(), op);

		//		// TODO to be tested
		//		if(const core::FunctionTypePtr funTy = core::dynamic_pointer_cast<const core::FunctionType>(opFunc->getType()))
		//			// check if we can use the type of the first argument as return type
		//			if(funTy->getReturnType() == funTy->getParameterTypeList().at(0)) {
		//				return (retIr = builder.callExpr(lhs->getType(), opFunc, lhs, core::types::smartCast(rhs, lhs->getType())));
		//			} else { // let deduce it otherwise
		//				return (retIr = builder.callExpr(opFunc, lhs, core::types::smartCast(rhs, lhs->getType())));
		//			}
		//		else {
		//			frontend_assert(false) << "old stuff needed, tell Klaus\n";
		//			return (retIr = builder.callExpr(lhsTy, opFunc, lhs, rhs));
		//		}
		//	} else if((binOp->getLHS()->getType().getUnqualifiedType()->isVectorType() || binOp->getRHS()->getType().getUnqualifiedType()->isVectorType())) {
		//		lhs = core::types::smartCast(lhs, exprTy);
		//		rhs = core::types::smartCast(rhs, exprTy);

		//		const auto& ext = mgr.getLangExtension<insieme::core::lang::SIMDVectorExtension>();
		//		auto type = lhs->getType();
		//		frontend_assert(core::lang::isSIMDVector(type));

		//		core::LiteralPtr simdOp;
		//		switch(binOp->getOpcode()) {
		//		case clang::BO_Add: simdOp = ext.getSIMDAdd(); break;
		//		case clang::BO_Mul: simdOp = ext.getSIMDMul(); break;
		//		case clang::BO_Div: simdOp = ext.getSIMDDiv(); break;
		//		case clang::BO_Rem: simdOp = ext.getSIMDMod(); break;
		//		case clang::BO_And: simdOp = ext.getSIMDAnd(); break;
		//		case clang::BO_Or: simdOp = ext.getSIMDOr(); break;
		//		case clang::BO_Xor: simdOp = ext.getSIMDXor(); break;
		//		case clang::BO_Shl: simdOp = ext.getSIMDLShift(); break;
		//		case clang::BO_Shr: simdOp = ext.getSIMDRShift(); break;
		//		case clang::BO_EQ: simdOp = ext.getSIMDEq(); break;
		//		case clang::BO_NE: simdOp = ext.getSIMDNe(); break;
		//		case clang::BO_LT: simdOp = ext.getSIMDLt(); break;
		//		case clang::BO_LE: simdOp = ext.getSIMDLe(); break;
		//		case clang::BO_GT: simdOp = ext.getSIMDGt(); break;
		//		case clang::BO_GE: simdOp = ext.getSIMDGe(); break;

		//		default: { frontend_assert(false) << "Operator for simd-vectortypes not supported\n"; }
		//		}

		//		auto retTy = lhs->getType();
		//		retIr = builder.callExpr(retTy, simdOp, lhs, rhs);
		//		return retIr;
		//	}


		//	// pointer arithmetic only allowed for additive operation
		//	if(baseOp == clang::BO_Add || baseOp == clang::BO_Sub) {
		//		// This is the required pointer arithmetic in the case we deal with pointers
		//		if(!core::analysis::isRefType(rhs->getType()) && core::analysis::isRefType(lhs->getType())) {
		//			rhs = doPointerArithmetic();
		//			return (retIr = rhs);
		//		}

		//		// it might be all the way round, left side is the one to do pointer arithmetics on, is not very usual, but it happens
		//		if(!core::analysis::isRefType(lhs->getType()) && core::analysis::isRefType(rhs->getType())) {
		//			std::swap(rhs, lhs);
		//			rhs = doPointerArithmetic();
		//			return (retIr = rhs);
		//		}
		//	}

		//	// especial case to deal with the pointer distance operation
		//	//  x = ptr1 - ptr2
		//	if(core::types::isRefArray(lhs->getType()) && core::types::isRefArray(rhs->getType()) && baseOp == clang::BO_Sub) {
		//		return retIr = builder.callExpr(basic.getArrayRefDistance(), lhs, rhs);
		//	}

		//	if(isLogical) {
		//		// this is like some lines ahead, char and bool types are treated as integers by clang,
		//		// while they are converted into char or bool int IR. we need to recover the original
		//		// CLANG typing
		//		if(baseOp != clang::BO_LAnd && baseOp != clang::BO_LOr) {
		//			lhs = core::types::smartCast(lhs, converter.convertType(binOp->getLHS()->getType()));
		//			rhs = core::types::smartCast(rhs, converter.convertType(binOp->getRHS()->getType()));
		//		}

		//		exprTy = basic.getBool();
		//		VLOG(2) << "Lookup for operation: " << op << ", for type: " << lhs->getType();
		//		opFunc = basic.getOperator(lhs->getType(), op);
		//	} else if(lhsTy->getNodeType() != core::NT_RefType && rhsTy->getNodeType() != core::NT_RefType) {
		//		// TODO: would love to remove this, but some weirdos still need this cast
		//		// somehow related with char type. is treated as integer everywhere, not in ir
		//		lhs = core::types::smartCast(lhs, converter.convertType(binOp->getLHS()->getType()));
		//		rhs = core::types::smartCast(rhs, converter.convertType(binOp->getRHS()->getType()));


		//		if(binOp->isBitwiseOp() || binOp->isShiftOp()) {
		//			opFunc = basic.getOperator(builder.typeVariable("a"), op);
		//		} else {
		//			VLOG(2) << "Lookup for operation: " << op << ", for type: " << *exprTy;
		//			opFunc = basic.getOperator(lhs->getType(), op);
		//		}
		//	} else if(lhsTy->getNodeType() == core::NT_RefType && rhsTy->getNodeType() == core::NT_RefType) {
		//		frontend_assert((*lhsTy == *rhsTy)) << "Comparing incompatible types\n";
		//		VLOG(2) << "Lookup for operation: " << op << ", for type: " << lhsTy;
		//		opFunc = basic.getOperator(lhsTy, op);
		//	}
		//}

		//frontend_assert(opFunc) << "no operation code set\n"
		//                        << "\tOperator: " << binOp->getOpcodeStr().str() << "\n"
		//                        << "\t     LHS: \n" << dumpOneLine(lhs) << " \n of type: " << lhs->getType() << "\n"
		//                        << "\t     RHS: \n" << dumpOneLine(rhs) << " \n of type: " << rhs->getType() << "\n";

		//VLOG(2) << " Operator: " << binOp->getOpcodeStr().str();
		//VLOG(2) << " LHS: \n    " << dumpOneLine(lhs) << " \nof type: " << lhs->getType();
		//VLOG(2) << " RHS: \n    " << dumpOneLine(rhs) << " \nof type: " << rhs->getType();

		//retIr = builder.callExpr(exprTy, opFunc, lhs, rhs);
		assert_not_implemented();
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitUnaryOperator(const clang::UnaryOperator* unOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(unOp, retIr);

		//core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

		//// build lambda expression for post/pre increment/decrement unary operators
		//auto encloseIncrementOperator = [&](core::ExpressionPtr subExpr, core::lang::BasicGenerator::Operator op) -> core::ExpressionPtr {

		//	core::TypePtr type = subExpr->getType();
		//	frontend_assert(type->getNodeType() == core::NT_RefType && "Illegal increment/decrement operand - not a ref type");
		//	core::TypePtr elementType = GET_REF_ELEM_TYPE(type);

		//	if(core::analysis::isRefType(elementType) && (GET_REF_ELEM_TYPE(elementType)->getNodeType() == core::NT_ArrayType)) {
		//		// if this is a post/pre incremenet operator applied to an array we have to deal with it
		//		// immediatelly because the getOperator function wouldn't deal with such case

		//		core::ExpressionPtr opLit;
		//		switch(op) {
		//		case core::lang::BasicGenerator::PreInc: opLit = basic.getArrayViewPreInc(); break;
		//		case core::lang::BasicGenerator::PostInc: opLit = basic.getArrayViewPostInc(); break;
		//		case core::lang::BasicGenerator::PreDec: opLit = basic.getArrayViewPreDec(); break;
		//		case core::lang::BasicGenerator::PostDec: opLit = basic.getArrayViewPostDec(); break;
		//		default: frontend_assert(false) << "Operator not handled for pointer arithmetic\n";
		//		}
		//		return builder.callExpr(elementType, opLit, subExpr);
		//	}

		//	switch(op) {
		//	case core::lang::BasicGenerator::PreInc: return builder.preInc(subExpr);
		//	case core::lang::BasicGenerator::PostInc: return builder.postInc(subExpr);
		//	case core::lang::BasicGenerator::PreDec: return builder.preDec(subExpr);
		//	case core::lang::BasicGenerator::PostDec: return builder.postDec(subExpr);
		//	default: frontend_assert(false);
		//	}
		//	return core::ExpressionPtr(); // should not be reachable
		//};

		//switch(unOp->getOpcode()) {
		//// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		//// a++ ==> (__tmp = a, a=a+1, __tmp)
		//// ++a ==> ( a=a+1, a)
		//// --a
		//case clang::UO_PreDec:
		//	return retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreDec);
		//// a--
		//case clang::UO_PostDec:
		//	return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostDec));
		//// a++
		//case clang::UO_PreInc:
		//	return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreInc));
		//// ++a
		//case clang::UO_PostInc:
		//	return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostInc));
		//// &a
		//case clang::UO_AddrOf: {
		//	retIr = subExpr;

		//	// in the case we are getting the address of a function the & operator
		//	// has no effects, therefore we return
		//	if(retIr->getType()->getNodeType() == core::NT_FunctionType) { return retIr; }

		//	// make sure it is a L-Value
		//	retIr = asLValue(retIr);

		//	frontend_assert(retIr->getType().isa<core::RefTypePtr>()) << "not a ref? " << retIr << " : " << retIr->getType();
		//	return (retIr = core::types::refScalarToRefArray(retIr));
		//}
		//// *a
		//case clang::UO_Deref: {
		//	// make sure it is a L-Value
		//	retIr = asLValue(subExpr);

		//	frontend_assert(retIr->getType()->getNodeType() == core::NT_RefType) << "Impossible to apply * operator to an R-Value\n";

		//	const core::TypePtr& subTy = GET_REF_ELEM_TYPE(retIr->getType());

		//	return (retIr = (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) ? getCArrayElemRef(builder, retIr)
		//	                                                                                                            : converter.tryDeref(retIr));
		//}
		//// +a
		//case clang::UO_Plus:
		//	return retIr = subExpr;
		//// -a
		//case clang::UO_Minus:
		//	if(unOp->getSubExpr()->getType().getUnqualifiedType()->isVectorType()
		//	   && !unOp->getSubExpr()->getType().getUnqualifiedType()->isExtVectorType()) { // OpenCL vectors don't need special treatment, they just work
		//		const auto& ext = mgr.getLangExtension<insieme::core::lang::SIMDVectorExtension>();
		//		return (retIr = builder.callExpr(ext.getSIMDMinus(), subExpr));
		//	}

		//	return (retIr = builder.invertSign(converter.tryDeref(subExpr)));
		//// ~a
		//case clang::UO_Not:
		//	if(unOp->getSubExpr()->getType().getUnqualifiedType()->isVectorType()
		//	   && !unOp->getSubExpr()->getType().getUnqualifiedType()->isExtVectorType()) { // OpenCL vectors don't need special treatment, they just work
		//		const auto& ext = mgr.getLangExtension<insieme::core::lang::SIMDVectorExtension>();
		//		return (retIr = builder.callExpr(ext.getSIMDNot(), subExpr));
		//	}

		//	retIr = converter.tryDeref(subExpr);
		//	return (retIr = builder.callExpr(retIr->getType(), basic.getOperator(retIr->getType(), core::lang::BasicGenerator::Not), retIr));
		//// !a
		//case clang::UO_LNot:
		//	if(!basic.isBool(subExpr->getType())) { subExpr = core::types::smartCast(subExpr, basic.getBool()); }
		//	frontend_assert(basic.isBool(subExpr->getType()));

		//	return (retIr = builder.callExpr(subExpr->getType(), basic.getBoolLNot(), subExpr));

		//case clang::UO_Extension: return retIr = subExpr;

		//case clang::UO_Real: return mgr.getLangExtension<core::lang::ComplexExtension>().getReal(subExpr);

		//case clang::UO_Imag: return mgr.getLangExtension<core::lang::ComplexExtension>().getImg(subExpr);

		//default: frontend_assert(false && "Unary operator not supported");
		//}
		frontend_assert(false) << "Unary operator not supported";
		return core::ExpressionPtr(); // should not be reachable
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitConditionalOperator(const clang::ConditionalOperator* condOp) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(condOp, retIr);

		//core::TypePtr retTy = converter.convertType(condOp->getType());
		//core::ExpressionPtr trueExpr = Visit(condOp->getTrueExpr());
		//core::ExpressionPtr falseExpr = Visit(condOp->getFalseExpr());
		//core::ExpressionPtr condExpr = Visit(condOp->getCond());

		//condExpr = core::types::castToBool(condExpr);

		//// Dereference eventual references
		//if(retTy->getNodeType() == core::NT_RefType && !core::types::isRefArray(retTy) && !builder.getLangBasic().isAnyRef(retTy)) {
		//	retTy = GET_REF_ELEM_TYPE(retTy);
		//}

		//// fixes the return type to retTy of the given expression toFix
		//auto fixingThrowExprType = [&](core::ExpressionPtr toFix, const core::TypePtr& retTy) {
		//	// get address of callExpr: callExpr(lambdaExpr(throwExpr),(argument))
		//	core::CallExprPtr callExpr = toFix.as<core::CallExprPtr>();
		//	core::CallExprAddress callExprAddr(callExpr);

		//	// get returnType of callExpr
		//	core::NodeAddress addrTy0 = callExprAddr.getType();

		//	// get address of throwExpr
		//	core::LambdaExprAddress throwExprAddr(callExprAddr->getFunctionExpr().as<core::LambdaExprAddress>());

		//	// get address of returnType of throwExpr
		//	core::NodeAddress addrTy1 = throwExprAddr.getFunctionType().getReturnType();

		//	// get address of returnType of the lambdaVariable
		//	core::NodeAddress addrTy2 = throwExprAddr.getVariable().getType().as<core::FunctionTypeAddress>().getReturnType();

		//	// get address of returnType of the lambda
		//	core::NodeAddress addrTy3 = throwExprAddr.getLambda().getType().as<core::FunctionTypeAddress>().getReturnType();

		//	// get address of returnType of the lambdabinding
		//	core::NodeAddress addrTy4 =
		//	    throwExprAddr.getDefinition().getBindingOf(throwExprAddr.getVariable()).getVariable().getType().as<core::FunctionTypeAddress>().getReturnType();

		//	// setup replaceMap with the address of the types to be fixed to "retTy"
		//	std::map<core::NodeAddress, core::NodePtr> replaceMap;
		//	replaceMap.insert({addrTy0, retTy});
		//	replaceMap.insert({addrTy1, retTy});
		//	replaceMap.insert({addrTy2, retTy});
		//	replaceMap.insert({addrTy3, retTy});
		//	replaceMap.insert({addrTy4, retTy});

		//	toFix = core::transform::replaceAll(mgr, replaceMap).as<core::ExpressionPtr>();
		//	return toFix;
		//};

		//// if trueExpr or falseExpr is a CXXThrowExpr we need to fix the type
		//// of the throwExpr (and the according calls) to the type of the other branch of the
		//// conditional operator, as the c++ standard defines throwExpr always with void and the
		//// conditional operator expects on both branches the same returnType (except when used with
		//// throw then the type of the "nonthrowing" branch is used
		//if(llvm::isa<clang::CXXThrowExpr>(condOp->getTrueExpr())) {
		//	trueExpr = fixingThrowExprType(trueExpr, retTy);
		//} else if(llvm::isa<clang::CXXThrowExpr>(condOp->getFalseExpr())) {
		//	falseExpr = fixingThrowExprType(falseExpr, retTy);
		//}

		//// in C++, string literals with same size may produce an error, do not cast to avoid
		//// weird behaviour
		//if(!llvm::isa<clang::StringLiteral>(condOp->getTrueExpr())) {
		//	// if one of true or false exprs is a cpp
		//	// ref the other one has to be a cpp ref too
		//	if(core::analysis::isAnyCppRef(falseExpr->getType()) && !core::analysis::isAnyCppRef(trueExpr->getType())) {
		//		// ok, false is a ref, but is it a const cpp ref
		//		if(core::analysis::isConstCppRef(falseExpr->getType())) {
		//			trueExpr = builder.toConstCppRef(trueExpr);
		//		} else {
		//			trueExpr = builder.toCppRef(trueExpr);
		//		}
		//	}
		//	if(core::analysis::isAnyCppRef(trueExpr->getType()) && !core::analysis::isAnyCppRef(falseExpr->getType())) {
		//		// ok, false is a ref, but is it a const cpp ref
		//		if(core::analysis::isConstCppRef(trueExpr->getType())) {
		//			falseExpr = builder.toConstCppRef(falseExpr);
		//		} else {
		//			falseExpr = builder.toCppRef(falseExpr);
		//		}
		//	}

		//	if(trueExpr->getType() != falseExpr->getType()) {
		//		trueExpr = core::types::smartCast(trueExpr, retTy);
		//		falseExpr = core::types::smartCast(falseExpr, retTy);
		//	} else {
		//		retTy = trueExpr->getType();
		//	}
		//} else {
		//	retTy = trueExpr->getType();
		//}

		//// be carefull! createCallExpr turns given statements into lazy -- keep it that way
		//return (retIr = builder.callExpr(retTy, basic.getIfThenElse(),
		//                                 condExpr,                                                                                  // Condition
		//                                 converter.createCallExprFromBody(builder.returnStmt(trueExpr), trueExpr->getType(), true),  // True
		//                                 converter.createCallExprFromBody(builder.returnStmt(falseExpr), falseExpr->getType(), true) // False
		//                                 ));

		frontend_assert(false);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitArraySubscriptExpr(const clang::ArraySubscriptExpr* arraySubExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(arraySubExpr, retIr);

		///*
		// * CLANG introduces implicit cast for the base expression of array subscripts which cast the array type into a
		// * simple pointer. As insieme supports subscripts only for array or vector types, we skip eventual implicit
		// * cast operations.
		// */
		//const clang::Expr* baseExpr = arraySubExpr->getBase();

		//// IDX
		//core::ExpressionPtr idx = converter.tryDeref(Visit(arraySubExpr->getIdx()));
		//if(!basic.isUInt4(idx->getType())) { idx = core::types::castScalar(basic.getUInt4(), idx); }

		//// BASE
		//core::ExpressionPtr base = Visit(baseExpr);

		//core::TypePtr opType;
		//core::ExpressionPtr op;

		//if(base->getType()->getNodeType() == core::NT_RefType) {
		//	// The vector/array is an L-Value so we use the array.ref.elem
		//	// operator to return a reference to the addressed memory location
		//	core::TypePtr refSubTy = GET_REF_ELEM_TYPE(base->getType());

		//	// TODO: we need better checking for vector type
		//	frontend_assert((refSubTy->getNodeType() == core::NT_VectorType || refSubTy->getNodeType() == core::NT_ArrayType)
		//	                && "Base expression of array subscript is not a vector/array type.");

		//	op = refSubTy->getNodeType() == core::NT_ArrayType ? basic.getArrayRefElem1D() : basic.getVectorRefElem();

		//	opType = builder.refType(refSubTy.as<core::SingleElementTypePtr>()->getElementType());

		//} else {
		//	/*
		//	 * The vector/array is an R-value (e.g. (int[2]){0,1}[1] ) in this case the subscript returns an R-value so
		//	 * the array.subscript operator must be used
		//	 */
		//	// TODO: we need better checking for vector type
		//	frontend_assert((base->getType()->getNodeType() == core::NT_VectorType || base->getType()->getNodeType() == core::NT_ArrayType)
		//	                && "Base expression of array subscript is not a vector/array type.");

		//	op = base->getType()->getNodeType() == core::NT_ArrayType ? basic.getArraySubscript1D() : basic.getVectorSubscript();

		//	opType = core::static_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();
		//}

		//return (retIr = builder.callExpr(opType, op, base, idx));

		frontend_assert(false);
		return retIr;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitDeclRefExpr(const clang::DeclRefExpr* declRef) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(declRef, retIr);

		//// check whether this is a reference to a variable
		//core::ExpressionPtr retExpr;
		//if(const clang::ParmVarDecl* parmDecl = llvm::dyn_cast<clang::ParmVarDecl>(declRef->getDecl())) {
		//	VLOG(2) << "Parameter type: " << converter.convertType(parmDecl->getOriginalType());

		//	retIr = converter.lookUpVariable(parmDecl);
		//	auto fit = converter.wrapRefMap.find(retIr.as<core::VariablePtr>());
		//	if(fit == converter.wrapRefMap.end()) {
		//		fit = converter.wrapRefMap.insert(std::make_pair(retIr.as<core::VariablePtr>(), builder.variable(builder.refType(retIr->getType())))).first;

		//		VLOG(2) << "parmVar wrapped from " << retIr << " (" << retIr->getType() << ")"
		//		        << " to " << fit->second << "(" << fit->second->getType();
		//	}
		//	return (retIr = fit->second);
		//}

		//if(const clang::VarDecl* varDecl = llvm::dyn_cast<clang::VarDecl>(declRef->getDecl())) { return (retIr = converter.lookUpVariable(varDecl)); }

		//if(const clang::FunctionDecl* funcDecl = llvm::dyn_cast<clang::FunctionDecl>(declRef->getDecl())) {
		//	return (retIr = converter.getCallableExpression(funcDecl).as<core::ExpressionPtr>());
		//}

		//if(const clang::EnumConstantDecl* decl = llvm::dyn_cast<clang::EnumConstantDecl>(declRef->getDecl())) {
		//	string enumConstantname = insieme::frontend::utils::buildNameForEnumConstant(decl, converter.getSourceManager());
		//	const clang::EnumType* enumType = llvm::dyn_cast<clang::EnumType>(llvm::cast<clang::TypeDecl>(decl->getDeclContext())->getTypeForDecl());

		//	// Just dont you mesh with sys headers stuff
		//	auto expType = converter.convertType(enumType->getCanonicalTypeInternal());
		//	if(annotations::c::hasIncludeAttached(expType)) { enumConstantname = decl->getNameAsString(); }

		//	return builder.literal(enumConstantname, expType);
		//}

		//frontend_assert(false) << "clang::DeclRefExpr not supported!\n";
		//return core::ExpressionPtr();

		frontend_assert(false);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  VECTOR/STRUCT INITALIZATION EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitInitListExpr(const clang::InitListExpr* initList) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initList, retIr);

		//if(initList->isStringLiteralInit()) { frontend_assert(false) << "string literal\n"; }

		////	if (initList->hasArrayFiller ())
		////		frontend_assert(false && "array filler");

		//// if is a union initilization we build the field assigment, later we wont have it
		//if(const clang::FieldDecl* field = initList->getInitializedFieldInUnion()) {
		//	frontend_assert(initList->getNumInits() == 1);
		//	// AHA!! here is the trick, magic trick. we hide the union initialization into an expression
		//	// it has to be an expression, so we hide the thing in a fake funtion to cheat everyone!!
		//	// the name of the literal is the field !!! hahaha isn't it briliant???
		//	// twisted??? maybe i'm going crazy, but it works! and is criptic enought to piss off people
		//	string name = llvm::cast<clang::NamedDecl>(field)->getNameAsString();
		//	core::ExpressionPtr init = Visit(initList->getInit(0));
		//	auto dummyFuncType = builder.functionType(init->getType(), basic.getUnit());
		//	return builder.callExpr(builder.literal(name, dummyFuncType), init);
		//}

		//// if is anything else, we pack a list, we'll check what is it later on
		//vector<core::ExpressionPtr> inits;
		//for(size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
		//	const clang::Expr* subExpr = initList->getInit(i);
		//	inits.push_back(Visit(subExpr));
		//}

		//return retIr = core::encoder::toIR<ExpressionList, core::encoder::DirectExprListConverter>(mgr, inits);

		frontend_assert(false);
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

		//if(const clang::InitListExpr* initList = llvm::dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer())) {
		//	return (retIr = converter.convertInitExpr(NULL, initList, converter.convertType(compLitExpr->getType()), false));
		//}
		//return (retIr = Visit(compLitExpr->getInitializer()));

		frontend_assert(false);
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

		//// FIXME: tryAggregateStmts in stmt_wrapper _removes_ compoundStmt if compoundStmt contains only one stmt
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

		frontend_assert(false);
		return retIr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  ImplicitValueInit EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr Converter::ExprConverter::VisitImplicitValueInitExpr(const clang::ImplicitValueInitExpr* initExpr) {
		core::ExpressionPtr retIr;
		LOG_EXPR_CONVERSION(initExpr, retIr);
		//core::TypePtr elementType = converter.convertType(initExpr->getType());
		//frontend_assert(elementType) << "IR type creation failed (given element type not supported)\n";
		//retIr = converter.defaultInitVal(elementType);
		//return retIr;
		frontend_assert(false);
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
		frontend_assert(false);
		return retIr;
	}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

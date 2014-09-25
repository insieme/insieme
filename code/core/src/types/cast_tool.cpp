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

#include "insieme/core/types/cast_tool.h"
#include "insieme/core/ir_builder.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum_extension.h"


#include "insieme/utils/logging.h"

#include <boost/regex.hpp>

namespace insieme {
namespace core {
namespace types {


namespace {

	static const boost::regex numberFilter  ("([0-9]+)(\\.[0-9]+)?([ufl]*)");
	static const boost::regex precissionFilter  ("([0-9]|([0-9]*\\.[0-9]+))([ufl]*)");
	static const boost::regex zerosFilter  ("\\.[0]+");
	static const boost::regex zeroValueFilter  ("([0]+)(\\.[0]+)?([ufl]*)");

	core::ExpressionPtr castLiteral(const core::LiteralPtr lit, const core::TypePtr targetTy){

		std::string old = lit->getStringValue();

		core::IRBuilder builder( targetTy->getNodeManager() );
		const core::lang::BasicGenerator& gen = builder.getLangBasic();
		std::string res;
		boost::cmatch what;

		////////////////////////////////////////
		// CAST TO BOOLEAN
		if (gen.isBool(targetTy) ){
			if (gen.isChar(lit->getType())) throw std::exception();
			if (boost::regex_match(old, zeroValueFilter)) res.assign("false");
			else res.assign("true");
		}

		////////////////////////////////////////
		// CAST TO CHAR
		if (gen.isChar(targetTy) ){
			// do not rebuild the literal, might be a nightmare, just build a cast
			throw std::exception();
		}

		// behind this point  is needed to be a number
		if (!boost::regex_match(old, numberFilter)){
			throw std::exception();
		}

		// cleanup precission
		if(boost::regex_match(old.c_str(), what, precissionFilter)){
			// what[1] contains the number
			// what[2] contains the precission markers
			old.assign(what[1].first, what[1].second);
		}

		////////////////////////////////////////
		// CAST TO INT and UINT
		if (gen.isInt(targetTy) || gen.isUnsignedInt(targetTy)){
			// remove any decimal
			if(boost::regex_match(old.c_str(), what, numberFilter)){
				// what[0] contains the whole string
				// what[1] contains the integer part
				// what[2] contains the decimals
				res.assign(what[1].first, what[1].second);
			}
			else {
					assert(false && "something wrong modifying literals");
			}
			if (gen.isUnsignedInt(targetTy)){
				// append u
				res.append("u");
			}
			if (gen.isInt8(targetTy) || gen.isUInt8(targetTy)){
				res.append("l");
			}
			if (gen.isInt16(targetTy) || gen.isUInt16(targetTy)){
				res.append("ll");
			}
		}

		////////////////////////////////////////
		// CAST TO REAL
		if (gen.isReal(targetTy)){
			// make sure has decimal point

			if(boost::regex_match(old.c_str(), what, numberFilter)){
				res.assign(what[1].first, what[1].second);
				if (what[2].first == what[0].second){
					//no point
					res.append(".0");
				}
				else{
					// if only zeros, clean them
					std::string zeros (what[2].first, what[2].second);
					if (boost::regex_match(zeros, zerosFilter))
						res.append(".0");
					else
						res.append(zeros);
				}
			}
			else {
					assert(false && "something wrong modifying literals");
			}

			if (gen.isReal4(targetTy)){
				//append f
				res.append("f");
			}
		}
		return builder.literal (targetTy, res);
	}

}

	bool isArray(const core::TypePtr& type) {
		return type->getNodeType() == core::NT_ArrayType;
	}

	bool isRefArray(const core::TypePtr& type) {
		return type->getNodeType() == core::NT_RefType && 
			   type.as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_ArrayType;
	}

	bool isRefRef(const core::TypePtr& type) {
		return type->getNodeType() == core::NT_RefType && 
			   type.as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_RefType;
	}

	bool isVector(const core::TypePtr& type) {
		return type->getNodeType() == core::NT_VectorType;
	}

	bool isRefVector(const core::TypePtr& type) {
		return type->getNodeType() == core::NT_RefType && 
			   type.as<core::RefTypePtr>()->getElementType()->getNodeType() == core::NT_VectorType;
	}

	bool isNullPtrExpression(const core::ExpressionPtr& expr){
		auto mgr (expr->getNodeManager());

		return (core::analysis::isCallOf( expr, mgr.getLangBasic().getRefReinterpret()) && 
				*mgr.getLangBasic().getRefNull() == *(expr.as<core::CallExprPtr>()[0]) ) || 
				*mgr.getLangBasic().getRefNull() == *expr;
	}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	core::ExpressionPtr smartCast (const core::TypePtr& type, const core::ExpressionPtr& expr){

		if (type == expr->getType()){
			return expr;
		}

		abort();
	}

// FIXME: we can do this in a smarter way
std::size_t getPrecission(const core::TypePtr& type, const core::lang::BasicGenerator& gen){

	if (gen.isReal(type)){
		if 		(gen.isReal4(type)) return 4;
		else if (gen.isReal8(type)) return 8;
		else if (gen.isReal16(type)) return 16;
		else if (gen.isFloat(type)) return 4;
		else if (gen.isDouble(type)) return 8;
		else if (gen.isLongDouble(type)) return 16;
	}
	else if (gen.isSignedInt(type)){
		if 		(gen.isInt1(type)) return 1;
		else if (gen.isInt2(type)) return 2;
		else if (gen.isInt4(type)) return 4;
		else if (gen.isInt8(type)) return 8;
		else if (gen.isInt16(type))return 16;
	}
	else if (gen.isUnsignedInt(type)){
		if 		(gen.isUInt1(type)) return 1;
		else if (gen.isUInt2(type)) return 2;
		else if (gen.isUInt4(type)) return 4;
		else if (gen.isUInt8(type)) return 8;
		else if (gen.isUInt16(type))return 16;
	}
	else if (gen.isWChar(type)){
		if 		(gen.isWChar16(type)) return 16;
		else if (gen.isWChar32(type)) return 32;
	}
	else if (gen.isBool(type) || gen.isChar(type))
		return 1;
    else if (type.getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(type))
        return 4;


	return 0;
}

core::ExpressionPtr castToBool (const core::ExpressionPtr& expr){

	const core::TypePtr& exprTy = expr->getType();
	core::IRBuilder builder( exprTy->getNodeManager() );
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	if (gen.isBool(expr->getType())) return expr;

	if (isRefVector(expr->getType())) {
		auto tmp = builder.callExpr(gen.getRefVectorToRefArray(), expr);
		return builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getRefIsNull(), tmp));
	}

	if (isRefArray(expr->getType())) {
		return builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getRefIsNull(), expr));
	}

	if (gen.isAnyRef(exprTy)) {
		return builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getRefIsNull(), expr));
	}

	if( exprTy.isa<core::FunctionTypePtr>()) {
		return builder.callExpr(gen.getBool(), gen.getGenNe(), expr, builder.getZero(exprTy));
	}

	if (core::analysis::isLongLong (exprTy)){
	    return castScalar (gen.getBool(),core::analysis::castFromLongLong( expr));
	}

	if (!gen.isInt(expr->getType())  && !gen.isReal(expr->getType()) && !gen.isChar(expr->getType())){
		dumpDetail(expr);
		std::cout << "****" << std::endl;
		dumpDetail(expr->getType());
		assert(false && "this type can not be converted now to bool. implement it! ");
	}

	return castScalar (gen.getBool(), expr);
}



/**
 *
 */
core::ExpressionPtr castScalar(const core::TypePtr& trgTy, core::ExpressionPtr expr){
	core::TypePtr exprTy = expr->getType();

	// check if cast is needed at all
	if(exprTy == trgTy) return expr;

	core::TypePtr targetTy = trgTy;
	core::IRBuilder builder( exprTy->getNodeManager() );
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	core::NodeManager& mgr = exprTy.getNodeManager();

	//std::cout << "========= SCALAR CAST =====================" <<std::endl;
	//std::cout << "Expr: " << expr << " : " << expr->getType() << std::endl;
	//std::cout << "target Type: " << targetTy << std::endl;

	// check if casting to cpp ref, rightside values are assigned to refs in clang without any
	// conversion, because a right side is a ref and viceversa. this is invisible to us, we need to
	// handle it carefully
	if (core::analysis::isAnyCppRef(targetTy)) {
		return expr;
	}

	bool isLongLong = false;

	if (core::analysis::isLongLong (targetTy) && core::analysis::isLongLong(expr->getType())){
		if (core::analysis::isSignedLongLong(targetTy) == core::analysis::isSignedLongLong(expr->getType())){
			return expr;
		}
		else{
			return core::analysis::castBetweenLongLong(expr);
		}
	}

	// casts from long to longlong and long
	if (core::analysis::isLongLong (targetTy)){
		isLongLong = true;
		if (core::analysis::isSignedLongLong (targetTy))
			targetTy = gen.getInt8();
		else
			targetTy = gen.getUInt8();

	}

	// cast from long long
	if (core::analysis::isLongLong (exprTy)){
		expr = core::analysis::castFromLongLong( expr);
		exprTy = expr->getType();
	}

	auto lastStep = [&isLongLong, &gen] (const core::ExpressionPtr& expr) -> core::ExpressionPtr {
		if (isLongLong)
			return core::analysis::castToLongLong(expr, gen.isSignedInt(expr->getType()));
		else
			return expr;
	};

	// is this the cast of a literal: to simplify code we'll return
	// a literal of the spected type
	if (expr->getNodeType() == core::NT_Literal){
		try{
			return lastStep(castLiteral ( expr.as<core::LiteralPtr>(), targetTy));
		}catch (std::exception& e){
			// literal upgrade not supported, continue with regular cast
		}
	}


   	unsigned char code;
	// identify source type, to write right cast
	if (gen.isSignedInt (exprTy)) 	code = 1;
	if (gen.isUnsignedInt (exprTy)) code = 2;
	if (gen.isReal (exprTy))		code = 3;
	if (gen.isChar (exprTy))		code = 4;
	if (gen.isBool (exprTy))		code = 5;
    if (gen.isWChar(exprTy))		code = 6;
    if (mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(exprTy)) code = 7;

	// identify target type
	if (gen.isSignedInt (targetTy)) 	code += 10;
	if (gen.isUnsignedInt (targetTy)) 	code += 20;
	if (gen.isReal (targetTy))			code += 30;
	if (gen.isChar (targetTy))			code += 40;
	if (gen.isBool (targetTy))			code += 50;
	if (gen.isWChar(targetTy))			code += 60;
	if (mgr.getLangExtension<core::lang::EnumExtension>().isEnumType(targetTy)) code += 70;


	auto doCast = [&](const core::ExpressionPtr& op,  const core::ExpressionPtr& expr, std::size_t precision) -> core::ExpressionPtr{
		if (precision) {
			core::ExpressionList args;
			args.push_back(expr);
			args.push_back(builder.getIntParamLiteral(precision));
			return builder.callExpr(targetTy, op, args);

		}
		else
			return builder.callExpr(targetTy, op, expr);
	};
	core::ExpressionPtr resIr;

	std::size_t bytes    = getPrecission(targetTy, gen);
	switch(code){
		case 11:
			// only if target precission is smaller, we may have a precission loosse.
			if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getIntPrecisionFix(), expr, bytes);
			else resIr = expr;
			break;
        case 17:
            resIr = builder.callExpr(gen.getInt4(), mgr.getLangExtension<core::lang::EnumExtension>().getEnumElementAsInt(), expr);
			if (bytes != getPrecission(resIr->getType(), gen)) resIr = doCast(gen.getIntPrecisionFix(), resIr, bytes);
			break;
		case 22:
			if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getUintPrecisionFix(), expr, bytes);
			else resIr = expr;
			break;
        case 27:
            resIr = builder.callExpr(gen.getUInt4(), mgr.getLangExtension<core::lang::EnumExtension>().getEnumElementAsUInt(), expr);
			if (bytes != getPrecission(resIr->getType(), gen)) resIr = doCast(gen.getUintPrecisionFix(), resIr, bytes);
			break;
		case 33:
			if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getRealPrecisionFix(), expr, bytes);
			else resIr = expr;
			break;
		case 44:
		case 55: // no cast;
			// this is a cast withing the same type.
			// is a preccision adjust, if is on the same type,
			// no need to adjust anything
			resIr = expr;
			break;
		case 66:
			if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getWCharPrecisionFix(), expr, bytes);
			else resIr = expr;
			break;

		case 12: resIr = doCast(gen.getUnsignedToInt(), expr, bytes); break;
		case 13: resIr = doCast(gen.getRealToInt(), expr, bytes); break;
		case 14: resIr = doCast(gen.getCharToInt(), expr, bytes); break;
		case 15: resIr = doCast(gen.getBoolToInt(), expr, bytes); break;
		case 16: resIr = doCast(gen.getWCharToInt(), expr, bytes); break;

		case 21: resIr = doCast(gen.getSignedToUnsigned(), expr, bytes); break;
		case 23: resIr = doCast(gen.getRealToUnsigned(), expr, bytes); break;
		case 24: resIr = doCast(gen.getCharToUnsigned(), expr, bytes); break;
		case 25: resIr = doCast(gen.getBoolToUnsigned(), expr, bytes); break;
		case 26: resIr = doCast(gen.getWCharToUnsigned(), expr, bytes); break;

		case 31: resIr = doCast(gen.getSignedToReal(), expr, bytes); break;
		case 32: resIr = doCast(gen.getUnsignedToReal(), expr, bytes); break;
		case 34: resIr = doCast(gen.getCharToReal(), expr, bytes); break;
		case 35: resIr = doCast(gen.getBoolToReal(), expr, bytes); break;

		case 41: resIr = doCast(gen.getSignedToChar(), expr, 0);   break;
		case 42: resIr = doCast(gen.getUnsignedToChar(), expr, 0); break;
		case 43: resIr = doCast(gen.getRealToChar(), expr, 0);     break;
		case 45: resIr = doCast(gen.getBoolToChar(), expr, 0);     break;

		case 51: resIr = doCast(gen.getSignedToBool(), expr, 0);   break;
		case 52: resIr = doCast(gen.getUnsignedToBool(), expr, 0); break;
		case 53: resIr = doCast(gen.getRealToBool(), expr, 0);     break;
		case 54: resIr = doCast(gen.getCharToBool(), expr, 0);     break;

		case 57: resIr = doCast(mgr.getLangExtension<core::lang::EnumExtension>().getEnumElementAsBool(), expr, 0); break;

		case 61: resIr = doCast(gen.getSignedToWChar(), expr, bytes); break;
		case 62: resIr = doCast(gen.getUnsignedToWChar(), expr, bytes); break;
		case 63: resIr = doCast(gen.getRealToWChar(), expr, bytes); break;
		case 64: resIr = doCast(gen.getCharToWChar(), expr, bytes); break;
		case 65: resIr = doCast(gen.getBoolToWChar(), expr, bytes); break;

		case 71: resIr = builder.deref(builder.callExpr(builder.refType(targetTy), gen.getRefReinterpret(),
                                     builder.refVar(expr), builder.getTypeLiteral(targetTy))); break;
		case 72: resIr = builder.deref(builder.callExpr(builder.refType(targetTy), gen.getRefReinterpret(),
                                     builder.refVar(expr), builder.getTypeLiteral(targetTy))); break;

        case 77: resIr = expr; break;

		default:
				 std::cerr << "expr type: " << exprTy << std::endl;
				 std::cerr << "targ type: " << targetTy << std::endl;
				 std::cerr << "code: " << (int) code << std::endl;
				 assert(false && "cast not defined");
	}


	// idelayed casts from long to longlong and long
	return lastStep(resIr);
}

} 
} 
}

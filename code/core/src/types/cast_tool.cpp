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

#include "insieme/core/ir_builder.h"
#include "insieme/core/types/cast_tool.h"

#include "insieme/core/ir_types.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/enum_extension.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/arithmetic/arithmetic_utils.h"

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
		// convertExprToType TO BOOLEAN
		if (gen.isBool(targetTy) ){
			if (gen.isChar(lit->getType())) throw std::exception();
			if (boost::regex_match(old, zeroValueFilter)) res.assign("false");
			else res.assign("true");
		}

		////////////////////////////////////////
		// convertExprToType TO CHAR
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
		// convertExprToType TO INT and UINT
		if (gen.isInt(targetTy) || gen.isUnsignedInt(targetTy)){
			// remove any decimal
			if(boost::regex_match(old.c_str(), what, numberFilter)){
				// what[0] contains the whole string
				// what[1] contains the integer part
				// what[2] contains the decimals
				res.assign(what[1].first, what[1].second);
			}
			else {
					assert_fail() << "something wrong modifying literals";
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
		// convertExprToType TO REAL
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
					assert_fail() << "something wrong modifying literals";
			}

			if (gen.isReal4(targetTy)){
				//append f
				res.append("f");
			}
		}
		return builder.literal (targetTy, res);
	}



} // anonymous namespace
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


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

	core::ExpressionPtr smartCast (const core::ExpressionPtr& expr, const core::TypePtr& type){
		return smartCast(type, expr);
	}
	core::ExpressionPtr smartCast (const core::TypePtr& type, const core::ExpressionPtr& expr){

		if (type == expr->getType()){
			return expr;
		}

		core::IRBuilder builder( expr->getNodeManager() );
		return types::convertExprToType(builder, type, expr);
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

		if (!gen.isInt(expr->getType())  && !gen.isReal(expr->getType()) && !gen.isChar(expr->getType())){
			dumpDetail(expr);
			std::cout << "****" << std::endl;
			dumpDetail(expr->getType());
			assert_fail() << "this type can not be converted now to bool. implement it! ";
		}

		return castScalar (gen.getBool(), expr);
	}


#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())

	/**
	 *
	 */
	core::ExpressionPtr castScalar(const core::TypePtr& trgTy, core::ExpressionPtr expr){

		core::TypePtr exprTy = expr->getType();

		// check if cast is needed at all
		if(exprTy == trgTy) return expr;

		// check whenever this expression is a literal, if so, use the right tool
		try{
			if(auto lit = expr.isa<core::LiteralPtr>()) return castLiteral(lit, trgTy);
		}catch(...) { }  // if no literal cast, continue

		core::TypePtr targetTy = trgTy;
		core::IRBuilder builder( exprTy->getNodeManager() );
		const core::lang::BasicGenerator& gen = builder.getLangBasic();
		core::NodeManager& mgr = exprTy.getNodeManager();

		// check if casting to cpp ref, rightside values are assigned to refs in clang without any
		// conversion, because a right side is a ref and viceversa. this is invisible to us, we need to
		// handle it carefully
		if (core::analysis::isAnyCppRef(targetTy)) {
			return expr;
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
		
		const core::lang::EnumExtension& enumExt = mgr.getLangExtension<core::lang::EnumExtension>();

		std::size_t bytes    = getPrecission(targetTy, gen);
		switch(code){
			case 11:
				// only if target precission is smaller, we may have a precission loosse.
				if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getIntPrecisionFix(), expr, bytes);
				else resIr = expr;
				break;
			case 17:
				resIr = builder.callExpr(gen.getInt4(), enumExt.getEnumElementAsInt(), expr);
				if (bytes != getPrecission(resIr->getType(), gen)) resIr = doCast(gen.getIntPrecisionFix(), resIr, bytes);
				break;
			case 22:
				if (bytes != getPrecission(exprTy, gen)) resIr = doCast(gen.getUintPrecisionFix(), expr, bytes);
				else resIr = expr;
				break;
			case 27:
				resIr = builder.callExpr(gen.getUInt4(), enumExt.getEnumElementAsUInt(), expr);
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

			case 57: resIr = doCast(enumExt.getEnumElementAsBool(), expr, 0); break;

			case 61: resIr = doCast(gen.getSignedToWChar(), expr, bytes); break;
			case 62: resIr = doCast(gen.getUnsignedToWChar(), expr, bytes); break;
			case 63: resIr = doCast(gen.getRealToWChar(), expr, bytes); break;
			case 64: resIr = doCast(gen.getCharToWChar(), expr, bytes); break;
			case 65: resIr = doCast(gen.getBoolToWChar(), expr, bytes); break;


			case 71: resIr = builder.callExpr(enumExt.getIntAsEnum(), expr, builder.getTypeLiteral(targetTy)); break;
			case 72: resIr = builder.callExpr(enumExt.getUIntAsEnum(), expr, builder.getTypeLiteral(targetTy)); break;

			case 77: resIr = expr; break;

			default:
					 std::cerr << "expr type: " << exprTy << std::endl;
					 std::cerr << "targ type: " << targetTy << std::endl;
					 std::cerr << "code: " << (int) code << std::endl;
					 assert_fail() << "cast not defined";
		}


		// idelayed casts from long to longlong and long
		return resIr;
	}

	// This function performs the requires type conversion, from converting an expression. 
	ExpressionPtr convertExprToType(const IRBuilder& builder, const TypePtr& trgTy, ExpressionPtr expr) {
		// list the all possible conversions 
		const core::TypePtr& argTy = expr->getType();
		const core::lang::BasicGenerator& gen = builder.getLangBasic();
		
		if ( *trgTy == *argTy ) { return expr; }

		if ( gen.isVarList(trgTy) ) { 
			// what to do here? deref or not deref?
			if (argTy->getNodeType() == core::NT_RefType) {
				// because ref<array<>> are used to represent R-value C pointers we can pass it 
				// to the caller function, the semantics is that the function can potentially 
				// change the content of the array
				if (GET_REF_ELEM_TYPE(argTy)->getNodeType() != core::NT_ArrayType &&
					GET_REF_ELEM_TYPE(argTy)->getNodeType() != core::NT_VectorType) {
					return builder.deref( expr );
				}
			}
			return expr;
		}
			
		// in the case of FuncType check against the return type
		if ( argTy->getNodeType() == core::NT_FunctionType && 
				*argTy.as<core::FunctionTypePtr>()->getReturnType() == *trgTy ) 
		{
			return expr;
		}

		///////////////////////////////////////////////////////////////////////////////////////
		//                          [ volatile<'a> -> 'a ]
		///////////////////////////////////////////////////////////////////////////////////////
		if ( core::analysis::isVolatileType(argTy) ) {
			return smartCast(trgTy, builder.callExpr( core::analysis::getVolatileType(argTy), gen.getVolatileRead(), expr));
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 							SCALAR cast
		///////////////////////////////////////////////////////////////////////////////////////
		if( (gen.isPrimitive (trgTy) || builder.getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(trgTy))
			&& (gen.isPrimitive(argTy) || builder.getNodeManager().getLangExtension<core::lang::EnumExtension>().isEnumType(argTy)))
			return core::types::castScalar (trgTy, expr);


		///////////////////////////////////////////////////////////////////////////////////////
		// 									Boolean -> Int
		///////////////////////////////////////////////////////////////////////////////////////
		// cast a boolean value to an integer
		if ( gen.isInt(trgTy) && gen.isBool(argTy) ) {
			return builder.castExpr(trgTy, builder.callExpr(gen.getInt4(), gen.getBoolToInt(), toVector(expr) ) );
		}

		///////////////////////////////////////////////////////////////////////////////////////
		//							anything -> Boolean
		///////////////////////////////////////////////////////////////////////////////////////
		if( gen.isBool(trgTy) ) {
			return castToBool(expr);
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 									Char -> Generic Integer
		///////////////////////////////////////////////////////////////////////////////////////
		// Take the integer value of the char literal and create an int literal out of it (int)c
		///////////////////////////////////////////////////////////////////////////////////////
		if ( gen.isChar(argTy) && gen.isInt(trgTy) &&  expr->getNodeType() == core::NT_Literal ) {

			assert_fail() << "deprecated: who uses this?";
			const core::LiteralPtr& lit = expr.as<core::LiteralPtr>();

			char val = ' ';
			if ( lit->getStringValue().length() == 3) {
				val = lit->getStringValue()[1]; 
				// chars are encoded as 'V', therefore position 1 always contains the char value
			} else if ( lit->getStringValue().length() == 4 ) {
				// this char literal contains some escaped sequence which is represented with 2 chars' 
				std::string strVal = lit->getStringValue().substr(1,2);
				assert(strVal.at(0) == '\\' && "Wrong encoding");
				switch (strVal.at(1) ) {
					case '\\': val = '\\';   break;
					case 'n' : val = '\n';   break;
					case 'r' : val = '\r';   break;
					case 't' : val = '\t';   break;
					case '0' : val = '\0';   break;
					case 'v' : val = '\v';   break;
					default :
						assert_fail() << "missing escape sequence.";
				}
			} else {
				assert_fail() << "Wrong encoding for char literals!";
			}	

			return builder.literal( utils::numeric_cast<std::string>(static_cast<short>(val)), trgTy );
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 									ref<any> -> ref<'a>
		///////////////////////////////////////////////////////////////////////////////////////
		// Converts anyRef to the required ref target type. If the target type is not a ref this is 
		// considered a frontend error, therefore we are allowed to fail.
		///////////////////////////////////////////////////////////////////////////////////////
		if ( gen.isAnyRef(argTy) ) {
			assert( trgTy->getNodeType() == core::NT_RefType && 
					"AnyRef can only be converted to an L-Value (RefType)" 
				);
			const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
			return builder.callExpr(trgTy, gen.getRefReinterpret(), expr, builder.getTypeLiteral(subTy));
		}

		
		///////////////////////////////////////////////////////////////////////////////////////
		//	 								ref<'a> -> ref<any>
		///////////////////////////////////////////////////////////////////////////////////////
		// Convert a ref<'a> type to ref<any>.
		///////////////////////////////////////////////////////////////////////////////////////
		if ( argTy->getNodeType() == core::NT_RefType && gen.isAnyRef(trgTy) ) {
			assert( argTy->getNodeType() == core::NT_RefType && 
					"AnyRef can only be converted to an L-Value (RefType)" );
			return expr;		// conversion is implicit
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 							0 -> anyRef 
		///////////////////////////////////////////////////////////////////////////////////////
		// Convert a ref<'a> type to anyRef. 
		///////////////////////////////////////////////////////////////////////////////////////
		if ( gen.isAnyRef(trgTy) &&  *expr == *builder.literal(argTy,"0") ) 
		{
			// just use the null literal
			return gen.getRefNull();
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 							0 -> ref<array<'a,#n>>
		///////////////////////////////////////////////////////////////////////////////////////
		// Convert a ref<'a> type to anyRef. 
		///////////////////////////////////////////////////////////////////////////////////////
		if ( core::types::isRefArray(trgTy) && *expr == *builder.literal(argTy,"0") ) 
		{
			// just use the null literal
			return gen.getRefNull();
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 									'a -> ref<'a>
		///////////////////////////////////////////////////////////////////////////////////////
		// Convert an expression of non-ref type to an expression with ref-type. This is allowed for example 
		// for string literals which can be converted to ref<arrays<>> (because of the C semantics) 
		///////////////////////////////////////////////////////////////////////////////////////
		if ( trgTy->getNodeType() == core::NT_RefType && *GET_REF_ELEM_TYPE(trgTy) == *argTy) {

			const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
			
			// if last call was a deref (*) => undo call
			if ( *subTy == *argTy && core::analysis::isCallOf(expr, gen.getRefDeref()) ) {
				return expr.as<core::CallExprPtr>()->getArgument(0);
			}

			// call the function recursively
			return builder.refVar( expr );
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 									'a -> ref<'b>
		///////////////////////////////////////////////////////////////////////////////////////
		// Convert an expression of non-ref type to an expression with ref-type. This is allowed for example 
		// for string literals which can be converted to ref<arrays<>> (because of the C semantics) 
		///////////////////////////////////////////////////////////////////////////////////////
		if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() != core::NT_RefType) {
			
			assert_fail() << "unused" << std::endl;
			const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
			
			if (core::types::isArray(subTy) && core::types::isVector(argTy)) {
				return convertExprToType(builder, subTy, expr);
			}

			// call the function recursively
			return builder.refVar( convertExprToType(builder, subTy, expr) );
		}

		// NOTE: from this point on we are sure the type of the target type and the argument type are
		// the same meaning that either we have a ref-type or non-ref type.


		///////////////////////////////////////////////////////////////////////////////////////
		// 						ref<vector<'a, #n>> -> ref<array<'a,1>>
		///////////////////////////////////////////////////////////////////////////////////////
		// convert a reference to a vector to a reference to an array using the refVector2RefArray literal  
		///////////////////////////////////////////////////////////////////////////////////////
		if ( core::types::isRefArray(trgTy) && core::types::isRefVector(argTy) ) {
			// we are sure at this point the type of arg is of ref-type as well
			const core::TypePtr& elemTy = GET_REF_ELEM_TYPE(trgTy);
			const core::TypePtr& argSubTy = GET_REF_ELEM_TYPE(argTy);

			if(elemTy->getNodeType() == core::NT_ArrayType && argSubTy->getNodeType() == core::NT_VectorType) {
				const core::TypePtr& elemVecTy = GET_VEC_ELEM_TYPE(argSubTy);

				return builder.callExpr( 
						builder.refType(builder.arrayType(elemVecTy)), gen.getRefVectorToRefArray(), expr 
					);
			}
		}

			
		///////////////////////////////////////////////////////////////////////////////////////
		// 							vector<'a, #n> -> vector<'b, #n> 
		///////////////////////////////////////////////////////////////////////////////////////
		if (core::types::isRefVector(argTy) && core::types::isRefVector(trgTy)) {
			return builder.refVar(convertExprToType(builder, GET_REF_ELEM_TYPE(trgTy), builder.deref(expr)));
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 							vector<'a, #n> -> vector<'b, #m> 
		///////////////////////////////////////////////////////////////////////////////////////
		// this conversion is only valid if 'a and 'b are the same type and #m >= #n, in the rest of the
		// cases we produce a compiler error saying this cast is not allowed within the IR type system
		///////////////////////////////////////////////////////////////////////////////////////
		if ( trgTy->getNodeType() == core::NT_VectorType && argTy->getNodeType() == core::NT_VectorType ) {
			// if we are here is because the two types are not the same, check whether the problem is the 
			// element type or the dimension
			auto vecTrgTy = trgTy.as<core::VectorTypePtr>();
			auto vecArgTy = argTy.as<core::VectorTypePtr>();

			// check the type first 
			if ( *vecArgTy->getElementType() != *vecTrgTy->getElementType() ) {
				if((*vecArgTy->getElementType() == *builder.getNodeManager().getLangBasic().getBool()
						&& *vecTrgTy->getElementType() == *builder.getNodeManager().getLangBasic().getInt4())
					|| (*vecTrgTy->getElementType() == *builder.getNodeManager().getLangBasic().getBool()
						&& *vecArgTy->getElementType() == *builder.getNodeManager().getLangBasic().getInt4())) {
					LOG(ERROR) << "Casting vector of type " << *vecArgTy << " to vector of type " << *vecTrgTy <<
							"! This is only an SC12 workaround to store the result of vector comparisons in an int<4> vector. You should NOT do this!";
					return builder.castExpr(vecTrgTy, expr);
				}

				// converting from a vector of a type to a vector of another type, this is not possible
				assert_fail() << "Converting from " << *vecArgTy << " to " << vecTrgTy;
			}


			if ( *vecArgTy->getSize() != *vecTrgTy->getSize() ) {
				// converting from a vector size X to vector size Y, only possible if X <= Y
				__unused size_t vecTrgSize = vecTrgTy->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();
				size_t vecArgSize = vecArgTy->getSize().as<core::ConcreteIntTypeParamPtr>()->getValue();

				core::ExpressionPtr plainExpr = expr;
				if (core::analysis::isCallOf(plainExpr, gen.getRefDeref())) {
					plainExpr = plainExpr.as<core::CallExprPtr>()->getArgument(0);
				}

				core::ExpressionPtr initList;

				if (plainExpr->getNodeType() == core::NT_Literal) {
					//  this is a literal string 
					assert(vecArgTy->getElementType()->getNodeType() != core::NT_RefType && 
							"conversion of string literals to vector<ref<'a>> not yet supported");

					assert_eq(vecArgTy->getSize()->getNodeType(), core::NT_ConcreteIntTypeParam);

					// do conversion from a string to an array of char
					std::string strVal = plainExpr.as<core::LiteralPtr>()->getStringValue();

					// because string literals are stored with the corresponding " " we iterate from 1 to length()-2
					// but we need an additional character to store the string terminator \0
					
					assert(strVal.length()-1 <= vecTrgSize && 
							"Target vector type not large enough to hold string literal"
						); 

					// FIXME: Use clang error report for this
					ExpressionList vals(vecArgSize);
					size_t it, escapes_count = 0;
					for(it=0; it<strVal.length()-2; ++it) {
						char c = strVal.at(it+1);
						std::string str(1,c);

						if(c == '\\')
						{
							c = strVal.at(it+2);
							switch(c) {
							case 'n': str = "\\n";     break;
							case '\\': str = "\\\\";   break;
							case 'r': str = "\\r";     break;
							case 't': str = "\\t";     break;
							case '0': str = "\\0";     break;
							}
							escapes_count++;
							it++;
						}

						vals[it - escapes_count] = builder.literal( std::string("\'") + str + "\'", gen.getChar() );
					}
					// put '\0' terminators on the remaining elements
					vals[it - escapes_count] = builder.literal( std::string("\'") + "\\0" + "\'", gen.getChar() ); // Add the string terminator
					initList = core::encoder::toIR<ExpressionList, core::encoder::DirectExprListConverter>(plainExpr->getNodeManager(), vals);
				} else {
					// we assume that the expr is an initializer for a vector, a vector expr
					auto vecExpr = plainExpr.as<core::VectorExprPtr>()->getExpressions();
					initList = core::encoder::toIR<ExpressionList, core::encoder::DirectExprListConverter>(plainExpr->getNodeManager(),
						   std::vector<core::ExpressionPtr>(vecExpr.begin(), vecExpr.end())
						);
				}
				
				return builder.callExpr(
						gen.getVectorInitPartial(), 
						initList, 
						builder.getIntTypeParamLiteral(vecTrgTy->getSize())
					);
			}
		}


		///////////////////////////////////////////////////////////////////////////////////////
		// 							ref<'a> -> ref<array<'a>>
		///////////////////////////////////////////////////////////////////////////////////////
		// Use the scalarToArray literal to perform this kind of conversion
		///////////////////////////////////////////////////////////////////////////////////////
		if ( trgTy->getNodeType() == core::NT_RefType ) {

			const core::TypePtr& subTrgTy = core::analysis::getReferencedType(trgTy);

			if ( subTrgTy->getNodeType() == core::NT_ArrayType ) {
				// If the sub type of the arrat is 'a as well as the referenced type of ref, then apply
				// the cast 
				if (*core::analysis::getReferencedType(argTy) == *subTrgTy.as<core::ArrayTypePtr>()->getElementType())
					return refScalarToRefArray(expr);
			}
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 							'a -> vector<'a,L>
		///////////////////////////////////////////////////////////////////////////////////////
		if (core::types::isVector(trgTy) && *GET_VEC_ELEM_TYPE(trgTy) == *argTy ) {
			auto vecTrgTy = trgTy.as<core::VectorTypePtr>();

			return builder.callExpr(gen.getVectorInitUniform(), 
						expr,
						builder.getIntTypeParamLiteral(vecTrgTy->getSize())
					);
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 							  ref<'a> -> ref<'b>
		///////////////////////////////////////////////////////////////////////////////////////
		if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {

			const core::TypePtr& subArgTy = GET_REF_ELEM_TYPE(argTy);
			if (*subArgTy == *trgTy) { return builder.deref( expr ); }

			///////////////////////////////////////////////////////////////////////////////////////
			// pointer (ref<ref< converted to base class... no need to reinterpret
			///////////////////////////////////////////////////////////////////////////////////////
			if(core::types::isSubTypeOf (argTy, trgTy)){
				return expr;
			}

			///////////////////////////////////////////////////////////////////////////////////////
			// 							  <ref<'a> -> ref<array<'b,1>>
			//////////////////////////////////////////////////////////////////////////////////////
			const core::TypePtr& trgInnerTy = core::analysis::getReferencedType(trgTy);
			const core::TypePtr& argInnerTy = core::analysis::getReferencedType(argTy);
			if(trgInnerTy->getNodeType() == core::NT_ArrayType &&
			   argInnerTy->getNodeType() != core::NT_ArrayType){
				
				expr = builder.callExpr(builder.getLangBasic().getScalarToArray(), expr);
			}

			///////////////////////////////////////////////////////////////////////////////////////
			// 							  ref<'a> -> ref<'b>
			///////////////////////////////////////////////////////////////////////////////////////
			core::TypePtr nonRefTrgTy = trgTy.as<core::RefTypePtr>()->getElementType();
			return builder.callExpr(
					trgTy, 
					builder.getNodeManager().getLangBasic().getRefReinterpret(), 
					expr, 
					builder.getTypeLiteral(nonRefTrgTy)
				);
		}

		///////////////////////////////////////////////////////////////////////////////////////
		// 									FunctionType -> FunctionType
		///////////////////////////////////////////////////////////////////////////////////////
		// cast a function pointer to a different type function ptr 
		if ( argTy.isa<core::FunctionTypePtr>() && trgTy.isa<core::FunctionTypePtr>() ) {
			assert_fail() << "unused" << std::endl;
			return builder.castExpr(trgTy, expr);
		}

		std::cout << " =======================================================================\n" ;
		std::cout << " FALL-TROW CAST this should be fixed if you expect the analysis to work\n" ;
		std::cout << " expr: " << expr << std::endl;
		std::cout << " to type: " << trgTy << std::endl;
		std::cout << " **********************************\n" ;
		dumpPretty(expr);
		std::cout << " **********************************\n" ;
		dumpPretty(expr->getType());
		std::cout << " **********************************\n" ;
		dumpPretty(trgTy);
		std::cout << " =======================================================================\n" ;

		assert_fail() << "as your see, we should not be here" << std::endl;

		return builder.castExpr(trgTy, expr);
		//assert_fail() << "Cast conversion not supported!";
	}

	core::ExpressionPtr refScalarToRefArray(const core::ExpressionPtr& expr) {
		assert_eq(expr->getType()->getNodeType(), core::NT_RefType);
		
		core::IRBuilder builder( expr->getNodeManager() );

		// construct result type
		core::TypePtr&& resType = 
			builder.refType(builder.arrayType(core::analysis::getReferencedType(expr->getType())));

		// simple case distinction among structure of expression
		if (expr->getNodeType() == core::NT_CallExpr) {
			const core::lang::BasicGenerator& basic = builder.getLangBasic();
			core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(expr);

			// check invoked function
			try {
				// if it is vector-ref-element:
				if (basic.isVectorRefElem(call->getFunctionExpr()) && 
					core::arithmetic::toFormula(call->getArgument(1)).isZero()
				) {
					// convert vector to array instead
					return builder.callExpr(resType, basic.getRefVectorToRefArray(), call->getArgument(0));
				}

				// ... or array_ref_element ...
				if (basic.isArrayRefElem1D(call->getFunctionExpr()) && 
					core::arithmetic::toFormula(call->getArgument(1)).isZero()
				) {
					// skip this step!
					return call->getArgument(0);
				}

			} catch (const core::arithmetic::NotAFormulaException& ne) {
				// => subscript is not zero => use default handling
			}
		}
		// fall-back solution => use scalar to array literal
		return builder.callExpr(resType, builder.getLangBasic().getScalarToArray(), expr);
	}

}  // types
}  // core
}  // insieme

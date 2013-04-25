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

#include "insieme/frontend/convert.h"
#include "insieme/frontend/utils/ir_cast.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/unused.h"

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/encoder/lists.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include <boost/regex.hpp>

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS
#include <clang/AST/Expr.h>

#define IS_IR_REF(type) \
	(type->getNodeType() == core::NT_RefType)

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())


using namespace insieme;
using namespace insieme::frontend::utils;


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

namespace insieme {
namespace frontend {
namespace utils {



//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// FIXME: we can do this in a smarter way
std::size_t getPrecission(const core::TypePtr& type, const core::lang::BasicGenerator& gen){

	if (gen.isReal(type)){
		if 		(gen.isReal4(type)) return 4;
		else if (gen.isReal8(type)) return 8;
		else if (gen.isFloat(type)) return 4;
		else if (gen.isDouble(type)) return 8;
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
	else if (gen.isBool(type) || gen.isChar(type))
		return 1;

	return 0;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
core::ExpressionPtr castScalar(const core::TypePtr& targetTy, const core::ExpressionPtr& expr){
	const core::TypePtr& exprTy = expr->getType();
	core::IRBuilder builder( exprTy->getNodeManager() );
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	//std::cout << "####### Expr: #######" << std::endl;
	//dumpDetail(expr);
	//std::cout << "####### Expr Type: #######" << std::endl;
	//dumpDetail(exprTy);
	//std::cout << "####### cast Type: #######" << std::endl;
	//dumpDetail(targetTy);
	
	// is this the cast of a literal: to simplify code we'll return
	// a literal of the spected type
	if (expr->getNodeType() == core::NT_Literal){
		try{
		return castLiteral ( expr.as<core::LiteralPtr>(), targetTy);
		}catch (std::exception& e){
			// literal upgrade not supported, create cast
		}
	}

   	unsigned char code;
	// identify source type, to write right cast
	if (gen.isSignedInt (exprTy)) 	code = 1;
	if (gen.isUnsignedInt (exprTy)) code = 2;
	if (gen.isReal (exprTy))		code = 3;
	if (gen.isChar (exprTy))		code = 4;
	if (gen.isBool (exprTy))		code = 5;

	// identify target type
	if (gen.isSignedInt (targetTy)) 	code += 10;
	if (gen.isUnsignedInt (targetTy)) 	code += 20;
	if (gen.isReal (targetTy))			code += 30;
	if (gen.isChar (targetTy))			code += 40;
	if (gen.isBool (targetTy))			code += 50;

	core::ExpressionPtr op;
	bool precision = true;
	std::size_t bytes    = getPrecission(targetTy, gen);
	std::size_t bytesSrc = getPrecission(exprTy, gen);
	switch(code){
		case 11: 
			// only if target precission is smaller, we may have a precission loosse. 
			if (bytes != bytesSrc) return builder.callExpr(gen.getIntPrecisionFix(), expr, builder.getIntParamLiteral(bytes));
			else return expr;
		case 22: 
			if (bytes != bytesSrc) return builder.callExpr(gen.getUintPrecisionFix(), expr, builder.getIntParamLiteral(bytes));
			else return expr;
		case 33:
			if (bytes != bytesSrc) return builder.callExpr(gen.getRealPrecisionFix(), expr, builder.getIntParamLiteral(bytes));
			else return expr;
		case 44: 
		case 55: // no cast;
			// this is a cast withing the same type.
			// is a preccision adjust, if is on the same type,
			// no need to adjust anything
			return expr;
			
			break;
		case 12: op = gen.getUnsignedToInt(); break;
		case 13: op = gen.getRealToInt(); break;
		case 14: op = gen.getCharToInt(); break;
		case 15: op = gen.getBoolToInt(); break;

		case 21: op = gen.getSignedToUnsigned(); break;
		case 23: op = gen.getRealToUnsigned(); break;
		case 24: op = gen.getCharToUnsigned(); break;
		case 25: op = gen.getBoolToUnsigned(); break;

		case 31: op = gen.getSignedToReal(); break;
		case 32: op = gen.getUnsignedToReal(); break;
		case 34: op = gen.getCharToReal(); break;
		case 35: op = gen.getBoolToReal(); break;

		case 41: op = gen.getSignedToChar();  precision=false; break;
		case 42: op = gen.getUnsignedToChar();precision=false; break;
		case 43: op = gen.getRealToChar();    precision=false; break;
		case 45: op = gen.getBoolToChar();    precision=false; break;

		case 51: op = gen.getSignedToBool();  precision=false; break;
		case 52: op = gen.getUnsignedToBool();precision=false; break;
		case 53: op = gen.getRealToBool();    precision=false; break;
		case 54: op = gen.getCharToBool();    precision=false; break;

		default: 
				 std::cerr << "code: " << (int) code << std::endl;
				 assert(false && "cast not defined");
	}
	if (precision) {
		core::ExpressionList args;
		args.push_back(expr);
		args.push_back(builder.getIntParamLiteral(bytes));
		return builder.callExpr(targetTy, op, args);
	}
	else 
		return builder.callExpr(targetTy, op, expr);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
core::ExpressionPtr castToBool (const core::ExpressionPtr& expr){
	
	const core::TypePtr& exprTy = expr->getType();
	core::IRBuilder builder( exprTy->getNodeManager() );
	const core::lang::BasicGenerator& gen = builder.getLangBasic();

	if (gen.isBool(expr->getType())) return expr;

	if (isRefArray(expr->getType())) {
		return builder.callExpr(gen.getBool(), gen.getBoolLNot(), builder.callExpr(gen.getBool(), gen.getIsNull(), expr));
	}
	if (!gen.isInt(expr->getType())  && !gen.isReal(expr->getType()) && !gen.isChar(expr->getType())){

		dumpDetail(expr);
		std::cout << "****" << std::endl;
		dumpDetail(expr->getType());
		assert(false && "this type can not be converted now to bool. implement it! ");
	}

	return castScalar (gen.getBool(), expr);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
core::ExpressionPtr performClangCastOnIR (const insieme::core::IRBuilder& builder, 
										  const clang::CastExpr* castExpr, 
										  const core::TypePtr&    targetTy,
										  const core::ExpressionPtr& expr){
	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	core::TypePtr&& exprTy = expr->getType();

	if (VLOG_IS_ON(2)){
		VLOG(2) << "####### Expr: #######" << std::endl;
		dumpDetail(expr);
		VLOG(2) << "####### Expr Type: #######" << std::endl;
		dumpDetail(exprTy);
		VLOG(2)<< "####### cast Type: #######" << std::endl;
		dumpDetail(targetTy);
		VLOG(2)  << "####### clang: #######" << std::endl;
		castExpr->dump();
	}

	// it might be that the types are already fixed:
	// like LtoR in arrays, they will allways be a ref<...>
	if (*exprTy == *targetTy)
		return expr;

	// handle implicit casts according to their kind
	switch (castExpr->getCastKind()) {

		case clang::CK_LValueToRValue 	:
		//CLANG: case clang::CK_LValueToRValue - A conversion which causes the extraction of an r-value from the operand gl-value. 
		// The result of an r-value conversion is always unqualified.
		//
		// IR: this is the same as out ref deref ref<a'> -> a'
		{
			if(IS_IR_REF(exprTy))
				return builder.deref(expr);
			else{
				// arguments by value are not refs... but in C they are lefsides
				return expr;
			}
			break;
		}

		case clang::CK_IntegralCast 	:
		//CLANG: - A cast between integral types (other than to boolean). Variously a bitcast, a truncation,
		//a sign-extension, or a zero-extension. long l = 5; (unsigned) i
		//IR: convert to int or uint
		case clang::CK_IntegralToBoolean 	:
		// - Integral to boolean. A check against zero. (bool) i
		case clang::CK_IntegralToFloating 	:
		// - Integral to floating point. float f = i;
		case clang::CK_FloatingToIntegral 	:
		//case clang::CK_FloatingToIntegral - Floating point to integral. Rounds towards zero, discarding any fractional component.
		// (int) f
		case clang::CK_FloatingToBoolean 	:
		//case clang::CK_FloatingToBoolean - Floating point to boolean. (bool) f
		case clang::CK_FloatingCast 	:
		//case clang::CK_FloatingCast - Casting between floating types of different size. (double) f (float) ld
			return castScalar( targetTy, expr);
		
		case clang::CK_NoOp 	:
		/*case clang::CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. i
		* int -> int char** -> const char * const *
		* */
			// types equality has been already checked, if is is a NoOp is because clang identifies
			// this as the same type. but we might intepret it in a diff way. (ex, char literals are
			// int for clang, we build a char type
			
			if (gen.isPrimitive(expr->getType()) )
				return castScalar( targetTy, expr);
			else 
				return expr;


		case clang::CK_ArrayToPointerDecay 	:
		/*case clang::CK_ArrayToPointerDecay - Array to pointer decay. int[10] -> int* char[5][6] -> char(*)[6]
		* */
		{
			// if inner expression is not ref.... it might be a compound initializer
			core::ExpressionPtr retIr = expr;
			if (!IS_IR_REF(exprTy)){
				retIr = builder.callExpr(gen.getRefVar(),expr);
			}
		
			return builder.callExpr(gen.getRefVectorToRefArray(), retIr);
		}

		case clang::CK_BitCast 	:
		/*case clang::CK_BitCast - A conversion which causes a bit pattern of one type to be reinterpreted as a bit pattern 
		* of another type. 
		* Generally the operands must have equivalent size and unrelated types.  The pointer conversion 
		* char* -> int* is a bitcast. A conversion from any pointer type to a C pointer type is a bitcast unless 
		* it's actually BaseToDerived or DerivedToBase. A conversion to a block pointer or ObjC pointer type is a 
		* bitcast only if the operand has the same type kind; otherwise, it's one of the specialized casts below.  
		* Vector coercions are bitcasts.
		* */
		{
			// char* -> const char* is a bitcast in clang, and not a Noop, but we drop qualifiers
			if (*targetTy == *exprTy) return expr;

			// cast to void*
			if (gen.isAnyRef(targetTy)) {
				//return builder.callExpr(builder.getLangBasic().getRefToAnyRef(), expr); }
				return expr;
			}

			// is a cast of Null to another pointer type: 
			// we rebuild null
			if (*expr == *builder.literal(expr->getType(), "0")) {
				return builder.callExpr(gen.getGetNull(), builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
			}

			// cast from void*
			if (gen.isAnyRef(exprTy)) { 
				core::TypePtr elementType = core::analysis::getReferencedType(targetTy);
				return builder.callExpr(targetTy, gen.getNodeManager().getLangBasic().getRefReinterpret(), 
										expr, builder.getTypeLiteral(elementType));
			}

			// otherwhise, we just reinterpret
			core::ExpressionPtr innerExpr = expr;
			if (gen.isRefDeref(expr)){
				//clang does LtoR always, but we want refs in the cast. if there is a deref in the inner
				//expression remove it
				innerExpr = expr.as<core::LambdaExprPtr>()->getParameterList()[0];
			}
			return builder.callExpr(targetTy, builder.getNodeManager().getLangBasic().getRefReinterpret(), 
									innerExpr, builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
		}

		case clang::CK_VectorSplat 	:
		/*case clang::CK_VectorSplat - A conversion from an arithmetic type to a vector of that element type. Fills all elements 
		* ("splats") with the source value. __attribute__((ext_vector_type(4))) int v = 5;
		* */
			//return expr;
			return builder.callExpr(gen.getVectorInitUniform(), 
					expr,
					builder.getIntTypeParamLiteral(targetTy.as<core::VectorTypePtr>()->getSize())
				);

		case clang::CK_IntegralToPointer 	:
		/*case clang::CK_IntegralToPointer - Integral to pointer. A special kind of reinterpreting conversion. Applies to normal,
		* ObjC, and block pointers. (char*) 0x1001aab0 reinterpret_cast<int*>(0)
		* */
	
			// is a cast of Null to another pointer type: 
			// we rebuild null
			if (*expr == *builder.literal(expr->getType(), "0")) {
				return builder.callExpr(gen.getGetNull(), builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
			}
			else{
				std::cout << std::endl;
				dumpDetail(expr);
				dumpDetail(targetTy);
				assert(false && "Non NULL casts to pointer not supported");
			}



		///////////////////////////////////////
		//  PARTIALY IMPLEMENTED
		///////////////////////////////////////
		
		case clang::CK_NullToPointer 	:
		/*case clang::CK_NullToPointer - Null pointer constant to pointer, ObjC pointer, or block pointer.
		 * (void*) 0;
	 	 */
		{
		
			if (gen.isAnyRef(targetTy)) { return expr; } 

			// cast NULL to anything else
			if ((targetTy->getNodeType() == core::NT_RefType) && (*expr == *builder.literal(expr->getType(), "0"))) {
				return builder.callExpr(gen.getGetNull(), builder.getTypeLiteral(GET_REF_ELEM_TYPE(targetTy)));
			}
			else{
				//FIXME:: might be a cast to function type
				return expr;
			}
		}

		case clang::CK_ToVoid 	:
		/*case clang::CK_ToVoid - Cast to void, discarding the computed value. (void) malloc(2048)
		* */

		// this cast ignores inner expression, is not very usual, but might show off when dealing
		// with null pointer.  
		if (gen.isUnit(targetTy)) { return gen.getUnitConstant(); }

		///////////////////////////////////////
		//  NOT IMPLEMENTED
		///////////////////////////////////////

		case clang::CK_Dependent 	:
		/*case clang::CK_Dependent - A conversion which cannot yet be analyzed because either the expression 
		* or target type is dependent. These are created only for explicit casts; dependent ASTs aren't 
		* required to even approximately type-check. (T*) malloc(sizeof(T)) reinterpret_cast<intptr_t>(A<T>::alloc());
		* */

		case clang::CK_LValueBitCast 	:
		/* case clang::CK_LValueBitCast - A conversion which reinterprets the address of an l-value as an l-value of a different 
		* kind. Used for reinterpret_casts of l-value expressions to reference types. bool b; reinterpret_cast<char&>(b) = 'a';
		* */

		case clang::CK_BaseToDerived 	:
		/*case clang::CK_BaseToDerived - A conversion from a C++ class pointer/reference to a derived class
		* pointer/reference. B *b = static_cast<B*>(a);
		* */

		case clang::CK_DerivedToBase 	:
		/*case clang::CK_DerivedToBase - A conversion from a C++ class pointer to a base class pointer. A *a = new B();
		* */

		case clang::CK_UncheckedDerivedToBase 	:
		/*case clang::CK_UncheckedDerivedToBase - A conversion from a C++ class pointer/reference to a base class 
		* that can assume that the derived pointer is not null. const A &a = B(); b->method_from_a();
		* */

		case clang::CK_Dynamic 	:
		/*case clang::CK_Dynamic - A C++ dynamic_cast.
		* */

		case clang::CK_ToUnion 	:
		/*case clang::CK_ToUnion - The GCC cast-to-union extension. i
		* int -> union { int x; float y; } float -> union { int x; float y; }
		* */

		case clang::CK_FunctionToPointerDecay 	:
		/*case clang::CK_FunctionToPointerDecay - Function to pointer decay. void(int) -> void(*)(int)
		* */

		case clang::CK_NullToMemberPointer 	:
		/*case clang::CK_NullToMemberPointer - Null pointer constant to member pointer.
		 * int A::*mptr = 0; int (A::*fptr)(int) = nullptr;
		* */

		case clang::CK_BaseToDerivedMemberPointer 	:
		/*case clang::CK_BaseToDerivedMemberPointer - Member pointer in base class to member pointer in derived class.
		 * int B::*mptr = &A::member;
		* */

		case clang::CK_DerivedToBaseMemberPointer 	:
		/*case clang::CK_DerivedToBaseMemberPointer - Member pointer in derived class to member pointer in base class.
		* int A::*mptr = static_cast<int A::*>(&B::member);
		* */

		case clang::CK_MemberPointerToBoolean 	:
		/*case clang::CK_MemberPointerToBoolean - Member pointer to boolean. A check against the null member pointer.
		* */

		case clang::CK_ReinterpretMemberPointer 	:
		/*case clang::CK_ReinterpretMemberPointer - Reinterpret a member pointer as a different kind of member pointer.
		* C++ forbids this from crossing between function and object types, but otherwise does not restrict it. 
		* However, the only operation that is permitted on a "punned" member pointer is casting it back to the original type,
		* which is required to be a lossless operation (although many ABIs do not guarantee this on all possible intermediate types).
		* */

		case clang::CK_UserDefinedConversion 	:
		/*case clang::CK_UserDefinedConversion - Conversion using a user defined type conversion function.i
		* struct A { operator int(); }; int i = int(A());
		* */

		case clang::CK_ConstructorConversion 	:
		/*case clang::CK_ConstructorConversion - Conversion by constructor. struct A { A(int); }; A a = A(10);
		* */


		case clang::CK_PointerToIntegral 	:
		/*case clang::CK_PointerToIntegral - Pointer to integral. A special kind of reinterpreting conversion. Applies to normal, 
		* ObjC, and block pointers. (intptr_t) "help!"
		* */

		case clang::CK_PointerToBoolean 	:
		/*case clang::CK_PointerToBoolean - Pointer to boolean conversion. A check against null. Applies to normal, ObjC, 
		 * and block pointers.
		* */


		case clang::CK_CPointerToObjCPointerCast 	:
		/*case clang::CK_CPointerToObjCPointerCast - Casting a C pointer kind to an Objective-C pointer.
		* */

		case clang::CK_BlockPointerToObjCPointerCast 	:
		/*case clang::CK_BlockPointerToObjCPointerCast - Casting a block pointer to an ObjC pointer.
		* */

		case clang::CK_AnyPointerToBlockPointerCast 	:
		/*case clang::CK_AnyPointerToBlockPointerCast - Casting any non-block pointer to a block pointer. Block-to-block casts are bitcasts.
		* */

		case clang::CK_ObjCObjectLValueCast 	:
		/*Converting between two Objective-C object types, which can occur when performing reference binding to an Objective-C object.
		* */

		case clang::CK_FloatingRealToComplex 	:
		/*A conversion of a floating point real to a floating point complex of the original type. Injects the value as the
		* real component with a zero imaginary component. float -> _Complex float.
		* */

		case clang::CK_FloatingComplexToReal 	:
		/*Converts a floating point complex to floating point real of the source's element type. Just discards the imaginary 
		* component. _Complex long double -> long double.
		* */

		case clang::CK_FloatingComplexToBoolean 	:
		/*Converts a floating point complex to bool by comparing against 0+0i.
		* */

		case clang::CK_FloatingComplexCast 	:
		/*Converts between different floating point complex types. _Complex float -> _Complex double.
		* */

		case clang::CK_FloatingComplexToIntegralComplex 	:
		/*Converts from a floating complex to an integral complex. _Complex float -> _Complex int.
		* */

		case clang::CK_IntegralRealToComplex 	:
		/*Converts from an integral real to an integral complex whose element type matches the source. Injects the value as the 
		* real component with a zero imaginary component. long -> _Complex long.
		* */

		case clang::CK_IntegralComplexToReal 	:
		/*Converts an integral complex to an integral real of the source's element type by discarding the imaginary component. 
		* _Complex short -> short.
		* */

		case clang::CK_IntegralComplexToBoolean 	:
		/*Converts an integral complex to bool by comparing against 0+0i.
		* */

		case clang::CK_IntegralComplexCast 	:
		/*Converts between different integral complex types. _Complex char -> _Complex long long _Complex unsigned int -> 
		* _Complex signed int.
		* */

		case clang::CK_IntegralComplexToFloatingComplex 	:
		/*Converts from an integral complex to a floating complex. _Complex unsigned -> _Complex float.
		* */

		case clang::CK_ARCProduceObject 	:
		/*[ARC] Produces a retainable object pointer so that it may be consumed, e.g. by being passed to a consuming parameter.
		* Calls objc_retain.
		* */

		case clang::CK_ARCConsumeObject 	:
		/*[ARC] Consumes a retainable object pointer that has just been produced, e.g. as the return value of a retaining call. 
		* Enters a cleanup to call objc_release at some indefinite time.
		* */

		case clang::CK_ARCReclaimReturnedObject 	:
		/*[ARC] Reclaim a retainable object pointer object that may have been produced and autoreleased as part of a function 
		* return sequence.
		* */

		case clang::CK_ARCExtendBlockObject 	:
		/*[ARC] Causes a value of block type to be copied to the heap, if it is not already there. A number of other operations in
		* ARC cause blocks to be copied; this is for cases where that would not otherwise be guaranteed, such as when casting to a 
		* non-block pointer type.
		* */

		case clang::CK_AtomicToNonAtomic 	:
		case clang::CK_NonAtomicToAtomic 	:
		case clang::CK_CopyAndAutoreleaseBlockObject 	:
		case clang::CK_BuiltinFnToFnPtr 	:
			std::cout << " \nCAST: " << castExpr->getCastKindName () << " not supported!!"<< std::endl;
			assert(false);
		default:
			assert(false && "not all options listed, is this clang 3.2? maybe should upgrade Clang support");
	}
	
	assert(false && "control reached an invalid point!");

	return expr;

}

} // end utils namespace
} // end frontend namespace 
} // end insieme namespace 


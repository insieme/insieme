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

#include "insieme/core/types/subtyping.h"

#define CAST(expr, type) convertExprToType(builder, expr, type)

#define GET_REF_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::RefType>(type)->getElementType())

#define GET_VEC_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::VectorType>(type)->getElementType())

#define GET_ARRAY_ELEM_TYPE(type) \
	(core::static_pointer_cast<const core::ArrayType>(type)->getElementType())


using namespace insieme;
using namespace insieme::frontend::utils;

typedef std::vector<core::ExpressionPtr> ExpressionList;

namespace {

// This function performs the requires type conversion, from converting an expression. 
core::ExpressionPtr convertExprToType(const core::IRBuilder& 		builder, 
									  core::ExpressionPtr 	expr, 
									  const core::TypePtr& 			trgTy) 
{
	// list the all possible conversions 
	VLOG(2)<< "\t~ Starting converting into Type "<< trgTy->getNodeType();
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

	VLOG(1) << "\t~ CAST expr '" << *expr << "' : " << *argTy  << " -> " << *trgTy;

	///////////////////////////////////////////////////////////////////////////////////////
	//							ref<array<'a,#n>> -> Boolean
	///////////////////////////////////////////////////////////////////////////////////////
	// This happens when a reference is used in a conditional operation. In those situation 
	// the case is invalid and we hare to replace it with a comparison with the NULL reference.
	// therefore:
	//		if( ref )  ->  if( ref != Null )
	if ( gen.isBool(trgTy) && isRefArray(argTy)) 
	{
		// convert NULL (of type AnyRef) to the same ref type as the LHS expression
		return builder.callExpr(gen.getBoolLNot(), 
								builder.callExpr( gen.getBool(), gen.getIsNull(), expr ) 
							);
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 							ref<vector<'a,#n>> -> Boolean
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isBool(trgTy) && isRefVector(argTy)) 
	{
		// convert NULL (of type AnyRef) to the same ref type as the LHS expression
		return CAST(builder.callExpr(gen.getRefVectorToRefArray(), expr), trgTy);
	}

	
	///////////////////////////////////////////////////////////////////////////////////////
	// 								 anyref -> Boolean
	///////////////////////////////////////////////////////////////////////////////////////
	//		if( ref )  ->  if( ref != Null )
	//
	if ( gen.isBool(trgTy) && gen.isAnyRef(argTy) ) {
		return builder.callExpr(gen.getBoolLNot(), 
				builder.callExpr(gen.getBool(), gen.getIsNull(), 
						CAST(expr, builder.refType(gen.getUnit())) 
					)
				);
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 							Signed integer -> Boolean
	///////////////////////////////////////////////////////////////////////////////////////
	// cast a signed integer to boolean value, this happens for integer numbers when appear in
	// conditional expressions, for loop exit conditions or while stmt
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isBool(trgTy) && gen.isSignedInt(argTy) ) {
		return builder.callExpr(gen.getBool(), gen.getSignedIntNe(), {expr, builder.intLit(0)});
	}
	

	///////////////////////////////////////////////////////////////////////////////////////
	//							Unsigned integer -> Boolean
	///////////////////////////////////////////////////////////////////////////////////////
	// cast an unsigned integer to boolean value, this happens for integer numbers when appear in
	// conditional expressions, for loop exit conditions or while stmt
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isBool(trgTy) && gen.isUnsignedInt(argTy) ) {
		return builder.callExpr(gen.getBool(), gen.getUnsignedIntNe(), {expr, builder.uintLit(0)});
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 									Boolean -> Int
	///////////////////////////////////////////////////////////////////////////////////////
	// cast a boolean value to an integer
	if ( gen.isInt(trgTy) && gen.isBool(argTy) ) {
		return builder.castExpr(trgTy, builder.callExpr(gen.getInt4(), gen.getBoolToInt(), toVector(expr) ) );
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 									Char -> Generic Integer
	///////////////////////////////////////////////////////////////////////////////////////
	// Take the integer value of the char literal and create an int literal out of it (int)c
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isChar(argTy) && gen.isInt(trgTy) &&  expr->getNodeType() == core::NT_Literal ) {

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
					assert(false && "missing escape sequence.");
			}
		} else {
			assert(false && "Wrong encoding for char literals!");
		}	

		return builder.literal( utils::numeric_cast<std::string>(static_cast<short>(val)), trgTy );
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 									anyRef -> ref<'a>
	///////////////////////////////////////////////////////////////////////////////////////
	// Converts anyRef to the required ref target type. If the target type is not a ref this is 
	// considered a frontend error, therefore we are allowed to fail.
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isAnyRef(argTy) ) {
		assert( trgTy->getNodeType() == core::NT_RefType && 
				"AnyRef can only be converted to an L-Value (RefType)" 
			);
		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
		return builder.callExpr(trgTy, gen.getAnyRefToRef(), expr, builder.getTypeLiteral(subTy));
	}

	
	///////////////////////////////////////////////////////////////////////////////////////
	//	 								ref<'a> -> anyRef
	///////////////////////////////////////////////////////////////////////////////////////
	// Convert a ref<'a> type to anyRef. 
	///////////////////////////////////////////////////////////////////////////////////////
	if ( argTy->getNodeType() == core::NT_RefType && gen.isAnyRef(trgTy) ) {
		assert( argTy->getNodeType() == core::NT_RefType && 
				"AnyRef can only be converted to an L-Value (RefType)" );
		return builder.callExpr(trgTy, gen.getRefToAnyRef(), expr);
	}

	///////////////////////////////////////////////////////////////////////////////////////
	// 							0 -> anyRef 
	///////////////////////////////////////////////////////////////////////////////////////
	// Convert a ref<'a> type to anyRef. 
	///////////////////////////////////////////////////////////////////////////////////////
	if ( gen.isAnyRef(trgTy) &&  *expr == *builder.literal(argTy,"0") ) 
	{
		// FIXME: not sure about this being correct, we have to get a ref from a null in order to
		// convert it to the anyref value
		return builder.callExpr( gen.getGetNull(), builder.getTypeLiteral(argTy) );
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 							0 -> ref<array<'a,#n>>
	///////////////////////////////////////////////////////////////////////////////////////
	// Convert a ref<'a> type to anyRef. 
	///////////////////////////////////////////////////////////////////////////////////////
	if ( isRefArray(trgTy) && *expr == *builder.literal(argTy,"0") ) 
	{
		return builder.callExpr( gen.getGetNull(), builder.getTypeLiteral(GET_REF_ELEM_TYPE(trgTy)) );
	}

	///////////////////////////////////////////////////////////////////////////////////////
	// 									ref<'a> -> 'a
	///////////////////////////////////////////////////////////////////////////////////////
	// Converts a ref<'a> to a. This is required anywhere where a non ref type is needed and the 
	// current expression is of ref type. 
	///////////////////////////////////////////////////////////////////////////////////////
	if ( trgTy->getNodeType() != core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {
		// Recursively call the cast function to make sure the subtype and the target type matches
		return CAST(builder.deref(expr), trgTy);
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

		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
		
		if (isArray(subTy) && isVector(argTy)) {
			return CAST(expr, subTy);
		}

		// call the function recursively
		return builder.refVar( CAST(expr, subTy) );
	}

	// NOTE: from this point on we are sure the type of the target type and the argument type are
	// the same meaning that either we have a ref-type or non-ref type.

	
	///////////////////////////////////////////////////////////////////////////////////////
	// 						vector<'a, #n> -> array<'a,1>
	//
	// 	This is not directly allowed by the IR, but we can 
	///////////////////////////////////////////////////////////////////////////////////////

	//if ( builder.matchType("array<'a,#n>",trgTy) && builder.matchType("vector<'a,#n>",argTy) ) {
	//	return CAST(builder.refVar(expr), builder.refType(trgTy));
	//}


	///////////////////////////////////////////////////////////////////////////////////////
	// 						ref<vector<'a, #n>> -> ref<array<'a,1>>
	///////////////////////////////////////////////////////////////////////////////////////
	// convert a reference to a vector to a reference to an array using the refVector2RefArray literal  
	///////////////////////////////////////////////////////////////////////////////////////
	if ( isRefArray(trgTy) && isRefVector(argTy) ) {
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

//	// [ string -> vector<char,#n> ]
//	//
//	// Converts a string literal to a vector<char, #n>
//	if ( trgTy->getNodeType() == core::NT_VectorType && gen.isString(argTy) ) {
//		const core::VectorTypePtr& vecTy = core::static_pointer_cast<const core::VectorType>(trgTy);
//
//		assert(vecTy->getElementType()->getNodeType() != core::NT_RefType && 
//				"conversion of string literals to vector<ref<'a>> not yet supported");
//
//		assert(vecTy->getSize()->getNodeType() == core::NT_ConcreteIntTypeParam);
//		size_t vecSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTy->getSize())->getValue();
//
//		// do conversion from a string to an array of char
//		const core::LiteralPtr& strLit = core::static_pointer_cast<const core::Literal>(expr);
//		std::string strVal = strLit->getStringValue();
//		// because string literals are stored with the corresponding " " we iterate from 1 to length()-2
//		// but we need an additional character to store the string terminator \0
//		
//		assert(strVal.length() - 1 <= vecSize && "Target vector type not large enough to hold string literal"); 
//		// FIXME: Use clang error report for this
//		
//		ExpressionList vals(vecSize);
//		size_t it;
//		for(it=0; it<strVal.length()-2; ++it) {
//			char c = strVal.at(it+1);
//			std::string str(1,c);
//			switch(c) {
//				case '\n': str = "\\n";	   break;
//				case '\\': str = "\\\\";   break;
//				case '\r': str = "\\r";	   break;
//				case '\t': str = "\\t";	   break;
//				case '\0': str = "\\0";	   break;
//			}
//			vals[it] = builder.literal( std::string("\'") + str + "\'", gen.getChar() );
//		}
//		// put '\0' terminators on the remaining elements
//		for (; it<vecSize; ++it ) {
//			vals[it] = builder.literal( std::string("\'") + "\\0" + "\'", gen.getChar() ); // Add the string terminator
//		}
//		return builder.vectorExpr(vecTy , vals);
//	}


	if (isRefVector(argTy) && isRefVector(trgTy)) {
		return builder.refVar(CAST(builder.deref(expr), GET_REF_ELEM_TYPE(trgTy)));
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
			assert(false && "Converting from vector<'a> to vector<'b>"); 
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

				assert(vecArgTy->getSize()->getNodeType() == core::NT_ConcreteIntTypeParam);

				// do conversion from a string to an array of char
				std::string strVal = plainExpr.as<core::LiteralPtr>()->getStringValue();

				// because string literals are stored with the corresponding " " we iterate from 1 to length()-2
				// but we need an additional character to store the string terminator \0
				
				assert(strVal.length()-1 <= vecTrgSize && 
						"Target vector type not large enough to hold string literal"
					); 

				// FIXME: Use clang error report for this
				ExpressionList vals(vecArgSize);
				size_t it;
				for(it=0; it<strVal.length()-2; ++it) {
					char c = strVal.at(it+1);
					std::string str(1,c);
					switch(c) {
						case '\n': str = "\\n";	   break;
						case '\\': str = "\\\\";   break;
						case '\r': str = "\\r";	   break;
						case '\t': str = "\\t";	   break;
						case '\0': str = "\\0";	   break;
					}
					vals[it] = builder.literal( std::string("\'") + str + "\'", gen.getChar() );
				}
				// put '\0' terminators on the remaining elements
				vals[it] = builder.literal( std::string("\'") + "\\0" + "\'", gen.getChar() ); // Add the string terminator
				initList = core::encoder::toIR(plainExpr->getNodeManager(), vals);
			} else {

				// we assume that the expr is an initializer for a vector, a vector expr
				auto vecExpr = plainExpr.as<core::VectorExprPtr>()->getExpressions();
				initList = core::encoder::toIR(plainExpr->getNodeManager(), 
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
	if (isVector(trgTy) && *GET_VEC_ELEM_TYPE(trgTy) == *argTy ) {
		auto vecTrgTy = trgTy.as<core::VectorTypePtr>();

		return builder.callExpr(gen.getVectorInitUniform(), 
					expr,
					builder.getIntTypeParamLiteral(vecTrgTy->getSize())
				);
	}


	///////////////////////////////////////////////////////////////////////////////////////
	// 							ref<'a> -> ref<ref<'a>>
	///////////////////////////////////////////////////////////////////////////////////////
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {
		const core::TypePtr& subArgTy = GET_REF_ELEM_TYPE(argTy);
		if (*subArgTy == *trgTy) { return builder.deref( expr ); }
	}



	///////////////////////////////////////////////////////////////////////////////////////
	// 							  ref<'a> -> ref<'b>
	///////////////////////////////////////////////////////////////////////////////////////
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {

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

	// [ volatile<'a> -> 'a ]
	if ( core::analysis::isVolatileType(argTy) ) {
		return cast(builder.callExpr( trgTy, gen.getVolatileRead(), expr), trgTy);
	}

	// [ 'a -> volatile<'a> ]
	if ( core::analysis::isVolatileType(trgTy) ) {
		return builder.callExpr( trgTy, gen.getVolatileRead(), cast(expr, core::analysis::getVolatileType(trgTy)) );
	}

	return builder.castExpr(trgTy, expr);
	//assert(false && "Cast conversion not supported!");
}

} // end anonymous namespace

namespace insieme {
namespace frontend {
namespace utils {


core::ExpressionPtr refScalarToRefArray(const core::ExpressionPtr& expr) {
	assert(expr->getType()->getNodeType() == core::NT_RefType);
	
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

			// ... or array.ref.element ...
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

core::ExpressionPtr cast(const core::ExpressionPtr& expr, const core::TypePtr& trgTy) {

	core::IRBuilder builder( trgTy->getNodeManager() );

	VLOG(1) << "@ Converting expression '" << *expr 
			<< "' with type '" << *expr->getType() 
			<< "' to target type '" << *trgTy << "'";

	core::ExpressionPtr&& ret = convertExprToType(builder, expr, trgTy);
	// assert(*trgTy == *expr->getType() && "Casting non supported!");
	
	VLOG(1) << "@ Expression converted to '" << *ret 
			<< "' with type '" << *ret->getType() << "'" << std::endl;

	return ret;
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



} // end utils namespace
} // end frontend namespace 
} // end insieme namespace 


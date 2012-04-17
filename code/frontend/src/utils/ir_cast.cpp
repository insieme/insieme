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

#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

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
									  const core::ExpressionPtr& 	expr, 
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
			if (GET_REF_ELEM_TYPE(argTy)->getNodeType() != core::NT_ArrayType) {
				return builder.deref( expr );
			}
		}
		return expr;
	}
		
	// in the case of FuncType check against the return type
	if ( argTy->getNodeType() == core::NT_FunctionType && 
			*core::static_pointer_cast<const core::FunctionType>(argTy)->getReturnType() == *trgTy ) 
	{
		return expr;
	}

	VLOG(1) << "\t~ CAST expr '" << *expr << "' : " << *argTy  << " -> " << *trgTy;

	// [ ref<array<'a>> -> Boolean ]
	// 
	// This happens when a reference is used in a conditional operation. In those situation 
	// the case is invalid and we hare to replace it with a comparison with the NULL reference.
	// therefore:
	//		if( ref )  ->  if( ref != Null )
	if ( gen.isBool(trgTy) && argTy->getNodeType() == core::NT_RefType && 
			GET_REF_ELEM_TYPE(argTy)->getNodeType() == core::NT_ArrayType ) 
	{
		// convert NULL (of type AnyRef) to the same ref type as the LHS expression
		return builder.callExpr(gen.getBoolLNot(), builder.callExpr( gen.getBool(), gen.getIsNull(), expr ) );
	}

	// [ anyref -> Boolean ]
	// 
	//		if( ref )  ->  if( ref != Null )
	if ( gen.isBool(trgTy) && gen.isAnyRef(argTy) ) {
		return builder.callExpr(gen.getBoolLNot(), 
				builder.callExpr( 
					gen.getBool(), 
					gen.getIsNull(), 
					CAST(expr, builder.refType(gen.getUnit())) ) 
				);
	}

	// [ Signed integer -> Boolean ]
	//
	// cast a signed integer to boolean value, this happens for integer numbers when appear in conditional
	// expressions, for loop exit conditions or while stmt
	if ( gen.isBool(trgTy) && gen.isSignedInt(argTy) ) {
		return builder.callExpr(gen.getBool(), gen.getSignedIntNe(), toVector(expr, builder.intLit(0)));
	}
	
	// [ Unsigned integer -> Boolean ]
	//
	// cast an unsigned integer to boolean value, this happens for integer numbers when appear in conditional
	// expressions, for loop exit conditions or while stmt
	if ( gen.isBool(trgTy) && gen.isUnsignedInt(argTy) ) {
		return builder.callExpr(gen.getBool(), gen.getUnsignedIntNe(), toVector(expr, builder.uintLit(0)));
	}

	// [ Boolean -> Int ]
	//
	// cast a boolean value to an integer
	if ( gen.isInt(trgTy) && gen.isBool(argTy) ) {
		return builder.castExpr(trgTy, builder.callExpr(gen.getInt4(), gen.getBoolToInt(), toVector(expr) ) );
	}

	// [ Char -> Generic Integer ] 
	// 
	// Take the integer value of the char literal and create an int literal out of it (int)c
	if ( gen.isChar(argTy) && gen.isInt(trgTy) &&  expr->getNodeType() == core::NT_Literal ) {
		const core::LiteralPtr& lit = core::static_pointer_cast<const core::Literal>(expr);
		char val;
		if ( lit->getStringValue().length() == 3) {
			val = lit->getStringValue()[1]; // chars are encoded as 'V', therefore position 1 always contains the char value
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

	// [ anyRef -> ref<'a> ]
	//
	// Converts anyRef to the required ref target type. If the target type is not a ref this is 
	// considered a frontend error, therefore we are allowed to fail.
	if ( gen.isAnyRef(argTy) ) {
		assert( trgTy->getNodeType() == core::NT_RefType && 
				"AnyRef can only be converted to an L-Value (RefType)" 
			);
		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
		return builder.callExpr(trgTy, gen.getAnyRefToRef(), 
				toVector<core::ExpressionPtr>(expr, builder.getTypeLiteral(subTy))
			);
	}
	
	// [ ref<'a> -> anyRef ]
	//
	// Convert a ref<'a> type to anyRef. 
	if ( argTy->getNodeType() == core::NT_RefType && gen.isAnyRef(trgTy) ) {
		assert( argTy->getNodeType() == core::NT_RefType && 
				"AnyRef can only be converted to an L-Value (RefType)" );
		return builder.callExpr(trgTy, gen.getRefToAnyRef(), toVector<core::ExpressionPtr>(expr));
	}

	// [ 0 -> anyRef ]
	//
	// Convert a ref<'a> type to anyRef. 
	if ( gen.isAnyRef(trgTy) && (*expr == *builder.literal(argTy,"0")) ) {
		// FIXME: not sure about this being correct, we have to get a ref from a null in order to convert it to 
		// the anyref value
		return CAST(builder.callExpr( gen.getGetNull(), builder.getTypeLiteral(argTy) ), trgTy);
	}

	// [ ref<'a> -> 'a ]
	//
	// Converts a ref<'a> to a. This is required anywhere where a non ref type is needed and the 
	// current expression is of ref type. 
	if ( trgTy->getNodeType() != core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {
		// Recursively call the cast function to make sure the subtype and the target type matches
		return CAST(builder.deref(expr), trgTy);
	}

	// [ 'a -> ref<'a> ]
	//
	// Convert an expression of non-ref type to an expression with ref-type. This is allowed for example 
	// for string literals which can be converted to ref<arrays<>> (because of the C semantics) 
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() != core::NT_RefType ) {
		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(trgTy);
		// The function requires a refType and the current argument is of non-ref type
		if ( subTy->getNodeType() == core::NT_ArrayType && builder.getLangBasic().isString(argTy) ) {
			// If the argument is a string then we have to convert the string into a char pointer
			// because of C semantics 
			return builder.callExpr( gen.getStringToCharPointer(), expr );
		} 

		// if last call was a deref (*) => undo call
		if ( *subTy == *argTy && core::analysis::isCallOf(expr, gen.getRefDeref()) ) {
			return static_pointer_cast<const core::CallExpr>(expr)->getArgument(0);
		}

		// call the function recursively
		return builder.refVar( CAST(expr, subTy) );
	}

	// NOTE: from this point on we are sure the type of the target type and the argument type are the same 
	//       meaning that either we have a ref-type or non-ref type.

	// [ ref<vector<'a, #n>> -> ref<array<'a,1>> ]
	//
	// convert a reference to a vector to a reference to an array using the refVector2RefArray literal  
	if ( trgTy->getNodeType() == core::NT_RefType) {
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

	// [ vector<'a, #n> -> array<'a,1> ]
	//
	// convert a vector to an array using the Vector2Array literal  
	if ( trgTy->getNodeType() == core::NT_ArrayType && argTy->getNodeType() == core::NT_VectorType ) {
		// we are sure at this point the type of arg is of ref-type as well
		const core::TypePtr& trgSubTy = GET_ARRAY_ELEM_TYPE(trgTy);
		const core::TypePtr& argSubTy = GET_VEC_ELEM_TYPE(argTy);
	
		assert(*trgSubTy == *argSubTy && "Cannot convert vector<'a> to array<'b>.");
		return builder.callExpr( trgTy,	gen.getVectorToArray(), expr );
	}

	// [ string -> vector<char,#n> ]
	//
	// Converts a string literal to a vector<char, #n>
	if ( trgTy->getNodeType() == core::NT_VectorType && gen.isString(argTy) ) {
		const core::VectorTypePtr& vecTy = core::static_pointer_cast<const core::VectorType>(trgTy);

		assert(vecTy->getElementType()->getNodeType() != core::NT_RefType && 
				"conversion of string literals to vector<ref<'a>> not yet supported");

		assert(vecTy->getSize()->getNodeType() == core::NT_ConcreteIntTypeParam);
		size_t vecSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTy->getSize())->getValue();

		// do conversion from a string to an array of char
		const core::LiteralPtr& strLit = core::static_pointer_cast<const core::Literal>(expr);
		std::string strVal = strLit->getStringValue();
		// because string literals are stored with the corresponding " " we iterate from 1 to length()-2
		// but we need an additional character to store the string terminator \0
		
		assert(strVal.length() - 1 <= vecSize && "Target vector type not large enough to hold string literal"); 
		// FIXME: Use clang error report for this
		
		ExpressionList vals(vecSize);
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
		for (; it<vecSize; ++it ) {
			vals[it] = builder.literal( std::string("\'") + "\\0" + "\'", gen.getChar() ); // Add the string terminator
		}
		return builder.vectorExpr(vecTy , vals);
	}

	// [ vector<'a, #n> -> vector<'b, #m> ] 
	//
	// this conversion is only valid if 'a and 'b are the same type and #m >= #n, in the rest of the cases 
	// we produce a compiler error saying this cast is not allowed within the IR type system
	if ( trgTy->getNodeType() == core::NT_VectorType && argTy->getNodeType() == core::NT_VectorType ) {
		// if we are here is because the two types are not the same, check whether the problem is the 
		// element type or the dimension
		const core::VectorTypePtr& vecTrgTy = core::static_pointer_cast<const core::VectorType>(trgTy);
		const core::VectorTypePtr& vecArgTy = core::static_pointer_cast<const core::VectorType>(argTy);
		// check the type first 
		if ( *vecArgTy->getElementType() != *vecTrgTy->getElementType() ) {
			if(*vecArgTy->getElementType() == *builder.getNodeManager().getLangBasic().getBool()
					&& *vecTrgTy->getElementType() == *builder.getNodeManager().getLangBasic().getInt4()) {
				LOG(ERROR) << "Casting vector of type " << vecArgTy << " to vector of type " << *vecTrgTy->getElementType() <<
						"! This is only an SC12 workaround to store the result of vector comparisons in an int<4> vector. You should NOT do this!";
				return builder.castExpr(vecTrgTy, expr);
			}

			// converting from a vector of a type to a vector of another type, this is not possible
			assert(false && "Converting from vector<'a> to vector<'b>"); 
		}
		if ( *vecArgTy->getSize() != *vecTrgTy->getSize() ) {
			// converting from a vector size X to vector size Y, only possible if X <= Y
			size_t vecTrgSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTrgTy->getSize())->getValue();
			size_t vecArgSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecArgTy->getSize())->getValue();
			assert(vecTrgSize >= vecArgSize && "Conversion not possible");

			// TODO report it as an error ? 
			assert(false && "Casting between two different vector sizes not yet implemented!");
			return expr;
		}
	}

	// [ string -> array<char,1> ]
	//
	// Converts a string literal to an array of chars, we do this by converting the string to a vector of chars 
	// and then converting the vector to an array. 
	if ( trgTy->getNodeType() == core::NT_ArrayType && gen.isString(argTy) ) {
		const core::ArrayTypePtr& arrTy = core::static_pointer_cast<const core::ArrayType>(trgTy);
		assert( gen.isChar(arrTy->getElementType()) && "Converting a string to something which is not a char*" );
		
		// convert the string into a vector and then use vector.to.array to get the desired array
		core::ExpressionPtr&& ret = 
			CAST(expr, 
				builder.vectorType(gen.getChar(), 
					core::ConcreteIntTypeParam::get(
						builder.getNodeManager(), 
						core::static_pointer_cast<const core::Literal>(expr)->getStringValue().length()-1) )
				);

		// now convert the vector<char, #n> into an array<char, #n>
		return builder.callExpr( trgTy, gen.getVectorToArray(), toVector(ret) );
	}
	
	// [ 'a -> array<'a,1> ]
	//
	// builds an array from a scalar value
	if ( trgTy->getNodeType() == core::NT_ArrayType && 	argTy->getNodeType() != core::NT_ArrayType && 
			argTy->getNodeType() != core::NT_VectorType )
	{
		// This is done by creating a wrapping array containing the argument
		const core::TypePtr& subTy = GET_ARRAY_ELEM_TYPE(trgTy);
		core::ConcreteIntTypeParamPtr&& size = core::ConcreteIntTypeParam::get(builder.getNodeManager(), 1);
		core::ExpressionPtr vecExpr = builder.callExpr( 
				builder.vectorType(subTy, size), // vec<subTy,1>
				gen.getVectorInitUniform(), 
				toVector( CAST(expr, subTy), builder.getIntTypeParamLiteral(size) )
			);
		return builder.callExpr( trgTy, gen.getVectorToArray(), toVector(vecExpr) );
	}

	// [ ref<'a> -> ref<array<'a>> ]
	//
	// Use the scalarToArray literal to perform this kind of conversion
	if ( trgTy->getNodeType() == core::NT_RefType ) {
		assert( argTy->getNodeType() == core::NT_RefType );
		const core::TypePtr& subTrgTy = core::analysis::getReferencedType(trgTy);
		if ( subTrgTy->getNodeType() == core::NT_ArrayType ) {
			// If the sub type of the arrat is 'a as well as the referenced type of ref, then apply
			// the cast 
			if (*core::analysis::getReferencedType(argTy) == 
					*core::static_pointer_cast<const core::ArrayType>(subTrgTy)->getElementType())
				return refScalarToRefArray(expr);
		}
	}

	// [ ref<'a> -> ref<ref<'a>> ]
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {
		const core::TypePtr& subArgTy = GET_REF_ELEM_TYPE(argTy);
		if (*subArgTy == *trgTy) { return builder.deref( expr ); }
	}

	// [ ref<'a> -> ref<'b> ]
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {

		core::TypePtr nonRefTrgTy = trgTy.as<core::RefTypePtr>()->getElementType();
		return builder.callExpr(trgTy, builder.getNodeManager().getLangBasic().getRefReinterpret(), expr, builder.getTypeLiteral(nonRefTrgTy));
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
	//LOG(ERROR) << ": converting expression '" << *expr << "' of type '" << *expr->getType() << "' to type '" 
			   //<< *trgTy << "' not yet supported!";
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

} // end utils namespace
} // end frontend namespace 
} // end insieme namespace 


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

#include "insieme/annotations/ocl/ocl_annotations.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_compiler.h"

#include "insieme/frontend/insieme_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"

#include "insieme/annotations/c/naming.h"

#include "clang/AST/StmtVisitor.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

#include <clang/AST/DeclCXX.h>
#include <clang/AST/ExprCXX.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace std {
std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
}
} // end std namespace

namespace {
// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	/*
	 *  we use the getDecomposedSpellingLoc() method because in case we read macros values we have to read the expanded
	 *  value
	 */
	std::pair<FileID, unsigned>&& startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef&& startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart,
			clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions())
		);
}

/*
 * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments with the pack
 * operation provided by the IR.
 */
vector<core::ExpressionPtr>
tryPack(const core::ASTBuilder& builder, core::FunctionTypePtr funcTy, const ExpressionList& args) {

	// check if the function type ends with a VAR_LIST type
	const core::TypeList& argsTy = funcTy->getParameterTypes();
	// assert(argsTy && "Function argument is of not type TupleType");

	// if the tuple type is empty it means we cannot pack any of the arguments
	if( argsTy.empty() ) {
		return args;
	}

	const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
	if ( gen.isVarList(argsTy.back()) ) {
		ExpressionList ret;
		assert(args.size() >= argsTy.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin()+argsTy.size()-1, std::back_inserter(ret));

		ExpressionList toPack;
		if ( args.size() > argsTy.size()-1 ) {
			std::copy(args.begin()+argsTy.size()-1, args.end(), std::back_inserter(toPack));
		}

		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
		ret.push_back(
			builder.callExpr(gen.getVarList(), gen.getVarlistPack(), builder.tupleExpr(toPack))
		);
		return ret;
	}
	return args;
}

core::CallExprPtr getSizeOfType(const core::ASTBuilder& builder, const core::TypePtr& type) {
	core::LiteralPtr size;

	const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type) ) {
		return builder.callExpr(
			gen.getUnsignedIntMul(),
			builder.literal( gen.getUInt8(), toString(*(vecTy->getSize())) ),
			getSizeOfType( builder, vecTy->getElementType() )
		);
	}
	// in case of ref<'a>, recurr on 'a
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type) ) {
		return getSizeOfType( builder, refTy->getElementType() );
	}

	return builder.callExpr( gen.getSizeof(), gen.getTypeLiteral(type) );
}

core::ExpressionPtr
handleMemAlloc(const core::ASTBuilder& builder, const core::TypePtr& type, const core::ExpressionPtr& subExpr) {

	if( core::CallExprPtr&& callExpr = core::dynamic_pointer_cast<const core::CallExpr>(subExpr) ) {

		if ( core::LiteralPtr&& lit = core::dynamic_pointer_cast<const core::Literal>(callExpr->getFunctionExpr()) ) {

			if ( lit->getValue() == "malloc" || lit->getValue() == "calloc" ) {
                assert(((lit->getValue() == "malloc" && callExpr->getArguments().size() == 1) || 
						(lit->getValue() == "calloc" && callExpr->getArguments().size() == 2)) && 
							"malloc() and calloc() takes respectively 1 and 2 arguments"
					  );

				const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
				// The type of the cast should be ref<array<'a>>, and the sizeof('a) need to be derived
				assert(type->getNodeType() == core::NT_RefType);
				assert(core::analysis::getReferencedType(type)->getNodeType() == core::NT_ArrayType);

				const core::RefTypePtr& refType = core::static_pointer_cast<const core::RefType>(type);
				const core::ArrayTypePtr& arrayType = core::static_pointer_cast<const core::ArrayType>(refType->getElementType());
				const core::TypePtr& elemType = arrayType->getElementType();

				/*
				 * The number of elements to be allocated of type 'targetType' is:
				 * 		-> 	expr / sizeof(targetType)
				 */
				core::CallExprPtr&& size = builder.callExpr(
					gen.getUInt8(), gen.getUnsignedIntDiv(), callExpr->getArguments().front(), getSizeOfType(builder, elemType)
				);

				return builder.refNew(builder.callExpr(arrayType, gen.getArrayCreate1D(),
						gen.getTypeLiteral(elemType), size)
					);
			}
		}
	}
	return core::ExpressionPtr();
}

core::ExpressionPtr getCArrayElemRef(const core::ASTBuilder& builder, const core::ExpressionPtr& expr) {
	const core::TypePtr& exprTy = expr->getType();
	if (exprTy->getNodeType() == core::NT_RefType) {
		const core::TypePtr& subTy = core::static_pointer_cast<const core::RefType>(exprTy)->getElementType();

		if(subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
			core::TypePtr elemTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();
			return builder.callExpr( 
				builder.refType(elemTy), 
			 	( subTy->getNodeType() == core::NT_VectorType ? 
				 	builder.getBasicGenerator().getVectorRefElem():
				 	builder.getBasicGenerator().getArrayRefElem1D() ), 
			 	expr, 
				builder.uintLit(0)
			);
		}
	}
	return expr;
}

/**
 * This function tries to restructure the given expression of a reference to a scalar
 * into a reference to an array - if possible without using the scalar.to.ref.array literal.
 *
 * @param expr the expression to be converted
 * @return the rewritten, equivalent expression exposing a reference to an array
 */
core::ExpressionPtr convertRefScalarToRefArray(const core::ASTBuilder& builder, const core::ExpressionPtr& expr) {
	assert(expr->getType()->getNodeType() == core::NT_RefType);

	// construct result type
	core::TypePtr resType = builder.refType(builder.arrayType(core::analysis::getReferencedType(expr->getType())));

	// simple case distinction among structure of expression
	if (expr->getNodeType() == core::NT_CallExpr) {
		const core::lang::BasicGenerator& basic = builder.getBasicGenerator();
		core::CallExprPtr call = static_pointer_cast<const core::CallExpr>(expr);

		// check invoked function
		try {

			// if it is vector-ref-element:
			if (basic.isVectorRefElem(call->getFunctionExpr()) && core::arithmetic::toFormula(call->getArgument(1)).isZero()) {
				// convert vector to array instead
				return builder.callExpr(resType, basic.getRefVectorToRefArray(), call->getArgument(0));
			}

			// ... or array.ref.element ...
			if (basic.isArrayRefElem1D(call->getFunctionExpr()) && core::arithmetic::toFormula(call->getArgument(1)).isZero()) {
				// skip this step!
				return call->getArgument(0);
			}

		} catch (const core::arithmetic::NotAFormulaException& ne) {
			// => subscript is not zero => use default handling
		}
	}

	// fall-back solution => use scalar to array literal
	return builder.callExpr(resType, builder.getBasicGenerator().getScalarToArray(), expr);
}

// This function performs the requires type conversion, from converting an expression. 
core::ExpressionPtr 
convertExprTo(const core::ASTBuilder& builder, const core::TypePtr& trgTy, 	const core::ExpressionPtr& expr) {
	// list the all possible conversions 
	const core::TypePtr& argTy = expr->getType();
	const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
	
	if ( *trgTy == *argTy ) { return expr; }

	if ( gen.isVarList(trgTy) ) { 
		// what to do here? deref or not deref?
		if (argTy->getNodeType() == core::NT_RefType) {
			const core::TypePtr& subTy = core::static_pointer_cast<const core::RefType>(argTy)->getElementType();
			// because ref<array<>> are used to represent R-value C pointers we can pass it 
			// to the caller function, the semantics is that the function can potentially 
			// change the content of the array
			if (subTy->getNodeType() != core::NT_ArrayType) {
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
			core::static_pointer_cast<const core::RefType>(argTy)->getElementType()->getNodeType() == core::NT_ArrayType ) 
	{
		//const core::TypePtr& subTy = core::static_pointer_cast<const core::RefType>(argTy)->getElementType();
		// convert NULL (of type AnyRef) to the same ref type as the LHS expression
		//core::ExpressionPtr&& nullRef = builder.callExpr(argTy, gen.getAnyRefToRef(), 
				//toVector<core::ExpressionPtr>(gen.getNull(), gen.getTypeLiteral(subTy) ) );

		return builder.callExpr(gen.getBoolLNot(), builder.callExpr( gen.getBool(), gen.getIsNull(), expr ) );
	}

	// [ Signed integer -> Boolean ]
	//
	// cast a signed integer to boolean value, this happens for integer numbers when appear in conditional
	// expressions, for loop exit conditions or while stmt
	if ( gen.isBool(trgTy) && gen.isInt(argTy) ) {
		return builder.callExpr(gen.getBool(), gen.getSignedIntNe(), toVector(expr, builder.intLit(0)));
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
		if ( lit->getValue().length() == 3) {
			val = lit->getValue()[1]; // chars are encoded as 'V', therefore position 1 always contains the char value	
		} else if ( lit->getValue().length() == 4 ) {
			// this char literal contains some escaped sequence which is represented with 2 chars' 
			std::string strVal = lit->getValue().substr(1,2);
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
		assert( trgTy->getNodeType() == core::NT_RefType && "AnyRef can only be converted to an L-Value (RefType)" );
		const core::TypePtr& subTy = core::static_pointer_cast<const core::RefType>(trgTy)->getElementType();
		return builder.callExpr(trgTy, gen.getAnyRefToRef(), toVector<core::ExpressionPtr>(expr, gen.getTypeLiteral(subTy)));
	}
	
	// [ ref<'a> -> anyRef ]
	//
	// Convert a ref<'a> type to anyRef. 
	if ( argTy->getNodeType() == core::NT_RefType && gen.isAnyRef(trgTy) ) {
		assert( argTy->getNodeType() == core::NT_RefType && "AnyRef can only be converted to an L-Value (RefType)" );
		return builder.callExpr(trgTy, gen.getRefToAnyRef(), toVector<core::ExpressionPtr>(expr));
	}

	// [ 0 -> anyRef ]
	//
	// Convert a ref<'a> type to anyRef. 
	if ( gen.isAnyRef(trgTy) && (*expr == *builder.literal(argTy,"0")) ) {
		// FIXME: not sure about this being correct, we have to get a ref from a null in order to convert it to 
		// the anyref value
		return convertExprTo(builder, trgTy, builder.callExpr( gen.getGetNull(), gen.getTypeLiteral(argTy) ) );
	}

	// [ ref<'a> -> 'a ]
	//
	// Converts a ref<'a> to a. This is required anywhere where a non ref type is needed and the 
	// current expression is of ref type. 
	if ( trgTy->getNodeType() != core::NT_RefType && argTy->getNodeType() == core::NT_RefType ) {
		// Recursively call the cast function to make sure the subtype and the target type matches
		return convertExprTo(builder, trgTy, builder.deref(expr));
	}

	// [ 'a -> ref<'a> ]
	//
	// Convert an expression of non-ref type to an expression with ref-type. This is allowed for example 
	// for string literals which can be converted to ref<arrays<>> (because of the C semantics) 
	if ( trgTy->getNodeType() == core::NT_RefType && argTy->getNodeType() != core::NT_RefType ) {
		const core::TypePtr& subTy = core::static_pointer_cast<const core::RefType>(trgTy)->getElementType();
		// The function requires a refType and the current argument is of non-ref type
		if ( subTy->getNodeType() == core::NT_ArrayType && builder.getBasicGenerator().isString(argTy) ) {
			// If the argument is a string then we have to convert the string into a char pointer
			// because of C semantics 
			return builder.callExpr( gen.getStringToCharPointer(), expr );
		} 

		// if last call was a deref (*) => undo call
		if ( core::analysis::isCallOf(expr, gen.getRefDeref()) ) {
			return static_pointer_cast<const core::CallExpr>(expr)->getArgument(0);
		}

		// call the function recursively
		return builder.refVar( convertExprTo(builder, subTy, expr) );
	}

	// NOTE: from this point on we are sure the type of the target type and the argument type are the same 
	//       meaning that either we have a ref-type or non-ref type.

	// [ ref<vector<'a, #n>> -> ref<array<'a,1>> ]
	//
	// convert a reference to a vector to a reference to an array using the refVector2RefArray literal  
	if ( trgTy->getNodeType() == core::NT_RefType) {
		// we are sure at this point the type of arg is of ref-type as well
		const core::TypePtr& elemTy = core::static_pointer_cast<const core::RefType>(trgTy)->getElementType();
		const core::TypePtr& argSubTy = core::static_pointer_cast<const core::RefType>(argTy)->getElementType();
		if(elemTy->getNodeType() == core::NT_ArrayType && argSubTy->getNodeType() == core::NT_VectorType) {
			const core::TypePtr& elemVecTy = core::static_pointer_cast<const core::VectorType>(argSubTy)->getElementType();
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
		const core::TypePtr& trgSubTy = core::static_pointer_cast<const core::ArrayType>(trgTy)->getElementType();
		const core::TypePtr& argSubTy = core::static_pointer_cast<const core::VectorType>(argTy)->getElementType();
	
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
		std::string strVal = strLit->getValue();
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
			// converting from a vector of a type to a vector of another type, this is not possible
			assert(false && "Converting from vector<'a> to vector<'b>"); 
		}
		if ( *vecArgTy->getSize() != *vecTrgTy->getSize() ) {
			// converting from a vector size X to vector size Y, only possible if X <= Y
			size_t vecTrgSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTrgTy->getSize())->getValue();
			size_t vecArgSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecArgTy->getSize())->getValue();
			assert(vecTrgSize >= vecArgSize && "Conversion not possible");

			// TODO report it as an error ? 
			assert(false && "Casting between two different vector types not yet implemented!");
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
			convertExprTo(builder, builder.vectorType(gen.getChar(), 
				core::ConcreteIntTypeParam::get(
					builder.getNodeManager(), 
					core::static_pointer_cast<const core::Literal>(expr)->getValue().length()-1) ), expr );

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
		const core::TypePtr& subTy = core::static_pointer_cast<const core::ArrayType>(trgTy)->getElementType();
		core::ConcreteIntTypeParamPtr&& size = core::ConcreteIntTypeParam::get(builder.getNodeManager(), 1); 
		core::ExpressionPtr vecExpr = builder.callExpr( 
				builder.vectorType(subTy, size), // vec<subTy,1>
				gen.getVectorInitUniform(), 
				toVector( convertExprTo(builder, subTy, expr), gen.getIntTypeParamLiteral(size) )
			);
		return builder.callExpr( trgTy, gen.getVectorToArray(), toVector(vecExpr) );
	}

	// [ ref<'a> -> ref<array<'a>> ]
	//
	// Use the scalarToArray literal to perform this kind of conversion
	if ( trgTy->getNodeType() == core::NT_RefType ) {
		assert( argTy->getNodeType() == core::NT_RefType );
		const core::TypePtr& subTrgTy = core::analysis::getReferencedType(trgTy);
//		const core::TypePtr& argSubTy = core::analysis::getReferencedType(argTy);
		if ( subTrgTy->getNodeType() == core::NT_ArrayType ) {
//			const core::ArrayTypePtr& arrTy = core::static_pointer_cast<const core::ArrayType>( subTrgTy );
			core::ExpressionPtr subExpr = expr;
//			if ( *arrTy->getElementType() != *argSubTy ) {
//				subExpr = convertExprTo(builder, arrTy->getElementType(), expr );
//			}
			return convertRefScalarToRefArray(builder, subExpr);
		}
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

namespace {

struct CallExprVisitor: public clang::StmtVisitor<CallExprVisitor> {

	clang::idx::Indexer& indexer;
	typedef std::set<const clang::FunctionDecl*> CallGraph;
	CallGraph callGraph;

	CallExprVisitor (clang::idx::Indexer& indexer): indexer(indexer) { }

	CallGraph getCallGraph (const clang::FunctionDecl* func) {
		assert(func->hasBody() && "Function in the dependency graph has no body");

		Visit(func->getBody());
		return callGraph;
	}

	void addFunctionDecl(FunctionDecl* funcDecl) {
		const clang::FunctionDecl* def = NULL;
		/*
		 * this will find function definitions if they are declared in  the same translation unit
		 * (also defined as static)
		 */
		if ( !funcDecl->hasBody(def) ) {
			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already
			 * loaded use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity = clang::idx::Entity::get( funcDecl, indexer.getProgram() );
			conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
			if ( ret.first ) { def = ret.first;	}
		}

		if ( def ) { callGraph.insert(def); }
	}

	void VisitCallExpr (clang::CallExpr* callExpr) {
		if ( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee()) ) {
			addFunctionDecl(funcDecl);
		}
		VisitStmt(callExpr);
	}

	void VisitDeclRefExpr(DeclRefExpr* expr) {
		// if this variable is used to invoke a function (therefore is a
		// function pointer) and it has been defined here, we add a potentially
		// dependency to the current definition 
		if ( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(expr->getDecl()) ) {
			addFunctionDecl(funcDecl);
		}
	}

	void VisitStmt (clang::Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
			[ this ](clang::Stmt* curr) { if(curr) this->Visit(curr); });
	}
};

} // end anonymous namespace 

/**
 * In order for DepGraph to build the dependency graph for functions the clang indexer is needed,
 * FunctionDependencyGraph adds the indexer to member functions of DependencyGraph
 */
class FunctionDepenencyGraph : public DependencyGraph<const clang::FunctionDecl*> {
	clang::idx::Indexer& idx;
public:
	FunctionDepenencyGraph(clang::idx::Indexer& idx) : DependencyGraph<const clang::FunctionDecl*>(), idx(idx) { }
	clang::idx::Indexer& getIndexer() const { return idx; }
};

template <>
void DependencyGraph<const clang::FunctionDecl*>::Handle(const clang::FunctionDecl* func,
							const DependencyGraph<const clang::FunctionDecl*>::VertexTy& v) {
	// This is potentially dangerous
	FunctionDepenencyGraph& funcDepGraph = static_cast<FunctionDepenencyGraph&>(*this);

	CallExprVisitor callExprVis(funcDepGraph.getIndexer());
	CallExprVisitor::CallGraph&& graph = callExprVis.getCallGraph(func);

	std::for_each(graph.begin(), graph.end(),
			[ this, v ](const clang::FunctionDecl* currFunc) { assert(currFunc); this->addNode(currFunc, &v); }
	);
}

} // end namespace utils

namespace conversion {

#define START_LOG_EXPR_CONVERSION(expr) \
	assert(convFact.currTU && "Translation unit not correctly set"); \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" <<	\
				utils::location(expr->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	VLOG(1) << "Converted into IR expression: "; \
	VLOG(1) << "\t" << *expr << " type:( " << *expr->getType() << " )";


//---------------------------------------------------------------------------------------------------------------------
//										CLANG EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::ClangExprConverter: public StmtVisitor<ClangExprConverter, core::ExpressionPtr> {
	ConversionFactory& convFact;
	ConversionContext& ctx;

	core::ExpressionPtr wrapVariable(clang::Expr* expr) {
		const DeclRefExpr* ref = utils::skipSugar<const DeclRefExpr>(expr);
		if ( ref && isa<const ParmVarDecl>(ref->getDecl()) ) {
			const core::VariablePtr& parmVar =
				core::static_pointer_cast<const core::Variable>( convFact.convertExpr(ref) );

			auto fit = ctx.wrapRefMap.find(parmVar);
			if ( fit == ctx.wrapRefMap.end() ) {
				fit = ctx.wrapRefMap.insert(
					std::make_pair( parmVar, convFact.builder.variable(convFact.builder.refType(parmVar->getType())) )
				).first;
			}
			return fit->second;
		}
		return convFact.convertExpr(expr);
	}


	core::ExpressionPtr asLValue(const core::ExpressionPtr& value) {
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = convFact.mgr.basic;

		// this only works for call-expressions
		if (value->getNodeType() != core::NT_CallExpr || value->getType()->getNodeType() == core::NT_RefType) {
			return value;
		}

		// extract the call
		const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(value);

		// check final state - deref has been encountered => drop
		if (core::analysis::isCallOf(call, gen.getRefDeref())) {
			return call->getArgument(0);
		}

		// check whether it is a array-subscript instruction and argument has been de-refernced
		if (core::analysis::isCallOf(value, gen.getArraySubscript1D())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getArrayRefElem1D(), inner, call->getArgument(1));
			}
		}

		// check whether it is a vector-subscript instruction and argument has been de-refernced
		if (core::analysis::isCallOf(value, gen.getVectorSubscript())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getVectorRefElem(), inner, call->getArgument(1));
			}
		}

		// check whether it is a struct element access
		if (core::analysis::isCallOf(value, gen.getCompositeMemberAccess())) {
			const core::ExpressionPtr arg = call->getArgument(0);
			const core::ExpressionPtr inner = asLValue(arg);
			if (*inner != *arg) {
				return builder.callExpr(builder.refType(value->getType()), gen.getCompositeRefElem(), inner, call->getArgument(1), call->getArgument(2));
			}
		}

		// there is nothing to do
		return value;
	}

	core::ExpressionPtr asRValue(const core::ExpressionPtr& value) {

		// check whether value is parameter to the current function
		if (value->getNodeType() == core::NT_Variable) {
			core::VariablePtr var = static_pointer_cast<const core::Variable>(value);
			if (ctx.curParameter && contains(*ctx.curParameter, var)) {
				// => parameters are always r-values
				return var;
			}
		}

		// adds a deref to expression in case expression is of a ref type
		if (core::analysis::isRefType(value->getType())) {
			return convFact.builder.deref(value);
		}
		return value;
	}



public:

	// CallGraph for functions, used to resolved eventual recursive functions
	utils::FunctionDepenencyGraph funcDepGraph;

	ClangExprConverter(ConversionFactory& convFact, Program& program): convFact(convFact), ctx(convFact.ctx),
			funcDepGraph(program.getClangIndexer()) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		START_LOG_EXPR_CONVERSION(intLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), intLit->getExprLoc() ),
				convFact.convertType( GET_TYPE_PTR(intLit) )
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FLOATING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
		START_LOG_EXPR_CONVERSION(floatLit);
		core::ExpressionPtr&& retExpr =
			// retrieve the string representation from the source code
			convFact.builder.literal(
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), floatLit->getExprLoc()),
				convFact.convertType( GET_TYPE_PTR(floatLit) )
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCharacterLiteral(CharacterLiteral* charLit) {
		START_LOG_EXPR_CONVERSION(charLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), charLit->getExprLoc()),
					(charLit->isWide() ? convFact.mgr.basic.getWChar() : convFact.mgr.basic.getChar())
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitStringLiteral(clang::StringLiteral* stringLit) {
		START_LOG_EXPR_CONVERSION(stringLit);
		std::string&& strValue = GetStringFromStream(
				convFact.currTU->getCompiler().getSourceManager(), stringLit->getExprLoc()
			);
		core::ExpressionPtr&& retExpr =	convFact.builder.literal( strValue,	convFact.mgr.basic.getString() ); 
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr* boolLit) {
		START_LOG_EXPR_CONVERSION(boolLit);
		core::ExpressionPtr&& retExpr =
			// retrieve the string representation from the source code
			convFact.builder.literal(
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(),
						boolLit->getExprLoc()), convFact.mgr.basic.getBool()
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitParenExpr(clang::ParenExpr* parExpr) {
		core::ExpressionPtr&& retExpr = Visit( parExpr->getSubExpr() );
		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, parExpr, convFact);
		return annotatedNode;
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
	core::ExpressionPtr VisitGNUNullExpr(clang::GNUNullExpr* nullExpr) {
		const core::lang::BasicGenerator& gen = convFact.mgr.basic;
		core::TypePtr&& type = convFact.convertType(GET_TYPE_PTR(nullExpr));
		assert(type->getNodeType() != core::NT_ArrayType && "C pointer type must of type array<'a,1>");
	    return convFact.builder.callExpr( gen.getGetNull(), gen.getTypeLiteral( type ) );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						  IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);

		core::ExpressionPtr retExpr = Visit(castExpr->getSubExpr());

		// handle implicit casts according to their kind
		switch(castExpr->getCastKind()) {
		case CK_LValueToRValue: retExpr = asRValue(retExpr); break;
		default : {
			// use default cast expr handling (fallback)
			retExpr = VisitCastExpr(castExpr);
		}
		}

		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCastExpr(clang::CastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);
		
		const core::lang::BasicGenerator& gen = convFact.mgr.getBasicGenerator();
		const core::TypePtr& type = convFact.convertType( GET_TYPE_PTR(castExpr) );
		core::ExpressionPtr&& subExpr = Visit(castExpr->getSubExpr());

		core::ExpressionPtr&& nonRefExpr = convFact.tryDeref(subExpr);

		// if the cast is to a 'void*' type then we simply skip it
		if( gen.isAnyRef(type) ) { return subExpr; }

		if ( ( type->getNodeType() == core::NT_RefType ) &&
				(*subExpr == *convFact.builder.literal(subExpr->getType(),"0")) ) 
		{
			const core::TypePtr& subType = core::static_pointer_cast<const core::RefType>(type)->getElementType();
			return convFact.builder.callExpr(gen.getGetNull(), gen.getTypeLiteral(subType));
		}

		// Mallocs/Allocs are replaced with ref.new expression
		if(core::ExpressionPtr&& retExpr = handleMemAlloc(convFact.getASTBuilder(), type, subExpr))
			return retExpr;
	
		// If the subexpression is a string, remove the implicit casts
		if ( convFact.mgr.basic.isString(subExpr->getType()) ) {
			return subExpr;
		}

		const core::TypePtr& nonRefType = nonRefExpr->getType();
		// if the subexpression is an array or a vector, remove all the C implicit casts
		if ( nonRefType->getNodeType() == core::NT_ArrayType || nonRefType->getNodeType() == core::NT_VectorType || 
				nonRefType->getNodeType() == core::NT_FunctionType ) 
		{
			return subExpr;
		}

		// handle truncation of floating point numbers
		const core::TypePtr& subExprType = subExpr->getType();
		if (subExprType->getNodeType() == core::NT_RefType) {
			const core::RefTypePtr& sourceType = static_pointer_cast<const core::RefType>(subExprType);

			// check whether it is a truncation
			if (gen.isReal(sourceType->getElementType()) && gen.isSignedInt(type)) {
				const core::GenericTypePtr& intType = static_pointer_cast<const core::GenericType>(type);
				return convFact.builder.callExpr(type, 
						gen.getRealToInt(), nonRefExpr, 
						gen.getIntTypeParamLiteral(intType->getIntTypeParameter()[0])
					);
			}

			return subExpr;  // do not treat ref types
		}

		// LOG(DEBUG) << *subExpr << " -> " << *type;
		// Convert casts form scalars to vectors to vector init exrpessions
		subExpr = convFact.castToType(type, subExpr);
		
		END_LOG_EXPR_CONVERSION(subExpr);
		return subExpr;
	}

private:
	ExpressionList getFunctionArguments(const core::ASTBuilder& builder, 
			clang::CallExpr* callExpr, const core::FunctionTypePtr& funcTy) 
	{
		ExpressionList args;
		for ( size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId ) {
			core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
			// core::TypePtr&& argTy = arg->getType();
			if ( argId < funcTy->getParameterTypes().size() ) {
				const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
				arg = convFact.castToType(funcArgTy, arg);
			} else {
				arg = convFact.castToType(builder.getNodeManager().basic.getVarList(), arg);
			}
			args.push_back( arg );
		}
		return args;
	}

	ExpressionList getFunctionArguments(const core::ASTBuilder& builder,
			clang::CXXConstructExpr* callExpr, const core::FunctionTypePtr& funcTy)
	{
		ExpressionList args;
		for ( size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId ) {
			core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
			// core::TypePtr&& argTy = arg->getType();
			if ( argId < funcTy->getParameterTypes().size() ) {
				const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
				arg = convFact.castToType(funcArgTy, arg);
			} else {
				arg = convFact.castToType(builder.getNodeManager().basic.getVarList(), arg);
			}
			args.push_back( arg );
		}
		return args;
	}

public:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		const core::ASTBuilder& builder = convFact.builder;

		// return converted node
		core::ExpressionPtr irNode;

		if ( callExpr->getDirectCallee() ) {

			FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee());

			core::FunctionTypePtr funcTy =
				core::static_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(funcDecl) ) );

			// collects the type of each argument of the expression
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
			
			assert( convFact.currTU && "Translation unit not set.");

			const TranslationUnit* oldTU = convFact.currTU;
			const FunctionDecl* definition = NULL;
			/*
			 * this will find function definitions if they are declared in  the same translation unit
			 * (also defined as static)
			 */
			if( !funcDecl->hasBody(definition) ) {
				/*
				 * if the function is not defined in this translation unit, maybe it is defined in another we already
				 * loaded use the clang indexer to lookup the definition for this function declarations
				 */
				FunctionDecl* fd = funcDecl;
				const clang::idx::TranslationUnit* clangTU = convFact.getTranslationUnitForDefinition(fd);

				if ( clangTU ) { 
					convFact.currTU = &Program::getTranslationUnit(clangTU); 
				}
				
				if ( clangTU && fd->hasBody() ) { 
					definition = fd; 
				}
			}

			if ( !definition ) {
				//-----------------------------------------------------------------------------------------------------
				//     						Handle of 'special' built-in functions
				//-----------------------------------------------------------------------------------------------------
				// free(): check whether this is a call to the free() function
				if ( funcDecl->getNameAsString() == "free" && callExpr->getNumArgs() == 1 ) {
					// in the case the free uses an input parameter
					if ( args.front()->getType()->getNodeType() == core::NT_RefType ) {
						return builder.callExpr( builder.getBasicGenerator().getRefDelete(), args.front() );
					}
					// otherwise this is not a L-Value so it needs to be wrapped into a variable
					return builder.callExpr( builder.getBasicGenerator().getRefDelete(),
							wrapVariable(callExpr->getArg(0))
						);
				}
			}

			ExpressionList&& packedArgs = tryPack(convFact.builder, funcTy, args);

			if ( !definition ) {
				// No definition has been found in any of the translation units, we mark this function as extern!
				core::ExpressionPtr&& irNode = convFact.builder.callExpr(
						funcTy->getReturnType(), builder.literal(funcDecl->getNameAsString(), funcTy), packedArgs
					);
				// handle eventual pragmas attached to the Clang node
				core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);
				convFact.currTU = oldTU;
				return annotatedNode;
			}

			/*
			 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
			 * list needs to be initialized with the value of global variable in the current scope
			 */
			if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
				/*
				 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
				 * current context
				 */
				assert(ctx.globalVar && "No global definitions forwarded to this point");
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}

			/*
			 * If we are resolving the body of a recursive function we have to return the associated variable every
			 * time a function in the strongly connected graph of function calls is encountered.
			 */
			if ( ctx.isResolvingRecFuncBody ) {
				// check if this type has a typevar already associated, in such case return it
				ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
				if( fit != ctx.recVarExprMap.end() ) {
					/*
					 * we are resolving a parent recursive type, so when one of the recursive functions in the
					 * connected components are called, the introduced mu variable has to be used instead.
					 */
					convFact.currTU = oldTU;
					return builder.callExpr(
							funcTy->getReturnType(), static_cast<core::ExpressionPtr>(fit->second), packedArgs
						);
				}
			}

			if ( !ctx.isResolvingRecFuncBody ) {
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);
				if ( fit != ctx.lambdaExprCache.end() ) {
					convFact.currTU = oldTU;
					core::ExpressionPtr&& irNode =
							builder.callExpr(funcTy->getReturnType(),
									static_cast<core::ExpressionPtr>(fit->second), packedArgs
								);
					// handle eventual pragmas attached to the Clang node
					core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);

					convFact.currTU = oldTU;
					return annotatedNode;
				}
			}

			assert(definition && "No definition found for function");
			core::ExpressionPtr lambdaExpr =
					core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(definition) );

			convFact.currTU = oldTU;
			irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);

		} else if ( callExpr->getCallee() ) {
			core::ExpressionPtr funcPtr = convFact.tryDeref( Visit( callExpr->getCallee() ) );
			core::TypePtr subTy = funcPtr->getType();
			if ( subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
				subTy = core::static_pointer_cast<const core::SingleElementType>( subTy )->getElementType();
				funcPtr = builder.callExpr( subTy, builder.getBasicGenerator().getArraySubscript1D(), funcPtr, builder.uintLit(0) );
			}
			assert(subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");
			const core::FunctionTypePtr& funcTy = core::static_pointer_cast<const core::FunctionType>(subTy);
			ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
			irNode = builder.callExpr( funcPtr, args );

		} else {
			assert(false && "Call expression not referring a function");
		}
		assert(irNode && "CallExpr has not been correctly converted into an IR Expression.");
		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitPredefinedExpr(clang::PredefinedExpr* preExpr) {
		const core::lang::BasicGenerator& gen = convFact.mgr.basic;
	    return convFact.builder.callExpr(gen.getGetNull(), 
				gen.getTypeLiteral( convFact.convertType(GET_TYPE_PTR(preExpr)) )
			);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						SIZEOF ALIGNOF EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitSizeOfAlignOfExpr(clang::SizeOfAlignOfExpr* expr) {
		START_LOG_EXPR_CONVERSION(expr);
		if ( expr->isSizeOf() ) {
			core::TypePtr&& type = expr->isArgumentType() ?
				convFact.convertType( expr->getArgumentType().getTypePtr() ) :
				convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
			return getSizeOfType(convFact.getASTBuilder(), type);
		}
		assert(false && "SizeOfAlignOfExpr not yet supported");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
		//todo: CXX extensions
		core::ExpressionPtr retExpr;
		const core::ASTBuilder& builder = convFact.builder;
		core::ExpressionPtr funcPtr = convFact.tryDeref( Visit( callExpr->getCallee() ) );

		const Expr * callee = callExpr->getCallee()->IgnoreParens();
		const MemberExpr * memberExpr = cast<MemberExpr>(callee);
		const CXXMethodDecl * methodDecl = cast<CXXMethodDecl>(memberExpr->getMemberDecl());

		if (methodDecl->isStatic()) {
			// static method
		}



		core::TypePtr subTy = funcPtr->getType();
		if ( subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
			subTy = core::static_pointer_cast<const core::SingleElementType>( subTy )->getElementType();
			funcPtr = builder.callExpr( subTy, builder.getBasicGenerator().getArraySubscript1D(), funcPtr, builder.uintLit(0) );
		}
		assert(subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");
		const core::FunctionTypePtr& funcTy = core::static_pointer_cast<const core::FunctionType>(subTy);
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

		//currentThisPtr.ptr->printTo(std::cerr);
		//args.push_back(currentThisPtr);

		retExpr = builder.callExpr( funcPtr, args );

		return retExpr;

		////clang::Expr * callObject = callExpr->getImplicitObjectArgument();

		assert(false && "CXXMemberCallExpr not yet handled");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX OPERATOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXOperatorCallExpr(clang::CXXOperatorCallExpr* callExpr) {
		//todo: CXX extensions
		// call to an overloaded operator
		assert(false && "CXXOperatorCallExpr not yet handled");
	}

private:
	void dumpDecl(clang::Decl * decl) {
		std::cout << "********\n***\n*********\n";
		decl->dump();
	}

public:
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX CONSTRUCTOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXConstructExpr(clang::CXXConstructExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();

		CXXConstructorDecl* constructorDecl = dyn_cast<CXXConstructorDecl>(callExpr->getConstructor());
		assert(constructorDecl);
		dumpDecl(constructorDecl);


		FunctionDecl* funcDecl = constructorDecl;
		core::FunctionTypePtr funcTy =
			core::static_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(funcDecl) ) );
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);


		core::ExpressionPtr retExpr;
		retExpr = core::static_pointer_cast<const core::Expression>( convFact.convertFunctionDecl(funcDecl) );


		////clang::Stmt* Body = constructorDecl->getBody();
		////const FunctionProtoType *FnType = callExpr->getType()->getAs<FunctionProtoType>();

		// get class declaration
		CXXRecordDecl * callingClass = constructorDecl->getParent();
		assert(callingClass);
		callingClass->viewInheritance(callingClass->getASTContext());

		core::IdentifierPtr ident = builder.identifier(constructorDecl->getNameAsString());
		core::ExpressionPtr op = gen.getCompositeMemberAccess();

		////Expr** functionArgs = callExpr->getArgs();
		////unsigned numArgs = callExpr->getNumArgs();

		return retExpr;

		//assert(false && "VisitCXXConstructExpr not yet handled");
		//return NULL;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX NEW CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXNewExpr(clang::CXXNewExpr* callExpr) {
		assert(false && "VisitCXXNewExpr not yet handled");
		//return NULL;
	}
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX DELETE CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXDeleteExpr(clang::CXXDeleteExpr* callExpr) {
		assert(false && "VisitCXXDeleteExpr not yet handled");
		//return NULL;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX THIS CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThisExpr(clang::CXXThisExpr* callExpr) {
		clang::SourceLocation&& source = callExpr->getLocation();
		//source.dump(convFact.currTU->getCompiler().getSourceManager());

		std::cerr << "CXXThisExpr: \n";
		callExpr->dump();

		std::cerr << "***************Function graph\n";
		convFact.exprConv->funcDepGraph.print( std::cerr );
		std::cerr << "*****\n";
		std::cerr << convFact.ctx.thisStack;
		std::cerr << "*****\n";

		return convFact.ctx.thisStack;
		//assert(false && "VisitCXXThisExpr not yet handled");
		//return NULL;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					EXCEPTION CXX THROW EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXThrowExpr(clang::CXXThrowExpr* throwExpr) {
		assert(false && "VisitCXXThrowExpr not yet handled");
		//return NULL;
	}


	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							MEMBER EXPRESSION
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitMemberExpr(clang::MemberExpr* membExpr)  {
		START_LOG_EXPR_CONVERSION(membExpr);
		const core::ASTBuilder& builder = convFact.builder;

		core::ExpressionPtr&& base = Visit(membExpr->getBase());

		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
		if(membExpr->isArrow()) {
			/*
			 * we have to check whether we currently have a ref or probably an array (which is used to represent
			 * C pointers)
			 */
			assert( base->getType()->getNodeType() == core::NT_RefType);
			base = getCArrayElemRef(builder, base);
		}

		core::IdentifierPtr ident = builder.identifier(membExpr->getMemberDecl()->getNameAsString());
		core::ExpressionPtr retExpr;

		core::ExpressionPtr op = gen.getCompositeMemberAccess();
		core::TypePtr structTy = base->getType();

		if (structTy->getNodeType() == core::NT_RefType) {
			// skip over reference wrapper
			structTy = core::analysis::getReferencedType(structTy);
			op = gen.getCompositeRefElem();
		}


		// There are 2 basic cases which need to be handled: Struct/Unions and Recursive Types
		assert((structTy->getNodeType() == core::NT_StructType || structTy->getNodeType() == core::NT_UnionType  ||
				structTy->getNodeType() == core::NT_RecType) &&
				"Using a member access operation on a non struct/union type"
			);

		// if the inner type is a RecType then we need to unroll it to get the contained composite type
		if ( structTy->getNodeType() == core::NT_RecType ) {
			structTy = core::static_pointer_cast<const core::RecType>(structTy)->unroll(convFact.mgr);
		}

		// derive type of accessed member
		const core::TypePtr& memberTy =
				core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

		// derive result type (type of accessed member)
		core::TypePtr resType = memberTy;
		if (base->getType()->getNodeType() == core::NT_RefType) {
			resType = builder.refType(resType);
		}

		// cache "this"
		convFact.ctx.thisStack = base;

		// build member access expression
		retExpr = builder.callExpr(resType, op, base, gen.getIdentifierLiteral(ident), gen.getTypeLiteral(memberTy));

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, membExpr, convFact);

		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitBinaryOperator(clang::BinaryOperator* binOp)  {
		START_LOG_EXPR_CONVERSION(binOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();

 		core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
		core::ExpressionPtr&& lhs = Visit(binOp->getLHS());

		/*
		 * if the binary operator is a comma separated expression, we convert it into a function call which returns the
		 * value of the last expression
		 */
		if ( binOp->getOpcode() == BO_Comma ) {

			core::CompoundStmtPtr&& body = builder.compoundStmt(toVector<core::StatementPtr>(lhs,
					(gen.isUnit(rhs->getType()) ? static_cast<core::StatementPtr>(rhs) : builder.returnStmt(rhs)) )
				);
			return builder.createCallExprFromBody(body, rhs->getType());
		}

		// the type of this expression is the type of the LHS expression
		core::TypePtr exprTy = lhs->getType()->getNodeType() == core::NT_RefType ?
				core::static_pointer_cast<const core::RefType>(lhs->getType())->getElementType() : lhs->getType();

		// get basic element type
        core::ExpressionPtr&& subExprLHS = convFact.tryDeref(lhs);

        /*
         * we take care of compound operators first, we rewrite the RHS expression in a normal form, i.e.:
         * 		->		a op= b  ---->  a = a op b
         */
		clang::BinaryOperatorKind baseOp;
		core::lang::BasicGenerator::Operator op;
		bool isCompound = true;

		switch ( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: 	op = core::lang::BasicGenerator::Mul; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: 	op = core::lang::BasicGenerator::Div; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign:	op = core::lang::BasicGenerator::Mod; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: 	op = core::lang::BasicGenerator::Add; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign:	op = core::lang::BasicGenerator::Sub; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: 	op = core::lang::BasicGenerator::LShift; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: 	op = core::lang::BasicGenerator::RShift; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: 	op = core::lang::BasicGenerator::And; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: 	op = core::lang::BasicGenerator::Or; baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: 	op = core::lang::BasicGenerator::Xor; baseOp = BO_Xor; break;
		default:
			isCompound = false;
		}

		if ( isCompound ) {
			// we check if the RHS is a ref, in that case we use the deref operator
			rhs = convFact.tryDeref(rhs);
			core::ExpressionPtr&& opFunc = gen.getOperator(exprTy, op);
			rhs = builder.callExpr(exprTy, opFunc, subExprLHS, rhs);
		}

		bool isAssignment = false;
		bool isLogical = false;

		baseOp = binOp->getOpcode();

		core::ExpressionPtr opFunc;
		switch ( binOp->getOpcode() ) {
		case BO_PtrMemD:
		case BO_PtrMemI:
			assert(false && "Operator not yet supported!");

		// a * b
		case BO_Mul: 	op = core::lang::BasicGenerator::Mul;  break;
		// a / b
		case BO_Div: 	op = core::lang::BasicGenerator::Div;  break;
		// a % b
		case BO_Rem: 	op = core::lang::BasicGenerator::Mod;  break;
		// a + b
		case BO_Add: 	op = core::lang::BasicGenerator::Add;  break;
		// a - b
		case BO_Sub: 	op = core::lang::BasicGenerator::Sub;  break;
		// a << b
		case BO_Shl: 	op = core::lang::BasicGenerator::LShift;  break;
		// a >> b
		case BO_Shr: 	op = core::lang::BasicGenerator::RShift;  break;
		// a & b
		case BO_And: 	op = core::lang::BasicGenerator::And;  break;
		// a ^ b
		case BO_Xor: 	op = core::lang::BasicGenerator::Xor;  break;
		// a | b
		case BO_Or:  	op = core::lang::BasicGenerator::Or; 	 break;

		// Logic operators

		// a && b
		case BO_LAnd: 	op = core::lang::BasicGenerator::LAnd; isLogical=true; break;
		// a || b
		case BO_LOr:  	op = core::lang::BasicGenerator::LOr;  isLogical=true; break;
		// a < b
		case BO_LT:	 	op = core::lang::BasicGenerator::Lt;   isLogical=true; break;
		// a > b
		case BO_GT:  	op = core::lang::BasicGenerator::Gt;   isLogical=true; break;
		// a <= b
		case BO_LE:  	op = core::lang::BasicGenerator::Le;   isLogical=true; break;
		// a >= b
		case BO_GE:  	op = core::lang::BasicGenerator::Ge;   isLogical=true; break;
		// a == b
		case BO_EQ:  	op = core::lang::BasicGenerator::Eq;   isLogical=true; break;
		// a != b
		case BO_NE:	 	op = core::lang::BasicGenerator::Ne;   isLogical=true; break;

		case BO_MulAssign: case BO_DivAssign: case BO_RemAssign: case BO_AddAssign: case BO_SubAssign:
		case BO_ShlAssign: case BO_ShrAssign: case BO_AndAssign: case BO_XorAssign: case BO_OrAssign:
		case BO_Assign:
		{
			baseOp = BO_Assign;
			/*
			 * poor C codes assign value to function parameters, this is not allowed here as input parameters are of
			 * non REF type. What we need to do is introduce a declaration for these variables and use the created
			 * variable on the stack instead of the input parameters
			 */
			lhs = wrapVariable(binOp->getLHS());

			// make sure the lhs is a L-Value
			lhs = asLValue(lhs);

			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( lhs->getType()->getNodeType() == core::NT_RefType && "LHS operand must be of type ref<'a>.");

			rhs = convFact.castToType(core::static_pointer_cast<const core::RefType>(lhs->getType())->getElementType(), rhs);	
			isAssignment = true;
			opFunc = gen.getRefAssign();
			exprTy = gen.getUnit();
			break;
		}
		default:
			assert(false && "Operator not supported");
		}
		
		// Operators && and || introduce short circuit operations, this has to be directly supported in the IR.
		if ( baseOp == BO_LAnd || baseOp == BO_LOr ) {
			lhs = convFact.castToType(gen.getBool(), lhs);
			rhs = convFact.castToType(gen.getBool(), rhs);
			// lazy evaluation of RHS
			exprTy = gen.getBool();
			rhs = builder.createCallExprFromBody(builder.returnStmt(rhs), gen.getBool(), true);
		}

		if( !isAssignment ) {

			core::TypePtr&& lhsTy = lhs->getType();
			core::TypePtr&& rhsTy = rhs->getType();
			VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
				      " RHS(" << *rhs << "[" << *rhs->getType() << "])";

			if ( lhsTy->getNodeType() != core::NT_RefType || rhsTy->getNodeType() != core::NT_RefType) {

				// ----------------------------- Hack begin --------------------------------
				// TODO: this is a quick solution => maybe clang allows you to determine the actual type
				// => otherwise the sub-type checks within the core may be used
				//
				// Bug fixed by this:
				//		when multiplying an integer with a double, the double is casted to an integer and the
				//		results is an integer.
				//

				// check whether result type needs to be adjusted
				if (*lhsTy != *rhsTy) {
					// if second argument is a real
					if (!gen.isReal(lhsTy) && gen.isReal(rhsTy)) {
						exprTy = rhsTy;
					}
				}

				// ----------------------------- Hack end --------------------------------
				lhs = convFact.castToType(exprTy, lhs);
				rhs = convFact.castToType(exprTy, rhs);
				// Handle pointers arithmetic
				VLOG(2) << "Lookup for operation: " << op << ", for type: " << *exprTy;
				opFunc = gen.getOperator(exprTy, op);
			} else {
				assert(lhsTy->getNodeType() == core::NT_RefType
						&& rhsTy->getNodeType() == core::NT_RefType && "Comparing pointers");

				core::ExpressionPtr retExpr = convFact.builder.callExpr( gen.getBool(), gen.getPtrEq(), lhs, rhs );
				if ( baseOp == BO_NE ) {
					// comparing two refs
					retExpr = convFact.builder.callExpr( gen.getBool(), gen.getBoolLNot(), retExpr );
				} 
				
				// handle eventual pragmas attached to the Clang node
				core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, binOp, convFact);
				return annotatedNode;
			}

			if ( DeclRefExpr* declRefExpr = utils::skipSugar<DeclRefExpr>(binOp->getLHS()) ) {
				if ( isa<ArrayType>(declRefExpr->getDecl()->getType().getTypePtr()) )
					assert(false && "Pointer arithmetic not yet supported");
			}

			if(isLogical) { exprTy = gen.getBool(); }

		} else {
		    // check if there is a kernelFile annotation
		    ocl::attatchOclAnnotation(rhs, binOp, convFact);
		}
		assert(opFunc);
		// add source code annotation to the rhs if present
		VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc << 
			      " RHS(" << *rhs << "[" << *rhs->getType() << "])";
        core::ExpressionPtr&& retExpr = convFact.builder.callExpr( exprTy, opFunc, lhs, rhs );

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, binOp, convFact);

		END_LOG_EXPR_CONVERSION( retExpr );
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitUnaryOperator(clang::UnaryOperator *unOp) {
		START_LOG_EXPR_CONVERSION(unOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
		core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

		// build lambda expression for post/pre increment/decrement unary operators
		auto encloseIncrementOperator =
			[ this, &builder, &gen ]
			(core::ExpressionPtr subExpr, core::lang::BasicGenerator::Operator op) -> core::ExpressionPtr {
				core::TypePtr type = subExpr->getType();
				assert( type->getNodeType() == core::NT_RefType &&
						"Illegal increment/decrement operand - not a ref type" );
				core::TypePtr elementType = core::static_pointer_cast<const core::RefType>(type)->getElementType();

				core::TypePtr genType;
				if ( gen.isSignedInt(elementType) ) {
					genType = gen.getIntGen();
				} else if ( gen.isUnsignedInt(elementType) ) {
					genType = gen.getUIntGen();
				} else {
					assert(false && "Illegal operand type for increment/decrement operator.");
				}
				return convFact.builder.callExpr(elementType, convFact.mgr.basic.getOperator(genType, op), subExpr);
			};

		switch ( unOp->getOpcode() ) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (__tmp = a, a=a+1, __tmp)
		// ++a ==> ( a=a+1, a)
		// --a
		case UO_PreDec:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreDec);
			break;
		// a--
		case UO_PostDec:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostDec);
			break;
		// a++
		case UO_PreInc:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreInc);
			break;
		// ++a
		case UO_PostInc:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostInc);
			break;
		// &a
		case UO_AddrOf:
		{
			/*
			 * We need to be careful paramvars are not dereferenced and the address passed around. If this happens
			 * we have to declare a variable holding the memory location for that value and replace every use of
			 * the paramvar with the newly generated variable: the structure needRef in the ctx is used for this
			 */
			subExpr = wrapVariable(unOp->getSubExpr());

			// in the case we are getting the address of a function the & operator 
			// has no effects, therefore we return
			if (subExpr->getType()->getNodeType() == core::NT_FunctionType) {
				break;
			}

			// make sure it is a L-Value
			subExpr = asLValue(subExpr);

			assert(subExpr->getType()->getNodeType() == core::NT_RefType);
			subExpr = convertRefScalarToRefArray(builder, subExpr);
			break;
		}
		// *a
		case UO_Deref: {
			// make sure it is a L-Value
			subExpr = asLValue(subExpr);

			assert(subExpr->getType()->getNodeType() == core::NT_RefType &&
					"Impossible to apply * operator to an R-Value");
			
			const core::TypePtr& subTy =
					core::static_pointer_cast<const core::RefType>(subExpr->getType())->getElementType();
			if ( subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
				subExpr = getCArrayElemRef(builder, subExpr);
			} else {
				subExpr = convFact.tryDeref(subExpr);
			}
			break;
		}
		// +a
		case UO_Plus:
			// just return the subexpression
			break;
		// -a
		case UO_Minus:
			subExpr = builder.invertSign( convFact.tryDeref(subExpr) );
			break;
		// ~a
		case UO_Not:
			subExpr = convFact.tryDeref(subExpr);
			subExpr = builder.callExpr(
					subExpr->getType(), gen.getOperator(subExpr->getType(), core::lang::BasicGenerator::Not), subExpr
				);
			break;
		// !a
		case UO_LNot:
			if( !gen.isBool(subExpr->getType()) ) {
				subExpr = convFact.castToType(gen.getBool(), subExpr);
			}
			assert( gen.isBool(subExpr->getType()) );

			subExpr = builder.callExpr( subExpr->getType(), gen.getBoolLNot(), subExpr );
			break;
		case UO_Real:
		case UO_Imag:
		case UO_Extension: //TODO:
		default:
			assert(false && "Unary operator not supported");
		}

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(subExpr, unOp, convFact);

		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp) {
		START_LOG_EXPR_CONVERSION(condOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();

		core::TypePtr&& retTy = convFact.convertType( GET_TYPE_PTR(condOp) );
		core::ExpressionPtr&& trueExpr = Visit(condOp->getTrueExpr());
		core::ExpressionPtr&& falseExpr = Visit(condOp->getFalseExpr());
		core::ExpressionPtr&& condExpr = Visit( condOp->getCond() );

		condExpr = convFact.castToType(gen.getBool(), condExpr);

		// Dereference eventual references
		if ( retTy->getNodeType() == core::NT_RefType ) {
			retTy= core::static_pointer_cast<const core::RefType>(retTy)->getElementType();
		}

		core::ExpressionPtr&& retExpr = builder.callExpr(retTy, gen.getIfThenElse(),
				condExpr,	// Condition
				builder.createCallExprFromBody( builder.returnStmt(convFact.castToType(retTy, trueExpr)), retTy, true ), // True
				builder.createCallExprFromBody( builder.returnStmt(convFact.castToType(retTy, falseExpr)), retTy, true ) // False
		);

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, condOp, convFact);

		END_LOG_EXPR_CONVERSION(retExpr);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
		START_LOG_EXPR_CONVERSION(arraySubExpr);

		const core::lang::BasicGenerator& gen = convFact.builder.getBasicGenerator();
		/*
		 * CLANG introduces implicit cast for the base expression of array subscripts which cast the array type into a
		 * simple pointer. As insieme supports subscripts only for array or vector types, we skip eventual implicit
		 * cast operations.
		 */
		Expr* baseExpr = arraySubExpr->getBase();

		// IDX
		core::ExpressionPtr idx = convFact.tryDeref( Visit( arraySubExpr->getIdx() ) );
		if (!gen.isUInt4(idx->getType())) {
			idx = convFact.builder.castExpr(gen.getUInt4(), idx);
		}

		// BASE
		core::ExpressionPtr base = Visit( baseExpr );

		core::TypePtr opType;
		core::LiteralPtr op;

		if ( base->getType()->getNodeType() == core::NT_RefType ) {
			// The vector/array is an L-Value so we use the array.ref.elem
			// operator to return a reference to the addressed memory location
			const core::RefTypePtr& refSubTy = core::static_pointer_cast<const core::RefType>(base->getType());

			// TODO: we need better checking for vector type
			assert( (refSubTy->getElementType()->getNodeType() == core::NT_VectorType ||
					 refSubTy->getElementType()->getNodeType() == core::NT_ArrayType) &&
					"Base expression of array subscript is not a vector/array type.");

			op =  refSubTy->getElementType()->getNodeType() == core::NT_ArrayType ? gen.getArrayRefElem1D() : gen.getVectorRefElem();

			opType = convFact.builder.refType(
				core::static_pointer_cast<const core::SingleElementType>(refSubTy->getElementType())->getElementType()
			);

		} else {
			/*
			 * The vector/array is an R-value (e.g. (int[2]){0,1}[1] ) in this case the subscript returns an R-value so
			 * the array.subscript operator must be used
			 */
			// TODO: we need better checking for vector type
			assert( (base->getType()->getNodeType() == core::NT_VectorType ||
					 base->getType()->getNodeType() == core::NT_ArrayType) &&
					"Base expression of array subscript is not a vector/array type.");

			op =  base->getType()->getNodeType() == core::NT_ArrayType ? gen.getArraySubscript1D() : gen.getVectorSubscript();

			opType = core::static_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();
		}

		core::ExpressionPtr&& retExpr =
				convFact.builder.callExpr( opType, op, base, idx);

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, arraySubExpr, convFact);
		END_LOG_EXPR_CONVERSION(retExpr);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXT VECTOR ELEMENT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExtVectorElementExpr(ExtVectorElementExpr* vecElemExpr){
        START_LOG_EXPR_CONVERSION(vecElemExpr);
        core::ExpressionPtr&& base = Visit( vecElemExpr->getBase() );
        const core::lang::BasicGenerator& gen = convFact.builder.getBasicGenerator();

        std::string pos;
        llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

        core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(vecElemExpr) );

        //translate OpenCL accessor string to index
        if ( accessor == "x" ) 		pos = "0";
        else if ( accessor == "y" ) pos = "1";
        else if ( accessor == "z" )	pos = "2";
        else if ( accessor == "w" )	pos = "3";
	    else if ( accessor.front() == 's' || accessor.front() == 'S' ){
        	// the input string is in a form sXXX
        	// we skip the s and return the value to get the number
        	llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
        	assert( insieme::utils::numeric_cast<unsigned int>(numStr.data()) >= 0 &&
        			"Vector accessing string is not a number" );
        	pos = numStr;
	    } else if ( accessor.size() <= 4 ){ // opencl vector permutation
            vector<core::ExpressionPtr> args;

            for ( auto I = accessor.begin(), E = accessor.end(); I != E; ++I ) {
                args.push_back(convFact.builder.intLit(*I == 'w' ? 3 : (*I)-'x')); //convert x, y, z, w to 0, 1, 2, 3
            }
            return convFact.builder.callExpr(
            		gen.getVectorPermute(), convFact.tryDeref(base), convFact.builder.vectorExpr(args)
            	);
        } else {
            assert(accessor.size() <= 4 && "ExtVectorElementExpr has unknown format");
        }

        // The type of the indes is always uint<4>
        core::ExpressionPtr&& idx = convFact.builder.literal(pos, gen.getUInt4());
        // if the type of the vector is a refType, we deref it
        base = convFact.tryDeref(base);

        core::ExpressionPtr&& retExpr = convFact.builder.callExpr(exprTy, gen.getVectorSubscript(), base, idx);

        END_LOG_EXPR_CONVERSION(retExpr);
        return retExpr;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		START_LOG_EXPR_CONVERSION(declRef);
		// check whether this is a reference to a variable
		core::ExpressionPtr retExpr;
		if ( VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl()) ) {
			retExpr = convFact.lookUpVariable( varDecl );
		} else if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(declRef->getDecl()) ) {
			retExpr = core::static_pointer_cast<const core::Expression>( convFact.convertFunctionDecl(funcDecl) );
		} else if (EnumConstantDecl* enumDecl = dyn_cast<EnumConstantDecl>(declRef->getDecl() ) ) {
			retExpr = convFact.builder.literal(enumDecl->getInitVal().toString(10), convFact.builder.getBasicGenerator().getInt4());
		} else {
			// todo: C++ check whether this is a reference to a class field, or method (function).
			assert(false && "DeclRefExpr not supported!");
		}

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, declRef, convFact);
		END_LOG_EXPR_CONVERSION(retExpr);

		return annotatedNode;
	}

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //                  VECTOR/STRUCT INITALIZATION EXPRESSION
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList) {
		assert(false && "Visiting of initializer list is not allowed!");
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
	core::ExpressionPtr VisitCompoundLiteralExpr(clang::CompoundLiteralExpr* compLitExpr) {

		if ( clang::InitListExpr* initList = dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer()) ) {
			return convFact.convertInitExpr(initList, convFact.convertType(compLitExpr->getType().getTypePtr()), false);
		}
		return Visit(compLitExpr->getInitializer());
	}
};

ConversionFactory::ClangExprConverter* ConversionFactory::makeExprConvert(ConversionFactory& fact, Program& program) {
	return new ClangExprConverter(fact, program);
}

void ConversionFactory::cleanExprConvert(ConversionFactory::ClangExprConverter* exprConv) {
	delete exprConv;
}

core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return exprConv->Visit( const_cast<Expr*>(expr) );
}

/**
 * InitListExpr describes an initializer list, which can be used to initialize objects of different types,
 * InitListExpr including struct/class/union types, arrays, and vectors. For example:
 *
 * struct foo x = { 1, { 2, 3 } };
 *
 * In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the type of the
 * LHS expression.
 */
core::ExpressionPtr
ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
	bool isRef = false;
	core::TypePtr currType = type;
	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type) ) {
		isRef = true;
		currType = refType->getElementType();
	}

	core::ExpressionPtr retExpr;
	if ( currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType ) {
		const core::TypePtr& elemTy =
				core::static_pointer_cast<const core::SingleElementType>(currType)->getElementType();
		ExpressionList elements;
		// get all values of the init expression
		for ( size_t i = 0, end = initList->getNumInits(); i < end; ++i ) {
			const clang::Expr* subExpr = initList->getInit(i);
			core::ExpressionPtr&& convExpr = convertInitExpr(subExpr, elemTy, false);
			// If the type is a refType we have to add a VAR.REF operation
			// if ( !core::analysis::isCallOf(convExpr, mgr.basic.getRefVar()) ) {
			// 	convExpr = builder.refVar(convExpr);
			// }
			elements.push_back( castToType(elemTy, convExpr) );
		}
		if (elements.size() == 1 && currType->getNodeType() == core::NT_VectorType) { 
			const core::VectorTypePtr& vecTy = core::static_pointer_cast<const core::VectorType>(currType);
			// In C when the initializer list contains 1 elements then all the elements of the
			// vector (or array) must be initialized with the same value 
			const core::ConcreteIntTypeParamPtr& vecArgSize = core::static_pointer_cast<const core::ConcreteIntTypeParam>(vecTy->getSize());
			retExpr = builder.callExpr(vecTy, builder.getBasicGenerator().getVectorInitUniform(), elements.front(), 
						builder.getBasicGenerator().getIntTypeParamLiteral(vecArgSize) );
		} else {
			retExpr = builder.vectorExpr(elements);
		}
	}

	/*
	 * in the case the initexpr is used to initialize a struct/class we need to create a structExpr to initialize the
	 * structure
	 */
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType) ) {
		core::StructExpr::Members members;
		for ( size_t i = 0, end = initList->getNumInits(); i < end; ++i ) {
			const core::NamedCompositeType::Entry& curr = structTy->getEntries()[i];
			members.push_back(
				core::StructExpr::Member(curr.first, convertInitExpr(initList->getInit(i), curr.second, false))
			);
		}
		retExpr = builder.structExpr(members);
	}

	assert(retExpr && "Couldn't convert initialization expression");

	if ( isRef ) {
		retExpr = builder.refVar( retExpr );
	}
	// create vector initializator
	return retExpr;
}

core::ExpressionPtr ConversionFactory::castToType(const core::TypePtr& trgTy, const core::ExpressionPtr& expr) const {
	VLOG(1) << "@@ Converting expression '" << *expr << "' with type '" << *expr->getType() << "' to target type '" << *trgTy << "'";
	// const core::TypePtr& srcTy = expr->getType();
	core::ExpressionPtr&& ret = convertExprTo(builder, trgTy, expr);
	// assert(*trgTy == *expr->getType() && "Casting non supported!");
	VLOG(1) << "@@ Expression converted to '" << *ret << "' with type '" << *ret->getType() << "'" << std::endl;
	return ret;
}

core::ExpressionPtr
ConversionFactory::convertInitExpr(const clang::Expr* expr, const core::TypePtr& type, const bool zeroInit) const {
	// get kind of initialized value
	core::NodeType&& kind =
		(type->getNodeType() != core::NT_RefType ?
				type->getNodeType() :
				core::static_pointer_cast<const core::RefType>(type)->getElementType()->getNodeType()
			);

	if ( !expr ) {
		// if no init expression is provided => use undefined for given set of types
		if ( kind == core::NT_StructType || kind == core::NT_UnionType || kind == core::NT_ArrayType ||
				kind == core::NT_VectorType ) {
			if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type) ) {
				const core::TypePtr& res = refTy->getElementType();
				return builder.refVar(
					builder.callExpr( res,
						(zeroInit ? mgr.basic.getInitZero() : mgr.basic.getUndefined()), mgr.basic.getTypeLiteral(res)
					)
				);
			}
			return builder.callExpr( type,
				(zeroInit ? mgr.basic.getInitZero() : mgr.basic.getUndefined()), mgr.basic.getTypeLiteral(type)
			);
		} else {
			return defaultInitVal(type);
		}
	}

	/*
	 * if an expression is provided as initializer first check if this is an initializer list which is used for arrays,
	 * structs and unions
	 */
	if ( const clang::InitListExpr* listExpr = dyn_cast<const clang::InitListExpr>( expr ) ) {
		return convertInitializerList( listExpr, type );
	}

	// Convert the expression like any other expression
	core::ExpressionPtr&& retExpr = convertExpr( expr );

	if ( core::analysis::isCallOf(retExpr, mgr.basic.getArrayCreate1D()) ) {
		retExpr = builder.callExpr(builder.refType(retExpr->getType()), mgr.basic.getRefNew(), retExpr);
	}

	// fix type if necessary (also converts "Hello" into ['H','e',...])
	core::TypePtr valueType = type;
	if (type->getNodeType() == core::NT_RefType) {
		valueType = core::analysis::getReferencedType(valueType);
	}
	retExpr = castToType(valueType, retExpr);

	// if result is a reference type => create new local variable
	if (type->getNodeType() == core::NT_RefType) {
		retExpr = builder.callExpr(type, mgr.basic.getRefVar(), retExpr);
	}

	return retExpr;
}


namespace {

core::FunctionTypePtr addGlobalsToFunctionType(const core::ASTBuilder& builder,
						 	 	 	 	 	   const core::TypePtr& globals,
						 	 	 	 	 	   const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getParameterTypes();

	std::vector<core::TypePtr> argTypes(oldArgs.size()+1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin()+1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType( argTypes, funcType->getReturnType() );

}

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {
	// the function is not extern, a lambdaExpr has to be created
	assert(currTU && funcDecl->hasBody() && "Function has no body!");

	VLOG(1) << "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;

	VLOG(1) << "#----------------------------------------------------------------------------------#";
	VLOG(1) << "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl
			 << "-> at location: ("
			 << utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager())
			 << "): " << std::endl
			 << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
			 << "\tisResolvingRecFuncBody: " << ctx.isResolvingRecFuncBody << std::endl
			 << "\tEmpty map: "    << ctx.recVarExprMap.size();

	bool isConstructor = false;
	const clang::CXXConstructorDecl * ctorDecl;
	if (ctorDecl = dyn_cast<CXXConstructorDecl>(funcDecl)){
		isConstructor = true;
	}

	if ( !ctx.isRecSubFunc ) {
		// add this type to the type graph (if not present)
		exprConv->funcDepGraph.addNode(funcDecl);
		if( VLOG_IS_ON(2) ) {
			exprConv->funcDepGraph.print( std::cout );
		}
	}

	// check if we already resolved this function
	// look up the lambda cache to see if this function has been
	// already converted into an IR lambda expression.
	ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find( funcDecl );

	if ( fit != ctx.lambdaExprCache.end() ) {
		return fit->second;
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConv->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	// save the current translation unit 
	const TranslationUnit* oldTU = currTU;
	std::set<const FunctionDecl*>&& subComponents = exprConv->funcDepGraph.getSubComponents( funcDecl );

	std::for_each(subComponents.begin(), subComponents.end(), 
		[&](const FunctionDecl* cur){ 

			FunctionDecl* decl = const_cast<FunctionDecl*>(cur);
			const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);

			if ( clangTU ) {
				// update the translation unit
				this->currTU = &Program::getTranslationUnit(clangTU);
				// look up the lambda cache to see if this function has been
				// already converted into an IR lambda expression. 
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(decl);
				if ( fit == ctx.lambdaExprCache.end() ) {
					// perfrom the conversion only if this is the first time this
					// function is encountred 

					//convertFunctionDecl(decl, false); //TODO why is main subcomp of a constructor???
					//ctx.recVarExprMap.clear();		//TODO
				}
			}
		}
	);

	// reset the translation unit
	currTU = oldTU;

	if ( !components.empty() ) {
		// we are dealing with a recursive type
		VLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
				<< "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(),
			[ ] (std::set<const FunctionDecl*>::value_type c) {
				VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
			}
		);

		if ( !ctx.isRecSubFunc ) {
			if ( ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end() ) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert( std::make_pair(funcDecl, ctx.currVar) );
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if ( !ctx.isRecSubFunc ) {
			std::for_each(components.begin(), components.end(),
				[ this ] (std::set<const FunctionDecl*>::value_type fd) {

					if ( this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end() ) {
						core::FunctionTypePtr funcType =
							core::static_pointer_cast<const core::FunctionType>( this->convertType(GET_TYPE_PTR(fd)) );
						// In the case the function is receiving the global variables the signature needs to be
						// modified by allowing the global struct to be passed as an argument
						if ( this->ctx.globalFuncMap.find(fd) != this->ctx.globalFuncMap.end() ) {
							funcType = addGlobalsToFunctionType(this->builder, this->ctx.globalStruct.first, funcType);
						}
						core::VariablePtr&& var = this->builder.variable( funcType );
						this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
					}
				}
			);
		}
		if ( VLOG_IS_ON(2) ) {
			VLOG(2) << "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
				[] (ConversionContext::RecVarExprMap::value_type c) {
					VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
				}
			);
		}
	}

	// init parameter set
	vector<core::VariablePtr> params;

	/*
	 * before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	 * global struct or not
	 */
	core::VariablePtr parentGlobalVar = ctx.globalVar;
	if ( !isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end() ) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );
		ctx.globalVar = var;
	}

	std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
		[ &params, this ] (ParmVarDecl* currParam) {
			params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
		}
	);

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert( (components.empty() || (!components.empty() && !ctx.isResolvingRecFuncBody)) && 
			"~~~ Something odd happened, you are allowed by all means to blame Simone ~~~" );
	if (!components.empty()) {
		ctx.isResolvingRecFuncBody = true;
	}

	// set up context to contain current list of parameters and convert body
	ConversionContext::ParameterList oldList = ctx.curParameter;
	ctx.curParameter = &params;

	std::cerr << "******************stmt body dump\n";
	funcDecl->getBody()->dump();

	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	ctx.curParameter = oldList;

	/*
	 * if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	 * the body of this function
	 */
	vector<core::StatementPtr> decls;
	std::for_each(params.begin(), params.end(),
		[ &decls, &body, this ] (core::VariablePtr currParam) {
			auto fit = this->ctx.wrapRefMap.find(currParam);

			if ( fit != this->ctx.wrapRefMap.end() ) {
				decls.push_back( this->builder.declarationStmt(fit->second,	this->builder.refVar( fit->first ) ));
				/*
				 * replace this parameter in the body, example:
				 *
				 * int f(int a) {
				 *  for (...) {
				 *   x = a; <- if all the occurencies of a will not be replaced the semantics of
				 *   		   the code will not be preserved
				 *   a = i;
				 *  }
				 * }
				 *
				 *  as the variable can olny appear in the RHS of expression, we have to sobstitute it with its
				 *  dereference
				 */
				body = core::static_pointer_cast<const core::Statement>(
						core::transform::replaceAll( this->builder.getNodeManager(), body, fit->first,
								this->tryDeref(fit->second))
				);
			}

		}
	);

	// if we introduce new decls we have to introduce them just before the body of the function
	if ( !decls.empty() ) {
		// push the old body
		decls.push_back(body);
		body = builder.compoundStmt(decls);
	}

	if ( !components.empty() ) {
		ctx.isResolvingRecFuncBody = false;
	}

	// ADD THE GLOBALS
	if ( isEntryPoint && ctx.globalVar ) {
		const core::CompoundStmtPtr& compStmt = core::static_pointer_cast<const core::CompoundStmt>(body);
		assert(ctx.globalVar && ctx.globalStruct.second);

		const StatementList& oldStmts = compStmt->getStatements();

		std::vector<core::StatementPtr> stmts(oldStmts.size()+1);
		stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew( ctx.globalStruct.second ));
		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin()+1);

		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType( GET_TYPE_PTR(funcDecl) );
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	// if this function gets the globals in the capture list we have to create a different type
	if ( !isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end() ) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	// reset old global var
	ctx.globalVar = parentGlobalVar;

	if ( components.empty() ) {
		core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr( funcType, params, body);
		// attach name annotation to the lambda
		retLambdaExpr->getLambda()->addAnnotation(
			std::make_shared<annotations::c::CNameAnnotation>( funcDecl->getNameAsString() )
		);

        // Adding the lambda function to the list of converted functions
        ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );

        return attachFuncAnnotations(retLambdaExpr, funcDecl);
		return retLambdaExpr;
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );
	// attach name annotation to the lambda
	retLambdaNode->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>( funcDecl->getNameAsString() ) );
	// this is a recurive function call
	if ( ctx.isRecSubFunc ) {
		/*
		 * if we are visiting a nested recursive type it means someone else will take care of building the rectype
		 * node, we just return an intermediate type
		 */
		return retLambdaNode;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	core::LambdaDefinition::Definitions definitions;
	definitions.insert( std::make_pair(recVarRef, retLambdaNode) );

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

	std::for_each(components.begin(), components.end(),
		[ this, &definitions ] (std::set<const FunctionDecl*>::value_type fd) {

			ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
			assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
			this->ctx.currVar = tit->second;

			/*
			 * we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
			 * for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
			 * is enabled only when the isRecSubType flag is true
			 */
			this->ctx.recVarExprMap.erase(fd);

			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already loaded
			 * use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity =
					clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), this->program.getClangProgram());
			ConversionFactory::TranslationUnitPair&& ret = this->program.getClangIndexer().getDefinitionFor(funcEntity);
			const TranslationUnit* oldTU = this->currTU;
			if ( ret.first ) {
				fd = ret.first;
				assert(ret.second && "Error loading translation unit for function definition");
				this->currTU = &Program::getTranslationUnit(ret.second);
			}

			const core::LambdaPtr& lambda =
					core::static_pointer_cast<const core::Lambda>(this->convertFunctionDecl(fd));
			assert(lambda && "Resolution of sub recursive lambda yields a wrong result");
			this->currTU = oldTU;
			// attach name annotation to the lambda
			lambda->addAnnotation( std::make_shared<annotations::c::CNameAnnotation>( fd->getNameAsString() ) );
			definitions.insert( std::make_pair(this->ctx.currVar, lambda) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;
		}
	);
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
		[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
			auto fit = this->ctx.recVarExprMap.find(fd);
			assert(fit != this->ctx.recVarExprMap.end());
			
			FunctionDecl* decl = const_cast<FunctionDecl*>(fd);
			const clang::idx::TranslationUnit* clangTU = this->getTranslationUnitForDefinition(decl);
			
			assert ( clangTU );
			// save old TU
			const TranslationUnit* oldTU = this->currTU;

			// update the translation unit
			this->currTU = &Program::getTranslationUnit(clangTU);
			
			core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(decl, func) );

			func = this->attachFuncAnnotations(func, decl);

			currTU = oldTU;
		}
	);
	VLOG(2) << "Converted Into: " << *retLambdaExpr;

	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

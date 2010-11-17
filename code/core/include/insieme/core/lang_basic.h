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

#pragma once

#ifdef WIN32 
#pragma warning "lang_basic.h is deprecated. Use lang/basic.h instead (via NodeManager::basic)"
#else
#warning "lang_basic.h is deprecated. Use lang/basic.h instead (via NodeManager::basic)"
#endif

#include "insieme/core/ast_node.h"
#include "insieme/core/types.h"
#include "insieme/core/expressions.h"
#include "insieme/core/statements.h"

namespace insieme {
namespace core {
namespace lang {



// TODO: fix problem of annotations!


// Definition of basic data types

#define DEF_TYPE(TYPE, NAME) \
	typedef TYPE NAME; \
	typedef TYPE ## Ptr NAME ## Ptr;

#define ADD_TYPE(TYPE, NAME) \
	extern const TYPE TYPE_ ## NAME ## _VAL; \
	extern const TYPE ## Ptr TYPE_ ## NAME; \
	extern const TYPE ## Ptr TYPE_ ## NAME ## _PTR;

// -------------------------------- Unit Type -------------------------------

DEF_TYPE(GenericType, UnitType);
bool isUnitType(const Type& type);

ADD_TYPE(TypeVariable, ALPHA);
ADD_TYPE(RefType, REF_ALPHA);
ADD_TYPE(UnitType, UNIT);

// -------------------------------- Boolean Type -------------------------------

DEF_TYPE(GenericType, BoolType);

bool isBoolType(const Type& type);

ADD_TYPE(BoolType, BOOL);

// -------------------------------- Integer Types ------------------------------

DEF_TYPE(GenericType, IntegerType);

bool isIntegerType(const Type& type);
IntegerType getIntegerType(unsigned short size, bool _signed);
int getNumBytes(const IntegerType& type);

DEF_TYPE(GenericType, IntType);

IntType intType(unsigned short size);
IntType getGenIntType(char symbol);
bool isIntType(const Type& type);

ADD_TYPE(IntType, INT_GEN);
ADD_TYPE(IntType, INT_1);
ADD_TYPE(IntType, INT_2);
ADD_TYPE(IntType, INT_4);
ADD_TYPE(IntType, INT_8);
ADD_TYPE(IntType, INT_INF);


DEF_TYPE(GenericType, UIntType);

UIntType uintType(unsigned short size);
UIntType getGenUIntType(char symbol);
bool isUIntType(const Type& type);

ADD_TYPE(UIntType, UINT_GEN);
ADD_TYPE(UIntType, UINT_1);
ADD_TYPE(UIntType, UINT_2);
ADD_TYPE(UIntType, UINT_4);
ADD_TYPE(UIntType, UINT_8);
ADD_TYPE(UIntType, UINT_INF);

// -------------------------------- Real Types ------------------------------

DEF_TYPE(GenericType, RealType);

RealType realType(unsigned short size);
RealType getGenRealType(char symbol);
bool isRealType(const Type& type);

ADD_TYPE(RealType, REAL_GEN);
ADD_TYPE(RealType, REAL_1);
ADD_TYPE(RealType, REAL_2);
ADD_TYPE(RealType, REAL_4);
ADD_TYPE(RealType, REAL_8);
ADD_TYPE(RealType, REAL_INF);

// -------------------------------- Vector Types ------------------------------

bool isVectorType(const Type& type);

// ------------------------ Character and String Types -----------------------

ADD_TYPE(GenericType, CHAR);
ADD_TYPE(GenericType, WCHAR);

// -------------------------------- Misc Types ------------------------------

ADD_TYPE(GenericType, STRING);
ADD_TYPE(GenericType, WSTRING);
ADD_TYPE(GenericType, VAR_LIST);

ADD_TYPE(FunctionType, OP_ASSIGN);

// -------------------------------- Constants ------------------------------

#define ADD_CONST(NAME) \
	extern const Literal CONST_ ## NAME; \
	extern const LiteralPtr CONST_ ## NAME ## _PTR;

//ADD_CONST(UINT_ZERO);
//ADD_CONST(UINT_ONE);

ADD_CONST(BOOL_TRUE);
ADD_CONST(BOOL_FALSE);

ADD_CONST(NULL_PTR);

#undef ADD_CONST

// -------------------------------- Operator ------------------------------

DEF_TYPE(Literal, Operator);

#define ADD_OP(Name) \
		extern const Operator OP_ ## Name ## _VAL; \
		extern const OperatorPtr OP_ ## Name; \
		extern const OperatorPtr OP_ ## Name ## _PTR; \

ADD_TYPE(FunctionType, NO_ARGS_OP);

ADD_TYPE(FunctionType, UNARY_BOOL_OP);
ADD_TYPE(FunctionType, BINARY_BOOL_OP);

ADD_OP(BOOL_NOT);
ADD_OP(BOOL_AND);
ADD_OP(BOOL_OR);
ADD_OP(BOOL_EQ);

// --- Arithmetic ---

ADD_TYPE(FunctionType, BINARY_INT_OP);

ADD_OP(INT_ADD);
ADD_OP(INT_SUB);
ADD_OP(INT_MUL);
ADD_OP(INT_DIV);
ADD_OP(INT_MOD);

ADD_TYPE(FunctionType, GUARD_OP);

ADD_TYPE(FunctionType, BINARY_UINT_OP);

ADD_OP(UINT_ADD);
ADD_OP(UINT_SUB);
ADD_OP(UINT_MUL);
ADD_OP(UINT_DIV);
ADD_OP(UINT_MOD);

ADD_TYPE(FunctionType, BINARY_REAL_OP);

ADD_OP(REAL_ADD);
ADD_OP(REAL_SUB);
ADD_OP(REAL_MUL);
ADD_OP(REAL_DIV);

// --- Comparison ---

ADD_TYPE(FunctionType, COMPARISON_INT_OP);

ADD_OP(INT_EQ);
ADD_OP(INT_NE);
ADD_OP(INT_LT);
ADD_OP(INT_GT);
ADD_OP(INT_LE);
ADD_OP(INT_GE);

ADD_TYPE(FunctionType, COMPARISON_UINT_OP);

ADD_OP(UINT_EQ);
ADD_OP(UINT_NE);
ADD_OP(UINT_LT);
ADD_OP(UINT_GT);
ADD_OP(UINT_LE);
ADD_OP(UINT_GE);

ADD_TYPE(FunctionType, COMPARISON_REAL_OP);

ADD_OP(REAL_EQ);
ADD_OP(REAL_NE);
ADD_OP(REAL_LT);
ADD_OP(REAL_GT);
ADD_OP(REAL_LE);
ADD_OP(REAL_GE);

// --- Bitwise ---

ADD_TYPE(FunctionType, BITWISE_INT_OP);

ADD_OP(INT_NOT);
ADD_OP(INT_AND);
ADD_OP(INT_OR);
ADD_OP(INT_XOR);

ADD_TYPE(FunctionType, SHIFT_INT_OP);

ADD_OP(INT_LEFT_SHIFT);
ADD_OP(INT_RIGHT_SHIFT);

ADD_TYPE(FunctionType, BITWISE_UINT_OP);

ADD_OP(UINT_NOT);
ADD_OP(UINT_AND);
ADD_OP(UINT_OR);
ADD_OP(UINT_XOR);

ADD_TYPE(FunctionType, SHIFT_UINT_OP);

ADD_OP(UINT_LEFT_SHIFT);
ADD_OP(UINT_RIGHT_SHIFT);


// --- References ---

ADD_OP(REF_VAR);
ADD_OP(REF_NEW);
ADD_OP(REF_DELETE);
ADD_OP(REF_ASSIGN);
ADD_OP(REF_DEREF);


// --- Arrays ---
ADD_OP(SUBSCRIPT);
ADD_OP(SUBSCRIPT_SINGLE);
ADD_OP(LENGTH);


// --- Vectors ---
ADD_OP(VECTOR_INIT_UNIFORM);
ADD_OP(VECTOR_INIT_UNDEFINED);

// --- Channels ---
//ADD_OP(SEND);
//ADD_OP(TRY_SEND);
//ADD_OP(RECV);
//ADD_OP(TRY_RECV);
//ADD_OP(EMPTY);

// --- VAR_LIST packing ---

ADD_OP(VAR_LIST_PACK);

// --- if-then-else ---

ADD_OP(ITE);

// ---------------------------- Parallel Constructs -------------------------

ADD_TYPE(GenericType, JOB);
ADD_TYPE(GenericType, THREAD_GROUP);

ADD_OP(PARALLEL);
ADD_OP(PARALLEL_DETACHED);

ADD_OP(MERGE);
ADD_OP(MERGEALL);

ADD_OP(BARRIER);
ADD_OP(REDUCE);
//ADD_OP(REDISTRIBUTE);
//ADD_OP(PFOR);

// -------------------------------- Statements ------------------------------

DEF_TYPE(CompoundStmt, NoOpStmt);

extern const NoOpStmt STMT_NO_OP;
extern const NoOpStmtPtr STMT_NO_OP_PTR;

#undef ADD_OP
#undef ADD_TYPE
#undef DEF_TYPE

// ---------------------------------- Utility -------------------------------

/**
 * Tests whether there is a build-in literal using the given value.
 *
 * @param name the value of the literal to be looking for
 * @return true if there is such a literal, false otherwise
 */
bool isBuildIn(const string& value);

/**
 * Tests whether there is a build-in literal equivalent to the given literal.
 *
 * @param literal the literal to be looking for
 * @return true if there is such a literal, false otherwise
 */
bool isBuildIn(const LiteralPtr& literal);

/**
 * Looks up a build in literal with the given value.
 *
 * @param value the value to be looking for
 * @return a pointer to the build-in literal
 */
LiteralPtr getBuildInForValue(const string& value);

} // end namespace: lang
} // end namespace: core
} // end namespace: insieme


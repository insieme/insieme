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

#include "ast_node.h"
#include "types.h"
#include "expressions.h"
#include "definition.h"
#include "statements.h"

namespace insieme {
namespace core {
namespace lang {



// TODO: fix problem of annotations!


// Definition of basic data types

#define DEF_TYPE(TYPE, NAME) \
	typedef TYPE NAME; \
	typedef TYPE ## Ptr NAME ## Ptr;

#define ADD_TYPE(TYPE, NAME) \
	extern const TYPE TYPE_ ## NAME; \
	extern const TYPE ## Ptr TYPE_ ## NAME ## _PTR;


// -------------------------------- Unit Type -------------------------------

DEF_TYPE(GenericType, UnitType);
bool isUnitType(const Type& type);

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

IntType getIntType(unsigned short size);
IntType getGenIntType(char symbol);
bool isIntType(const Type& type);

ADD_TYPE(IntType, INT_GEN);
ADD_TYPE(IntType, INT_1);
ADD_TYPE(IntType, INT_2);
ADD_TYPE(IntType, INT_4);
ADD_TYPE(IntType, INT_8);
ADD_TYPE(IntType, INT_INF);


DEF_TYPE(GenericType, UIntType);

UIntType getUIntType(unsigned short size);
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

RealType getRealType(unsigned short size);
RealType getGenRealType(char symbol);
bool isRealType(const Type& type);

ADD_TYPE(RealType, REAL_GEN);
ADD_TYPE(RealType, REAL_1);
ADD_TYPE(RealType, REAL_2);
ADD_TYPE(RealType, REAL_4);
ADD_TYPE(RealType, REAL_8);
ADD_TYPE(RealType, REAL_INF);


// -------------------------------- Operator ------------------------------

DEF_TYPE(VarExpr, UnaryOp);
DEF_TYPE(VarExpr, BinaryOp);

#define ADD_UNARY_OP(Name) \
		extern const UnaryOp OP_ ## Name; \
		extern const UnaryOpPtr OP_ ## Name ## _PTR; \
		extern const Definition DEF_ ## Name; \
		extern const DefinitionPtr DEF_ ## Name ## _PTR; \

#define ADD_BINARY_OP(Name) \
		extern const BinaryOp OP_ ## Name; \
		extern const BinaryOpPtr OP_ ## Name ## _PTR; \
		extern const Definition DEF_ ## Name; \
		extern const DefinitionPtr DEF_ ## Name ## _PTR; \

ADD_TYPE(FunctionType, UNARY_BOOL_OP);
ADD_TYPE(FunctionType, BINARY_BOOL_OP);

ADD_UNARY_OP(BOOL_NOT);
ADD_BINARY_OP(BOOL_AND);


ADD_TYPE(FunctionType, BINARY_INT_OP);

ADD_BINARY_OP(INT_ADD);
ADD_BINARY_OP(INT_SUB);
ADD_BINARY_OP(INT_MUL);
ADD_BINARY_OP(INT_DIV);
ADD_BINARY_OP(INT_MOD);

ADD_TYPE(FunctionType, BINARY_UINT_OP);

ADD_BINARY_OP(UINT_ADD);
ADD_BINARY_OP(UINT_SUB);
ADD_BINARY_OP(UINT_MUL);
ADD_BINARY_OP(UINT_DIV);
ADD_BINARY_OP(UINT_MOD);

ADD_TYPE(FunctionType, BINARY_REAL_OP);

ADD_BINARY_OP(REAL_ADD);
ADD_BINARY_OP(REAL_SUB);
ADD_BINARY_OP(REAL_MUL);
ADD_BINARY_OP(REAL_DIV);

#undef ADD_UNARY_OP
#undef ADD_BINARY_OP


// -------------------------------- Statements ------------------------------

DEF_TYPE(CompoundStmt, NoOpStmt);

extern const NoOpStmt STMT_NO_OP;
extern const NoOpStmtPtr STMT_NO_OP_PTR;

#undef ADD_TYPE
#undef DEF_TYPE

} // end namespace: lang
} // end namespace: core
} // end namespace: insieme


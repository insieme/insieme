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

namespace insieme {
namespace core {
namespace lang {

// TODO: fix problem of annotations!


// Definition of basic data types

// -------------------------------- Boolean Type -------------------------------

typedef GenericType BoolType;
bool isBoolType(const Type& type);
extern const BoolType TYPE_BOOL;

// -------------------------------- Integer Types ------------------------------

typedef GenericType IntegerType;
bool isIntegerType(const Type& type);
IntegerType getIntegerType(unsigned short size, bool _signed);
int getNumBytes(const IntegerType& type);


typedef GenericType IntType;
IntType getIntType(unsigned short size);
IntType getGenIntType(char symbol);
bool isIntType(const Type& type);

extern const IntType TYPE_INT_GEN;
extern const IntType TYPE_INT_1;
extern const IntType TYPE_INT_2;
extern const IntType TYPE_INT_4;
extern const IntType TYPE_INT_8;
extern const IntType TYPE_INT_INF;


typedef GenericType UIntType;
UIntType getUIntType(unsigned short size);
UIntType getGenUIntType(char symbol);
bool isUIntType(const Type& type);

extern const UIntType TYPE_UINT_GEN;
extern const UIntType TYPE_UINT_1;
extern const UIntType TYPE_UINT_2;
extern const UIntType TYPE_UINT_4;
extern const UIntType TYPE_UINT_8;
extern const UIntType TYPE_UINT_INF;

// -------------------------------- Real Types ------------------------------

typedef GenericType RealType;
RealType getRealType(unsigned short size);
RealType getGenRealType(char symbol);
bool isRealType(const Type& type);

extern const RealType TYPE_REAL_GEN;
extern const RealType TYPE_REAL_1;
extern const RealType TYPE_REAL_2;
extern const RealType TYPE_REAL_4;
extern const RealType TYPE_REAL_8;
extern const RealType TYPE_REAL_INF;



// -------------------------------- Operator ------------------------------

typedef VarExpr UnaryOp;
typedef VarExpr BinaryOp;

#define ADD_UNARY_OP(Name) \
		extern const UnaryOp OP_ ## Name; \
		extern const Definition DEF_ ## Name; \

#define ADD_BINARY_OP(Name) \
		extern const BinaryOp OP_ ## Name; \
		extern const Definition DEF_ ## Name; \

extern const FunctionType TYPE_UNARY_BOOL_OP;
extern const FunctionType TYPE_BINARY_BOOL_OP;

ADD_UNARY_OP(BOOL_NOT)
ADD_BINARY_OP(BOOL_AND);


extern const FunctionType TYPE_BINARY_INT_OP;

ADD_BINARY_OP(INT_ADD);
ADD_BINARY_OP(INT_SUB);
ADD_BINARY_OP(INT_MUL);
ADD_BINARY_OP(INT_DIV);
ADD_BINARY_OP(INT_MOD);

extern const FunctionType TYPE_BINARY_UINT_OP;

ADD_BINARY_OP(UINT_ADD);
ADD_BINARY_OP(UINT_SUB);
ADD_BINARY_OP(UINT_MUL);
ADD_BINARY_OP(UINT_DIV);
ADD_BINARY_OP(UINT_MOD);

extern const FunctionType TYPE_BINARY_REAL_OP;

ADD_BINARY_OP(REAL_ADD);
ADD_BINARY_OP(REAL_SUB);
ADD_BINARY_OP(REAL_MUL);
ADD_BINARY_OP(REAL_DIV);

#undef ADD_UNARY_OP
#undef ADD_BINARY_OP

} // end namespace: lang
} // end namespace: core
} // end namespace: insieme


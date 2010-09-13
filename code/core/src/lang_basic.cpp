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

#include <cassert>

#include "lang_basic.h"

#include "container_utils.h"

namespace insieme {
namespace core {
namespace lang {


#define ADD_TYPE(TYPE, NAME, VALUE) \
	const TYPE TYPE_ ## NAME = VALUE; \
	const TYPE ## Ptr TYPE_ ## NAME ## _PTR = TYPE ## Ptr(&TYPE_ ## NAME);


const IntTypeParam INT_TYPE_PARAM_INF = IntTypeParam::getInfiniteIntParam();

// -------------------------------- Unit Type -------------------------------


const Identifier TYPE_NAME_UNIT("unit");

bool isUnitType(const Type& type) {
	return type == TYPE_UNIT;
}

ADD_TYPE(UnitType, UNIT, GenericType(TYPE_NAME_UNIT));

// -------------------------------- Boolean Type -------------------------------

const Identifier TYPE_NAME_BOOL("bool");

bool isBoolType(const Type& type) {
	return type == TYPE_BOOL;
}

ADD_TYPE(BoolType, BOOL, GenericType(TYPE_NAME_BOOL));

// -------------------------------- Integer Types -------------------------------

const Identifier TYPE_NAME_INT("int");

bool isIntegerType(const Type& type) {
	return isIntType(type) || isUIntType(type);
}

IntegerType getIntegerType(unsigned short size, bool _signed) {
	return (_signed)?getIntType(size):getUIntType(size);
}

int getNumBytes(const IntegerType& type) {
	assert( isIntegerType(type) && "Given argument is not an integer type!");
	IntTypeParam param = type.getIntTypeParameter()[0];
	if (param.getType()==IntTypeParam::CONCRETE) {
		return param.getValue();
	}
	return -1;
}


IntType getIntType(unsigned short size) {
	return GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(IntTypeParam(size)));
}

IntType getGenIntType(char symbol) {
	return GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(IntTypeParam(symbol)));
}

bool isIntType(const Type& type) {
	// TODO: replace with unification test ?

	// check type
	if (typeid(type) != typeid(IntType)) {
		return false;
	}

	// test whether the criteria are matching
	const GenericType& other = static_cast<const GenericType&>(type);
	return	other.getFamilyName() == TYPE_NAME_INT
			&& other.getTypeParameter().size() == 0
			&& other.getIntTypeParameter().size() == 1
			&& other.getBaseType() == TypePtr(NULL);
}

ADD_TYPE(IntType, INT_GEN, getGenIntType('a'));
ADD_TYPE(IntType, INT_1, getIntType(1));
ADD_TYPE(IntType, INT_2, getIntType(2));
ADD_TYPE(IntType, INT_4, getIntType(4));
ADD_TYPE(IntType, INT_8, getIntType(8));
ADD_TYPE(IntType, INT_INF, (GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(INT_TYPE_PARAM_INF))));


const Identifier TYPE_NAME_UINT("uint");

UIntType getUIntType(unsigned short size) {
	return GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(IntTypeParam(size)));
}

UIntType getGenUIntType(char symbol) {
	return GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(IntTypeParam(symbol)));
}

bool isUIntType(const Type& type) {
	// TODO: replace with unification test ?

	// check type
	if (typeid(type) != typeid(IntType)) {
		return false;
	}

	// test whether the criteria are matching
	const GenericType& other = static_cast<const GenericType&>(type);
	return	other.getFamilyName() == TYPE_NAME_UINT
			&& other.getTypeParameter().size() == 0
			&& other.getIntTypeParameter().size() == 1
			&& other.getBaseType() == TypePtr(NULL);
}

ADD_TYPE(UIntType, UINT_GEN, getGenUIntType('a'));
ADD_TYPE(UIntType, UINT_1, getUIntType(1));
ADD_TYPE(UIntType, UINT_2, getUIntType(2));
ADD_TYPE(UIntType, UINT_4, getUIntType(4));
ADD_TYPE(UIntType, UINT_8, getUIntType(8));
ADD_TYPE(UIntType, UINT_INF, (GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(INT_TYPE_PARAM_INF))));


// -------------------------------- Real Types ------------------------------

const Identifier TYPE_NAME_REAL("real");

RealType getRealType(unsigned short size) {
	return GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(IntTypeParam(size)));
}

RealType getGenRealType(char symbol) {
	return GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(IntTypeParam(symbol)));
}

bool isRealType(const Type& type) {
	// TODO: replace with unification test ?

	// check type
	if (typeid(type) != typeid(RealType)) {
		return false;
	}

	// test whether the criteria are matching
	const GenericType& other = static_cast<const GenericType&>(type);
	return	other.getFamilyName() == TYPE_NAME_REAL
			&& other.getTypeParameter().size() == 0
			&& other.getIntTypeParameter().size() == 1
			&& other.getBaseType() == TypePtr(NULL);
}

ADD_TYPE(RealType, REAL_GEN, getGenRealType('a'));
ADD_TYPE(RealType, REAL_1, getRealType(1));
ADD_TYPE(RealType, REAL_2, getRealType(2));
ADD_TYPE(RealType, REAL_4, getRealType(4));
ADD_TYPE(RealType, REAL_8, getRealType(8));
ADD_TYPE(RealType, REAL_INF, (GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(INT_TYPE_PARAM_INF))));


// -------------------------------- Constants ------------------------------

#define ADD_CONST(NAME, VALUE) \
	const Literal CONST_ ## NAME = VALUE; \
	const LiteralPtr CONST_ ## NAME ## _PTR = LiteralPtr(&CONST_ ## NAME);

ADD_CONST(UINT_ZERO, Literal(TYPE_UINT_1_PTR, "0"));
ADD_CONST(UINT_ONE, Literal(TYPE_UINT_1_PTR, "1"));

ADD_CONST(BOOL_TRUE, Literal(TYPE_BOOL_PTR, "true"));
ADD_CONST(BOOL_FALSE, Literal(TYPE_BOOL_PTR, "false"));

#undef ADD_CONST

// -------------------------------- Operator ------------------------------

#define ADD_UNARY_OP(Name, Type, Symbol) \
		const UnaryOp OP_ ## Name (Type, Symbol); \
		const UnaryOpPtr OP_ ## Name ## _PTR = UnaryOpPtr(&OP_ ## Name); \
		const Definition DEF_ ## Name (Symbol, Type, true, NULL); \
		const DefinitionPtr DEF_ ## Name ## _PTR = DefinitionPtr(&DEF_ ## Name); \

#define ADD_BINARY_OP(Name, Type, Symbol) \
		const BinaryOp OP_ ## Name (Type, Symbol); \
		const UnaryOpPtr OP_ ## Name ## _PTR = UnaryOpPtr(&OP_ ## Name); \
		const Definition DEF_ ## Name (Symbol, Type, true, NULL); \
		const DefinitionPtr DEF_ ## Name ## _PTR = DefinitionPtr(&DEF_ ## Name); \

ADD_TYPE(TupleType, BOOL_SINGLE, TupleType(TYPE_BOOL_PTR));
ADD_TYPE(TupleType, BOOL_PAIR, TupleType(TYPE_BOOL_PTR,TYPE_BOOL_PTR));

ADD_TYPE(FunctionType, UNARY_BOOL_OP, (FunctionType(TYPE_BOOL_SINGLE_PTR, TYPE_BOOL_PTR)));
ADD_TYPE(FunctionType, BINARY_BOOL_OP, (FunctionType(TYPE_BOOL_PAIR_PTR, TYPE_BOOL_PTR)));

ADD_UNARY_OP(BOOL_NOT, TYPE_UNARY_BOOL_OP_PTR, "bool.not");
ADD_BINARY_OP(BOOL_AND, TYPE_BINARY_BOOL_OP_PTR, "bool.and");
ADD_BINARY_OP(BOOL_OR, TYPE_BINARY_BOOL_OP_PTR, "bool.or");

ADD_TYPE(TupleType, INT_SINGLE, TupleType(TYPE_INT_GEN_PTR));
ADD_TYPE(TupleType, INT_PAIR, TupleType(TYPE_INT_GEN_PTR,TYPE_INT_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_INT_GEN_PTR)));

ADD_BINARY_OP(INT_ADD, TYPE_BINARY_INT_OP_PTR, "int.add");
ADD_BINARY_OP(INT_SUB, TYPE_BINARY_INT_OP_PTR, "int.sub");
ADD_BINARY_OP(INT_MUL, TYPE_BINARY_INT_OP_PTR, "int.mul");
ADD_BINARY_OP(INT_DIV, TYPE_BINARY_INT_OP_PTR, "int.div");
ADD_BINARY_OP(INT_MOD, TYPE_BINARY_INT_OP_PTR, "int.mod");

ADD_TYPE(TupleType, UINT_SINGLE, TupleType(TYPE_UINT_GEN_PTR));
ADD_TYPE(TupleType, UINT_PAIR, TupleType(TYPE_UINT_GEN_PTR,TYPE_UINT_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_UINT_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_UINT_GEN_PTR)));

ADD_BINARY_OP(UINT_ADD, TYPE_BINARY_UINT_OP_PTR, "uint.add");
ADD_BINARY_OP(UINT_SUB, TYPE_BINARY_UINT_OP_PTR, "uint.sub");
ADD_BINARY_OP(UINT_MUL, TYPE_BINARY_UINT_OP_PTR, "uint.mul");
ADD_BINARY_OP(UINT_DIV, TYPE_BINARY_UINT_OP_PTR, "uint.div");
ADD_BINARY_OP(UINT_MOD, TYPE_BINARY_UINT_OP_PTR, "uint.mod");

ADD_TYPE(TupleType, REAL_SINGLE, TupleType(TYPE_REAL_GEN_PTR));
ADD_TYPE(TupleType, REAL_PAIR, TupleType(TYPE_REAL_GEN_PTR,TYPE_REAL_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_REAL_OP, (FunctionType(TYPE_REAL_PAIR_PTR, TYPE_REAL_GEN_PTR)));

ADD_BINARY_OP(REAL_ADD, TYPE_BINARY_REAL_OP_PTR, "real.add");
ADD_BINARY_OP(REAL_SUB, TYPE_BINARY_REAL_OP_PTR, "real.sub");
ADD_BINARY_OP(REAL_MUL, TYPE_BINARY_REAL_OP_PTR, "real.mul");
ADD_BINARY_OP(REAL_DIV, TYPE_BINARY_REAL_OP_PTR, "real.div");


ADD_TYPE(FunctionType, COMPARISON_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_BOOL_PTR)));
ADD_TYPE(FunctionType, COMPARISON_UINT_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_BOOL_PTR)));
ADD_TYPE(FunctionType, COMPARISON_REAL_OP, (FunctionType(TYPE_REAL_PAIR_PTR, TYPE_BOOL_PTR)));


#undef ADD_UNARY_OP
#undef ADD_BINARY_OP

// -------------------------------- Statements ------------------------------

const NoOpStmt STMT_NO_OP(toVector<StatementPtr>());
const NoOpStmtPtr STMT_NO_OP_PTR = NoOpStmtPtr(&STMT_NO_OP);

} // end namespace: lang
} // end namespace: core
} // end namespace: insieme


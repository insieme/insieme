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
#include <unordered_set>

#include "core/lang_basic.h"

#include "utils/container_utils.h"

namespace insieme {
namespace core {
namespace lang {


// collect list of all literals
namespace {

	typedef std::unordered_map<const string, LiteralPtr, boost::hash<string>> LiteralMap;

	LiteralMap buildInLiterals;
}

#define ADD_TYPE(TYPE, NAME, VALUE) \
	const TYPE TYPE_ ## NAME ## _VAL = VALUE; \
	const TYPE ## Ptr TYPE_ ## NAME = TYPE ## Ptr(&TYPE_ ## NAME ## _VAL); \
	const TYPE ## Ptr TYPE_ ## NAME ## _PTR = TYPE ## Ptr(&TYPE_ ## NAME ## _VAL);


// some most essential types ...
ADD_TYPE(TypeVariable, ALPHA, (TypeVariable("a")));
ADD_TYPE(TupleType, SINGLE_ALPHA_TUPLE, (TupleType(TYPE_ALPHA_PTR)));
ADD_TYPE(TupleType, ALPHA_PAIR, (TupleType(TYPE_ALPHA_PTR, TYPE_ALPHA_PTR)));

ADD_TYPE(TupleType, EMPTY_TUPLE, (TupleType()));

// -------------------------------- Unit Type -------------------------------


const Identifier TYPE_NAME_UNIT("unit");

bool isUnitType(const Type& type) {
	return type == TYPE_UNIT_VAL;
}

ADD_TYPE(UnitType, UNIT, GenericType(TYPE_NAME_UNIT));

// -------------------------------- Boolean Type -------------------------------

const Identifier TYPE_NAME_BOOL("bool");

bool isBoolType(const Type& type) {
	return type == TYPE_BOOL_VAL;
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

int getNumBytes(const GenericType& type) {
	assert( (isIntegerType(type) || isRealType(type)) && "Given argument is not a numeric type!");
	IntTypeParam param = type.getIntTypeParameter()[0];
	if (param.getType()==IntTypeParam::CONCRETE) {
		return param.getValue();
	}
	return -1;
}


IntType getIntType(unsigned short size) {
	return GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(size)));
}

IntType getGenIntType(char symbol) {
	return GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam(symbol)));
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
ADD_TYPE(IntType, INT_INF, (GenericType(TYPE_NAME_INT, toVector<TypePtr>(), toVector(IntTypeParam::INF))));


const Identifier TYPE_NAME_UINT("uint");

UIntType getUIntType(unsigned short size) {
	return GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(size)));
}

UIntType getGenUIntType(char symbol) {
	return GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam(symbol)));
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
ADD_TYPE(UIntType, UINT_INF, (GenericType(TYPE_NAME_UINT, toVector<TypePtr>(), toVector(IntTypeParam::INF))));


// -------------------------------- Real Types ------------------------------

const Identifier TYPE_NAME_REAL("real");

RealType getRealType(unsigned short size) {
	return GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(size)));
}

RealType getGenRealType(char symbol) {
	return GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam(symbol)));
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
ADD_TYPE(RealType, REAL_INF, (GenericType(TYPE_NAME_REAL, toVector<TypePtr>(), toVector(IntTypeParam::INF))));

// -------------------------------- Vector Types ------------------------------

bool isVectorType(const Type& type) {
    return type.getNodeType() == NT_VectorType;
}


// ------------------------ Character and String Types -----------------------

ADD_TYPE(GenericType, CHAR, GenericType("char"));
ADD_TYPE(GenericType, WCHAR, GenericType("wchar"));

// -------------------------------- Misc Types ------------------------------

ADD_TYPE(GenericType, STRING, GenericType("string"));
ADD_TYPE(GenericType, WSTRING, GenericType("wstring"));
ADD_TYPE(GenericType, VAR_LIST, GenericType("var_list"));


// -------------------------------- Constants ------------------------------

#define ADD_CONST(NAME, VALUE) \
	const Literal CONST_ ## NAME = VALUE; \
	const LiteralPtr CONST_ ## NAME ## _PTR = LiteralPtr(&CONST_ ## NAME); \
	namespace { auto dummy_const_ ## NAME = buildInLiterals.insert(std::make_pair(VALUE.getValue(), CONST_ ## NAME ## _PTR)); } \

//ADD_CONST(UINT_ZERO, Literal(TYPE_UINT_1_PTR, "0"));
//ADD_CONST(UINT_ONE, Literal(TYPE_UINT_1_PTR, "1"));

ADD_CONST(BOOL_TRUE, Literal(TYPE_BOOL_PTR, "true"));
ADD_CONST(BOOL_FALSE, Literal(TYPE_BOOL_PTR, "false"));

ADD_TYPE(RefType, REF_GEN, RefType(TYPE_ALPHA_PTR));
ADD_CONST(NULL_PTR, Literal(TYPE_REF_GEN, "null"));

#undef ADD_CONST

// -------------------------------- Operator ------------------------------

#define ADD_OP(Name, Type, Symbol) \
		const Operator OP_ ## Name ##_VAL(Type, Symbol); \
		const OperatorPtr OP_ ## Name ## _PTR = OperatorPtr(&OP_ ## Name ## _VAL); \
		const OperatorPtr OP_ ## Name = OperatorPtr(&OP_ ## Name ## _VAL); \
		namespace { auto dummy_op_ ## Name = buildInLiterals.insert(std::make_pair(Symbol, OP_ ## Name ## _PTR)); } \

ADD_TYPE(TupleType, EMPTY, TupleType());

// Unit function without input parameters
ADD_TYPE(FunctionType, NO_ARGS_OP, (FunctionType(TYPE_EMPTY_PTR, TYPE_UNIT_PTR)));

ADD_TYPE(TupleType, BOOL_SINGLE, TupleType(TYPE_BOOL_PTR));
ADD_TYPE(TupleType, BOOL_PAIR, TupleType(TYPE_BOOL_PTR,TYPE_BOOL_PTR));

ADD_TYPE(FunctionType, UNARY_BOOL_OP, (FunctionType(TYPE_BOOL_SINGLE_PTR, TYPE_BOOL_PTR)));
ADD_TYPE(FunctionType, BINARY_BOOL_OP, (FunctionType(TYPE_BOOL_PAIR_PTR, TYPE_BOOL_PTR)));

ADD_OP(BOOL_NOT, TYPE_UNARY_BOOL_OP_PTR, "bool.not");
ADD_OP(BOOL_AND, TYPE_BINARY_BOOL_OP_PTR, "bool.and");
ADD_OP(BOOL_OR, TYPE_BINARY_BOOL_OP_PTR, "bool.or");
ADD_OP(BOOL_EQ, TYPE_BINARY_BOOL_OP_PTR, "bool.eq");


ADD_TYPE(TupleType, INT_SINGLE, TupleType(TYPE_INT_GEN_PTR));
ADD_TYPE(TupleType, INT_PAIR, TupleType(TYPE_INT_GEN_PTR,TYPE_INT_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_INT_GEN_PTR)));

ADD_OP(INT_ADD, TYPE_BINARY_INT_OP_PTR, "int.add");
ADD_OP(INT_SUB, TYPE_BINARY_INT_OP_PTR, "int.sub");
ADD_OP(INT_MUL, TYPE_BINARY_INT_OP_PTR, "int.mul");
ADD_OP(INT_DIV, TYPE_BINARY_INT_OP_PTR, "int.div");
ADD_OP(INT_MOD, TYPE_BINARY_INT_OP_PTR, "int.mod");

ADD_TYPE(TupleType, UINT_SINGLE, TupleType(TYPE_UINT_GEN_PTR));
ADD_TYPE(TupleType, UINT_PAIR, TupleType(TYPE_UINT_GEN_PTR,TYPE_UINT_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_UINT_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_UINT_GEN_PTR)));

// function type of job guards: (uint, uint) -> bool
ADD_TYPE(FunctionType, GUARD_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_UNIT_PTR)));

ADD_OP(UINT_ADD, TYPE_BINARY_UINT_OP_PTR, "uint.add");
ADD_OP(UINT_SUB, TYPE_BINARY_UINT_OP_PTR, "uint.sub");
ADD_OP(UINT_MUL, TYPE_BINARY_UINT_OP_PTR, "uint.mul");
ADD_OP(UINT_DIV, TYPE_BINARY_UINT_OP_PTR, "uint.div");
ADD_OP(UINT_MOD, TYPE_BINARY_UINT_OP_PTR, "uint.mod");

ADD_TYPE(TupleType, REAL_SINGLE, TupleType(TYPE_REAL_GEN_PTR));
ADD_TYPE(TupleType, REAL_PAIR, TupleType(TYPE_REAL_GEN_PTR,TYPE_REAL_GEN_PTR));

ADD_TYPE(FunctionType, BINARY_REAL_OP, (FunctionType(TYPE_REAL_PAIR_PTR, TYPE_REAL_GEN_PTR)));

ADD_OP(REAL_ADD, TYPE_BINARY_REAL_OP_PTR, "real.add");
ADD_OP(REAL_SUB, TYPE_BINARY_REAL_OP_PTR, "real.sub");
ADD_OP(REAL_MUL, TYPE_BINARY_REAL_OP_PTR, "real.mul");
ADD_OP(REAL_DIV, TYPE_BINARY_REAL_OP_PTR, "real.div");


ADD_TYPE(FunctionType, COMPARISON_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_BOOL_PTR)));

ADD_OP(INT_EQ, TYPE_COMPARISON_INT_OP_PTR, "int.eq");
ADD_OP(INT_NE, TYPE_COMPARISON_INT_OP_PTR, "int.ne");
ADD_OP(INT_LT, TYPE_COMPARISON_INT_OP_PTR, "int.lt");
ADD_OP(INT_GT, TYPE_COMPARISON_INT_OP_PTR, "int.gt");
ADD_OP(INT_LE, TYPE_COMPARISON_INT_OP_PTR, "int.le");
ADD_OP(INT_GE, TYPE_COMPARISON_INT_OP_PTR, "int.ge");

ADD_TYPE(FunctionType, COMPARISON_UINT_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_BOOL_PTR)));

ADD_OP(UINT_EQ, TYPE_COMPARISON_UINT_OP_PTR, "uint.eq");
ADD_OP(UINT_NE, TYPE_COMPARISON_UINT_OP_PTR, "uint.ne");
ADD_OP(UINT_LT, TYPE_COMPARISON_UINT_OP_PTR, "uint.lt");
ADD_OP(UINT_GT, TYPE_COMPARISON_UINT_OP_PTR, "uint.gt");
ADD_OP(UINT_LE, TYPE_COMPARISON_UINT_OP_PTR, "uint.le");
ADD_OP(UINT_GE, TYPE_COMPARISON_UINT_OP_PTR, "uint.ge");

ADD_TYPE(FunctionType, COMPARISON_REAL_OP, (FunctionType(TYPE_REAL_PAIR_PTR, TYPE_BOOL_PTR)));

ADD_OP(REAL_EQ, TYPE_COMPARISON_REAL_OP_PTR, "real.eq");
ADD_OP(REAL_NE, TYPE_COMPARISON_REAL_OP_PTR, "real.ne");
ADD_OP(REAL_LT, TYPE_COMPARISON_REAL_OP_PTR, "real.lt");
ADD_OP(REAL_GT, TYPE_COMPARISON_REAL_OP_PTR, "real.gt");
ADD_OP(REAL_LE, TYPE_COMPARISON_REAL_OP_PTR, "real.le");
ADD_OP(REAL_GE, TYPE_COMPARISON_REAL_OP_PTR, "real.ge");

// --- Bitwise ---

ADD_TYPE(FunctionType, BITWISE_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_INT_GEN_PTR)));

ADD_OP(INT_NOT, TYPE_BITWISE_INT_OP_PTR, "int.not");
ADD_OP(INT_AND, TYPE_BITWISE_INT_OP_PTR, "int.and");
ADD_OP(INT_OR,  TYPE_BITWISE_INT_OP_PTR, "int.or");
ADD_OP(INT_XOR, TYPE_BITWISE_INT_OP_PTR, "int.xor");

ADD_TYPE(FunctionType, SHIFT_INT_OP, (FunctionType(TYPE_INT_PAIR_PTR, TYPE_INT_GEN_PTR)));

ADD_OP(INT_LEFT_SHIFT, TYPE_SHIFT_INT_OP_PTR, "int.leftShift");
ADD_OP(INT_RIGHT_SHIFT, TYPE_SHIFT_INT_OP_PTR, "int.rightShift");

ADD_TYPE(FunctionType, BITWISE_UINT_OP, (FunctionType(TYPE_UINT_PAIR_PTR, TYPE_UINT_GEN_PTR)));

ADD_OP(UINT_NOT, TYPE_BITWISE_UINT_OP_PTR, "uint.not");
ADD_OP(UINT_AND, TYPE_BITWISE_UINT_OP_PTR, "uint.and");
ADD_OP(UINT_OR,  TYPE_BITWISE_UINT_OP_PTR, "uint.or");
ADD_OP(UINT_XOR, TYPE_BITWISE_UINT_OP_PTR, "uint.xor");

ADD_TYPE(TupleType, INT_UINT_PAIR, (TupleType(TYPE_UINT_GEN_PTR,TYPE_INT_GEN_PTR)));
ADD_TYPE(FunctionType, SHIFT_UINT_OP, (FunctionType(TYPE_INT_UINT_PAIR_PTR, TYPE_UINT_GEN_PTR)));

ADD_OP(UINT_LEFT_SHIFT, TYPE_SHIFT_UINT_OP_PTR, "uint.leftShift");
ADD_OP(UINT_RIGHT_SHIFT, TYPE_SHIFT_UINT_OP_PTR, "uint.rightShift");

// --- References ---

ADD_TYPE(RefType, REF_ALPHA, (RefType(TYPE_ALPHA_PTR)));

ADD_TYPE(TupleType, ALPHA_SINGLE, (TupleType(TYPE_ALPHA_PTR)));
ADD_TYPE(TupleType, REF_ALPHA_SINGLE, (TupleType(TYPE_REF_ALPHA_PTR)));
ADD_TYPE(TupleType, REF_ALPHA_ALPHA_PAIR, (TupleType(TYPE_REF_ALPHA_PTR, TYPE_ALPHA_PTR)));

ADD_TYPE(FunctionType, OP_ALLOCATION, (FunctionType(TYPE_ALPHA_SINGLE_PTR, TYPE_REF_ALPHA_PTR)));

ADD_OP(REF_VAR, TYPE_OP_ALLOCATION_PTR, "ref.var");
ADD_OP(REF_NEW, TYPE_OP_ALLOCATION_PTR, "ref.new");

ADD_TYPE(FunctionType, OP_FREE, (FunctionType(TYPE_REF_ALPHA_SINGLE_PTR, TYPE_UNIT_PTR)));
ADD_OP(REF_DELETE, TYPE_OP_FREE_PTR, "ref.delete");

ADD_TYPE(FunctionType, OP_ASSIGN, (FunctionType(TYPE_REF_ALPHA_ALPHA_PAIR_PTR, TYPE_UNIT_PTR)));
ADD_OP(REF_ASSIGN, TYPE_OP_ASSIGN_PTR, "ref.assign");

ADD_TYPE(FunctionType, OP_DEREF, (FunctionType(TYPE_REF_ALPHA_SINGLE_PTR, TYPE_ALPHA_PTR)));
ADD_OP(REF_DEREF, TYPE_OP_DEREF_PTR, "ref.deref");


// --- Array ---

IntTypeParam intParamN = IntTypeParam::getVariableIntParam('n');

ADD_TYPE(ArrayType, ARRAY_GEN, (ArrayType(TYPE_ALPHA_PTR, intParamN)));
ADD_TYPE(VectorType, VEC_GEN, (VectorType(TYPE_ALPHA_PTR, intParamN)));
ADD_TYPE(VectorType, VEC_INT_GEN, (VectorType(TYPE_UINT_GEN_PTR, intParamN)));

ADD_TYPE(TupleType, SUBSCRIPT_ARGUMENT, (TupleType(TYPE_ARRAY_GEN_PTR, TYPE_VEC_INT_GEN_PTR)));
ADD_TYPE(FunctionType, OP_ARRAY_SUBSCRIPT, (FunctionType(TYPE_SUBSCRIPT_ARGUMENT, TYPE_ALPHA_PTR)));

// SUBSCRIPT SINGLE
ADD_TYPE(TupleType, SUBSCRIPT_SINGLE_ARGUMENT, (TupleType(TYPE_VEC_GEN_PTR, TYPE_INT_GEN_PTR)));
ADD_TYPE(FunctionType, OP_ARRAY_SUBSCRIPT_SINGLE, (FunctionType(TYPE_SUBSCRIPT_SINGLE_ARGUMENT, TYPE_ALPHA_PTR)));

ADD_OP(SUBSCRIPT, TYPE_OP_ARRAY_SUBSCRIPT, "subscript");
ADD_OP(SUBSCRIPT_SINGLE, TYPE_OP_ARRAY_SUBSCRIPT_SINGLE, "subscript_single");

ADD_TYPE(TupleType, SINGLE_ARRAY_GEN, (TupleType(TYPE_ARRAY_GEN_PTR)));
ADD_TYPE(FunctionType, OP_ARRAY_LENGTH, (FunctionType(TYPE_SINGLE_ARRAY_GEN_PTR, TYPE_INT_GEN_PTR)));
ADD_OP(LENGTH, TYPE_OP_ARRAY_LENGTH, "length");

// --- Vector ---

ADD_TYPE(VectorType, VECTOR_GEN, (VectorType(TYPE_ALPHA_PTR, IntTypeParam::getVariableIntParam('a'))));
ADD_TYPE(FunctionType, OP_VECTOR_INIT_UNIFORM, (FunctionType(TYPE_SINGLE_ALPHA_TUPLE_PTR, TYPE_VECTOR_GEN_PTR)))

ADD_OP(VECTOR_INIT_UNIFORM, TYPE_OP_VECTOR_INIT_UNIFORM, "vector.initUniform");

ADD_TYPE(FunctionType, OP_VECTOR_INIT_UNDEFINED, (FunctionType(TYPE_EMPTY_TUPLE_PTR, TYPE_VECTOR_GEN_PTR)))
ADD_OP(VECTOR_INIT_UNDEFINED, TYPE_OP_VECTOR_INIT_UNDEFINED, "vector.initUndefined");

// --- Channels ---

ADD_TYPE(ChannelType, CHANNEL_GEN, (ChannelType(TYPE_ALPHA_PTR, IntTypeParam::getVariableIntParam('n'))));
ADD_TYPE(TupleType, SINGLE_CHANNEL_GEN, (TupleType(TYPE_CHANNEL_GEN)));


//ADD_TYPE(FunctionType, OP_SEND, (FunctionType(SINGLE_CHANNEL_GEN)));
//ADD_OP(SEND);
//ADD_OP(TRY_SEND);
//ADD_OP(RECV);
//ADD_OP(TRY_RECV);
//ADD_OP(EMPTY);

// --- VAR_LIST packing ---
ADD_TYPE(FunctionType, OP_VAR_LIST_PACK, FunctionType(TYPE_SINGLE_ALPHA_TUPLE_PTR, TYPE_VAR_LIST_PTR));
ADD_OP(VAR_LIST_PACK, TYPE_OP_VAR_LIST_PACK_PTR, "pack");

// ---------------------------- Parallel Constructs -------------------------

const Identifier TYPE_NAME_JOB("Job");

ADD_TYPE(GenericType, JOB, GenericType(TYPE_NAME_JOB));
ADD_TYPE(GenericType, THREAD_GROUP, GenericType("ThreadGroup"));

ADD_TYPE(TupleType, PARALLEL_ARGUMENT, (TupleType(toVector<TypePtr>(TYPE_UINT_4_PTR, TYPE_UINT_4_PTR, TYPE_JOB_PTR))));
ADD_TYPE(FunctionType, OP_PARALLEL, (FunctionType(TYPE_PARALLEL_ARGUMENT_PTR, TYPE_THREAD_GROUP_PTR)));


ADD_OP(PARALLEL, TYPE_OP_PARALLEL_PTR ,"parallel");
ADD_OP(PARALLEL_DETACHED, TYPE_OP_PARALLEL_PTR ,"parallel&");

ADD_TYPE(TupleType, SINGLE_THREAD_GROUP, (TupleType(TYPE_THREAD_GROUP_PTR)));
ADD_TYPE(FunctionType, OP_MERGE, (FunctionType(TYPE_SINGLE_THREAD_GROUP_PTR, TYPE_UNIT_PTR)));

ADD_OP(MERGE, TYPE_OP_MERGE_PTR ,"merge");

ADD_TYPE(FunctionType, OP_MERGE_ALL, (FunctionType(TYPE_EMPTY_TUPLE, TYPE_UNIT_PTR)));
ADD_OP(MERGEALL, TYPE_OP_MERGE_ALL_PTR ,"mergeAll");

ADD_OP(BARRIER, TYPE_OP_MERGE_PTR ,"barrier");

ADD_TYPE(FunctionType, REDUCTION_FUNCTION, (FunctionType(TYPE_ALPHA_PAIR_PTR, TYPE_ALPHA_PTR)));
ADD_TYPE(TupleType, REDUCE_ARGUMENTS, (TupleType(toVector<TypePtr>(TYPE_THREAD_GROUP_PTR, TYPE_REDUCTION_FUNCTION_PTR, TYPE_ALPHA_PTR, TYPE_ALPHA_PTR))));
ADD_TYPE(FunctionType, OP_REDUCE, (FunctionType(TYPE_REDUCE_ARGUMENTS_PTR, TYPE_ALPHA_PTR)));

ADD_OP(REDUCE, TYPE_OP_REDUCE_PTR ,"reduce");

// TODO: with type parser!
//ADD_OP(REDISTRIBUTE,,"redistribute");
//ADD_OP(PFOR,,"pfor");


// --- if-then-else ---
ADD_TYPE(FunctionType, LAZY_GEN, (FunctionType(TYPE_EMPTY_TUPLE_PTR, TYPE_ALPHA_PTR)));
ADD_TYPE(TupleType, ITE_ARGUMENTS, (TupleType(toVector<TypePtr>(TYPE_BOOL_PTR, TYPE_LAZY_GEN_PTR, TYPE_LAZY_GEN_PTR))));
ADD_TYPE(FunctionType, ITE_TYPE, (FunctionType(TYPE_ITE_ARGUMENTS_PTR, TYPE_ALPHA_PTR)));

ADD_OP(ITE, TYPE_ITE_TYPE, "ite");

#undef ADD_OP

// -------------------------------- Statements ------------------------------

const NoOpStmt STMT_NO_OP(toVector<StatementPtr>());
const NoOpStmtPtr STMT_NO_OP_PTR = NoOpStmtPtr(&STMT_NO_OP);


// ---------------------------------- Utility -------------------------------

bool isBuildIn(const string& value) {
	// check whether identifier is within the map
	return buildInLiterals.find(value) != buildInLiterals.end();
}

bool isBuildIn(const LiteralPtr& literal) {
	// check whether the literal is pointing to null ...
	if (!literal) {
		return false;
	}
	// lookup literal within map ..
	auto pos = buildInLiterals.find(literal->getValue());

	// see whether something has been found + compare associated element
	return pos != buildInLiterals.end() && *(*pos).second == *literal;
}


LiteralPtr getBuildInForValue(const string& value) {
	auto pos = buildInLiterals.find(value);
	if (pos == buildInLiterals.end()) {
		return LiteralPtr();
	}
	return (*pos).second;
}


} // end namespace: lang
} // end namespace: core
} // end namespace: insieme


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

#include <gtest/gtest.h>

#include <boost/functional/hash.hpp>

#include "insieme/core/ast_address.h"
#include "insieme/core/ast_builder.h"
#include "insieme/core/ast_visitor.h"
#include "insieme/core/type_utils.h"

#include "insieme/core/checks/ir_checks.h"
#include "insieme/core/checks/typechecks.h"

#include "insieme/core/parser/ir_parse.h"


namespace insieme {
namespace core {


bool unifyable(const TypePtr& typeA, const TypePtr& typeB) {
	return isUnifyable(typeA, typeB);
}

bool matchable(const TypePtr& pattern, const TypePtr& type) {
	return isMatching(pattern, type);
}

bool notUnifable(const TypePtr& typeA, const TypePtr& typeB) {
	return !isUnifyable(typeA, typeB);
}

bool notMatchable(const TypePtr& pattern, const TypePtr& type) {
	return !isMatching(pattern, type);
}

TEST(TypeUtils, Substitution) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	IntTypeParam paramVarX = IntTypeParam::getVariableIntParam('x');
	IntTypeParam paramVarY = IntTypeParam::getVariableIntParam('y');

	TypePtr constType = builder.genericType("constType");
	IntTypeParam constParam = IntTypeParam::getConcreteIntParam(15);

	TypePtr typeA = builder.genericType("type", toVector<TypePtr>(varA), toVector<IntTypeParam>(paramVarX));
	TypePtr typeB = builder.genericType("type", toVector<TypePtr>(varA, varB), toVector<IntTypeParam>(paramVarX, paramVarY));
	TypePtr typeC = builder.genericType("type", toVector<TypePtr>(typeB, varB), toVector<IntTypeParam>(paramVarY, paramVarY));


	EXPECT_EQ("'A", toString(*varA));
	EXPECT_EQ("'B", toString(*varB));
	EXPECT_EQ("#x", toString(paramVarX));
	EXPECT_EQ("#y", toString(paramVarY));

	EXPECT_EQ("constType", toString(*constType));
	EXPECT_EQ("15", toString(constParam));

	EXPECT_EQ("type<'A,#x>", toString(*typeA));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*typeB));
	EXPECT_EQ("type<type<'A,'B,#x,#y>,'B,#y,#y>", toString(*typeC));

	// test empty substitution
	auto all = toVector<TypePtr>(varA, varB, typeA, typeB, typeC);
	Substitution identity;
	for_each(all, [&](const TypePtr& cur) {
		EXPECT_EQ(cur, identity.applyTo(manager, cur));
	});

	// test one variable replacement
	Substitution substitution(varA, varB);
	EXPECT_EQ(varB, substitution.applyTo(manager, varA));
	EXPECT_EQ(varB, substitution.applyTo(manager, varB));

	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'B,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'B,'B,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'B,'B,#x,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one variable replacement
	substitution = Substitution(varA, constType);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,'B,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,'B,#x,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one int type parameter replacement
	substitution = Substitution(paramVarX, paramVarY);
	EXPECT_EQ(paramVarY, substitution.applyTo(paramVarX));
	EXPECT_EQ(paramVarY, substitution.applyTo(paramVarY));

	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#y>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,'B,#y,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,'B,#y,#y>,'B,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// test one int type parameter replacement
	substitution = Substitution(paramVarY, constParam);
	EXPECT_EQ(paramVarX, substitution.applyTo(paramVarX));
	EXPECT_EQ(constParam, substitution.applyTo(paramVarY));

	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,'B,#x,15>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,'B,#x,15>,'B,15,15>", toString(*substitution.applyTo(manager, typeC)));


	// add replacement for variable B
	substitution = Substitution(varA, constType);
	substitution.addMapping(varB, typeA);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A,#x>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A,#x>,#x,#y>,type<'A,#x>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));

	// override replacement for second variable
	substitution.addMapping(varB, typeB);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A,'B,#x,#y>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A,'B,#x,#y>,#x,#y>,type<'A,'B,#x,#y>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));


	// remove one mapping
	substitution.remMappingOf(varA);
	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B,#x,#y>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A,#x>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,type<'A,'B,#x,#y>,#x,#y>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,type<'A,'B,#x,#y>,#x,#y>,type<'A,'B,#x,#y>,#y,#y>", toString(*substitution.applyTo(manager, typeC)));


	// test substitution composition
	Substitution subA(varA, typeB);

	Substitution subB(varB, constType);
	subB.addMapping(paramVarX, constParam);


	EXPECT_EQ("{AP('A)=AP(type<'A,'B,#x,#y>)}", toString(subA.getMapping()));
	EXPECT_EQ("{}", toString(subA.getIntTypeParamMapping()));
	EXPECT_EQ("{AP('B)=AP(constType)}", toString(subB.getMapping()));
	EXPECT_EQ("{#x=15}", toString(subB.getIntTypeParamMapping()));

	Substitution combinedAA = Substitution::compose(manager, subA, subA);
	Substitution combinedAB = Substitution::compose(manager, subA, subB);
	Substitution combinedBA = Substitution::compose(manager, subB, subA);
	Substitution combinedBB = Substitution::compose(manager, subB, subB);

	EXPECT_EQ("{AP('A)=AP(type<'A,constType,15,#y>), AP('B)=AP(constType)}", toString(combinedAB.getMapping()));
	EXPECT_EQ("{AP('A)=AP(type<'A,'B,#x,#y>), AP('B)=AP(constType)}", toString(combinedBA.getMapping()));
	EXPECT_EQ("{AP('B)=AP(constType)}", toString(combinedBB.getMapping()));

	EXPECT_EQ("{}", toString(combinedAA.getIntTypeParamMapping()));
	EXPECT_EQ("{#x=15}", toString(combinedAB.getIntTypeParamMapping()));
	EXPECT_EQ("{#x=15}", toString(combinedBA.getIntTypeParamMapping()));
	EXPECT_EQ("{#x=15}", toString(combinedBB.getIntTypeParamMapping()));
}

TEST(TypeUtils, Unification) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	TypePtr constType = builder.genericType("constType");

	TypePtr typeA = builder.genericType("type", toVector<TypePtr>(varA));
	TypePtr typeB = builder.genericType("type", toVector<TypePtr>(varA, varB));
	TypePtr typeC = builder.genericType("type", toVector<TypePtr>(typeB, varB));


	// simple case - unify the same
	EXPECT_TRUE(isUnifyable(varA, varA));
	EXPECT_TRUE(isUnifyable(constType, constType));
	EXPECT_TRUE(isUnifyable(typeA, typeA));

	// unify two variables
	EXPECT_TRUE(isUnifyable(varA, varB));
	auto res = unify(manager, varA, varB);
	EXPECT_TRUE(res);
	if (res) {
		EXPECT_FALSE(res->getMapping().empty());
	}

	Substitution sub = *res;
	EXPECT_EQ("'B", toString(*sub.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*sub.applyTo(manager, varB)));


	// large example
	TypePtr varX = builder.typeVariable("x");
	TypePtr varY = builder.typeVariable("y");
	TypePtr varZ = builder.typeVariable("z");
	TypePtr varU = builder.typeVariable("u");

	{
		TypePtr termGY = builder.genericType("g", toVector(varY));
		TypePtr termA = builder.genericType("f", toVector(varX, termGY, varX));

		TypePtr termGU = builder.genericType("g", toVector(varU));
		TypePtr termHU = builder.genericType("h", toVector(varU));
		TypePtr termB = builder.genericType("f", toVector(varZ, termGU, termHU));

		EXPECT_EQ("f<'x,g<'y>,'x>", toString(*termA));
		EXPECT_EQ("f<'z,g<'u>,h<'u>>", toString(*termB));

		ASSERT_PRED2(unifyable, termA, termB);

		auto unifyingMap = *unify(manager, termA, termB);
		EXPECT_EQ("f<h<'u>,g<'u>,h<'u>>", toString(*unifyingMap.applyTo(manager, termA)));
		EXPECT_EQ("f<h<'u>,g<'u>,h<'u>>", toString(*unifyingMap.applyTo(manager, termB)));
		EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
	}

	{
		TypePtr termArrayY = builder.genericType("array", toVector(varY));
		TypePtr termA = builder.tupleType(toVector(varX, termArrayY, varX));

		TypePtr termArrayU = builder.genericType("array", toVector(varU));
		TypePtr termSetU = builder.genericType("vector", toVector(varU));
		TypePtr termB = builder.tupleType(toVector(varZ, termArrayU, termSetU));

		EXPECT_EQ("('x,array<'y>,'x)", toString(*termA));
		EXPECT_EQ("('z,array<'u>,vector<'u>)", toString(*termB));

		EXPECT_PRED2(unifyable, termA, termB);
		auto unifyingMap = *unify(manager, termA, termB);
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termA)));
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termB)));
		EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
	}
}


TEST(TypeUtils, Matching) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	// create some types to "play"
	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	TypePtr constType = builder.genericType("constType");

	TypePtr genTypeA = builder.genericType("type", toVector<TypePtr>(varA));
	TypePtr genTypeB = builder.genericType("type", toVector<TypePtr>(varB));

	TypePtr specializedType = builder.genericType("type", toVector<TypePtr>(constType));

	TypePtr genIntTypeA = builder.genericType("type", toVector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam('a')));
	TypePtr genIntTypeB = builder.genericType("type", toVector<TypePtr>(), toVector(IntTypeParam::getVariableIntParam('b')));
	TypePtr specIntType = builder.genericType("type", toVector<TypePtr>(), toVector(IntTypeParam::getConcreteIntParam(123)));


	// case: one side is a variable
	EXPECT_PRED2(matchable, varA, constType);
	EXPECT_PRED2(notMatchable, constType, varA);
	EXPECT_PRED2(unifyable, varA, constType);
	EXPECT_PRED2(unifyable, constType, varA);

	// case: both sides are variables
	EXPECT_PRED2(matchable, varA, varB);
	EXPECT_PRED2(matchable, varB, varA);
	EXPECT_PRED2(unifyable, varA, varB);
	EXPECT_PRED2(unifyable, varB, varA);

	// more complex case: type wit
	EXPECT_PRED2(matchable, genTypeA, specializedType);
	EXPECT_PRED2(notMatchable, specializedType, genTypeA);
	EXPECT_PRED2(matchable, genTypeB, specializedType);
	EXPECT_PRED2(notMatchable, specializedType, genTypeB);

	EXPECT_PRED2(unifyable, genTypeA, specializedType);
	EXPECT_PRED2(unifyable, specializedType, genTypeA);
	EXPECT_PRED2(unifyable, genTypeB, specializedType);
	EXPECT_PRED2(unifyable, specializedType, genTypeB);

	// case: int type parameter
	EXPECT_PRED2(matchable, genIntTypeA, specIntType);
	EXPECT_PRED2(notMatchable, specIntType, genIntTypeA);
	EXPECT_PRED2(matchable, genIntTypeB, specIntType);
	EXPECT_PRED2(notMatchable, specIntType, genIntTypeB);

	EXPECT_PRED2(unifyable, genIntTypeA, specIntType);
	EXPECT_PRED2(unifyable, specIntType, genIntTypeA);
	EXPECT_PRED2(unifyable, genIntTypeB, specIntType);
	EXPECT_PRED2(unifyable, specIntType, genIntTypeB);

	// check result of matching process
	auto unifier = *match(manager, genTypeA, specializedType);
	EXPECT_EQ(*unifier.applyTo(manager, genTypeA), *unifier.applyTo(manager, specializedType));
}


TEST(TypeUtils, SubTyping) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	// check sub-type relation between int and uint

	TypePtr int1 = manager.basic.getInt1();
	TypePtr int2 = manager.basic.getInt2();
	TypePtr int4 = manager.basic.getInt4();
	TypePtr int8 = manager.basic.getInt8();
	TypePtr intInf = manager.basic.getIntInf();

	TypePtr uint1 = manager.basic.getUInt1();
	TypePtr uint2 = manager.basic.getUInt2();
	TypePtr uint4 = manager.basic.getUInt4();
	TypePtr uint8 = manager.basic.getUInt8();
	TypePtr uintInf = manager.basic.getUIntInf();


	EXPECT_TRUE(isMatching(int8, int4, false));
	EXPECT_TRUE(isMatching(int8, int4, true));

	EXPECT_TRUE(isMatching(intInf, int4));
	EXPECT_FALSE(isMatching(int4, intInf));

	// check some cases
	EXPECT_TRUE(isMatching(int1, int1));
	EXPECT_TRUE(isMatching(int2, int2));
	EXPECT_TRUE(isMatching(int4, int4));
	EXPECT_TRUE(isMatching(int8, int8));
	EXPECT_TRUE(isMatching(intInf, intInf));


	EXPECT_TRUE(isMatching(int2, int1));
	EXPECT_FALSE(isMatching(int1, int2));

	EXPECT_TRUE(isMatching(int4, int2));
	EXPECT_FALSE(isMatching(int2, int4));

	EXPECT_TRUE(isMatching(int8, int4));
	EXPECT_FALSE(isMatching(int4, int8));

	EXPECT_TRUE(isMatching(intInf, int8));
	EXPECT_FALSE(isMatching(int8, intInf));


	EXPECT_TRUE(isMatching(uint1, uint1));
	EXPECT_TRUE(isMatching(uint2, uint2));
	EXPECT_TRUE(isMatching(uint4, uint4));
	EXPECT_TRUE(isMatching(uint8, uint8));
	EXPECT_TRUE(isMatching(uintInf, uintInf));

	EXPECT_TRUE(isMatching(uint2, uint1));
	EXPECT_FALSE(isMatching(uint1, uint2));

	EXPECT_TRUE(isMatching(uint4, uint2));
	EXPECT_FALSE(isMatching(uint2, uint4));

	EXPECT_TRUE(isMatching(uint8, uint4));
	EXPECT_FALSE(isMatching(uint4, uint8));

	EXPECT_TRUE(isMatching(uintInf, uint8));
	EXPECT_FALSE(isMatching(uint8, uintInf));

	// cross signed / unsigned tests
	EXPECT_TRUE(isMatching(int8, uint4));
	EXPECT_FALSE(isMatching(int8, uint8));

	EXPECT_TRUE(isMatching(int2, uint1));
	EXPECT_FALSE(isMatching(int4, uint8));

	EXPECT_TRUE(isMatching(intInf, uintInf));
	EXPECT_FALSE(isMatching(uintInf, intInf));

}


TEST(TypeUtils, IntParamUnification) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypePtr typeAx = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getVariableIntParam('x')));
	TypePtr typeA3 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getConcreteIntParam(3)));
	TypePtr typeA4 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParam>(IntTypeParam::getConcreteIntParam(4)));

	EXPECT_PRED2(unifyable, typeA3, typeAx);
	EXPECT_PRED2(notUnifable, typeA3, typeA4);

	Substitution res = *unify(manager, typeA3, typeAx);
	EXPECT_EQ(*res.applyTo(manager, typeA3), *res.applyTo(manager, typeAx));
	EXPECT_EQ(*typeA3, *res.applyTo(manager, typeAx));
}

typedef std::unordered_set<TypeVariablePtr, hash_target<TypeVariablePtr>, equal_target<TypeVariablePtr>> VariableSet;
typedef std::unordered_set<IntTypeParam, boost::hash<IntTypeParam>> ParamSet;

VariableSet getTypeVariables(const TypePtr& ptr) {
	VariableSet res;
	visitAllNodesOnce(ptr, [&res](const NodePtr& node) {
		if (node->getNodeType() == NT_TypeVariable) {
			res.insert(static_pointer_cast<const TypeVariable>(node));
		}
	}, true);
	return res;
}

ParamSet getParamVariables(const TypePtr& ptr) {
	ParamSet res;
	visitAllNodesOnce(ptr, [&res](const NodePtr& node) {
		if (node->getNodeType() == NT_GenericType) {
			GenericTypePtr genType = static_pointer_cast<const GenericType>(node);
			for_each(genType->getIntTypeParameter(), [&res](const IntTypeParam& cur) {
				if (cur.getType() == IntTypeParam::VARIABLE) {
					res.insert(cur);
				}
			});
		}
	}, true);
	return res;
}


TEST(TypeUtils, ArrayVectorRelation) {

	NodeManager manager;

	TypePtr typeA = parse::parseType(manager, "(array<ref<char>,1>,var_list)");
	TypePtr typeB = parse::parseType(manager, "(vector<ref<char>,25>,var_list)");

	EXPECT_NE(typeA, typeB);

	EXPECT_PRED2(matchable, typeA, typeB);

}

TEST(TypeUtils, ReturnTypeDeduction) {

	NodeManager manager;

	// some variables and types
	TypePtr varA = TypeVariable::get(manager, "a");
	TypePtr varB = TypeVariable::get(manager, "b");

	TypePtr typeA = GenericType::get(manager, "typeA");
	TypePtr typeB = GenericType::get(manager, "typeB");

	TypePtr genA = GenericType::get(manager, "type", toVector<TypePtr>(varA));
	TypePtr genB = GenericType::get(manager, "type", toVector<TypePtr>(varB));

	TypePtr genSpecA = GenericType::get(manager, "type", toVector<TypePtr>(typeA));
	TypePtr genSpecB = GenericType::get(manager, "type", toVector<TypePtr>(typeB));

	// test some functions
	FunctionTypePtr funType;

	// a simple case
	funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(varA), varA);
	EXPECT_EQ("(('a)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA))));


	funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(varA, varB), varA);
	EXPECT_EQ("(('a,'b)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(typeA, typeB))));
	EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(typeB, typeA))));


	funType = FunctionType::get(manager, TypeList(), toVector<TypePtr>(genA, varA), varA);
	EXPECT_EQ("((type<'a>,'a)->'a)", toString(*funType));
	EXPECT_EQ("typeA", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecA, typeA))));
	EXPECT_EQ("typeB", toString(*deduceReturnType(funType, toVector<TypePtr>(genSpecB, typeB))));
	EXPECT_EQ("'a", toString(*deduceReturnType(funType, toVector<TypePtr>(genA, varA))));

}

TEST(TypeUtils, VariableSubstitutionBug) {

	// The basis of this Test case is the following type checker error:
	//		MSG: Invalid return type -
	// 				expected: uint<a>
	// 				actual:   uint<4>
	// 				function type: ((vector<'elem,l>,'res,(('elem,'res)->'res))->'res)
	//
	// This error occurs when the function is invoked using a literal as
	// its second argument and a generic integer operation is it's last.
	// The expected return type should be consistent with the typeo of the
	// second argument.

	// reconstruct test case
	NodeManager manager;
	ASTBuilder builder(manager);

	TypePtr intType = manager.basic.getUInt4();
	TypePtr vectorType = builder.vectorType(intType, IntTypeParam::getConcreteIntParam(8));
	TypePtr funType = parse::parseType(manager, "(vector<'elem,#l>,'res,('elem,'res)->'res)->'res");
	EXPECT_TRUE(funType);

	EXPECT_EQ(NT_VectorType, vectorType->getNodeType());
	EXPECT_EQ(NT_VectorType, static_pointer_cast<const FunctionType>(funType)->getArgumentTypes()[0]->getNodeType());

	LiteralPtr fun = Literal::get(manager, funType, "fun");
	LiteralPtr vector = Literal::get(manager, vectorType, "x");
	LiteralPtr zero = Literal::get(manager, intType, "0");
	LiteralPtr op = manager.basic.getUnsignedIntAdd();

	ExpressionPtr call = builder.callExpr(intType, fun, vector, zero, op);

	// run check
	CheckPtr callCheck = make_check<checks::CallExprTypeCheck>();
	auto res = check(call, callCheck);

	// there shouldn't be any errors
	EXPECT_TRUE(res.empty());
}

TEST(TypeUtils, ReturnTypeBug) {

	// MSG: Invalid return type
	//		- expected: vector<'res,#l>, actual: vector<uint<4>,3>
	//		- function type: ((vector<'elem,#l>,vector<'elem,#l>)->vector<'res,#l>)
	//
	// => occurs in conjunction with the vector.pointwise operator

	// build a pointwise sum ...
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypePtr uint4 = manager.basic.getUInt4();
	ExpressionPtr add = manager.basic.getOperator(uint4, lang::BasicGenerator::Add);
	ExpressionPtr pointwise = builder.callExpr(manager.basic.getVectorPointwise(), add);

//	EXPECT_EQ("", toString(*add->getType()));
//	EXPECT_EQ("", toString(*pointwise->getType()));

}

TEST(TypeUtils, AutoTypeInference_ArrayInitCall) {

	// The Bug:
	// 		Unable to deduce return type for call to function of type
	//		(('elem,uint<8>)->array<'elem,1>) using arguments
	//		ref<struct<top:ref<array<ref<rec 'elem.{'elem=struct<value:ref<int<4>>,next:ref<array<ref<'elem>,1>>>}>,1>>>>, uint<8>

	// The reason:
	// 		within the element type the same variable 'elem is used as within the
	//		function type => leading to a mess

	// The fix:
	//		before trying to match the given arguments to the function parameters
	//		all type variables are replaced by fresh variables.

	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();
	const lang::BasicGenerator& basic = manager.basic;

	// get element type
	TypePtr elementType = builder.genericType("Set", toVector<TypePtr>(builder.typeVariable("elem")));

	// create the call
	ExpressionPtr element = builder.literal(elementType, "X");
	ExpressionPtr size = builder.literal(basic.getUInt8(), "15");
	ExpressionPtr res = builder.callExpr(basic.getArrayCreate1D(), element, size);

	// check infered type
	TypePtr resType = builder.arrayType(elementType, IntTypeParam::getConcreteIntParam(1));
	EXPECT_EQ(*resType, *res->getType());

}


} // end namespace core
} // end namespace insieme


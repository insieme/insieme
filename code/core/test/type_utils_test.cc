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

TEST(TypeUtils, Substitution) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypeVariablePtr varA = builder.typeVariable("A");
	TypeVariablePtr varB = builder.typeVariable("B");

	TypePtr constType = builder.genericType("constType");

	TypePtr typeA = builder.genericType("type", toVector<TypePtr>(varA));
	TypePtr typeB = builder.genericType("type", toVector<TypePtr>(varA, varB));
	TypePtr typeC = builder.genericType("type", toVector<TypePtr>(typeB, varB));


	EXPECT_EQ("'A", toString(*varA));
	EXPECT_EQ("'B", toString(*varB));
	EXPECT_EQ("constType", toString(*constType));
	EXPECT_EQ("type<'A>", toString(*typeA));
	EXPECT_EQ("type<'A,'B>", toString(*typeB));
	EXPECT_EQ("type<type<'A,'B>,'B>", toString(*typeC));

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
	EXPECT_EQ("type<'B>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'B,'B>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'B,'B>,'B>", toString(*substitution.applyTo(manager, typeC)));

	// test one variable replacement
	substitution = Substitution(varA, constType);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("'B", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,'B>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,'B>,'B>", toString(*substitution.applyTo(manager, typeC)));

	// add replacement for variable B
	substitution.addMapping(varB, typeA);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A>>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A>>,type<'A>>", toString(*substitution.applyTo(manager, typeC)));

	// override replacement for second variable
	substitution.addMapping(varB, typeB);
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<constType>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<constType,type<'A,'B>>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<constType,type<'A,'B>>,type<'A,'B>>", toString(*substitution.applyTo(manager, typeC)));


	// remove one mapping
	substitution.remMappingOf(varA);
	EXPECT_EQ("'A", toString(*substitution.applyTo(manager, varA)));
	EXPECT_EQ("type<'A,'B>", toString(*substitution.applyTo(manager, varB)));
	EXPECT_EQ("constType", toString(*substitution.applyTo(manager, constType)));
	EXPECT_EQ("type<'A>", toString(*substitution.applyTo(manager, typeA)));
	EXPECT_EQ("type<'A,type<'A,'B>>", toString(*substitution.applyTo(manager, typeB)));
	EXPECT_EQ("type<type<'A,type<'A,'B>>,type<'A,'B>>", toString(*substitution.applyTo(manager, typeC)));
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

		ASSERT_PRED2(isUnifyable, termA, termB);

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

		EXPECT_TRUE(isUnifyable(termA, termB));
		auto unifyingMap = *unify(manager, termA, termB);
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termA)));
		EXPECT_EQ("(vector<'u>,array<'u>,vector<'u>)", toString(*unifyingMap.applyTo(manager, termB)));
		EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
	}
}

bool unifyable(const TypePtr& typeA, const TypePtr& typeB) {
	return isUnifyable(typeA, typeB);
}

bool notUnifable(const TypePtr& typeA, const TypePtr& typeB) {
	return !isUnifyable(typeA, typeB);
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
	});
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
	});
	return res;
}


TEST(TypeUtils, FreeTypeVariableAssignment) {
	ASTBuilder builder;
	NodeManager& manager = builder.getNodeManager();

	TypeVariablePtr typeVar = builder.typeVariable("X");
	IntTypeParam varParam = IntTypeParam::getVariableIntParam('x');
	TypePtr typeA = builder.genericType("a", toVector<TypePtr>(typeVar), toVector(varParam));

	VariableSet vars;
	vars.insert(typeVar);

	ParamSet params;
	params.insert(varParam);

	EXPECT_EQ(vars, getTypeVariables(typeA));
	EXPECT_EQ(params, getParamVariables(typeA));

	auto res = makeTypeVariablesUnique(manager, typeA, typeA);
	TypePtr resA = res.first;
	TypePtr resB = res.second;

	EXPECT_EQ("a<'FV1,#a>", toString(*resA));
	EXPECT_EQ("a<'FV2,#b>", toString(*resB));

	EXPECT_NE(vars, getTypeVariables(resA));
	EXPECT_NE(vars, getTypeVariables(resB));
	EXPECT_NE(getTypeVariables(resA), getTypeVariables(resB));

	EXPECT_NE(params, getParamVariables(resA));
	EXPECT_NE(params, getParamVariables(resB));
	EXPECT_NE(getParamVariables(resA), getParamVariables(resB));
}


TEST(TypeUtils, ArrayVectorRelation) {

	NodeManager manager;

	TypePtr typeA = parse::parseType(manager, "(array<ref<char>,1>,var_list)");
	TypePtr typeB = parse::parseType(manager, "(vector<ref<char>,25>,var_list)");

	EXPECT_NE(typeA, typeB);

	EXPECT_PRED2(unifyable, typeA, typeB);

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

} // end namespace core
} // end namespace insieme


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

#include "insieme/core/types/unification.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace types {

	bool unifyable(const TypePtr& typeA, const TypePtr& typeB) {
		return isUnifyable(typeA, typeB);
	}

	bool notUnifable(const TypePtr& typeA, const TypePtr& typeB) {
		return !isUnifyable(typeA, typeB);
	}


	TEST(Unification, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

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
			EXPECT_EQ("f<h<'u>,g<'u>,h<'u>>", toString(*unifyingMap.applyTo(manager, termA))) << unifyingMap;
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


	TEST(Unification, IntTypeParam) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeAx = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(VariableIntTypeParam::get(manager, 'x')));
		TypePtr typeA3 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 3)));
		TypePtr typeA4 = builder.genericType("a", toVector<TypePtr>(), toVector<IntTypeParamPtr>(ConcreteIntTypeParam::get(manager, 4)));

		EXPECT_PRED2(unifyable, typeA3, typeAx);
		EXPECT_PRED2(notUnifable, typeA3, typeA4);

		Substitution res = *unify(manager, typeA3, typeAx);
		EXPECT_EQ(*res.applyTo(manager, typeA3), *res.applyTo(manager, typeAx));
		EXPECT_EQ(*typeA3, *res.applyTo(manager, typeAx));
	}


	TEST(TypeUtils, ListUnification) {

		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeA = builder.genericType("A");
		TypePtr typeB = builder.genericType("B");

		TypePtr varX = builder.typeVariable("X");
		TypePtr varY = builder.typeVariable("Y");

		TypePtr genAX = builder.genericType("T", toVector(typeA, varX));
		TypePtr genXA = builder.genericType("T", toVector(varX, typeA));

		TypePtr genYY = builder.genericType("T", toVector(varY, varY));


		auto res = unifyAll(manager, toVector(typeA, typeB));
		EXPECT_FALSE(res);

		EXPECT_TRUE(unify(manager, varX, varY));
		res = unifyAll(manager, toVector(varX, varY));
		EXPECT_TRUE(res);

		res = unifyAll(manager, toVector(genAX, genXA));
		EXPECT_TRUE(res);
		if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA));

		res = unifyAll(manager, toVector(genAX, genXA, genYY));
		EXPECT_TRUE(res);
		if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA));
		if (res) EXPECT_EQ(res->applyTo(genAX), res->applyTo(genYY));

		EXPECT_FALSE(unifyAll(manager, toVector(genAX, genXA, varX)));
	}

	TEST(TypeUtils, ListUnificationBug1) {

		// The Problem:
		//		the list [AP('a),AP(uint<4>),AP(int<4>)] can apparently be unified to [AP(uint<4>)]
		// 		=> this should not be the case
		//
		// Cause of the problem:
		//		After each step, the current unifier was not applied on the next type / the current unified type.
		//
		// Fix:
		//		This is now down.

		NodeManager manager;
		IRBuilder builder(manager);
		const lang::BasicGenerator& basic = manager.getLangBasic();

		TypePtr varA = builder.typeVariable("a");
		TypePtr uint4 = basic.getUInt4();
		TypePtr int4 = basic.getInt4();

		auto list = toVector(varA, uint4, int4);
		EXPECT_EQ("[AP('a),AP(uint<4>),AP(int<4>)]", toString(list));

		auto res = unifyAll(manager, list);
		EXPECT_FALSE(res);
	}

	TEST(TypeUtils, IdenticalIntTypeParameterVariables) {

		//
		// The Problem:
		//		The Type T<#a,#a> can apparently be unified with T<1,2> using {#a->1}
		//
		// Cause of the problem:
		//		The substitution derived from the first parameter (#a->1) is not applied to the
		//		remaining parameters of the same type.
		//
		// Fix:
		//		Now the substitution is applied.
		//

		NodeManager manager;
		IRBuilder builder(manager);

		IntTypeParamPtr varA = builder.variableIntTypeParam('a');
		TypePtr gen = builder.genericType("T", toVector<TypePtr>(), toVector(varA, varA));
		EXPECT_EQ("T<#a,#a>", toString(*gen));

		IntTypeParamPtr param1 = builder.concreteIntTypeParam(1);
		IntTypeParamPtr param2 = builder.concreteIntTypeParam(2);
		TypePtr concrete = builder.genericType("T", toVector<TypePtr>(), toVector(param1, param2));
		EXPECT_EQ("T<1,2>", toString(*concrete));

		// this should not work ...
		auto unifier = unify(manager, gen, concrete);
		EXPECT_FALSE(unifier);
		if (unifier) EXPECT_EQ("", toString(*unifier));

		// ... but this should
		concrete = builder.genericType("T", toVector<TypePtr>(), toVector(param1, param1));
		EXPECT_TRUE(unify(manager, gen, concrete));
		EXPECT_TRUE(unify(manager, concrete, gen));
	}


} // end namespace types
} // end namespace core
} // end namespace insieme

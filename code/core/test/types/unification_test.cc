/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <gtest/gtest.h>

#include "insieme/core/types/unification.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace types {

	bool unifiable(const TypePtr& typeA, const TypePtr& typeB) {
		return isUnifyable(typeA, typeB);
	}

	bool notUnifiable(const TypePtr& typeA, const TypePtr& typeB) {
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
		if(res) { EXPECT_FALSE(res->empty()); }

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

			ASSERT_PRED2(unifiable, termA, termB);

			auto unifyingMap = *unify(manager, termA, termB);
			EXPECT_EQ("f<h<'y>,g<'y>,h<'y>>", toString(*unifyingMap.applyTo(manager, termA))) << unifyingMap;
			EXPECT_EQ("f<h<'y>,g<'y>,h<'y>>", toString(*unifyingMap.applyTo(manager, termB)));
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

			EXPECT_PRED2(unifiable, termA, termB);
			auto unifyingMap = *unify(manager, termA, termB);
			EXPECT_EQ("(vector<'y>,array<'y>,vector<'y>)", toString(*unifyingMap.applyTo(manager, termA)));
			EXPECT_EQ("(vector<'y>,array<'y>,vector<'y>)", toString(*unifyingMap.applyTo(manager, termB)));
			EXPECT_EQ(unifyingMap.applyTo(manager, termA), unifyingMap.applyTo(manager, termB));
		}
	}

	TEST(Unification, GenericTypeVariables) {
		NodeManager manager;
		IRBuilder builder(manager);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};

		EXPECT_PRED2(unifiable, type("A"), type("A"));
		EXPECT_PRED2(unifiable, type("A"), type("'a"));
		EXPECT_PRED2(unifiable, type("A"), type("'a<>"));

		EXPECT_PRED2(unifiable, type("A<B>"), type("'a<B>"));
		EXPECT_PRED2(unifiable, type("A<B>"), type("'a<'b>"));

		EXPECT_PRED2(notUnifiable, type("A<C>"), type("'a<B>"));
		EXPECT_PRED2(notUnifiable, type("A"), type("'a<'b>"));
		EXPECT_PRED2(notUnifiable, type("A"), type("'a<'b,'c>"));

		auto sub = unify(manager, type("A<B,C>"), type("'a<'b,'c<>>"));
		ASSERT_TRUE(sub);
		EXPECT_EQ("{'b->B,'c<>->C,'a<'_,'_<>>->A}",toString(*sub));
		EXPECT_EQ("A<B,C>",toString(*(*sub)(type("'a<'b,'c<>>"))));
		EXPECT_EQ("B",toString(*(*sub)(type("'b"))));
		EXPECT_EQ("C",toString(*(*sub)(type("'c<>"))));
		EXPECT_EQ("A<B,B>",toString(*(*sub)(type("'a<'b,'b>"))));

	}


	TEST(Unification, IntTypeParam) {
		NodeManager manager;
		IRBuilder builder(manager);

		TypePtr typeAx = builder.parseType("a<'x>");
		TypePtr typeA3 = builder.parseType("a<3>");
		TypePtr typeA4 = builder.parseType("a<4>");

		EXPECT_PRED2(unifiable, typeA3, typeAx);
		EXPECT_PRED2(notUnifiable, typeA3, typeA4);

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
		if(res) { EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA)); }

		res = unifyAll(manager, toVector(genAX, genXA, genYY));
		EXPECT_TRUE(res);
		if(res) { EXPECT_EQ(res->applyTo(genAX), res->applyTo(genXA)); }
		if(res) { EXPECT_EQ(res->applyTo(genAX), res->applyTo(genYY)); }

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

		TypePtr varA = builder.typeVariable("a");
		TypePtr gen = builder.genericType("T", toVector<TypePtr>(varA, varA));
		EXPECT_EQ("T<'a,'a>", toString(*gen));

		TypePtr param1 = builder.genericType("a");
		TypePtr param2 = builder.genericType("b");
		TypePtr concrete = builder.genericType("T", toVector(param1, param2));
		EXPECT_EQ("T<a,b>", toString(*concrete));

		// this should not work ...
		auto unifier = unify(manager, gen, concrete);
		EXPECT_FALSE(unifier);
		if(unifier) { EXPECT_EQ("", toString(*unifier)); }

		// ... but this should
		concrete = builder.genericType("T", toVector(param1, param1));
		EXPECT_TRUE(unify(manager, gen, concrete));
		EXPECT_TRUE(unify(manager, concrete, gen));
	}


} // end namespace types
} // end namespace core
} // end namespace insieme

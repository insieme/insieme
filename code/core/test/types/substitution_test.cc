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
 */

#include <gtest/gtest.h>

#include "insieme/core/types/substitution.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace core {
namespace types {

	TEST(Substitution, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);

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
		for_each(all, [&](const TypePtr& cur) { EXPECT_EQ(cur, identity.applyTo(manager, cur)); });

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
		substitution = Substitution(varA, constType);
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


		// test substitution composition
		Substitution subA(varA, typeB);

		Substitution subB(varB, constType);


		EXPECT_EQ("{AP('A)=AP(type<'A,'B>)}", toString(subA.getVariableMapping()));
		EXPECT_EQ("{AP('B)=AP(constType)}", toString(subB.getVariableMapping()));

		Substitution combinedAA = Substitution::compose(manager, subA, subA);
		Substitution combinedAB = Substitution::compose(manager, subA, subB);
		Substitution combinedBA = Substitution::compose(manager, subB, subA);
		Substitution combinedBB = Substitution::compose(manager, subB, subB);

		EXPECT_PRED2(containsSubString, toString(combinedAB.getVariableMapping()), "AP('A)=AP(type<'A,constType>)");
		EXPECT_PRED2(containsSubString, toString(combinedAB.getVariableMapping()), "AP('B)=AP(constType)");
		EXPECT_PRED2(containsSubString, toString(combinedBA.getVariableMapping()), "AP('A)=AP(type<'A,'B>)");
		EXPECT_PRED2(containsSubString, toString(combinedBA.getVariableMapping()), "AP('B)=AP(constType)");
		EXPECT_EQ("{AP('B)=AP(constType)}", toString(combinedBB.getVariableMapping()));
	}


	TEST(Substitution, VariadicTypeVariables) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<TypeVariablePtr>();
		};
		auto vvar = [&](const std::string& code) {
			return type(code).as<VariadicTypeVariablePtr>();
		};

		Substitution sub;
		sub.addMapping(vvar("'a..."),toVector(var("'a1"),var("'a2")));
		sub.addMapping(var("'a1"),type("A"));
		sub.addMapping(var("'a2"), type("B"));

		EXPECT_EQ(type("(A,B)"), sub(type("(A,B)")));
		EXPECT_EQ(type("(A,B)"), sub(type("('a1,'a2)")));
		EXPECT_EQ(type("(A,B)"), sub(type("('a...)")));


		EXPECT_EQ(type("(A,B)->C"), sub(type("(A,B)->C")));
		EXPECT_EQ(type("(A,B)->C"), sub(type("('a1,'a2)->C")));
		EXPECT_EQ(type("(A,B)->C"), sub(type("('a...)->C")));

		EXPECT_EQ(type("<A,B>(A,B)->C"), sub(type("<A,B>(A,B)->C")));
		EXPECT_EQ(type("<A,B>(A,B)->C"), sub(type("<'a1,'a2>('a1,'a2)->C")));
		EXPECT_EQ(type("<A,B>(A,B)->C"), sub(type("<'a...>('a...)->C")));


		sub.addMapping(vvar("'b..."), toVector(var("'b1"), var("'b2"), var("'b3")));
		sub.addMapping(var("'b1"), type("A"));
		sub.addMapping(var("'b2"), type("B"));
		sub.addMapping(var("'b3"), type("C"));

		sub.addMapping(vvar("'c..."), TypeVariableList());
		

		EXPECT_EQ(type("(A,B)"),   sub(type("('a...)")));
		EXPECT_EQ(type("(A,B,C)"), sub(type("('b...)")));
		EXPECT_EQ(type("()"),      sub(type("('c...)")));

		EXPECT_EQ(type("(A,B)->C"),   sub(type("('a...)->C")));
		EXPECT_EQ(type("(A,B,C)->C"), sub(type("('b...)->C")));
		EXPECT_EQ(type("()->C"),      sub(type("('c...)->C")));

		EXPECT_EQ(type("<A,B>(A,B,C)->C"),   sub(type("<'a...>('b...)->C")));
		EXPECT_EQ(type("<A,B,C>(A,B,C)->C"), sub(type("<'b...>('b...)->C")));
		EXPECT_EQ(type("(A,B,C)->C"),        sub(type("<'c...>('b...)->C")));

	}

	TEST(Substitution, GenericTypeVariables) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<GenericTypeVariablePtr>();
		};


		Substitution sub(var("'a<'b>"), type("T<_>"));
		EXPECT_EQ("{'a<'_>->T}", toString(sub));

		EXPECT_EQ(type("T<A>"), sub(type("'a<A>")));
		EXPECT_EQ(type("T<X>"), sub(type("'a<X>")));
		EXPECT_EQ(type("'a<X,Y>"), sub(type("'a<X,Y>")));


		sub = Substitution(var("'a<'b,'c,'d<'e,'f>>"), type("T<A,B,C<D,E>>"));
		EXPECT_EQ("{'a<'_,'_,'_<'_,'_>>->T}", toString(sub));

		EXPECT_EQ(type("T<A,B,C<D,E>>"), sub(type("'a<A,B,C<D,E>>")));
		EXPECT_EQ(type("T<X,Y,Z<U,V>>"), sub(type("'a<X,Y,Z<U,V>>")));

		EXPECT_EQ(type("'a<X,Y,Z<U>>"), sub(type("'a<X,Y,Z<U>>")));
		EXPECT_EQ(type("'a<X,Y,Z>"), sub(type("'a<X,Y,Z>")));


		// chains
		sub = Substitution();
		sub.addMapping(var("'a<'b>"), type("T<'a>"));
		sub.addMapping(var("'b<'a>"), var("'a<'a>"));
		EXPECT_EQ("{'b<'_>->'a,'a<'_>->T}", toString(sub));

		EXPECT_EQ(type("T<A>"), sub(type("'a<A>")));
		EXPECT_EQ(type("T<A>"), sub(type("'b<A>")));
		EXPECT_EQ(type("T<C>"), sub(type("'b<C>")));
		EXPECT_EQ(type("'c<A>"), sub(type("'c<A>")));
	}

	TEST(Substitution, VariadicGenericTypeVariables) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<GenericTypeVariablePtr>();
		};
		auto vvar = [&](const std::string& code) {
			return type(code).as<VariadicGenericTypeVariablePtr>();
		};

		Substitution sub;
		sub.addMapping(vvar("'a...<'X>"), toVector(var("'a1<'X>"), var("'a2<'X>")));
		sub.addMapping(var("'a1<'X>"), type("A<'X>"));
		sub.addMapping(var("'a2<'X>"), type("B<'X>"));

		EXPECT_EQ(type("(A,B)"), sub(type("(A,B)")));
		EXPECT_EQ(type("(A<R>,B<S>)"), sub(type("('a1<R>,'a2<S>)")));
		EXPECT_EQ(type("(A<R>,B<R>)"), sub(type("('a...<R>)")));


		EXPECT_EQ(type("(A,B)->C"), sub(type("(A,B)->C")));
		EXPECT_EQ(type("(A<R>,B<S>)->C"), sub(type("('a1<R>,'a2<S>)->C")));
		EXPECT_EQ(type("(A<R>,B<R>)->C"), sub(type("('a...<R>)->C")));

		EXPECT_EQ(type("<A,B>(A,B)->C"), sub(type("<A,B>(A,B)->C")));
		EXPECT_EQ(type("<A<R>,B<S>>(A<R>,B<S>)->C"), sub(type("<'a1<R>,'a2<S>>('a1<R>,'a2<S>)->C")));
		EXPECT_EQ(type("<A<T>,B<T>>(A<R>,B<R>)->C"), sub(type("<'a...<T>>('a...<R>)->C")));


		sub.addMapping(vvar("'b...<'_>"), toVector(var("'b1<'_>"), var("'b2<'_>"), var("'b3<'_>")));
		sub.addMapping(var("'b1<'_>"), type("A<'_>"));
		sub.addMapping(var("'b2<'_>"), type("B<'_>"));
		sub.addMapping(var("'b3<'_>"), type("C<'_>"));

		sub.addMapping(vvar("'c...<'_>"), GenericTypeVariableList());


		EXPECT_EQ(type("(A<T>,B<T>)"), sub(type("('a...<T>)")));
		EXPECT_EQ(type("(A<T>,B<T>,C<T>)"), sub(type("('b...<T>)")));
		EXPECT_EQ(type("()"), sub(type("('c...<T>)")));

		EXPECT_EQ(type("(A<T>,B<T>)->C"), sub(type("('a...<T>)->C")));
		EXPECT_EQ(type("(A<T>,B<T>,C<T>)->C"), sub(type("('b...<T>)->C")));
		EXPECT_EQ(type("()->C"), sub(type("('c...<T>)->C")));

		EXPECT_EQ(type("<A<T>,B<T>>(A<S>,B<S>,C<S>)->C"), sub(type("<'a...<T>>('b...<S>)->C")));
		EXPECT_EQ(type("<A<T>,B<T>,C<T>>(A<S>,B<S>,C<S>)->C"), sub(type("<'b...<T>>('b...<S>)->C")));
		EXPECT_EQ(type("(A<S>,B<S>,C<S>)->C"), sub(type("<'c...<T>>('b...<S>)->C")));

	}

	TEST(Substitution, Combined) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto type = [&](const std::string& code) {
			return builder.parseType(code);
		};
		auto var = [&](const std::string& code) {
			return type(code).as<TypeVariablePtr>();
		};
		auto vvar = [&](const std::string& code) {
			return type(code).as<VariadicTypeVariablePtr>();
		};
		auto gvar = [&](const std::string& code) {
			return type(code).as<GenericTypeVariablePtr>();
		};
		auto vgvar = [&](const std::string& code) {
			return type(code).as<VariadicGenericTypeVariablePtr>();
		};

		Substitution sub;
		
		// some variables
		sub.addMapping(var("'a"),type("A"));
		sub.addMapping(var("'b"), type("B"));

		// one variadic variable
		sub.addMapping(var("'x1"), type("X1"));
		sub.addMapping(var("'x2"), type("X2"));
		sub.addMapping(vvar("'x..."), toVector( var("'x1"), var("'x2") ));

		sub.addMapping(gvar("'c<'_>"), type("C<'_>"));
		sub.addMapping(gvar("'d<'_>"), type("D<'_>"));

		sub.addMapping(gvar("'y1<'_>"), type("Y1<'_>"));
		sub.addMapping(gvar("'y2<'_>"), type("Y2<'_>"));
		sub.addMapping(gvar("'y3<'_>"), type("Y3<'_>"));
		sub.addMapping(vgvar("'y...<'_>"), toVector(gvar("'y1<'_>"), gvar("'y2<'_>"), gvar("'y3<'_>")));


		EXPECT_EQ(type("A"), sub(type("'a")));

		EXPECT_EQ(type("(Y1<T>,Y2<T>,Y3<T>)"), sub(type("('y...<T>)")));
		EXPECT_EQ(type("(Y1<C<A>>,Y2<C<A>>,Y3<C<A>>)"), sub(type("('y...<'c<'a>>)")));
		EXPECT_EQ(type("(Y1<C<X1,X2>>,Y2<C<X1,X2>>,Y3<C<X1,X2>>)"), sub(type("('y...<'c<'x...>>)")));
	}


} // end namespace types
} // end namespace core
} // end namespace insieme

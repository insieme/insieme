/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/datapath/datapath.h"

namespace insieme {
namespace core {
namespace datapath {

	TEST(DataPathBuilder, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);

		auto build = [&](const std::string& code) {
			return DataPathBuilder(builder.parseType(code));
		};

		TypePtr typeA = GenericType::get(mgr, "A");

		EXPECT_EQ("dp_root(type<R>)", toString(*build("R").getPath()));
		EXPECT_EQ("dp_member(dp_root(type<struct {hello:bool,dtor()}>), hello, type<bool>)", toString(*build("struct { hello: bool; }").member("hello").getPath()));
		EXPECT_EQ("dp_element(dp_root(type<array<int<4>,20>>), 12)", toString(*build("array<int<4>,20>").element(12).getPath()));
		EXPECT_EQ("dp_component(dp_root(type<(bool,int<4>,real<4>,R)>), 3, type<R>)", toString(*build("(bool,int<4>,real<4>,R)").component(3).getPath()));
		EXPECT_EQ("dp_parent(dp_root(type<struct B : [struct A {dtor()}] {dtor()}>), type<A>)", toString(*build("alias A = struct A {}; struct B : [ A ] {}").parent(typeA).getPath()));

	}

	TEST(DataPath, Basic) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		TypePtr root = builder.parseType(
				"let A = struct A {} in "
				"let E = struct E : [A] {} in "
				"let T = (int<4>,bool,int<4>,E) in "
				"let S = struct { test: T; } in "
				"array<S,50>"
		);

		TypePtr typeA = GenericType::get(mgr, "A");

		DataPath path(root);

		EXPECT_EQ("<array<struct {test:(int<4>,bool,int<4>,struct E : [struct A {dtor()}] {dtor()}),dtor()},50>>", toString(path));
		EXPECT_EQ("<array<struct {test:(int<4>,bool,int<4>,struct E : [struct A {dtor()}] {dtor()}),dtor()},50>>[4]", toString(path.element(4)));
		EXPECT_EQ("<array<struct {test:(int<4>,bool,int<4>,struct E : [struct A {dtor()}] {dtor()}),dtor()},50>>[4].test", toString(path.element(4).member("test")));

		// "u" after "c3" added to match the printer
		EXPECT_EQ("<array<struct {test:(int<4>,bool,int<4>,struct E : [struct A {dtor()}] {dtor()}),dtor()},50>>[4].test.c3u", toString(path.element(4).member("test").component(3)));
		EXPECT_EQ("<array<struct {test:(int<4>,bool,int<4>,struct E : [struct A {dtor()}] {dtor()}),dtor()},50>>[4].test.c3u.as<A>", toString(path.element(4).member("test").component(3).parent(typeA)));
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

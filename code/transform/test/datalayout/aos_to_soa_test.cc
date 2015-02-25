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

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/utils/logging.h"
#include "insieme/transform/datalayout/aos_to_soa.h"
#include "insieme/core/pattern/pattern.h"
#include "insieme/core/pattern/pattern_utils.h"

#include "insieme/analysis/defuse_collect.h"

namespace insieme {
namespace transform {

using namespace core;
namespace ia = insieme::analysis;

int numberOfCompoundStmts(const NodePtr root) {
	int cnt = 0;
	visitDepthFirst(root, [&](const CompoundStmtPtr& csp) {
		++cnt;
	});

	return cnt;
}

int countMarshalledAccesses(const NodePtr root) {
	pattern::TreePattern marshalledAccesses = pattern::irp::arrayRefElem1D(pattern::irp::refDeref(
			pattern::irp::compositeRefElem()) | pattern::irp::compositeMemberAccess(),
			pattern::any);

//	pattern::irp::matchAllPairs(marshalledAccesses, root, [&](const NodePtr& match, const pattern::NodeMatch& toll) {
//dumpPretty(match);
//	});

	auto&& matches = pattern::irp::collectAllPairs(marshalledAccesses, root, false);
	return matches.size();
}

int countMarshalledAssigns(const NodePtr root) {
	pattern::TreePattern marshalledAccesses = pattern::irp::assignment(pattern::irp::arrayRefElem1D(pattern::irp::refDeref(
			pattern::irp::compositeRefElem()) | pattern::irp::compositeMemberAccess(), pattern::any), pattern::any) ;

	auto&& matches = pattern::irp::collectAllPairs(marshalledAccesses, root, false);
	return matches.size();
}

TEST(DataLayout, AosToSoa) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		"	ref<ref<array<twoElem,1>>> a = var(new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ))) ; "
		"	a = new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ));"
		"	for(int<4> i = 0 .. 100 : 1) {"
		"		ref<twoElem> tmp;"
		"		(*a)[i] = *tmp;"
		"	}"
		"	for(int<4> i = 0 .. 42 : 1) {"
		"		ref<twoElem> tmp = ref.var(*((*a)[i]));"
		"		ref<ref<array<twoElem,1>>> copy = ref.var(*a);"
		"		copy = *a;"
		"		a = *copy;"
		"		ref<ref<array<twoElem,1>>> uninitialized;"
		"		uninitialized = *a;"
		"		ref<ref<array<twoElem,1>>> ptr = ref.var(scalar.to.array((*a)[i]));"
		"		(*ptr)[i].int = i;"
		"		ref.deref(a)[i].int = i;"
		"	}"
		"	for(int<4> i = 0 .. 100 : 1) {"
		"		ref<twoElem> tmp;"
		"		tmp = *(*a)[i];"
		"	}"
		"	ref.delete(*a);"
		"}"
	));

//	ia::RefList&& refs = ia::collectDefUse(code);
//
//	// all the refs are usages
//	std::for_each(refs.begin(), refs.end(), [](const ia::RefPtr& cur){
////			EXPECT_TRUE(cur->getUsage() == ia::Ref::USE);
//			if (cur->getType() == ia::Ref::ARRAY) {
//				std::cout << "\nARRAY: " << cur << std::endl;
//			} else if(cur->getType() == ia::Ref::SCALAR){
//				std::cout << "\nSCALAR: " << cur << std::endl;
//			} else if(cur->getType() == ia::Ref::MEMBER){
//				std::cout << "\nMEMBER: " << cur << std::endl;
//			}
//
//			dumpPretty(cur->getBaseExpression());
//			cur->printTo(std::cout);
//		});
//
//	return;

	datalayout::AosToSoa ats(code, datalayout::findAllSuited);
	ats.transform();

//	dumpPretty(code);

	auto semantic = checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	EXPECT_EQ(98, numberOfCompoundStmts(code));
	EXPECT_EQ(10, countMarshalledAccesses(code));
	EXPECT_EQ(4, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:ref<array<int<4>,1>>,
	float:ref<array<real<4>,1>>
>;

{
    decl ref<ref<array<type000,1>>> v0 =  var( new(array.create.1D(type<type000>, 100u)));
    decl ref<type001> v90 =  var(undefined(type<type001>));
    v90->int :=  new(array.create.1D(type<int<4>>, 100u));
    v90->float :=  new(array.create.1D(type<real<4>>, 100u));
    v0 :=  new(array.create.1D(type<type000>, 100u));
    v90->int :=  new(array.create.1D(type<int<4>>, 100u));
    v90->float :=  new(array.create.1D(type<real<4>>, 100u));
    for(decl int<4> v1 = 0 .. 100 : 1) {
        decl ref<type000> v2 =  var(undefined(type<type000>));
         *v0&[v1] :=  *v2;
    };
    for(decl int<4> v97 = 0 .. 100 : 1) {
         *v90->int&[v97] :=  * *v0&[v97]->int;
         *v90->float&[v97] :=  * *v0&[v97]->float;
    };
    for(decl int<4> v3 = 0 .. 42 : 1) {
        decl ref<type000> v4 =  var(struct{int:= * *v90->int&[v3], float:= * *v90->float&[v3]});
        decl ref<ref<array<type000,1>>> v5 =  var( *v0);
        decl ref<type001> v91 =  var(undefined(type<type001>));
        v91->int :=  *v90->int;
        v91->float :=  *v90->float;
        v5 :=  *v0;
        v91->int :=  *v90->int;
        v91->float :=  *v90->float;
        v0 :=  *v5;
        v90->int :=  *v91->int;
        v90->float :=  *v91->float;
        decl ref<ref<array<type000,1>>> v6 =  var(undefined(type<ref<array<type000,1>>>));
        decl ref<type001> v92 =  var(undefined(type<type001>));
        v92->int := undefined(type<ref<array<int<4>,1>>>);
        v92->float := undefined(type<ref<array<real<4>,1>>>);
        v6 :=  *v0;
        v92->int :=  *v90->int;
        v92->float :=  *v90->float;
        decl ref<ref<array<type000,1>>> v7 =  var(scalar.to.array( *v0&[v3]));
        decl ref<type001> v93 =  var(undefined(type<type001>));
        v93->int := scalar.to.array( *v90->int&[v3]);
        v93->float := scalar.to.array( *v90->float&[v3]);
         *v93.int&[v3] := v3;
         *v90.int&[v3] := v3;
    };
    for(decl int<4> v98 = 0 .. 100 : 1) {
         *v0&[v98]->int :=  * *v90->int&[v98];
         *v0&[v98]->float :=  * *v90->float&[v98];
    };
    for(decl int<4> v8 = 0 .. 100 : 1) {
        decl ref<type000> v9 =  var(undefined(type<type000>));
        v9 :=  * *v0&[v8];
    };
     del( *v90->int);
     del( *v90->float);
     del( *v0);
}
*/

TEST(DataLayout, AosToSoa2) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		"	let store = lit(\"storeData\":(ref<array<twoElem,1>>)->unit);"
		"	let load = lit(\"loadData\":(ref<ref<array<twoElem,1>>>)->unit);"
		""
		"	let access = (ref<array<twoElem,1>> x, int<4> i)->unit {"
		"		for(int<4> i = 0 .. 42 : 1) {"
		"			x[i].int = i;"
		"		}"
		"	};"
		"	ref<ref<array<twoElem,1>>> a;"
		"	a = new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ));"
		"	load(a);"
		""
		"	ref<ref<array<twoElem,1>>> copy = ref.var(*a);"
		"	store(*copy);"
		""
		"	access(*a, 0);"
		"	store(*a);"
		"	ref.delete(*a);"
		"}"
	));

//	ia::RefList&& refs = ia::collectDefUse(code);

//	std::for_each(refs.begin(), refs.end(), [](const ia::RefPtr& cur){
////			EXPECT_TRUE(cur->getUsage() == ia::Ref::USE);
//			if (cur->getType() == ia::Ref::ARRAY) {
//				std::cout << "\nARRAY: " << cur << std::endl;
//			} else if(cur->getType() == ia::Ref::SCALAR){
//				std::cout << "\nSCALAR: " << cur << std::endl;
//			} else if(cur->getType() == ia::Ref::MEMBER){
//				std::cout << "\nMEMBER: " << cur << std::endl;
//			}
//
//			dumpPretty(cur->getBaseExpression());
//			cur->printTo(std::cout);
//		});
//

	datalayout::AosToSoa ats(code, datalayout::findAllSuited);
	ats.transform();

//	dumpPretty(code);

	auto semantic = checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	EXPECT_EQ(73, numberOfCompoundStmts(code));
	EXPECT_EQ(11, countMarshalledAccesses(code));
	EXPECT_EQ(7, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:ref<array<int<4>,1>>,
	float:ref<array<real<4>,1>>
>;

let fun000 = fun(ref<array<type000,1>> v1, int<4> v2, type001 v154) -> unit {
    for(decl int<4> v3 = 0 .. 42 : 1) {
        v154.int&[v3] := v3;
    };
};

{
    decl ref<ref<array<type000,1>>> v0 =  var(undefined(type<ref<array<type000,1>>>));
    decl ref<type001> v152 =  var(undefined(type<type001>));
    v152->int := undefined(type<ref<array<int<4>,1>>>);
    v152->float := undefined(type<ref<array<real<4>,1>>>);
    v0 :=  new(array.create.1D(type<type000>, 100u));
    v152->int :=  new(array.create.1D(type<int<4>>, 100u));
    v152->float :=  new(array.create.1D(type<real<4>>, 100u));
    loadData(v0);
    for(decl uint<4> v155 = 0 .. 100u : 1) {
         *v152->int&[v155] :=  * *v0&[v155]->int;
         *v152->float&[v155] :=  * *v0&[v155]->float;
    };
    decl ref<ref<array<type000,1>>> v1 =  var( *v0);
    decl ref<type001> v153 =  var(undefined(type<type001>));
    v153->int :=  *v152->int;
    v153->float :=  *v152->float;
    for(decl uint<4> v159 = 0 .. 100u : 1) {
         *v1&[v159]->int :=  * *v153->int&[v159];
         *v1&[v159]->float :=  * *v153->float&[v159];
    };
    storeData( *v1);
    for(decl uint<4> v157 = 0 .. 100u : 1) {
         *v153->int&[v157] :=  * *v1&[v157]->int;
         *v153->float&[v157] :=  * *v1&[v157]->float;
    };
    fun000( *v0, 0,  *v152);
    for(decl uint<4> v158 = 0 .. 100u : 1) {
         *v0&[v158]->int :=  * *v152->int&[v158];
         *v0&[v158]->float :=  * *v152->float&[v158];
    };
    storeData( *v0);
    for(decl uint<4> v156 = 0 .. 100u : 1) {
         *v152->int&[v156] :=  * *v0&[v156]->int;
         *v152->float&[v156] :=  * *v0&[v156]->float;
    };
     del( *v152->int);
     del( *v152->float);
     del( *v0);
}
*/

TEST(DataLayout, Globals) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		"	let store = lit(\"storeData\":(ref<array<twoElem,1>>)->unit);"
		"	let load = lit(\"loadData\":(ref<ref<array<twoElem,1>>>)->unit);"
		"	let a = lit(\"a\":ref<ref<array<twoElem,1>>>);"
		""
		"	let access = (ref<ref<array<twoElem,1>>> x)->unit {"
		"		for(int<4> i = 0 .. 42 : 1) {"
		"			ref.deref(x)[i].int = i;"
		"		}"
		"	};"
		"	let globalAccess = (int<4> idx)->unit {"
		"		ref<twoElem> tmp = ref.var(*((*a)[idx]));"
		"		ref<ref<array<twoElem,1>>> copy = ref.var(*a);"
		"		ref<ref<array<twoElem,1>>> ptr = ref.var(scalar.to.array((*a)[idx]));"
		"		(*ptr)[idx].int = idx;"
		"		ref.deref(a)[idx].int = idx;"
		"	};"
		""
		"	a = new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ));"
		"	load(a);"
		"	access(a);"
		"	globalAccess(7);"
		"	store(*a);"
		"	ref.delete(*a);"
		"}"
	));

	datalayout::AosToSoa ats(code, datalayout::findAllSuited);
	ats.transform();

//	dumpPretty(code);

	auto semantic = checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	EXPECT_EQ(72, numberOfCompoundStmts(code));
	EXPECT_EQ(13, countMarshalledAccesses(code));
	EXPECT_EQ(7, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:ref<array<int<4>,1>>,
	float:ref<array<real<4>,1>>
>;

let fun000 = fun(ref<ref<array<type000,1>>> v1, ref<type001> v154) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
         *v154.int&[v2] := v2;
    };
};

let fun001 = fun(int<4> v1) -> unit {
    decl ref<type000> v2 =  var(struct{int:= * *a_soa->int&[v1], float:= * *a_soa->float&[v1]});
    decl ref<ref<array<type000,1>>> v3 =  var( *a);
    decl ref<type001> v155 =  var(undefined(type<type001>));
    v155->int :=  *a_soa->int;
    v155->float :=  *a_soa->float;
    decl ref<ref<array<type000,1>>> v4 =  var(scalar.to.array( *a&[v1]));
    decl ref<type001> v156 =  var(undefined(type<type001>));
    v156->int := scalar.to.array( *a_soa->int&[v1]);
    v156->float := scalar.to.array( *a_soa->float&[v1]);
     *v156.int&[v1] := v1;
     *a_soa.int&[v1] := v1;
};

{
    a :=  new(array.create.1D(type<type000>, 100u));
    a_soa->int :=  new(array.create.1D(type<int<4>>, 100u));
    a_soa->float :=  new(array.create.1D(type<real<4>>, 100u));
    loadData(a);
    for(decl uint<4> v157 = 0 .. 100u : 1) {
         *a_soa->int&[v157] :=  * *a&[v157]->int;
         *a_soa->float&[v157] :=  * *a&[v157]->float;
    };
    fun000(a, a_soa);
    fun001(7);
    for(decl uint<4> v159 = 0 .. 100u : 1) {
         *a&[v159]->int :=  * *a_soa->int&[v159];
         *a&[v159]->float :=  * *a_soa->float&[v159];
    };
    storeData( *a);
    for(decl uint<4> v158 = 0 .. 100u : 1) {
         *a_soa->int&[v158] :=  * *a&[v158]->int;
         *a_soa->float&[v158] :=  * *a&[v158]->float;
    };
     del( *a_soa->int);
     del( *a_soa->float);
     del( *a);
}
*/

TEST(DataLayout, Tuple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		""
		"	let access = (ref<array<ref<array<twoElem,1>>, 1>> x)->unit {"
		"		for(int<4> i = 0 .. 42 : 1) {"
		"			ref.deref(x[0])[i].int = i;"
		"		}"
		"	};"
		""
		"	let writeToTuple = (ref<(ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)> lt, "
		"			ref<array<ref<array<twoElem,1>>,1>> x)->unit {"
		"		(*x[0])[3].int = 7;"
		"		tuple.ref.elem(lt,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = x;"
		"	};"
		""
		"	ref<ref<array<twoElem,1>>> a;"
		"	ref<ref<array<real<4>,1>>> b;"
		"	ref<uint<8>> c;"
		"	ref<ref<(ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)>> t;"
		"	t = new(undefined(lit( (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)) ));"
		"	ref.deref(a);"
		"	access(scalar.to.array(a));"
		"	tuple.ref.elem(*t,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = scalar.to.array(a);"
		"	writeToTuple(*t, scalar.to.array(a));"
		""
		"	ref.delete(*t);"
		"}"
	));

	datalayout::AosToSoa ats(code, datalayout::findAllSuited);
	ats.transform();

//	dumpPretty(code);

	auto semantic = checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	EXPECT_EQ(29, numberOfCompoundStmts(code));
	EXPECT_EQ(2, countMarshalledAccesses(code));
	EXPECT_EQ(2, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:ref<array<int<4>,1>>,
	float:ref<array<real<4>,1>>
>;

let fun000 = fun(ref<array<ref<array<type000,1>>,1>> v1, ref<array<type001,1>> v157) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
         *v157&[0].int&[v2] := v2;
    };
};

let fun001 = fun(ref<(ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)> v160, ref<array<ref<array<type000,1>>,1>> v2, ref<array<type001,1>> v158) -> unit {
     *v158&[0].int&[3] := 7;
    tuple.ref.elem(v160, 0, type<ref<array<type001,1>>>) := v158;
};

{
    decl ref<ref<array<type000,1>>> v0 =  var(undefined(type<ref<array<type000,1>>>));
    decl ref<type001> v156 =  var(undefined(type<type001>));
    v156->int := undefined(type<ref<array<int<4>,1>>>);
    v156->float := undefined(type<ref<array<real<4>,1>>>);
    decl ref<ref<array<real<4>,1>>> v1 =  var(undefined(type<ref<array<real<4>,1>>>));
    decl ref<uint<8>> v2 =  var(undefined(type<uint<8>>));
    decl ref<ref<(ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>> v159 =  var(undefined(type<ref<(ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>>));
    v159 :=  new(undefined(type<(ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>));
     *v0;
    fun000(scalar.to.array(v0), scalar.to.array(v156));
    tuple.ref.elem( *v159, 0, type<ref<array<type001,1>>>) := scalar.to.array(v156);
    fun001( *v159, scalar.to.array(v0), scalar.to.array(v156));
     del( *v159);
}
*/

} // transform
} // insieme

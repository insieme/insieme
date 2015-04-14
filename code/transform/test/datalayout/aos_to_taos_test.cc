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
#include "insieme/transform/datalayout/aos_to_taos.h"
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
	pattern::TreePattern marshalledAccesses = (pattern::irp::vectorSubscript(pattern::irp::compositeMemberAccess(
			pattern::irp::refDeref(pattern::irp::arrayRefElem1D())))) |
			pattern::irp::vectorRefElem(pattern::irp::compositeRefElem((pattern::irp::arrayRefElem1D())));

	auto&& matches = pattern::irp::collectAllPairs(marshalledAccesses, root, false);
	return matches.size();
}

int countMarshalledAssigns(const NodePtr root) {
	pattern::TreePattern marshalledAccesses = pattern::irp::assignment(pattern::irp::vectorRefElem(pattern::irp::compositeRefElem(
			(pattern::irp::arrayRefElem1D())), pattern::any)) ;

	auto&& matches = pattern::irp::collectAllPairs(marshalledAccesses, root, false);

	return matches.size();
}

TEST(DataLayout, AosToTaos) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		"	let store = lit(\"storeData\":(ref<array<twoElem,1>>)->unit);"
		"	let load = lit(\"loadData\":(ref<ref<array<twoElem,1>>>)->unit);"
		""
		"	let access = (ref<array<twoElem,1>> x)->unit {"
		"		for(int<4> i = 0 .. 42 : 1) {"
		"			x[i].int = i;"
		"		}"
		"	};"
		""
		"	ref<ref<array<twoElem,1>>> a = var(new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ))) ; "
		"	a = new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ));"
		"	for(int<4> i = 0 .. 100 : 1) {"
		"		ref<twoElem> tmp;"
		"		(*a)[i] = *tmp;"
		"	}"
		"	load(a);"
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
		"	access(*a);"
		""
		"	ref<int<4>> e = (*a)[5].int;"
		""
		"	store(*a);"
		"	for(int<4> i = 0 .. 100 : 1) {"
		"		ref<twoElem> tmp;"
		"		tmp = *(*a)[i];"
		"	}"
		"	ref.delete(*a);"
		"}"
	));


	datalayout::AosToTaos att(code);
	att.transform();

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

	EXPECT_EQ(132, numberOfCompoundStmts(code));
	EXPECT_EQ(18, countMarshalledAccesses(code));
	EXPECT_EQ(9, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:vector<int<4>,64>,
	float:vector<real<4>,64>
>;

let fun000 = fun(ref<array<type000,1>> v1, ref<array<type001,1>> v164) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
        v164&[v2/64]->int&[v2%64] := v2;
    };
};

{
    decl ref<ref<array<type000,1>>> v0 =  var( new(array.create.1D(type<type000>, 100u)));
    decl ref<ref<array<type001,1>>> v160 =  var( new(array.create.1D(type<type001>, 100u/64)));
    v0 :=  new(array.create.1D(type<type000>, 100u));
    v160 :=  new(array.create.1D(type<type001>, 100u/64));
    for(decl int<4> v1 = 0 .. 100 : 1) {
        decl ref<type000> v2 =  var(undefined(type<type000>));
         *v0&[v1] :=  *v2;
    };
    for(decl int<4> v165 = 0/64 .. 100/64 : 1) {
        for(decl int<4> v166 = 0 .. 64 : 1) {
             *v160&[v165]->int&[v166] :=  * *v0&[v165*64+v166]->int;
             *v160&[v165]->float&[v166] :=  * *v0&[v165*64+v166]->float;
        };
    };
    for(decl uint<4> v171 = 0/64 .. 100u/64 : 1) {
        for(decl uint<4> v172 = 0 .. 64 : 1) {
             *v0&[v171*64+v172]->int :=  * *v160&[v171]->int&[v172];
             *v0&[v171*64+v172]->float :=  * *v160&[v171]->float&[v172];
        };
    };
    loadData(v0);
    for(decl uint<4> v167 = 0/64 .. 100u/64 : 1) {
        for(decl uint<4> v168 = 0 .. 64 : 1) {
             *v160&[v167]->int&[v168] :=  * *v0&[v167*64+v168]->int;
             *v160&[v167]->float&[v168] :=  * *v0&[v167*64+v168]->float;
        };
    };
    for(decl int<4> v3 = 0 .. 42 : 1) {
        decl ref<type000> v4 =  var(struct{int:= * *v160&[v3/64]->int&[v3%64], float:= * *v160&[v3/64]->float&[v3%64]});
        decl ref<ref<array<type000,1>>> v5 =  var( *v0);
        decl ref<ref<array<type001,1>>> v161 =  var( *v160);
        v5 :=  *v0;
        v161 :=  *v160;
        v0 :=  *v5;
        v160 :=  *v161;
        decl ref<ref<array<type000,1>>> v6 =  var(undefined(type<ref<array<type000,1>>>));
        decl ref<ref<array<type001,1>>> v162 =  var(undefined(type<ref<array<type001,1>>>));
        v6 :=  *v0;
        v162 :=  *v160;
        decl ref<ref<array<type000,1>>> v7 =  var(scalar.to.array( *v0&[v3]));
        decl ref<ref<array<type001,1>>> v163 =  var(scalar.to.array( *v160&[v3]));
         *v163&[v3/64]->int&[v3%64] := v3;
         *v160&[v3/64]->int&[v3%64] := v3;
    };
    fun000( *v0,  *v160);
    decl ref<int<4>> v8 =  *v160&[5/64]->int&[5%64];
    for(decl uint<4> v173 = 0/64 .. 100u/64 : 1) {
        for(decl uint<4> v174 = 0 .. 64 : 1) {
             *v0&[v173*64+v174]->int :=  * *v160&[v173]->int&[v174];
             *v0&[v173*64+v174]->float :=  * *v160&[v173]->float&[v174];
        };
    };
    storeData( *v0);
    for(decl uint<4> v169 = 0/64 .. 100u/64 : 1) {
        for(decl uint<4> v170 = 0 .. 64 : 1) {
             *v160&[v169]->int&[v170] :=  * *v0&[v169*64+v170]->int;
             *v160&[v169]->float&[v170] :=  * *v0&[v169*64+v170]->float;
        };
    };
    for(decl int<4> v175 = 0/64 .. 100/64 : 1) {
        for(decl int<4> v176 = 0 .. 64 : 1) {
             *v0&[v175*64+v176]->int :=  * *v160&[v175]->int&[v176];
             *v0&[v175*64+v176]->float :=  * *v160&[v175]->float&[v176];
        };
    };
    for(decl int<4> v9 = 0 .. 100 : 1) {
        decl ref<type000> v10 =  var(undefined(type<type000>));
        v10 :=  * *v0&[v9];
    };
     del( *v160);
     del( *v0);
}
*/

TEST(DataLayout, Tuple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
			"{"
			"	let twoElem = struct{int<4> int; real<4> float;};"
			"	let tuple = (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>);"
			""
			"	let subfunction = (int<4> p, ref<array<twoElem,1>> a) -> real<4> {"
			"		ref<real<4>> retVal = (a[66].float);"
			"		return *retVal;"
			"	};"
			""
			"	let actualWork = (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, "
			"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
			"		ref<ref<array<twoElem,1>>> d = var(a);"
			"		d = a;"
			"		b;"
			"		c;"
			"		ref<int<4>> e = a[5].int;"
			"		ref<twoElem> f = var(a[7]);"
			"		f.float = subfunction(8, a);"
			"		f = a[7];"
			"	};"
			""
			"	let local = (ref<array<real<4>,1>> b, ref<array<twoElem,1>> a, uint<8> c, "
			"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
			"		parallel(job([vector.reduction(local_size, 1u, uint.mul)-vector.reduction(local_size, 1u, uint.mul)]"
//			"				[ref<array<twoElem,1>> a1 = a, ref<array<real<4>,1>> b1 = b] "
			"		,	actualWork(a, b, c, local_size, global_size)"
			"		));"
			"	};"
			""
			"	let global = (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, "
			"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
			"		vector<uint<8>,3> groups = vector.pointwise(uint.div)(global_size, local_size);"
			"		parallel(job([vector.reduction(groups, 1u, uint.mul)-vector.reduction(groups, 1u, uint.mul)]"
//			"				[ref<array<ref<array<twoElem,1>>,1>> a1 = a, ref<array<real<4>,1>> b1 = b] "
			"		,	local(b, a, c, local_size, global_size)"
			"		));"
			"		*a[0];"
			"	};"
			""
			"	let kernelFct = (tuple kernel, vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> int<4> {"
			"		global("
			"			*(tuple.member.access(kernel, 0u, lit(ref<array<ref<array<twoElem,1>>,1>>))[0]),"
			"			*(tuple.member.access(kernel, 1u, lit(ref<array<ref<array<real<4>,1>>,1>>))[0]),"
			"			*(tuple.member.access(kernel, 2u, lit(ref<array<uint<8>,1>>))[0]),"
			"			local_size, global_size);"
			""
			"		return 0;"
			"	};"
			""
			"	let writeToTuple = (ref<(ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)> lt, "
			"			ref<array<ref<array<twoElem,1>>,1>> x)->unit {"
			"		lt;"
//			"(*x[0])[3].int = 7;"
			"		tuple.ref.elem(lt,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = x;"
			"	};"
			""
			"	ref<ref<array<twoElem,1>>> a;"
			"	ref<ref<array<real<4>,1>>> b;"
			"	ref<uint<8>> c;"
			"	ref<ref<tuple>> t;"
			"	t = new(undefined(lit( tuple )));"
			""
			"	ref<array<ref<array<twoElem,1>>,1>> d = scalar.to.array(a);"
			"	ref<twoElem> e = (*d[0])[0];" // !!!!!
			"	tuple.ref.elem(*t, 0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = scalar.to.array(a);"
			"	tuple.ref.elem(*t, 1u, lit(ref<array<ref<array<real<4>,1>>,1>>)) = scalar.to.array(b);"
			"	tuple.ref.elem(*t, 2u, lit(ref<array<uint<8>,1>>)) = scalar.to.array(c);"
//			"	writeToTuple(*t, scalar.to.array(a));"
			""
			"	vector<uint<8>,3> ls;"
			"	vector<uint<8>,3> gs;"
			""
			"	kernelFct(**t, ls, gs);"
			""
			"	ref.delete(*t);"
			"}"
	));


	datalayout::AosToTaos att(code);
	att.transform();

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

	EXPECT_EQ(77, numberOfCompoundStmts(code));
	EXPECT_EQ(6, countMarshalledAccesses(code));
	EXPECT_EQ(0, countMarshalledAssigns(code));
}
/*
let type000 = struct<
	int:int<4>,
	float:real<4>
>;

let type001 = struct<
	int:vector<int<4>,64>,
	float:vector<real<4>,64>
>;

let fun000 = fun(vector<'elem1,#l> v1, vector<'elem2,#l> v2, ('elem1, 'elem2) -> 'res v3) -> vector<'res,#l> {
    decl ref<vector<'res,#l>> v4 =  var(undefined(type<vector<'res,#l>>));
    for(decl uint<8> v5 = 0u .. to.uint(#l)-1u : 1) {
        v4&[v5] := v3(v1[v5], v2[v5]);
    };
    return  *v4;
};

let fun001 = fun(int<4> v1, ref<array<type001,1>> v195) -> real<4> {
    decl ref<real<4>> v3 = v195&[66/64]->float&[66%64];
    return  *v3;
};

let fun002 = fun(ref<array<type001,1>> v193, ref<array<real<4>,1>> v2, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    decl ref<ref<array<type001,1>>> v194 =  var(v193);
    v194 := v193;
    v2;
    v3;
    decl ref<int<4>> v7 = v193&[5/64]->int&[5%64];
    decl ref<type000> v8 =  var(struct{int:= *v193&[7/64]->int&[7%64], float:= *v193&[7/64]->float&[7%64]});
    v8->float := fun001(8, v193);
    v8 := struct{int:= *v193&[7/64]->int&[7%64], float:= *v193&[7/64]->float&[7%64]};
};

let fun003 = fun(ref<array<real<4>,1>> v1, ref<array<type001,1>> v192, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    parallel(job([vector.reduction(v5, 1u, uint.mul)-vector.reduction(v5, 1u, uint.mul)]){
        bind(){fun002(v192, v1, v3, v5, v4)}
    });
};

let fun004 = fun(ref<array<type001,1>> v191, ref<array<real<4>,1>> v2, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    decl vector<uint<8>,3> v6 = vector.pointwise(uint.div)(v4, v5);
    parallel(job([vector.reduction(v6, 1u, uint.mul)-vector.reduction(v6, 1u, uint.mul)]){
        bind(){fun003(v2, v191, v3, v5, v4)}
    });
     *v191&[0];
};

let fun005 = fun((ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>) v196, vector<uint<8>,3> v2, vector<uint<8>,3> v3) -> int<4> {
    fun004( *tuple.member.access(v196, 0, type<ref<array<ref<array<type001,1>>,1>>>)&[0],  *tuple.member.access(v196, 1, type<ref<array<ref<array<real<4>,1>>,1>>>)&[0],  *tuple.member.access(v196, 2, type<ref<array<uint<8>,1>>>)&[0], v3, v2);
    return 0;
};

{
    decl ref<ref<array<type000,1>>> v0 =  var(undefined(type<ref<array<type000,1>>>));
    decl ref<ref<array<type001,1>>> v187 =  var(undefined(type<ref<array<type001,1>>>));
    decl ref<ref<array<real<4>,1>>> v1 =  var(undefined(type<ref<array<real<4>,1>>>));
    decl ref<uint<8>> v2 =  var(undefined(type<uint<8>>));
    decl ref<ref<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>> v189 =  var(undefined(type<ref<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>>));
    v189 :=  new(undefined(type<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>));
    decl ref<array<ref<array<type000,1>>,1>> v4 = scalar.to.array(v0);
    decl ref<array<ref<array<type001,1>>,1>> v188 = scalar.to.array(v187);
    decl ref<type000> v197 =  *v4&[0]&[0];
    tuple.ref.elem( *v189, 0, type<ref<array<ref<array<type001,1>>,1>>>) := scalar.to.array(v187);
    tuple.ref.elem( *v189, 1, type<ref<array<ref<array<real<4>,1>>,1>>>) := scalar.to.array(v1);
    tuple.ref.elem( *v189, 2, type<ref<array<uint<8>,1>>>) := scalar.to.array(v2);
    decl vector<uint<8>,3> v6 = undefined(type<vector<uint<8>,3>>);
    decl vector<uint<8>,3> v7 = undefined(type<vector<uint<8>,3>>);
    fun005( * *v189, v6, v7);
     del( *v189);
}
*/

} // transform
} // insieme

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

	NodePtr code = builder.normalize(builder.parseStmt(R"(
		{
			let twoElem = struct{int<4> int; real<4> float;};
			decl ref<ref<array<twoElem, 1>>> a = var(new(array_create_1D( lit(struct{int<4> int; real<4> float;}), 100u))) ; 
			a = new(array_create_1D( lit(struct{int<4> int; real<4> float;}), 100u));
			for(int<4> i = 0 .. 100 : 1) {
				decl ref<twoElem> tmp;
				(*a)[i] = *tmp;
			}
			for(int<4> i = 0 .. 42 : 1) {
				decl ref<twoElem> tmp = ref_var(*((*a)[i]));
				decl ref<ref<array<twoElem, 1>>> copy = ref_var(*a);
				copy = *a;
				a = *copy;
				decl ref<ref<array<twoElem, 1>>> uninitialized;
				uninitialized = *a;
				decl ref<ref<array<twoElem, 1>>> ptr = ref_var(array_view((*a), i));
				(*ptr)[i].int = i;
				ref_deref(a)[i].int = i;
			}
			for(int<4> i = 0 .. 100 : 1) {
				decl ref<twoElem> tmp;
				tmp = *(*a)[i];
			}
			ref_delete(*a);
		}
	)"));

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

	EXPECT_EQ(92, numberOfCompoundStmts(code));
	EXPECT_EQ(8, countMarshalledAccesses(code));
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
    decl ref<ref<array<type000,1>>> v0 = ( var(( new(array_create_1D(type<type000>, 100u)))));
    decl ref<type001> v23 = ( var(undefined(type<type001>)));
    ((v23->int) := ( new(array_create_1D(type<int<4>>, 100u))));
    ((v23->float) := ( new(array_create_1D(type<real<4>>, 100u))));
    (v0 := ( new(array_create_1D(type<type000>, 100u))));
    ((v23->int) := ( new(array_create_1D(type<int<4>>, 100u))));
    ((v23->float) := ( new(array_create_1D(type<real<4>>, 100u))));
    for(decl int<4> v1 = 0 .. 100 : 1) {
        decl ref<type000> v2 = ( var(undefined(type<type000>)));
        ((( *v0)&[v1]) := ( *v2));
    };
    for(decl int<4> v32 = 0 .. 100 : 1) {
        ((( *(v23->int))&[v32]) := ( *((( *v0)&[v32])->int)));
        ((( *(v23->float))&[v32]) := ( *((( *v0)&[v32])->float)));
    };
    for(decl int<4> v3 = 0 .. 42 : 1) {
        decl ref<type000> v4 = ( var(struct{int:=( *(( *(v23->int))&[v3])), float:=( *(( *(v23->float))&[v3]))}));
        decl ref<ref<array<type000,1>>> v5 = ( var(( *v0)));
        decl ref<type001> v25 = ( var(undefined(type<type001>)));
        ((v25->int) := ( *(v23->int)));
        ((v25->float) := ( *(v23->float)));
        (v5 := ( *v0));
        ((v25->int) := ( *(v23->int)));
        ((v25->float) := ( *(v23->float)));
        (v0 := ( *v5));
        ((v23->int) := ( *(v25->int)));
        ((v23->float) := ( *(v25->float)));
        decl ref<ref<array<type000,1>>> v6 = ( var(undefined(type<ref<array<type000,1>>>)));
        decl ref<type001> v26 = ( var(undefined(type<type001>)));
        ((v26->int) := undefined(type<ref<array<int<4>,1>>>));
        ((v26->float) := undefined(type<ref<array<real<4>,1>>>));
        (v6 := ( *v0));
        ((v26->int) := ( *(v23->int)));
        ((v26->float) := ( *(v23->float)));
        decl ref<ref<array<type000,1>>> v7 = ( var(array_view(( *v0), v3)));
        decl ref<type001> v27 = ( var(undefined(type<type001>)));
        ((v27->int) := array_view(( *(v23->int)), v3));
        ((v27->float) := array_view(( *(v23->float)), v3));
        (((( *v27).int)&[v3]) := v3);
        (((( *v23).int)&[v3]) := v3);
    };
    for(decl int<4> v33 = 0 .. 100 : 1) {
        (((( *v0)&[v33])->int) := ( *(( *(v23->int))&[v33])));
        (((( *v0)&[v33])->float) := ( *(( *(v23->float))&[v33])));
    };
    for(decl int<4> v8 = 0 .. 100 : 1) {
        decl ref<type000> v9 = ( var(undefined(type<type000>)));
        (v9 := ( *(( *v0)&[v8])));
    };
    ( del(( *(v23->int))));
    ( del(( *(v23->float))));
    ( del(( *v0)));
}

*/

TEST(DataLayout, AosToSoa1) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		R"({
			let twoElem = struct{int<4> int; real<4> float;};
			let store = expr lit("storeData":(ref<array<twoElem, 1>>)->unit);
			let load = expr lit("loadData":(ref<ref<array<twoElem, 1>>>)->unit);
		
			let access2 = lambda (src<array<twoElem, 1>> x)->unit {
				decl ref<real<4>> l = x[7].float;
			};
		
			let access1 = lambda (ref<array<twoElem, 1>> x)->unit {
				x[7].float = 0.0f;
			};
			let access = lambda (ref<array<twoElem, 1>> x, int<4> i)->unit {
				for(int<4> i = 0 .. 42 : 1) {
					x[i].int = i;
				}
				access1(x);
			};
			decl ref<ref<array<twoElem, 1>>> a;
			a = new(array_create_1D(lit(struct{int<4> int; real<4> float;}), 100u));
			load(a);
		
			decl ref<ref<array<twoElem, 1>>> copy = ref_var(*a);
			store(*copy);
		
			access(*a, 0);
			access2(*a);
			store(*a);
			ref_delete(*a);
		})"
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

	EXPECT_EQ(77, numberOfCompoundStmts(code));
	EXPECT_EQ(13, countMarshalledAccesses(code));
	EXPECT_EQ(8, countMarshalledAssigns(code));
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

let fun000 = fun(ref<array<type000,1>> v1, type001 v31) -> unit {
    (((v31.float)&[7]) := 0.0f);
};

let fun001 = fun(ref<array<type000,1>> v1, int<4> v2, type001 v30) -> unit {
    for(decl int<4> v3 = 0 .. 42 : 1) {
        (((v30.int)&[v3]) := v3);
    };
    fun000(v1, v30);
};

let fun002 = fun(src<array<type000,1>> v1, type001 v32) -> unit {
    decl ref<real<4>> v2 = ((v32.float)&[7]);
};

{
    decl ref<ref<array<type000,1>>> v0 = ( var(undefined(type<ref<array<type000,1>>>)));
    decl ref<type001> v28 = ( var(undefined(type<type001>)));
    ((v28->int) := undefined(type<ref<array<int<4>,1>>>));
    ((v28->float) := undefined(type<ref<array<real<4>,1>>>));
    (v0 := ( new(array_create_1D(type<type000>, 100u))));
    ((v28->int) := ( new(array_create_1D(type<int<4>>, 100u))));
    ((v28->float) := ( new(array_create_1D(type<real<4>>, 100u))));
    loadData(v0);
    for(decl uint<4> v36 = 0 .. 100u : 1) {
        ((( *(v28->int))&[v36]) := ( *((( *v0)&[v36])->int)));
        ((( *(v28->float))&[v36]) := ( *((( *v0)&[v36])->float)));
    };
    decl ref<ref<array<type000,1>>> v1 = ( var(( *v0)));
    decl ref<type001> v29 = ( var(undefined(type<type001>)));
    ((v29->int) := ( *(v28->int)));
    ((v29->float) := ( *(v28->float)));
    for(decl uint<4> v40 = 0 .. 100u : 1) {
        (((( *v1)&[v40])->int) := ( *(( *(v29->int))&[v40])));
        (((( *v1)&[v40])->float) := ( *(( *(v29->float))&[v40])));
    };
    storeData(( *v1));
    for(decl uint<4> v38 = 0 .. 100u : 1) {
        ((( *(v29->int))&[v38]) := ( *((( *v1)&[v38])->int)));
        ((( *(v29->float))&[v38]) := ( *((( *v1)&[v38])->float)));
    };
    fun001(( *v0), 0, ( *v28));
    fun002(( *v0), ( *v28));
    for(decl uint<4> v39 = 0 .. 100u : 1) {
        (((( *v0)&[v39])->int) := ( *(( *(v28->int))&[v39])));
        (((( *v0)&[v39])->float) := ( *(( *(v28->float))&[v39])));
    };
    storeData(( *v0));
    for(decl uint<4> v37 = 0 .. 100u : 1) {
        ((( *(v28->int))&[v37]) := ( *((( *v0)&[v37])->int)));
        ((( *(v28->float))&[v37]) := ( *((( *v0)&[v37])->float)));
    };
    ( del(( *(v28->int))));
    ( del(( *(v28->float))));
    ( del(( *v0)));
}
*/

TEST(DataLayout, Globals) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(R"(
		{
			let twoElem = struct{int<4> int; real<4> float;};
			let store = expr lit("storeData":(ref<array<twoElem, 1>>)->unit);
			let load = expr lit("loadData":(ref<ref<array<twoElem, 1>>>)->unit);
			let a = expr lit("a":ref<ref<array<twoElem, 1>>>);
		
			let access = lambda (ref<array<twoElem, 1>> x)->unit {
				for(int<4> i = 0 .. 42 : 1) {
					(x)[i].int = i;
				}
			};
			let globalAccess = lambda (int<4> idx)->unit {
				decl ref<twoElem> tmp = ref_var(*((*a)[idx]));
				decl ref<ref<array<twoElem, 1>>> copy = ref_var(*a);
				decl ref<ref<array<twoElem, 1>>> ptr = ref_var(array_view((*a), idx));
				(*ptr)[idx].int = idx;
				ref_deref(a)[idx].int = idx;
			};
		
			a = new(array_create_1D( lit(struct{int<4> int; real<4> float;}), 100u));
			load(a);
			access(*a);
			globalAccess(7);
			store(*a);
			delete(*a);
		}
	)"));

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

	EXPECT_EQ(66, numberOfCompoundStmts(code));
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

let fun000 = fun(ref<array<type000,1>> v1, type001 v23) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
        (((v23.int)&[v2]) := v2);
    };
};

let fun001 = fun(int<4> v1) -> unit {
    decl ref<type000> v2 = ( var(struct{int:=( *(( *(a_soa->int))&[v1])), float:=( *(( *(a_soa->float))&[v1]))}));
    decl ref<ref<array<type000,1>>> v3 = ( var(( *a)));
    decl ref<type001> v24 = ( var(undefined(type<type001>)));
    ((v24->int) := ( *(a_soa->int)));
    ((v24->float) := ( *(a_soa->float)));
    decl ref<ref<array<type000,1>>> v4 = ( var(array_view(( *a), v1)));
    decl ref<type001> v25 = ( var(undefined(type<type001>)));
    ((v25->int) := array_view(( *(a_soa->int)), v1));
    ((v25->float) := array_view(( *(a_soa->float)), v1));
    (((( *v25).int)&[v1]) := v1);
    (((( *a_soa).int)&[v1]) := v1);
};

{
    (a := ( new(array_create_1D(type<type000>, 100u))));
    ((a_soa->int) := ( new(array_create_1D(type<int<4>>, 100u))));
    ((a_soa->float) := ( new(array_create_1D(type<real<4>>, 100u))));
    loadData(a);
    for(decl uint<4> v29 = 0 .. 100u : 1) {
        ((( *(a_soa->int))&[v29]) := ( *((( *a)&[v29])->int)));
        ((( *(a_soa->float))&[v29]) := ( *((( *a)&[v29])->float)));
    };
    fun000(( *a), ( *a_soa));
    fun001(7);
    for(decl uint<4> v31 = 0 .. 100u : 1) {
        (((( *a)&[v31])->int) := ( *(( *(a_soa->int))&[v31])));
        (((( *a)&[v31])->float) := ( *(( *(a_soa->float))&[v31])));
    };
    storeData(( *a));
    for(decl uint<4> v30 = 0 .. 100u : 1) {
        ((( *(a_soa->int))&[v30]) := ( *((( *a)&[v30])->int)));
        ((( *(a_soa->float))&[v30]) := ( *((( *a)&[v30])->float)));
    };
    ( del(( *(a_soa->int))));
    ( del(( *(a_soa->float))));
    ( del(( *a)));
}
*/

TEST(DataLayout, Tuple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(R"(
		{
			let twoElem = struct{int<4> int; real<4> float;};
			let tuple = (ref<array<ref<array<twoElem, 1>>, 1>>, ref<array<ref<array<twoElem, 1>>, 1>>, ref<array<ref<array<real<4>, 1>>, 1>>, ref<array<uint<8>, 1>>);
		
			let access = lambda (ref<array<ref<array<twoElem, 1>>, 1>> x)->unit {
				for(int<4> i = 0 .. 42 : 1) {
					ref_deref(x[0])[i].int = i;
				}
			};
		
			let writeToTuple = lambda (ref<tuple> lt, 
					ref<array<ref<array<twoElem, 1>>, 1>> x)->unit {
				(*x[0])[3].int = 7;
				tuple_ref_elem(lt,0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>)) = x;
			};
		
			let actualWork = lambda (ref<array<twoElem, 1>> a, src<array<twoElem, 1>> a1, ref<array<real<4>, 1>> b, uint<8> c, 
					                 vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
				decl ref<ref<array<twoElem, 1>>> d = var(a);
				decl ref<ref<array<twoElem, 1>>> f = ref_var(array_view(a, 17));

				(*d)[13].int = 5;
				f = array_view(a, 42);
				decl ref<real<4>> scalar = var(*((*d)[55].float));
				for(int<4> i = 0 .. 99 : 1) {
					scalar = (*a[i]).float;
					scalar = *(a1[i].float);
					scalar = (*(*d)[i]).float;
				}

//				decl ref<array<twoElem, 1>> e; 	not supported
//				e = *a;							
			};
		
			let local = lambda (ref<array<twoElem, 1>> a, ref<array<real<4>, 1>> b, src<array<twoElem, 1>> a1, uint<8> c, 
					vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
				parallel(job([vector_reduction(local_size, 1u, uint_mul):vector_reduction(local_size, 1u, uint_mul)]
				,	actualWork(a, a1, b, c, local_size, global_size)
				));
			};
		
			let global = lambda (ref<array<twoElem, 1>> a, src<array<twoElem, 1>> a1, ref<array<real<4>, 1>> b, uint<8> c, 
					vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {
				decl vector<uint<8>,3> groups = vector_pointwise(uint_div)(global_size, local_size);
				parallel(job([vector_reduction(groups, 1u, uint_mul):vector_reduction(groups, 1u, uint_mul)]
				,	local(a, b, a1, c, local_size, global_size)
				));
			};
		
			let kernelFct = lambda (tuple kernel, vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> int<4> {
				global(
					*(tuple_member_access(kernel, 0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>))[0]),
					*ref_reinterpret(tuple_member_access(kernel, 1u, lit(ref<array<ref<array<twoElem, 1>>, 1>>))[0], lit(src<array<twoElem, 1>>)),
					*(tuple_member_access(kernel, 2u, lit(ref<array<ref<array<real<4>, 1>>, 1>>))[0]),
					*(tuple_member_access(kernel, 3u, lit(ref<array<uint<8>, 1>>))[0]),
					local_size, global_size);
		
				return 0;
			};
		
			decl ref<ref<array<twoElem, 1>>> a;
			decl ref<ref<array<twoElem, 1>>> a1;
			decl ref<ref<array<real<4>, 1>>> b;
			decl ref<uint<8>> c;
			decl ref<ref<tuple>> t;
			t = new(undefined( (ref<array<ref<array<twoElem, 1>>, 1>>, ref<array<ref<array<twoElem, 1>>, 1>>, 
					ref<array<ref<array<real<4>, 1>>, 1>>, ref<array<uint<8>, 1>>) ));
			ref_deref(a);
			access(scalar_to_array(a));
			tuple_ref_elem(*t,0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>)) = scalar_to_array(a);
			tuple_ref_elem(*t,1u, lit(ref<array<ref<array<twoElem, 1>>, 1>>)) = scalar_to_array(a1);
			writeToTuple(*t, scalar_to_array(a));
		
			decl vector<uint<8>,3> ls;
			decl vector<uint<8>,3> gs;
		
			kernelFct(**t, ls, gs);
		
			ref_delete(*t);
		}
	)"));

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

	EXPECT_EQ(89, numberOfCompoundStmts(code));
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

let fun000 = fun(ref<array<ref<array<type000,1>>,1>> v1, ref<array<type001,1>> v101) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
        (((( *(v101&[0])).int)&[v2]) := v2);
    };
};

let fun001 = fun(ref<(ref<array<type001,1>>,ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)> v104, ref<array<ref<array<type000,1>>,1>> v2, ref<array<type001,1>> v102) -> unit {
    (((( *(v102&[0])).int)&[3]) := 7);
    (tuple_ref_elem(v104, 0, type<ref<array<type001,1>>>) := v102);
};

let fun002 = fun(vector<'elem1,#l> v1, vector<'elem2,#l> v2, ('elem1, 'elem2) => 'res v3) -> vector<'res,#l> {
    decl ref<vector<'res,#l>> v4 = ( var(undefined(type<vector<'res,#l>>)));
    for(decl uint<8> v5 = 0u .. to_uint(#l) : 1) {
        ((v4&[v5]) := v3((v1[v5]), (v2[v5])));
    };
    return ( *v4);
};

let fun003 = fun(ref<array<int<4>,1>> v115, ref<array<real<4>,1>> v116, ref<array<int<4>,1>> v117, ref<array<real<4>,1>> v118, ref<array<real<4>,1>> v3, uint<8> v4, vector<uint<8>,3> v5, vector<uint<8>,3> v6) -> unit {
    decl ref<ref<array<int<4>,1>>> v119 = ( var(v115));
    decl ref<ref<array<real<4>,1>>> v120 = ( var(v116));
    decl ref<ref<array<int<4>,1>>> v121 = ( var(array_view(v115, 17)));
    decl ref<ref<array<real<4>,1>>> v122 = ( var(array_view(v116, 17)));
    ((( *v119)&[13]) := 5);
    (v121 := array_view(v115, 42));
    (v122 := array_view(v116, 42));
    decl ref<real<4>> v9 = ( var(( *(( *v120)&[55]))));
    for(decl int<4> v10 = 0 .. 99 : 1) {
        (v9 := ( *(v116&[v10])));
        (v9 := ( *(( *v120)&[v10])));
        (v9 := ( *(v118&[v10])));
    };
};

let fun004 = fun(ref<array<int<4>,1>> v111, ref<array<real<4>,1>> v112, ref<array<real<4>,1>> v2, ref<array<int<4>,1>> v113, ref<array<real<4>,1>> v114, uint<8> v4, vector<uint<8>,3> v5, vector<uint<8>,3> v6) -> unit {
    parallel(job(([vector_reduction(v6, 1u, uint_mul)-vector_reduction(v6, 1u, uint_mul)])){
        bind(){fun003(v111, v112, v113, v114, v2, v4, v6, v5)}
    });
};

let fun005 = fun(ref<array<int<4>,1>> v107, ref<array<real<4>,1>> v108, ref<array<int<4>,1>> v109, ref<array<real<4>,1>> v110, ref<array<real<4>,1>> v3, uint<8> v4, vector<uint<8>,3> v5, vector<uint<8>,3> v6) -> unit {
    decl vector<uint<8>,3> v7 = vector_pointwise(uint_div)(v5, v6);
    parallel(job(([vector_reduction(v7, 1u, uint_mul)-vector_reduction(v7, 1u, uint_mul)])){
        bind(){fun004(v107, v108, v3, v109, v110, v4, v6, v5)}
    });
};

let fun006 = fun((ref<array<type001,1>>,ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>) v106, vector<uint<8>,3> v2, vector<uint<8>,3> v3) -> int<4> {
    fun005(( *((tuple_member_access(v106, 0, type<ref<array<type001,1>>>)&[0])->int)), ( *((tuple_member_access(v106, 0, type<ref<array<type001,1>>>)&[0])->float)), ( *((tuple_member_access(v106, 1, type<ref<array<type001,1>>>)&[0])->int)), ( *((tuple_member_access(v106, 1, type<ref<array<type001,1>>>)&[0])->float)), ( *(tuple_member_access(v106, 2, type<ref<array<ref<array<real<4>,1>>,1>>>)&[0])), ( *(tuple_member_access(v106, 3, type<ref<array<uint<8>,1>>>)&[0])), v3, v2);
    return 0;
};

{
    decl ref<ref<array<type000,1>>> v0 = ( var(undefined(type<ref<array<type000,1>>>)));
    decl ref<type001> v99 = ( var(undefined(type<type001>)));
    ((v99->int) := undefined(type<ref<array<int<4>,1>>>));
    ((v99->float) := undefined(type<ref<array<real<4>,1>>>));
    decl ref<ref<array<type000,1>>> v1 = ( var(undefined(type<ref<array<type000,1>>>)));
    decl ref<type001> v100 = ( var(undefined(type<type001>)));
    ((v100->int) := undefined(type<ref<array<int<4>,1>>>));
    ((v100->float) := undefined(type<ref<array<real<4>,1>>>));
    decl ref<ref<array<real<4>,1>>> v2 = ( var(undefined(type<ref<array<real<4>,1>>>)));
    decl ref<uint<8>> v3 = ( var(undefined(type<uint<8>>)));
    decl ref<ref<(ref<array<type001,1>>,ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>> v103 = ( var(undefined(type<ref<(ref<array<type001,1>>,ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>>)));
    (v103 := ( new(undefined(type<(ref<array<type001,1>>,ref<array<type001,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>))));
    ( *v0);
    fun000(scalar_to_array(v0), scalar_to_array(v99));
    (tuple_ref_elem(( *v103), 0, type<ref<array<type001,1>>>) := scalar_to_array(v99));
    (tuple_ref_elem(( *v103), 1, type<ref<array<type001,1>>>) := scalar_to_array(v100));
    fun001(( *v103), scalar_to_array(v0), scalar_to_array(v99));
    decl vector<uint<8>,3> v5 = undefined(type<vector<uint<8>,3>>);
    decl vector<uint<8>,3> v6 = undefined(type<vector<uint<8>,3>>);
    fun006(( *( *v103)), v5, v6);
    ( del(( *v103)));
}
*/

} // transform
} // insieme

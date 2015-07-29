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

	NodePtr code = builder.normalize(builder.parseStmt(R"(
		{
			let twoElem = struct{int<4> int; real<4> float;};
			let store = expr lit("storeData":(ref<array<twoElem, 1>>)->unit);
			let load =  expr lit("loadData":(ref<ref<array<twoElem, 1>>>)->unit);
		
			let write = lambda (ref<array<twoElem, 1>> x)->unit {
				for(int<4> i = 0 .. 42 : 1) {
					x[i].int = i;
				}
			};
		
			let access = lambda (src<array<twoElem, 1>> x)->unit {
				for(int<4> i = 0 .. 42 : 1) {
					x[i].int;
				}
			};
		
			decl ref<ref<array<twoElem, 1>>> a = var(new(array_create_1D(lit(twoElem), 100ul))) ; 
			a = new(array_create_1D(lit(twoElem), 100ul));
			for(int<4> i = 0 .. 100 : 1) {
				decl ref<twoElem> tmp;
				(*a)[i] = *tmp;
			}
			load(a);
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
			write(*a);
			access(*a);
		
			decl ref<int<4>> e = (*a)[5].int;
		
			store(*a);
			for(int<4> i = 0 .. 100 : 1) {
				decl ref<twoElem> tmp;
				tmp = *(*a)[i];
			}
			ref_delete(*a);
		}
	)"));


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

	EXPECT_EQ(133, numberOfCompoundStmts(code));
	EXPECT_EQ(19, countMarshalledAccesses(code));
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

let fun000 = fun(ref<array<type000,1>> v1, ref<array<type001,1>> v37) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
        ((((v37&[(v2/64)])->int)&[(v2%64)]) := v2);
    };
};

let fun001 = fun(src<array<type000,1>> v1, src<array<type001,1>> v38) -> unit {
    for(decl int<4> v2 = 0 .. 42 : 1) {
        (((v38&[(v2/64)])->int)&[(v2%64)]);
    };
};

{
    decl ref<ref<array<type000,1>>> v0 = ( var(( new(array_create_1D(type<type000>, 100ul)))));
    decl ref<ref<array<type001,1>>> v32 = ( var(( new(array_create_1D(type<type001>, (100ul/64))))));
    (v0 := ( new(array_create_1D(type<type000>, 100ul))));
    (v32 := ( new(array_create_1D(type<type001>, (100ul/64)))));
    for(decl int<4> v1 = 0 .. 100 : 1) {
        decl ref<type000> v2 = ( var(undefined(type<type000>)));
        ((( *v0)&[v1]) := ( *v2));
    };
    for(decl int<4> v43 = (0/64) .. (100/64) : 1) {
        for(decl int<4> v44 = 0 .. 64 : 1) {
            ((((( *v32)&[v43])->int)&[v44]) := ( *((( *v0)&[((v43*64)+v44)])->int)));
            ((((( *v32)&[v43])->float)&[v44]) := ( *((( *v0)&[((v43*64)+v44)])->float)));
        };
    };
    for(decl uint<8> v51 = (0/64) .. (100ul/64) : 1) {
        for(decl uint<8> v52 = 0 .. 64 : 1) {
            (((( *v0)&[((v51*64)+v52)])->int) := ( *(((( *v32)&[v51])->int)&[v52])));
            (((( *v0)&[((v51*64)+v52)])->float) := ( *(((( *v32)&[v51])->float)&[v52])));
        };
    };
    loadData(v0);
    for(decl uint<8> v47 = (0/64) .. (100ul/64) : 1) {
        for(decl uint<8> v48 = 0 .. 64 : 1) {
            ((((( *v32)&[v47])->int)&[v48]) := ( *((( *v0)&[((v47*64)+v48)])->int)));
            ((((( *v32)&[v47])->float)&[v48]) := ( *((( *v0)&[((v47*64)+v48)])->float)));
        };
    };
    for(decl int<4> v3 = 0 .. 42 : 1) {
        decl ref<type000> v4 = ( var(struct{int:=( *(((( *v32)&[(v3/64)])->int)&[(v3%64)])), float:=( *(((( *v32)&[(v3/64)])->float)&[(v3%64)]))}));
        decl ref<ref<array<type000,1>>> v5 = ( var(( *v0)));
        decl ref<ref<array<type001,1>>> v34 = ( var(( *v32)));
        (v5 := ( *v0));
        (v34 := ( *v32));
        (v0 := ( *v5));
        (v32 := ( *v34));
        decl ref<ref<array<type000,1>>> v6 = ( var(undefined(type<ref<array<type000,1>>>)));
        decl ref<ref<array<type001,1>>> v35 = ( var(undefined(type<ref<array<type001,1>>>)));
        (v6 := ( *v0));
        (v35 := ( *v32));
        decl ref<ref<array<type000,1>>> v7 = ( var(array_view(( *v0), v3)));
        decl ref<ref<array<type001,1>>> v36 = ( var(array_view(( *v32), (v3/64))));
        ((((( *v36)&[(v3/64)])->int)&[(v3%64)]) := v3);
        ((((( *v32)&[(v3/64)])->int)&[(v3%64)]) := v3);
    };
    fun000(( *v0), ( *v32));
    fun001(( *v0), ( *v32));
    decl ref<int<4>> v8 = (((( *v32)&[(5/64)])->int)&[(5%64)]);
    for(decl uint<8> v53 = (0/64) .. (100ul/64) : 1) {
        for(decl uint<8> v54 = 0 .. 64 : 1) {
            (((( *v0)&[((v53*64)+v54)])->int) := ( *(((( *v32)&[v53])->int)&[v54])));
            (((( *v0)&[((v53*64)+v54)])->float) := ( *(((( *v32)&[v53])->float)&[v54])));
        };
    };
    storeData(( *v0));
    for(decl uint<8> v49 = (0/64) .. (100ul/64) : 1) {
        for(decl uint<8> v50 = 0 .. 64 : 1) {
            ((((( *v32)&[v49])->int)&[v50]) := ( *((( *v0)&[((v49*64)+v50)])->int)));
            ((((( *v32)&[v49])->float)&[v50]) := ( *((( *v0)&[((v49*64)+v50)])->float)));
        };
    };
    for(decl int<4> v55 = (0/64) .. (100/64) : 1) {
        for(decl int<4> v56 = 0 .. 64 : 1) {
            (((( *v0)&[((v55*64)+v56)])->int) := ( *(((( *v32)&[v55])->int)&[v56])));
            (((( *v0)&[((v55*64)+v56)])->float) := ( *(((( *v32)&[v55])->float)&[v56])));
        };
    };
    for(decl int<4> v9 = 0 .. 100 : 1) {
        decl ref<type000> v10 = ( var(undefined(type<type000>)));
        (v10 := ( *(( *v0)&[v9])));
    };
    ( del(( *v32)));
    ( del(( *v0)));
}*/

TEST(DataLayout, Tuple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(R"(
			{
				let twoElem = struct{int<4> int; real<4> float;};
				let tuple = (ref<array<ref<array<twoElem, 1>>, 1>>, ref<array<ref<array<real<4>, 1>>, 1>>, ref<array<uint<8>, 1>>);
			
				let subfunction = lambda (int<4> p, ref<array<twoElem, 1>> a) -> real<4> {
					decl ref<real<4>> retVal = (a[66].float);
					return *retVal;
				};
			
				let actualWork = lambda (ref<array<twoElem, 1>> a, 
                                         ref<array<real<4>, 1>> b, 
                                         uint<8> c, 
						                 vector<uint<8>,3> global_size, 
                                         vector<uint<8>,3> local_size) -> unit {

					decl ref<ref<array<twoElem, 1>>> d = var(array_view(a, 17));
					d = array_view(a, 17);
					d = a;
					b;
					c;
					decl ref<int<4>> e = a[5].int;
					decl ref<twoElem> f = var(*(a[7]));
					f.float = subfunction(8, a);
					f = *(a[7]);
				};
			
				let local = lambda (ref<array<real<4>, 1>> b, 
                                    ref<array<twoElem, 1>> a, 
                                    uint<8> c, 
						            vector<uint<8>,3> global_size, 
                                    vector<uint<8>,3> local_size) -> unit {

					parallel(job([vector_reduction(local_size, 1u, uint_mul):vector_reduction(local_size, 1u, uint_mul)]
						    , actualWork(a, b, c, local_size, global_size)
					));
				};
			
				let global = lambda (ref<array<twoElem, 1>> a, 
                                     ref<array<real<4>, 1>> b, 
                                     uint<8> c, 
						             vector<uint<8>,3> global_size, 
                                     vector<uint<8>,3> local_size) -> unit {

					decl vector<uint<8>,3> groups = vector_pointwise(uint_div)(global_size, local_size);
					parallel(job([vector_reduction(groups, 1u, uint_mul) : vector_reduction(groups, 1u, uint_mul)]
						    , local(b, a, c, local_size, global_size)
					));
					*a[0];
				};
			
				let kernelFct = lambda (tuple kernel, vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> int<4> {
					global(
						*(tuple_member_access(kernel, 0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>))[0]),
						*(tuple_member_access(kernel, 1u, lit(ref<array<ref<array<real<4>, 1>>, 1>>))[0]),
						*(tuple_member_access(kernel, 2u, lit(ref<array<uint<8>, 1>>))[0]),
						local_size, global_size);
			
					return 0;
				};
			
//				let writeToTuple = lambda (ref<(ref<array<ref<array<twoElem, 1>>, 1>>, 
//                                           ref<array<ref<array<real<4>, 1>>, 1>>, 
//                                           ref<array<uint<8>, 1>>)> lt, 
//						                   ref<array<ref<array<twoElem, 1>>, 1>> x)->unit {
//					lt;
//
////			(*x[0])[3].int = 7;
//
//					tuple_ref_elem(lt,0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>)) = x;
//				};
			
				decl ref<ref<array<twoElem, 1>>> a;
				decl ref<ref<array<real<4>, 1>>> b;

				decl ref<ref<array<twoElem, 1>>> x;
				x = array_view((*a), 23);

				decl ref<uint<8>> c;
				decl ref<ref<tuple>> t;
				t = new(undefined( tuple ));
			
				decl ref<array<ref<array<twoElem, 1>>, 1>> d = scalar_to_array(a);
				decl ref<twoElem> e = (*d[5])[5]; // !!!!
				tuple_ref_elem(*t, 0u, lit(ref<array<ref<array<twoElem, 1>>, 1>>)) = scalar_to_array(a);
				tuple_ref_elem(*t, 1u, lit(ref<array<ref<array<real<4>, 1>>, 1>>)) = scalar_to_array(b);
				tuple_ref_elem(*t, 2u, lit(ref<array<uint<8>, 1>>)) = scalar_to_array(c);
//				writeToTuple(*t, scalar_to_array(a));
			
				decl vector<uint<8>,3> ls;
				decl vector<uint<8>,3> gs;
			
				kernelFct(**t, ls, gs);
			
				ref_delete(*t);
			}
	)"));
	auto x = checks::check(code);
	EXPECT_EQ(0u, x.getErrors().size()) << x.getErrors();

	datalayout::AosToTaos att(code);
	att.transform();

	dumpPretty(code);

	auto semantic = checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	EXPECT_EQ(0u, semantic.getErrors().size()) << semantic.getErrors();

	EXPECT_EQ(83, numberOfCompoundStmts(code));
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

let fun000 = fun(vector<'elem1,#l> v1, vector<'elem2,#l> v2, ('elem1, 'elem2) => 'res v3) -> vector<'res,#l> {
    decl ref<vector<'res,#l>> v4 = ( var(undefined(type<vector<'res,#l>>)));
    for(decl uint<8> v5 = 0u .. to_uint(#l) : 1) {
        ((v4&[v5]) := v3((v1[v5]), (v2[v5])));
    };
    return ( *v4);
};

let fun001 = fun(int<4> v1, ref<array<type001,1>> v108) -> real<4> {
    decl ref<real<4>> v3 = (((v108&[(66/64)])->float)&[(66%64)]);
    return ( *v3);
};

let fun002 = fun(ref<array<type001,1>> v106, ref<array<real<4>,1>> v2, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    decl ref<ref<array<type001,1>>> v107 = ( var(array_view(v106, (17/64))));
    (v107 := array_view(v106, (17/64)));
    (v107 := v106);
    v2;
    v3;
    decl ref<int<4>> v7 = (((v106&[(5/64)])->int)&[(5%64)]);
    decl ref<type000> v8 = ( var(struct{int:=( *(((v106&[(7/64)])->int)&[(7%64)])), float:=( *(((v106&[(7/64)])->float)&[(7%64)]))}));
    ((v8->float) := fun001(8, v106));
    (v8 := struct{int:=( *(((v106&[(7/64)])->int)&[(7%64)])), float:=( *(((v106&[(7/64)])->float)&[(7%64)]))});
};

let fun003 = fun(ref<array<real<4>,1>> v1, ref<array<type001,1>> v105, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    parallel(job(([vector_reduction(v5, 1u, uint_mul)-vector_reduction(v5, 1u, uint_mul)])){
        bind(){fun002(v105, v1, v3, v5, v4)}
    });
};

let fun004 = fun(ref<array<type001,1>> v104, ref<array<real<4>,1>> v2, uint<8> v3, vector<uint<8>,3> v4, vector<uint<8>,3> v5) -> unit {
    decl vector<uint<8>,3> v6 = vector_pointwise(uint_div)(v4, v5);
    parallel(job(([vector_reduction(v6, 1u, uint_mul)-vector_reduction(v6, 1u, uint_mul)])){
        bind(){fun003(v2, v104, v3, v5, v4)}
    });
    ( *(v104&[0]));
};

let fun005 = fun((ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>) v103, vector<uint<8>,3> v2, vector<uint<8>,3> v3) -> int<4> {
    fun004(( *(tuple_member_access(v103, 0, type<ref<array<ref<array<type001,1>>,1>>>)&[0])), ( *(tuple_member_access(v103, 1, type<ref<array<ref<array<real<4>,1>>,1>>>)&[0])), ( *(tuple_member_access(v103, 2, type<ref<array<uint<8>,1>>>)&[0])), v3, v2);
    return 0;
};

{
    decl ref<ref<array<type000,1>>> v0 = ( var(undefined(type<ref<array<type000,1>>>)));
    decl ref<ref<array<type001,1>>> v98 = ( var(undefined(type<ref<array<type001,1>>>)));
    decl ref<ref<array<real<4>,1>>> v1 = ( var(undefined(type<ref<array<real<4>,1>>>)));
    decl ref<ref<array<type000,1>>> v2 = ( var(undefined(type<ref<array<type000,1>>>)));
    decl ref<ref<array<type001,1>>> v99 = ( var(undefined(type<ref<array<type001,1>>>)));
    (v2 := array_view(( *v0), 23));
    (v99 := array_view(( *v98), (23/64)));
    decl ref<uint<8>> v3 = ( var(undefined(type<uint<8>>)));
    decl ref<ref<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>> v102 = ( var(undefined(type<ref<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>>)));
    (v102 := ( new(undefined(type<(ref<array<ref<array<type001,1>>,1>>,ref<array<ref<array<real<4>,1>>,1>>,ref<array<uint<8>,1>>)>))));
    decl ref<array<ref<array<type000,1>>,1>> v5 = scalar_to_array(v0);
    decl ref<array<ref<array<type001,1>>,1>> v100 = scalar_to_array(v98);
    decl ref<type000> v6 = (( *(v5&[5]))&[5]);
    decl ref<type001> v101 = (( *(v100&[5]))&[(5/64)]);
    (tuple_ref_elem(( *v102), 0, type<ref<array<ref<array<type001,1>>,1>>>) := scalar_to_array(v98));
    (tuple_ref_elem(( *v102), 1, type<ref<array<ref<array<real<4>,1>>,1>>>) := scalar_to_array(v1));
    (tuple_ref_elem(( *v102), 2, type<ref<array<uint<8>,1>>>) := scalar_to_array(v3));
    decl vector<uint<8>,3> v7 = undefined(type<vector<uint<8>,3>>);
    decl vector<uint<8>,3> v8 = undefined(type<vector<uint<8>,3>>);
    fun005(( *( *v102)), v7, v8);
    ( del(( *v102)));
}
*/

} // transform
} // insieme

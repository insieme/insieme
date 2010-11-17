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

#include "ocl_device.h"

#pragma insieme mark
__kernel __attribute__((reqd_work_group_size(1,2,3))) void hallo(__constant double con, __global float* dfb, __local short pa,  int i) {
    float4 x = {0.0f, 0.1f, 0.2f, 5.3f};
    __local float a = 0.0f;
    __private short prvt = 3;
    unsigned int p = prvt;
/*    __local float4 b;// = {1.0f, 2.1f, 3.2f, 4.3f};
//    int2 c;
//    b = b + x;
    b.s1 = dfb[0];
*/
    p = get_global_size(0);

    barrier(CLK_LOCAL_MEM_FENCE);
}
/*
fun(real<8> v32, array<ref<real<4>>,1> v33, int<2> v28, int<4> v29, vector<uint<4>,3> v34, vector<uint<4>,3> v35)
{ {
    vector<uint<4>,3> v10 = {uint.div(subscript_single(v34, cast<uint<4>>(0)), subscript_single(v35, cast<uint<4>>(0))),
                             uint.div(subscript_single(v34, cast<uint<4>>(1)), subscript_single(v35, cast<uint<4>>(1))),
                             uint.div(subscript_single(v34, cast<uint<4>>(2)), subscript_single(v35, cast<uint<4>>(2)))};

    vector<uint<4>,3> v14 = parallel(1, uint.mul(uint.mul(subscript_single(v10, cast<uint<4>>(0)),
                                                          subscript_single(v10, cast<uint<4>>(1))),
                                                          subscript_single(v10, cast<uint<4>>(2))),
        job [real<8> v26 = v32,
             array<ref<real<4>>,1> v27 = v33,
             vector<uint<4>,3> v30 = v34,
             vector<uint<4>,3> v31 = v35]
        (,
            fun[real<8> v22 = v26,
                array<ref<real<4>>,1> v23 = v27,
                int<2> v24 = v28,
                int<4> v19 = v29,
                vector<uint<4>,3> v25 = v30,
                vector<uint<4>,3> v11 = v31]()
            {
                vector<uint<4>,3> v15 = parallel(1, uint.mul(uint.mul(subscript_single(v11, cast<uint<4>>(0)),
                                                                      subscript_single(v11, cast<uint<4>>(1))),
                                                                      subscript_single(v11, cast<uint<4>>(2))),
                    job [real<8> v16 = v22,
                         array<ref<real<4>>,1> v17 = v23,
                         int<2> v18 = v24,
                         ref<real<4>> v20 = ref.var(0.0f),
                         vector<uint<4>,3> v21 = v25]
                    (,
                        fun[real<8> v1 = v16,
                            array<ref<real<4>>,1> v2 = v17,
                            int<2> v3 = v18,
                            int<4> v4 = v19,
                            ref<real<4>> v6 = v20,
                            vector<uint<4>,3> v9 = v21]()
                        { {
                            ref<vector<ref<real<4>>,4>> v5 = ref.var({ref.var(0.0f),ref.var(0.1f),ref.var(0.2f),ref.var(5.3f)});
                            {};
                            ref<int<2>> v7 = ref.var(cast<int<2>>(3));
                            ref<uint<4>> v8 = ref.var(cast<uint<4>>(ref.deref(v7)));
                            ref.assign(v8, subscript_single(v9, cast<uint<4>>(0)));
                        } }
                ))
            }
    ));
} }*/



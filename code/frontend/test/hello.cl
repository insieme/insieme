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
//     float4 x;// = {0.0f, 0.1f, 0.2f, 5.3f};
    __local float a = 0.0f;
    __private short prvt = 3;
    int p = prvt;
/*    __local float4 b;// = {1.0f, 2.1f, 3.2f, 4.3f};
//    int2 c;
//    b = b + x;
    b.s1 = dfb[0];
*/
//    int id = get_global_id(0);


}

/* output:
fun(real<8> v31, array<ref<real<4>>,1> v32, int<2> v27, int<4> v28, vector<uint<4>,3> v33, vector<uint<4>,3> v34)
{
    parallel(1, int.mul(int.mul(subscript(v9, 0), subscript(v9, 1)), subscript(v9, 2)),
        job [real<8> v25 = v31,
             array<ref<real<4>>,1> v26 = v32,
             vector<uint<4>,3> v29 = v33,
             vector<uint<4>,3> v30 = v34]
        (,
            fun[real<8> v20 = v25,
                array<ref<real<4>>,1> v21 = v26,
                int<2> v22 = v27,
                int<4> v15 = v28,
                vector<uint<4>,3> v23 = v29,
                vector<uint<4>,3> v24 = v30]()
            {
                parallel(1, int.mul(int.mul(subscript(v8, 0), subscript(v8, 1)), subscript(v8, 2)),
                        job [real<8> v12 = v20,
                             array<ref<real<4>>,1> v13 = v21,
                             int<2> v14 = v22,
                             ref<real<4>> v16 = ref.var(0.0f),
                             ref<vector<ref<real<4>>,4>> v17 = ref.var({1.0f,2.1f,3.2f,4.3f}),
                             vector<uint<4>,3> v18 = v23, vector<uint<4>,3> v19 = v24]
                        (,
                            fun[real<8> v1 = v12,
                                array<ref<real<4>>,1> v2 = v13,
                                int<2> v3 = v14,
                                int<4> v4 = v15,
                                ref<real<4>> v6 = v16,
                                ref<vector<ref<real<4>>,4>> v7 = v17,
                                vector<uint<4>,3> v9 = v18,
                                vector<uint<4>,3> v8 = v19]()
                            { {
                                ref<vector<ref<real<4>>,4>> v5 = ref.var({0.0f,0.1f,0.2f,5.3f});
                                {}; {};
                                ref.assign(v7, vector<real>.add(ref.deref(v7), ref.deref(v5)));
                                ref.assign(subscript(ref.deref(v7), 1), ref.deref(subscript_single(v2, 0)));
                            } }
                         )
                  )
            }
        )
    )
}*/


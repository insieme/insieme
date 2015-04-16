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
#include "insieme/frontend/extensions/varuniq_extension.h"
#include "insieme/core/parser2/ir_parser.h"

using namespace insieme::core;
using namespace insieme::frontend::extensions;

TEST(VarUniq, Simple) {
	NodeManager man;
	IRBuilder builder(man);

	NodePtr fragment = builder.parseStmt(
	            "{"
				"	let twoElem = struct{int<4> int; real<4> float;};"
				"	let tuple = (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>);"
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
				"	let actualWork = (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, "
				"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
				"	};"
				""
				"	let local = (ref<array<real<4>,1>> b, ref<array<twoElem,1>> a, uint<8> c, "
				"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
				"		parallel(job([vector.reduction(local_size, 1u, uint.mul)-vector.reduction(local_size, 1u, uint.mul)]"
				"		,	actualWork(a, b, c, local_size, global_size)"
				"		));"
				"	};"
				""
				"	let global = (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, "
				"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
				"		vector<uint<8>,3> groups = vector.pointwise(uint.div)(global_size, local_size);"
				"		parallel(job([vector.reduction(groups, 1u, uint.mul)-vector.reduction(groups, 1u, uint.mul)]"
				"		,	local(b, a, c, local_size, global_size)"
				"		));"
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
				"	ref<ref<array<twoElem,1>>> a;"
				"	ref<ref<array<real<4>,1>>> b;"
				"	ref<uint<8>> c;"
				"	ref<ref<tuple>> t;"
				"	t = new(undefined(lit( (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>)) ));"
				"	ref.deref(a);"
				"	access(scalar.to.array(a));"
				"	tuple.ref.elem(*t,0u, lit(ref<array<ref<array<twoElem,1>>,1>>)) = scalar.to.array(a);"
				"	writeToTuple(*t, scalar.to.array(a));"
				""
				"	vector<uint<8>,3> ls;"
				"	vector<uint<8>,3> gs;"
				""
				"	kernelFct(**t, ls, gs);"
				""
				"	ref.delete(*t);"
				"}"
			);

	ASSERT_TRUE(fragment);

	VarUniqExtension vu((NodeAddress(fragment)));
	auto result=vu.IR();
	std::cout << *result;
	//EXPECT_PRED2(containsSubString, str, "{{}; {}; for(int<4> v5 = 0 .. 10 : 3) {ref<int<4>> v3 = v1; {};}; for(int<4> v4 = 4 .. 0 : -2) {{};}; return v1;}}");
}

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

TEST(DataLayout, Tuple) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
			"{"
			"	let twoElem = struct{int<4> int; real<4> float;};"
			"	let tuple = (ref<array<ref<array<twoElem,1>>,1>>, ref<array<ref<array<real<4>,1>>,1>>, ref<array<uint<8>,1>>);"
			""
			"	let actualWork = (ref<array<twoElem,1>> a, ref<array<real<4>,1>> b, uint<8> c, "
			"			vector<uint<8>,3> global_size, vector<uint<8>,3> local_size) -> unit {"
			"		ref<ref<array<twoElem,1>>> d = var(a);"
			"		d = a;"
			"		b;"
			"		c;"
			"		ref<int<4>> e = a[5].int;"
			"		ref<twoElem> f;"// = var((*a[0])[7]);"
	//		"		f = *a[7];"
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

//	EXPECT_EQ(117, numberOfCompoundStmts(code));
//	EXPECT_EQ(15, countMarshalledAccesses(code));
//	EXPECT_EQ(7, countMarshalledAssigns(code));
}


} // transform
} // insieme

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
			pattern::irp::compositeRefElem()), pattern::any), pattern::any) ;

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
//		"		ref<ref<array<twoElem,1>>> uninitialized;"
//		"		uninitialized = *a;"
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

	datalayout::AosToSoa ats(code);

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

	EXPECT_EQ(86, numberOfCompoundStmts(code));
	EXPECT_EQ(10, countMarshalledAccesses(code));
	EXPECT_EQ(4, countMarshalledAssigns(code));
}

TEST(DataLayout, AosToSoa2) {
	NodeManager mgr;
	IRBuilder builder(mgr);

	NodePtr code = builder.normalize(builder.parseStmt(
		"{"
		"	let twoElem = struct{int<4> int; real<4> float;};"
		"	let store = lit(\"storeData\":(ref<array<twoElem,1>>)->unit);"
		"	let load = lit(\"loadData\":(ref<ref<array<twoElem,1>>>)->unit);"
		""
		"	let access = (ref<ref<array<twoElem,1>>> x)->unit {"
		"		for(int<4> i = 0 .. 42 : 1) {"
		"			ref.deref(x)[i].int = i;"
		"		}"
		"	};"
		"	ref<ref<array<twoElem,1>>> a;"
		"	a = new(array.create.1D( lit(struct{int<4> int; real<4> float;}), 100u ));"
		"	load(a);"
		""
		"	ref<ref<array<twoElem,1>>> copy = ref.var(*a);"
		"	store(*copy);"
		""
		"	access(a);"
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

	datalayout::AosToSoa ats(code);

	dumpPretty(code);

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

//	EXPECT_EQ(35, numberOfCompoundStmts(code));
//	EXPECT_EQ(5, countMarshalledAccesses(code));
//	EXPECT_EQ(3, countMarshalledAssigns(code));
}

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

	return;
	dumpPretty(code);

	datalayout::AosToSoa ats(code);

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

	EXPECT_EQ(52, numberOfCompoundStmts(code));
	EXPECT_EQ(11, countMarshalledAccesses(code));
	EXPECT_EQ(5, countMarshalledAssigns(code));
}

} // transform
} // insieme

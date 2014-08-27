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
#include "insieme/utils/logging.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/simplify.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_visitor.h"

#include "insieme/transform/datalayout/aos_to_soa.h"

namespace insieme {
namespace transform {

using namespace core;

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

	datalayout::AosToSoa ats(code);

//	dumpPretty(code);

	auto semantic = core::checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	int cnt = 0;
	core::visitDepthFirst(code, [&](const core::CompoundStmtPtr& csp) {
		++cnt;
	});

	EXPECT_EQ(68, cnt);
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
		"	access(a);"
		"	store(*a);"
		"	ref.delete(*a);"
		"}"
	));

	datalayout::AosToSoa ats(code);

//	dumpPretty(code);

	auto semantic = core::checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	int cnt = 0;
	core::visitDepthFirst(code, [&](const core::CompoundStmtPtr& csp) {
		++cnt;
	});

	EXPECT_EQ(35, cnt);
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

	datalayout::AosToSoa ats(code);

//	dumpPretty(code);

	auto semantic = core::checks::check(code);
	auto warnings = semantic.getWarnings();
	std::sort(warnings.begin(), warnings.end());
	for_each(warnings, [](const core::checks::Message& cur) {
		LOG(INFO) << cur << std::endl;
	});

	auto errors = semantic.getErrors();
	EXPECT_EQ(0u, errors.size()) ;

	std::sort(errors.begin(), errors.end());
	for_each(errors, [](const core::checks::Message& cur) {
		std::cout << cur << std::endl;
	});

	int cnt = 0;
	core::visitDepthFirst(code, [&](const core::CompoundStmtPtr& csp) {
		++cnt;
	});

	EXPECT_EQ(52, cnt);
}

} // transform
} // insieme

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

// defines which are needed by LLVM
#define __STDC_LIMIT_MACROS
#define __STDC_CONSTANT_MACROS

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include <clang/AST/Decl.h>
#pragma GCC diagnostic pop
// DON'T MOVE THIS!

#include <gtest/gtest.h>

#include "insieme/annotations/expected_ir_annotation.h"

#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/utils/logging.h"

#include "insieme/frontend/translation_unit.h"
#include "insieme/utils/config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "test_utils.inc"

using namespace insieme::core;
using namespace insieme::driver;
using namespace insieme::core::checks;
using namespace insieme::utils::log;
using namespace insieme::frontend::extensions;
using namespace clang;

namespace ia = insieme::annotations;

void checkSemanticErrors(const NodePtr& node) {
	auto msgList = check( node, checks::getFullCheck() ).getAll();
	EXPECT_EQ(static_cast<unsigned int>(0), msgList.size());
	std::sort(msgList.begin(), msgList.end());
	std::for_each(msgList.begin(), msgList.end(), [&node](const Message& cur) {
		LOG(INFO) << *node;
		LOG(INFO) << cur << std::endl;
	});
}

std::string getPrettyPrinted(const NodePtr& node) {
	std::ostringstream ss;
	ss << insieme::core::printer::PrettyPrinter(node,
			insieme::core::printer::PrettyPrinter::OPTIONS_DETAIL |
			insieme::core::printer::PrettyPrinter::NO_LET_BINDINGS
	);

	// Remove new lines and leading spaces
	std::vector<char> res;
	std::string prettyPrint = ss.str();
	for(auto it = prettyPrint.begin(), end = prettyPrint.end(); it != end; ++it)
		if(!(*it == '\n' || (it + 1 != end && *it == ' ' && *(it+1) == ' ')))
			res.push_back(*it);

	return std::string(res.begin(), res.end());
}


TEST(StmtConversion, FileTest) {
	NodeManager manager;
	const std::string filename = FRONTEND_TEST_DIR "/inputs/stmt.c";
	std::vector<std::string> argv = { "compiler", filename };
	cmd::Options options = cmd::Options::parse(argv);
	options.job.frontendExtensionInit();
	insieme::frontend::TranslationUnit tu(manager, filename, options.job);

	auto filter = [](const insieme::frontend::pragma::Pragma& curr) { return curr.getType() == "test::expected"; };

	const auto begin = tu.pragmas_begin(filter);
	const auto end = tu.pragmas_end();

	EXPECT_NE(begin, end);

	NodeManager mgr;
	insieme::frontend::conversion::Converter convFactory(mgr, tu, options.job);
	convFactory.convert();

	auto resolve = [&](const NodePtr& cur) { return convFactory.getIRTranslationUnit().resolve(cur); };

	for(auto it = begin; it != end; ++it) {
		const auto pragma = *it;

		if(pragma->isStatement()) {
			NodePtr node = insieme::frontend::fixVariableIDs(resolve(convFactory.convertStmt(pragma->getStatement())));
			EXPECT_TRUE(node->hasAnnotation(ia::ExpectedIRAnnotation::KEY));
			EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(node), '\"' + getPrettyPrinted(node) + '\"' );
			// do semantics checking
			checkSemanticErrors(node);

		} else {
			if(const clang::TypeDecl* td = dyn_cast<const clang::TypeDecl>(pragma->getDecl())) {
				TypePtr type = resolve(convFactory.convertType(td->getTypeForDecl()->getCanonicalTypeInternal())).as<TypePtr>();
				EXPECT_TRUE(type->hasAnnotation(ia::ExpectedIRAnnotation::KEY));
				EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(type), '\"' + getPrettyPrinted(type) + '\"' );
				// do semantics checking
				checkSemanticErrors(type);
			} else if(const clang::FunctionDecl* fd = dyn_cast<const clang::FunctionDecl>(pragma->getDecl())) {
				LambdaExprPtr expr = insieme::core::dynamic_pointer_cast<const insieme::core::LambdaExpr>(resolve(convFactory.convertFunctionDecl(fd)).as<LambdaExprPtr>());
				ASSERT_TRUE(expr);
				EXPECT_TRUE(expr->hasAnnotation(ia::ExpectedIRAnnotation::KEY));
				EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(expr), '\"' + getPrettyPrinted(analysis::normalize(expr)) + '\"' );
				// do semantics checking
				checkSemanticErrors(expr);
			}
		}
	}
}

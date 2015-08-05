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

#include "insieme/frontend/clang.h"

#include "insieme/annotations/expected_ir_annotation.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/translation_unit.h"

#include "insieme/utils/config.h"
#include "insieme/utils/logging.h"

using namespace insieme;
using namespace insieme::core;
using namespace insieme::driver;
using namespace insieme::utils::log;
using namespace insieme::frontend::conversion;
using namespace insieme::frontend::extensions;

namespace ia = insieme::annotations;

TEST(TypeCast, FileTest) {

	NodeManager manager;
	const std::string filename = FRONTEND_TEST_DIR "/inputs/casts.c";
	std::vector<std::string> argv = { "compiler", filename };
	cmd::Options options = cmd::Options::parse(argv);
	options.job.frontendExtensionInit();
	insieme::frontend::TranslationUnit tu(manager, filename, options.job);
	
	auto filter = [](const insieme::frontend::pragma::Pragma& curr) {
		return curr.getType() == "test::expected";
	};
	
	const auto begin = tu.pragmas_begin(filter);
	const auto end = tu.pragmas_end();
	
	EXPECT_NE(begin, end);
	
	for(auto it = begin; it != end; ++it) {
		const auto pragma = *it;
		// we use an internal manager to have private counter for variables so we can write independent tests
		NodeManager mgr;
		
		Converter convFactory(mgr, tu);
		convFactory.convert();
		
		if(pragma->isStatement()) {
			NodePtr node = analysis::normalize(convFactory.convertStmt(pragma->getStatement()));
			EXPECT_TRUE(node->hasAnnotation(ia::ExpectedIRAnnotation::KEY));
			EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(node), '\"' + toString(printer::PrettyPrinter(node, printer::PrettyPrinter::PRINT_SINGLE_LINE)) + '\"');
		}
		else {
			if(const clang::TypeDecl* td = llvm::dyn_cast<const clang::TypeDecl>(pragma->getDecl())) {
				NodePtr node = convFactory.convertType(td->getTypeForDecl()->getCanonicalTypeInternal());
				EXPECT_TRUE(node->hasAnnotation(ia::ExpectedIRAnnotation::KEY));
				EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(node), '\"' + node->toString() + '\"');
			}
			else if(const clang::VarDecl* vd = llvm::dyn_cast<const clang::VarDecl>(pragma->getDecl())) {
				NodePtr node = convFactory.convertVarDecl(vd);
				EXPECT_EQ(ia::ExpectedIRAnnotation::getValue(node), '\"' + node->toString() + '\"');
			}
		}
	}
}

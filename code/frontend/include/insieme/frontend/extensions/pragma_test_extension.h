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

#ifndef PRAGMA_TEST_EXTENSION_H
#define PRAGMA_TEST_EXTENSION_H

#include <functional>
#include <string>

#include "insieme/core/ir_node.h"
#include "insieme/frontend/extensions/frontend_plugin.h"
#include "insieme/frontend/pragma/handler.h"
#include "insieme/frontend/pragma/matcher.h"

namespace clang {
class Preprocessor;
}

namespace insieme {
namespace frontend {
namespace conversion {

class Converter;

} // end convert namespace

namespace extensions {

/**
 * Custom pragma used for testing purposes;
 *
 * #pragma test "insieme-IR"
 * C stmt
 *
 * checks if the conversion of the C statement matches the one specified by the user
 */
class TestPragma: public insieme::frontend::pragma::Pragma, public FrontendPlugin {
	std::string expected;

	std::function<stmtutils::StmtWrapper(const insieme::frontend::pragma::MatchObject&, stmtutils::StmtWrapper)>
	getMarkerAttachmentLambda();

public:
	TestPragma();
	TestPragma(const clang::SourceLocation &s1, const clang::SourceLocation &s2, const std::string &str,
			   const pragma::MatchMap &mm);
	std::string getExpected() const { return expected; }
};

}}}

#endif // PRAGMA_TEST_EXTENSION_H

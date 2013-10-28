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

#include "insieme/frontend/stmt_converter.h"
#include "insieme/frontend/expr_converter.h"
#include "insieme/frontend/type_converter.h"

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/frontend/program.h"
#include "insieme/frontend/clang_config.h"
#include "insieme/frontend/convert.h"
#include "insieme/frontend/pragma/insieme.h"

#include "insieme/frontend/extensions/clang_stage_plugin.h"

bool declVisited = false;

/**
 *  This is a user provided clang stage plugin that converts
 *  every clang type to a generic type with the name value
 *  GenericTypeCreator.
 */
class ClangTestPlugin : public insieme::frontend::extensions::ClangStagePlugin {

	virtual core::TypePtr Visit(const clang::Type* type, frontend::conversion::Converter& convFact, core::IRBuilder builder) {
	    if(llvm::isa<clang::BuiltinType>(type))
            if(llvm::cast<clang::BuiltinType>(type)->getKind() == clang::BuiltinType::Kind::NullPtr)
                return builder.genericType("GenericTypeCreator");
        return nullptr;
	}

	virtual void Visit(const clang::Decl* decl, frontend::conversion::Converter& convFact, core::IRBuilder builder) {
        declVisited = true;
	}

};

/**
 *  This test checks if the user provided
 *  clang plugin registration works correctly.
 */
TEST(ClangStage, Initialization) {
	//initialization
	insieme::core::NodeManager mgr;
	insieme::frontend::Program p(mgr, "/home/stefanm/varargs.c");
	insieme::frontend::conversion::Converter conv(mgr, p);

	// register the clang stage plugin
	conv.registerClangHandler<ClangTestPlugin>();
	EXPECT_EQ(1, conv.getClangHandlers().size());
	conv.convert();
}

/**
 *  This test checks if the user provided
 *  clang plugin works correctly. A clang
 *  type is created and should be converted
 *  with the user provided visitor instead
 *  of the insieme type visitor.
 */

TEST(ClangStage, Conversion) {
	//initialization
	insieme::core::NodeManager mgr;
	insieme::frontend::Program p(mgr, SRC_DIR "/inputs/simple.c");
	insieme::frontend::conversion::Converter conv(mgr, p);

	// register the clang stage plugin
	conv.registerClangHandler<ClangTestPlugin>();

	//lets create a clang expression and pass it to the converter
	insieme::core::TypePtr convertedTy = conv.convertType(new clang::BuiltinType(clang::BuiltinType::Kind::NullPtr));

	//this should now be a generic type that contains the name GenericTypeCreator
	EXPECT_TRUE(convertedTy.isa<insieme::core::GenericTypePtr>());
	if(convertedTy.isa<insieme::core::GenericTypePtr>()) {
		std::string s = static_pointer_cast<insieme::core::GenericTypePtr>(convertedTy)->getName()->getValue();
		EXPECT_EQ("GenericTypeCreator", s);
	}

	//check if the decl visitors is visited correctly
	EXPECT_FALSE(declVisited);
	conv.convert();
	EXPECT_TRUE(declVisited);
}

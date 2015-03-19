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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_types.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/lang/extension.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

	//Helper extension used to test the named extensions system
	class NamedCoreExtensionTestExtension : public core::lang::Extension {

		/**
		 * Allow the node manager to create instances of this class.
		 */
		friend class core::NodeManager;

		/**
		 * Creates a new instance based on the given node manager.
		 */
		NamedCoreExtensionTestExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:

		LANG_EXT_TYPE_WITH_NAME(SimpleNamedType, "SimpleNamedType", "struct { 'a foo; }")

		LANG_EXT_TYPE_WITH_NAME(SimpleNamedTypeReusingUnknown, "SimpleNamedTypeReusingUnknown", "struct { FooType foo; }")

		LANG_EXT_TYPE_WITH_NAME(SimpleNamedTypeReusingKnown, "SimpleNamedTypeReusingKnown", "struct { SimpleNamedType foo; }")

		LANG_EXT_LITERAL_WITH_NAME(SimpleNamedLiteralUnknown, "SimpleNamedLiteralUnknown", "named_lit_unknown", "(FooType)->unit")

		LANG_EXT_LITERAL_WITH_NAME(SimpleNamedLiteral, "SimpleNamedLiteral", "named_lit", "(SimpleNamedType)->unit")
	};

	TEST(NamedCoreExtensionTest, NamedLookup) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();
		auto& definedNames = extension.getNamedIrExtensions();

		//Lookup unknown
		EXPECT_TRUE(definedNames.find("NotRegisteredName") == definedNames.end());

		//Lookup a registered type
		EXPECT_TRUE(definedNames.find("SimpleNamedType")->second == extension.getSimpleNamedType());

		//Lookup a registered literal
		EXPECT_TRUE(definedNames.find("SimpleNamedLiteral")->second == extension.getSimpleNamedLiteral());
	}

	TEST(NamedCoreExtensionTest, NamedTypes) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		auto& simpleNamedType = extension.getSimpleNamedType();
		EXPECT_EQ("struct<foo:'a>", toString(*simpleNamedType));

		//Test for the re-use of an unknown named extension
		auto& simpleNamedTypeReusingUnknown = extension.getSimpleNamedTypeReusingUnknown();
		EXPECT_EQ("struct<foo:FooType>", toString(*simpleNamedTypeReusingUnknown));

		//Test for correct handling of a known named extension
		auto& simpleNamedTypeReusingKnown = extension.getSimpleNamedTypeReusingKnown();
		EXPECT_EQ("struct<foo:struct<foo:'a>>", toString(*simpleNamedTypeReusingKnown));
	}

	TEST(NamedCoreExtensionTest, NamedLiterals) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		//Test for the re-use of an unknown named extension
		auto& simpleNamedLiteralUnknown = extension.getSimpleNamedLiteralUnknown();
		EXPECT_EQ("named_lit_unknown", toString(*simpleNamedLiteralUnknown));
		EXPECT_EQ("((FooType)->unit)", toString(*simpleNamedLiteralUnknown.getType()));

		//Test for correct handling of a known named extension
		auto& simpleNamedLiteral = extension.getSimpleNamedLiteral();
		EXPECT_EQ("named_lit", toString(*simpleNamedLiteral));
		EXPECT_EQ("((struct<foo:'a>)->unit)", toString(*simpleNamedLiteral.getType()));
	}

} // end namespace lang
} // end namespace core
} // end namespace insieme


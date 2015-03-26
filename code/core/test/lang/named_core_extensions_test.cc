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

#include "insieme/utils/assert.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

	//Helper extension used to test the named extensions system
	class NamedCoreExtensionTestExtension : public core::lang::Extension {

		friend class core::NodeManager;

		NamedCoreExtensionTestExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:

		LANG_EXT_TYPE_WITH_NAME(NamedTypeUsingBelow, "NamedTypeUsingBelow", "struct { NamedType foo; }")

		LANG_EXT_TYPE_WITH_NAME(NamedType, "NamedType", "struct { 'a foo; }")

		LANG_EXT_TYPE_WITH_NAME(NamedTypeReusingUnknown, "NamedTypeReusingUnknown", "struct { FooType foo; }")

		LANG_EXT_TYPE_WITH_NAME(NamedTypeReusingKnown, "NamedTypeReusingKnown", "struct { NamedType foo; }")

		LANG_EXT_LITERAL_WITH_NAME(NamedLiteralUnknown, "NamedLiteralUnknown", "named_lit_unknown", "(FooType)->unit")

		LANG_EXT_LITERAL_WITH_NAME(NamedLiteral, "NamedLiteral", "named_lit", "(NamedType)->unit")

		LANG_EXT_DERIVED_WITH_NAME(NamedDerivedUnknown, "NamedDerivedUnknown", "let foo = FooType in (foo x)->foo { return x; }")

		LANG_EXT_DERIVED_WITH_NAME(NamedDerived, "NamedDerived", "let foo = NamedType in (foo x)->foo { return x; }")
	};

	TEST(NamedCoreExtensionTest, NamedLookup) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();
		auto& definedNames = extension.getNamedIrExtensions();

		//Lookup unknown
		EXPECT_TRUE(definedNames.find("NotRegisteredName") == definedNames.end());

		//Lookup a registered type
		EXPECT_TRUE(definedNames.find("NamedType")->second == extension.getNamedType());

		//Lookup a registered literal
		EXPECT_TRUE(definedNames.find("NamedLiteral")->second == extension.getNamedLiteral());

		//Lookup a registered derived
		EXPECT_TRUE(definedNames.find("NamedDerived")->second == extension.getNamedDerived());
	}

	TEST(NamedCoreExtensionTest, NamedTypes) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		auto& namedType = extension.getNamedType();
		EXPECT_EQ("struct<foo:'a>", toString(*namedType));

		//Test for the re-use of a named extension which is defined below the current one and therefore won't be found
		auto& namedTypeUsingBelow = extension.getNamedTypeUsingBelow();
		EXPECT_EQ("struct<foo:NamedType>", toString(*namedTypeUsingBelow));

		//Test for the re-use of an unknown named extension
		auto& namedTypeReusingUnknown = extension.getNamedTypeReusingUnknown();
		EXPECT_EQ("struct<foo:FooType>", toString(*namedTypeReusingUnknown));

		//Test for correct handling of a known named extension
		auto& namedTypeReusingKnown = extension.getNamedTypeReusingKnown();
		EXPECT_EQ("struct<foo:struct<foo:'a>>", toString(*namedTypeReusingKnown));
	}

	TEST(NamedCoreExtensionTest, NamedLiterals) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		//Test for the re-use of an unknown named extension
		auto& namedLiteralUnknown = extension.getNamedLiteralUnknown();
		EXPECT_EQ("named_lit_unknown", toString(*namedLiteralUnknown));
		EXPECT_EQ("((FooType)->unit)", toString(*namedLiteralUnknown.getType()));

		//Test for correct handling of a known named extension
		auto& namedLiteral = extension.getNamedLiteral();
		EXPECT_EQ("named_lit", toString(*namedLiteral));
		EXPECT_EQ("((struct<foo:'a>)->unit)", toString(*namedLiteral.getType()));
	}

	TEST(NamedCoreExtensionTest, NamedDerived) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		//Test for the re-use of an unknown named extension
		auto& namedDerivedUnknown = extension.getNamedDerivedUnknown();
		EXPECT_EQ("rec v0.{v0=fun(FooType v1) {return v1;}}", toString(*namedDerivedUnknown));
		EXPECT_EQ("((FooType)->FooType)", toString(*namedDerivedUnknown.getType()));

		//Test for correct handling of a known named extension
		auto& namedDerived = extension.getNamedDerived();
		EXPECT_EQ("rec v0.{v0=fun(struct<foo:'a> v1) {return v1;}}", toString(*namedDerived));
		EXPECT_EQ("((struct<foo:'a>)->struct<foo:'a>)", toString(*namedDerived.getType()));
	}



	//Helper extension used to test assertion when using a name twice
	class NamedCoreExtensionTestDuplicatedExtension : public core::lang::Extension {

		friend class core::NodeManager;

		NamedCoreExtensionTestDuplicatedExtension(core::NodeManager& manager)
				: core::lang::Extension(manager) {}

	public:

		LANG_EXT_TYPE_WITH_NAME(NamedType, "NamedType", "struct { 'a foo; }")

		//Note the re-use of the same IR_NAME
		LANG_EXT_TYPE_WITH_NAME(NamedType2, "NamedType", "struct { 'a foo; }")
	};

	TEST(NamedCoreExtensionTest, AssertNameCollisionDeathTest) {
		NodeManager manager;

		assert_decl(ASSERT_DEATH(manager.getLangExtension<NamedCoreExtensionTestDuplicatedExtension>(), "IR_NAME \"NamedType\" already in use in this extension"););
	}
} // end namespace lang
} // end namespace core
} // end namespace insieme


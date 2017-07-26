/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <gtest/gtest.h>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_types.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/lang/extension.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/name_mangling.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

	// Helper extension used to test the named extensions system
	class NamedCoreExtensionTestExtension : public core::lang::Extension {
		friend class core::NodeManager;

		NamedCoreExtensionTestExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:

		TYPE_ALIAS("NamedType", "struct { foo : 'a; }")

		LANG_EXT_TYPE_WITH_NAME(NamedTypeUsingBelow, "NamedTypeUsingBelow", "struct { foo : NamedType; }")

		LANG_EXT_TYPE_WITH_NAME(NamedTypeReusingUnknown, "NamedTypeReusingUnknown", "struct { foo : FooType; }")

		LANG_EXT_TYPE_WITH_NAME(NamedTypeReusingKnown, "NamedTypeReusingKnown", "struct { foo : NamedType; }")

		LANG_EXT_LITERAL_WITH_NAME(NamedLiteralUnknown, "NamedLiteralUnknown", "named_lit_unknown", "(FooType)->unit")

		LANG_EXT_LITERAL_WITH_NAME(NamedLiteral, "NamedLiteral", "named_lit", "(NamedType)->unit")

		LANG_EXT_DERIVED_WITH_NAME(NamedDerivedUnknown, "NamedDerivedUnknown", "alias foo = FooType; (x : foo)->foo { return x; }")

		LANG_EXT_DERIVED(NamedDerived, "alias foo = NamedType; (x : foo)->foo { return x; }")
	};

	TEST(NamedCoreExtensionTest, NamedLookup) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();
		auto& definedNames = extension.getDefinedSymbols();

		// Lookup unknown
		EXPECT_TRUE(definedNames.find("NotRegisteredName") == definedNames.end());

		// Lookup a registered literal
		EXPECT_TRUE(definedNames.find("NamedLiteral")->second() == extension.getNamedLiteral());

		// Lookup a registered derived
		EXPECT_TRUE(definedNames.find("named_derived")->second() == extension.getNamedDerived());
	}

	TEST(NamedCoreExtensionTest, NamedTypes) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		// Test for the re-use of a named extension which is defined below the current one and therefore won't be found
		auto& namedTypeUsingBelow = extension.getNamedTypeUsingBelow();
		EXPECT_EQ("struct {foo:struct {foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>},"
				  "ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}",
		          toString(*namedTypeUsingBelow));

		// Test for the re-use of an unknown named extension
		auto& namedTypeReusingUnknown = extension.getNamedTypeReusingUnknown();
		EXPECT_EQ("struct {foo:FooType,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),"
				  "dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}",
		          toString(*namedTypeReusingUnknown));

		// Test for correct handling of a known named extension
		auto& namedTypeReusingKnown = extension.getNamedTypeReusingKnown();
		EXPECT_EQ("struct {foo:struct {foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>},"
				  "ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}",
		          toString(*namedTypeReusingKnown));
	}

	TEST(NamedCoreExtensionTest, NamedLiterals) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		// Test for the re-use of an unknown named extension
		auto& namedLiteralUnknown = extension.getNamedLiteralUnknown();
		EXPECT_EQ("named_lit_unknown", toString(*namedLiteralUnknown));
		EXPECT_EQ("((FooType)->unit)", toString(*namedLiteralUnknown.getType()));

		// Test for correct handling of a known named extension
		auto& namedLiteral = extension.getNamedLiteral();
		EXPECT_EQ("named_lit", toString(*namedLiteral));
		EXPECT_EQ("((struct {foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
				  + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
				  + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>})->unit)",
		          toString(*namedLiteral.getType()));
	}

	TEST(NamedCoreExtensionTest, NamedDerived) {
		NodeManager manager;

		auto& extension = manager.getLangExtension<NamedCoreExtensionTestExtension>();

		// Test for the re-use of an unknown named extension
		auto& namedDerivedUnknown = extension.getNamedDerivedUnknown();
		EXPECT_EQ("rec NamedDerivedUnknown.{NamedDerivedUnknown=fun(ref<FooType,f,f,plain> v0) {return ref_deref(v0);}}",
		          toString(*namedDerivedUnknown));
		EXPECT_EQ("((FooType)->FooType)", toString(*namedDerivedUnknown.getType()));

		// Test for correct handling of a known named extension
		auto& namedDerived = extension.getNamedDerived();
		EXPECT_EQ("rec named_derived.{named_derived=fun(ref<struct "
			      "{foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
			      + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>},f,f,plain> v0) {return ref_deref(v0);}}",
			      toString(*namedDerived));
		EXPECT_EQ("((struct {foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
			      + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
			      + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>})->struct {foo:'a,ctor(),ctor(ref<^,t,f,cpp_ref>),"
			      "ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
			      + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>})",
			      toString(*namedDerived.getType()));
	}


	// Helper extension used to test assertion when using a name twice
	class NamedCoreExtensionTestDuplicatedExtension : public core::lang::Extension {
		friend class core::NodeManager;

		NamedCoreExtensionTestDuplicatedExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		LANG_EXT_LITERAL_WITH_NAME(NamedLiteral, "NamedLiteral", "named_lit", "(NamedType)->unit")

		// Note the re-use of the same IR_NAME
		LANG_EXT_LITERAL_WITH_NAME(NamedLiteral2, "NamedLiteral", "named_lit", "(NamedType)->unit")

	};

	TEST(NamedCoreExtensionTest, AssertNameCollisionDeathTest) {
		NodeManager manager;

		assert_decl(
		    ASSERT_DEATH(manager.getLangExtension<NamedCoreExtensionTestDuplicatedExtension>(), "IR_NAME \"NamedLiteral\" already in use in this extension"););
	}
} // end namespace lang
} // end namespace core
} // end namespace insieme

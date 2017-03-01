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
 *
 */
#include <gtest/gtest.h>

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_node_types.h"
#include "insieme/core/ir_pointer.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/extension.h"
#include "insieme/core/parser/ir_parser.h"

#include "insieme/core/test/test_utils.h"

#include "insieme/utils/assert.h"
#include "insieme/utils/name_mangling.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

	TEST(NamedCoreExtensionParserTest, ParserAssertsDeathTest) {
		NodeManager manager;

		EXPECT_THROW(parser::parseStmt(manager, "using \"ext.unknown_extension\";", true), parser::IRParserException);
	}

	TEST(NamedCoreExtensionParserTest, ParserSingleStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_EQ("AP(ref<('a,'a),f,f,plain> v0 = ref_decl(type<ref<('a,'a),f,f,plain>>))",
		          toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; var ref<complex> a;"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserCompoundStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		// both instances of type complex should be expanded
		EXPECT_EQ("AP({ref<('a,'a),f,f,plain> v0 = ref_decl(type<ref<('a,'a),f,f,plain>>); ref<('a,'a),f,f,plain> v1 = ref_decl(type<ref<('a,'a),f,f,plain>>);})",
			      toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; { var ref<complex> a; var ref<complex> b; }"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserCompoundNestedStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		// the nested instance of complex should also be expanded
		EXPECT_EQ("AP({ref<('a,'a),f,f,plain> v0 = ref_decl(type<ref<('a,'a),f,f,plain>>); {ref<('a,'a),f,f,plain> v1 = ref_decl(type<ref<('a,'a),f,f,plain>>);};})",
			      toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; { var ref<complex> a; { var ref<complex> b; }}"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserMultipleUsing) {
		NodeManager manager;
		IRBuilder builder(manager);

		// both named extensions should be expanded
		EXPECT_TRUE(parser::parseStmt(manager, "using \"ext.complex\"; using \"ext.enum\"; { auto a = enum_to_int; var ref<complex> b; }"));
	}


	// Helper extension used to test the check for already existing named constructs in the parser
	class NamedCoreExtensionParserTestExtension : public core::lang::Extension {
		friend class core::NodeManager;

		NamedCoreExtensionParserTestExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		TYPE_ALIAS("complex","real<16>")
	};

	TEST(NamedCoreExtensionParserTest, ParserAlreadyExistingNameTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		const auto& existingNames = manager.getLangExtension<NamedCoreExtensionParserTestExtension>().getDefinedSymbols();
		const auto& typeAliases = manager.getLangExtension<NamedCoreExtensionParserTestExtension>().getTypeAliases();

		// As I passed the extension with the name "complex" already defined this should be expanded
		EXPECT_EQ("AP(ref<real<16>,f,f,plain> v0 = ref_decl(type<ref<real<16>,f,f,plain>>))",
			      toString(builder.normalize(parser::parseStmt(manager, "var ref<complex,f,f,plain> a;", false, existingNames, typeAliases))));

		// inside of a compound stmt we shadow previous declarations
		EXPECT_TRUE(parser::parseStmt(manager, "using \"ext.complex\"; var ref<complex> a;", false, existingNames));

	}

	TEST(NamedCoreExtensionParserTestExtension, Semantic) {
		NodeManager nm;

		const NamedCoreExtensionParserTestExtension& ext = nm.getLangExtension<NamedCoreExtensionParserTestExtension>();

		semanticCheckSecond(ext.getDefinedSymbols());
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme

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
#include "insieme/core/parser3/ir_parser.h"

#include "insieme/core/lang/complex_extension.h"
#include "insieme/core/test/test_utils.h"

#include "insieme/utils/assert.h"

#include <string>
#include <map>

namespace insieme {
namespace core {
namespace lang {

TEST(NamedCoreExtensionParserTest, ParserAssertsDeathTest) {
	NodeManager manager;
	
	assert_decl(ASSERT_DEATH(parser3::parse_stmt(manager, "using \"ext.unknown_extension\"; decl complex a;"),
	                         "Can't find extension with name \"ext.unknown_extension\". Please check the name and also register it in the constructor of ExtensionRegistry"););
}

TEST(NamedCoreExtensionParserTest, ParserSingleStatement) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	EXPECT_EQ("AP({struct<_real:'a,_img:'a> v0 = undefined(type<struct<_real:'a,_img:'a>>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ using \"ext.complex\"; decl complex a; }"))));
}

TEST(NamedCoreExtensionParserTest, ParserMultipleStatements) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	//the second instance of type complex shouldn't be expanded
	EXPECT_EQ("AP({{struct<_real:'a,_img:'a> v0 = undefined(type<struct<_real:'a,_img:'a>>);}; complex v1 = undefined(type<complex>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ {using \"ext.complex\"; decl complex a;} decl complex b;}"))));
}

TEST(NamedCoreExtensionParserTest, ParserCompoundStatement) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	//both instances of type complex should be expanded
	EXPECT_EQ("AP({struct<_real:'a,_img:'a> v0 = undefined(type<struct<_real:'a,_img:'a>>); struct<_real:'a,_img:'a> v1 = undefined(type<struct<_real:'a,_img:'a>>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{using \"ext.complex\"; decl complex a; decl complex b;}"))));
}

TEST(NamedCoreExtensionParserTest, ParserCompoundNestedStatement) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	//the nested instance of complex should also be expanded
	EXPECT_EQ("AP({struct<_real:'a,_img:'a> v0 = undefined(type<struct<_real:'a,_img:'a>>); {struct<_real:'a,_img:'a> v1 = undefined(type<struct<_real:'a,_img:'a>>);};})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ using \"ext.complex\"; decl complex a; { decl complex b; }}"))));
}

TEST(NamedCoreExtensionParserTest, ParserCompoundNestedStatementOutside) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	//only the nested instance of complex should be expanded
	EXPECT_EQ("AP({{struct<_real:'a,_img:'a> v0 = undefined(type<struct<_real:'a,_img:'a>>);}; complex v1 = undefined(type<complex>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ { using \"ext.complex\"; decl complex a; } decl complex b;}"))));
}

TEST(NamedCoreExtensionParserTest, ParserMultipleUsing) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	//both named extensions should be expanded
	EXPECT_EQ("AP({bool v0 = rec v0.{v0=fun('a v1) {return int_ne(enum_to_int(v1), 0);}}; struct<_real:'a,_img:'a> v1 = undefined(type<struct<_real:'a,_img:'a>>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ using \"ext.complex\",\"ext.enum\"; decl bool a = enum_element_as_bool; decl complex b; }"))));
}



//Helper extension used to test the check for already existing named constructs in the parser
class NamedCoreExtensionParserTestExtension : public core::lang::Extension {

	friend class core::NodeManager;
	
	NamedCoreExtensionParserTestExtension(core::NodeManager& manager)
		: core::lang::Extension(manager) {}
		
public:

	LANG_EXT_TYPE_WITH_NAME(NamedType, "complex", "struct { NamedType foo; }")
};

TEST(NamedCoreExtensionParserTest, ParserAlreadyExistingNameDeathTest) {
	NodeManager manager;
	IRBuilder builder(manager);
	
	const auto& existingNames = manager.getLangExtension<NamedCoreExtensionParserTestExtension>().getNamedIrExtensions();
	
	//As I passed the extension with the name "complex" already defined this should be expanded
	EXPECT_EQ("AP({struct<foo:NamedType> v0 = undefined(type<struct<foo:NamedType>>);})",
	          toString(builder.normalize(parser3::parse_stmt(manager, "{ decl complex a; }", false, existingNames))));
	          
	// inside of a compound stmt we shadow previous declarations
	EXPECT_TRUE(parser3::parse_stmt(manager, " { using \"ext.complex\"; decl complex a; }", false, existingNames));
	
	EXPECT_FALSE(parser3::parse_expr(manager, " using \"ext.complex\";  1 ", false, existingNames));
}

TEST(ConstTypeExtensionTest, Semantic) {
	NodeManager nm;
	
	const NamedCoreExtensionParserTestExtension& ext = nm.getLangExtension<NamedCoreExtensionParserTestExtension>();
	
	semanticCheckSecond(ext.getNamedIrExtensions());
}



} // end namespace lang
} // end namespace core
} // end namespace insieme


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

		EXPECT_THROW(parser::parseStmt(manager, "using \"ext.unknown_extension\"; var complex a;", true), parser::IRParserException);
	}

	TEST(NamedCoreExtensionParserTest, ParserSingleStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		EXPECT_EQ("AP(('a,'a) v0 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<('a,'a)>))",
		          toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; var complex a;"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserCompoundStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		// both instances of type complex should be expanded
		EXPECT_EQ("AP({('a,'a) v0 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<('a,'a)>); "
			      "('a,'a) v1 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<('a,'a)>);})",
			      toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; { var complex a; var complex b; }"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserCompoundNestedStatement) {
		NodeManager manager;
		IRBuilder builder(manager);

		// the nested instance of complex should also be expanded
		EXPECT_EQ("AP({('a,'a) v0 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<('a,'a)>); "
			      "{('a,'a) v1 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<('a,'a)>);};})",
			      toString(builder.normalize(parser::parseStmt(manager, "using \"ext.complex\"; { var complex a; { var complex b; }}"))));
	}

	TEST(NamedCoreExtensionParserTest, ParserMultipleUsing) {
		NodeManager manager;
		IRBuilder builder(manager);

		// both named extensions should be expanded
		EXPECT_TRUE(parser::parseStmt(manager, "using \"ext.complex\"; using \"ext.enum\"; { auto a = enum_to_int; var complex b; }"));
	}


	// Helper extension used to test the check for already existing named constructs in the parser
	class NamedCoreExtensionParserTestExtension : public core::lang::Extension {
		friend class core::NodeManager;

		NamedCoreExtensionParserTestExtension(core::NodeManager& manager) : core::lang::Extension(manager) {}

	  public:
		TYPE_ALIAS("complex","struct { foo : NamedType; }")
	};

	TEST(NamedCoreExtensionParserTest, ParserAlreadyExistingNameDeathTest) {
		NodeManager manager;
		IRBuilder builder(manager);

		const auto& existingNames = manager.getLangExtension<NamedCoreExtensionParserTestExtension>().getDefinedSymbols();
		const auto& typeAlises = manager.getLangExtension<NamedCoreExtensionParserTestExtension>().getTypeAliases();

		// As I passed the extension with the name "complex" already defined this should be expanded
		EXPECT_EQ("AP(struct "
			      "{foo:NamedType,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
			      + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>} v0 = rec ref_var.{ref_var=fun(ref<type<'a>,f,f,plain> v0) {return "
			      "ref_alloc(ref_deref(v0), mem_loc_stack);}}(type<struct "
			      "{foo:NamedType,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>,"
			      + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}>))",
			      toString(builder.normalize(parser::parseStmt(manager, "var complex a;", false, existingNames, typeAlises))));

		// inside of a compound stmt we shadow previous vararations
		EXPECT_TRUE(parser::parseStmt(manager, "using \"ext.complex\"; var complex a;", false, existingNames));

	}

	TEST(ConstTypeExtensionTest, Semantic) {
		NodeManager nm;

		const NamedCoreExtensionParserTestExtension& ext = nm.getLangExtension<NamedCoreExtensionParserTestExtension>();

		semanticCheckSecond(ext.getDefinedSymbols());
	}


} // end namespace lang
} // end namespace core
} // end namespace insieme

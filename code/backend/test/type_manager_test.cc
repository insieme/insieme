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

#include "insieme/utils/container_utils.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/converter.h"

#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/name_mangling.h"



namespace insieme {
namespace backend {

	using namespace c_ast;

	namespace {
		class TestNameManager : public SimpleNameManager {
		  public:
			TestNameManager() : SimpleNameManager("test"){};
			virtual string getName(const core::NodePtr& ptr, const string& fragment) {
				return "name";
			}
			virtual void setName(const core::NodePtr& ptr, const string& name) {
				/* ignore */
			}
		};
	}


	TEST(TypeManager, Basic) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		TypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");


		core::TypePtr type = basic.getInt4();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("int32_t", toC(info.lValueType));
		EXPECT_EQ("int32_t", toC(info.rValueType));
		EXPECT_EQ("int32_t", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE(info.definition == info.declaration);

		EXPECT_EQ(static_cast<std::size_t>(1), info.definition->getIncludes().size());
		EXPECT_EQ("stdint.h", *info.definition->getIncludes().begin());


		type = basic.getInt8();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("int64_t", toC(info.lValueType));
		EXPECT_EQ("int64_t", toC(info.rValueType));
		EXPECT_EQ("int64_t", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE(info.definition == info.declaration);

		EXPECT_EQ(static_cast<std::size_t>(1), info.definition->getIncludes().size());
		EXPECT_EQ("stdint.h", *info.definition->getIncludes().begin());


		type = basic.getUInt16();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("uint128_t", toC(info.lValueType));
		EXPECT_EQ("uint128_t", toC(info.rValueType));
		EXPECT_EQ("uint128_t", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE(info.definition == info.declaration);

		EXPECT_EQ(static_cast<std::size_t>(1), info.definition->getIncludes().size());
		EXPECT_EQ("stdint.h", *info.definition->getIncludes().begin());


		type = basic.getFloat();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("float", toC(info.lValueType));
		EXPECT_EQ("float", toC(info.rValueType));
		EXPECT_EQ("float", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_FALSE(info.definition);
		EXPECT_FALSE(info.declaration);


		type = basic.getDouble();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("double", toC(info.lValueType));
		EXPECT_EQ("double", toC(info.rValueType));
		EXPECT_EQ("double", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_FALSE(info.definition);
		EXPECT_FALSE(info.declaration);


		type = basic.getLongDouble();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("long double", toC(info.lValueType));
		EXPECT_EQ("long double", toC(info.rValueType));
		EXPECT_EQ("long double", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_FALSE(info.definition);
		EXPECT_FALSE(info.declaration);

		type = basic.getBool();
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("bool", toC(info.lValueType));
		EXPECT_EQ("bool", toC(info.rValueType));
		EXPECT_EQ("bool", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE(info.definition == info.declaration);

		EXPECT_EQ(static_cast<std::size_t>(1), info.definition->getIncludes().size());
		EXPECT_EQ("stdbool.h", *info.definition->getIncludes().begin());
	}

	TEST(TypeManager, StructTypes) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();


		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();


		TypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::TypePtr type = builder.structType(core::FieldList());
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);

		// members should not have an effect on the types
		vector<core::FieldPtr> elements;
		elements.push_back(builder.field(builder.stringValue("a"), basic.getInt4()));
		elements.push_back(builder.field(builder.stringValue("b"), basic.getBool()));
		type = builder.structType(elements);
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));

		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);

		EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t a;");
		EXPECT_PRED2(containsSubString, toC(info.definition), "bool b;");

		// the definition should depend on the definition of the boolean
		TypeInfo infoBool = typeManager.getTypeInfo(context, basic.getBool());
		EXPECT_TRUE((bool)infoBool.definition);
		auto dependencies = info.definition->getDependencies();
		EXPECT_TRUE(contains(dependencies, infoBool.definition));
	}


	TEST(TypeManager, ArrayTypes) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		ArrayTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");
		auto expr = cManager->create<c_ast::Variable>(TypePtr(), cManager->create("Y"));

		// array of undefined size
		core::GenericTypePtr type = builder.arrayType(basic.getInt4());
		info = typeManager.getArrayTypeInfo(context, type);
		EXPECT_EQ("int32_t[]", toC(info.lValueType));
		EXPECT_EQ("int32_t[]", toC(info.rValueType));
		EXPECT_EQ("int32_t[]", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_EQ("Y", toC(info.externalize(cManager, expr)));
		EXPECT_EQ("Y", toC(info.internalize(cManager, expr)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);

		// array of fixed size
		type = builder.arrayType(basic.getInt4(), 24);
		info = typeManager.getArrayTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("int32_t[24]", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("INS_INIT(name){X}", toC(info.internalize(cManager, lit)));
		EXPECT_EQ("Y.data", toC(info.externalize(cManager, expr)));
		EXPECT_EQ("INS_INIT(name){Y}", toC(info.internalize(cManager, expr)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
		EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t data[24];");


		// TODO: fix and enable this test case
//		// array of variable size
//		auto size = builder.variable(builder.parseType("int<inf>"), 0);
//		type = builder.arrayType(basic.getInt4(), size);
//		EXPECT_EQ("array<int<4>,v0>", toString(*type));
//		info = typeManager.getArrayTypeInfo(context, type);
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name", toC(info.rValueType));
//		EXPECT_EQ("int32_t[24]", toC(info.externalType));
//		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
//		EXPECT_EQ("(name){X}", toC(info.internalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);

	}


//	TEST(TypeManager, VectorTypes) {
//		core::NodeManager nodeManager;
//		core::IRBuilder builder(nodeManager);
//		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();
//
//		Converter converter(nodeManager);
//		ConversionContext context(converter);
//		converter.setNameManager(std::make_shared<TestNameManager>());
//		TypeManager& typeManager = converter.getTypeManager();
//
//		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
//		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();
//
//		ArrayTypeInfo info;
//		auto lit = cManager->create("X");
//
//		core::GenericTypePtr type = builder.arrayType(basic.getInt4(), 4);
//		info = typeManager.getArrayTypeInfo(context, type);
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name", toC(info.rValueType));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
//		EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t data[4];");
//
//		type = builder.arrayType(basic.getInt8(), 4);
//		info = typeManager.getArrayTypeInfo(context, type);
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name", toC(info.rValueType));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
//		EXPECT_PRED2(containsSubString, toC(info.definition), "int64_t data[4];");
//
//		core::TypePtr innerType = builder.structType(core::NamedCompositeType::Entries());
//		type = builder.arrayType(innerType, 4);
//		info = typeManager.getArrayTypeInfo(context, type);
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name", toC(info.rValueType));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
//		EXPECT_PRED2(containsSubString, toC(info.definition), "name data[4];");
//
//		EXPECT_TRUE(contains(info.definition->getDependencies(), typeManager.getTypeInfo(context, innerType).definition));
//
//
//		type = builder.arrayType(type, 84);
//		info = typeManager.getArrayTypeInfo(context, type);
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name", toC(info.rValueType));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
//		EXPECT_PRED2(containsSubString, toC(info.definition), "name data[84];");
//
//	}

	TEST(TypeManager, RefTypesPrimitives) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		RefTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::GenericTypePtr type = builder.refType(basic.getInt4());
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t", toC(info.lValueType));
		EXPECT_EQ("int32_t*", toC(info.rValueType));
		EXPECT_EQ("int32_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(basic.getInt8());
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int64_t", toC(info.lValueType));
		EXPECT_EQ("int64_t*", toC(info.rValueType));
		EXPECT_EQ("int64_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(basic.getFloat());
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("float", toC(info.lValueType));
		EXPECT_EQ("float*", toC(info.rValueType));
		EXPECT_EQ("float*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_FALSE((bool)info.declaration);
		EXPECT_FALSE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		// TODO: check dependency on struct declaration

		type = builder.refType(builder.structType());
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name*", toC(info.rValueType));
		EXPECT_EQ("name*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		// check const
		type = builder.refType(basic.getInt8(),true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t", toC(info.lValueType));
		EXPECT_EQ("const int64_t*", toC(info.rValueType));
		EXPECT_EQ("const int64_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		// check volatile
		type = builder.refType(basic.getInt8(),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t*", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		// check const volatile
		type = builder.refType(basic.getInt8(),true,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int64_t", toC(info.lValueType));
		EXPECT_EQ("const volatile int64_t*", toC(info.rValueType));
		EXPECT_EQ("const volatile int64_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	}

	TEST(TypeManager, RefTypesNested) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		RefTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::GenericTypePtr type = builder.refType(builder.refType(basic.getInt8(),false,false),false,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int64_t*", toC(info.lValueType));
		EXPECT_EQ("int64_t**", toC(info.rValueType));
		EXPECT_EQ("int64_t**", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,false),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int64_t* volatile", toC(info.lValueType));
		EXPECT_EQ("int64_t* volatile*", toC(info.rValueType));
		EXPECT_EQ("int64_t* volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,false),true,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int64_t* const", toC(info.lValueType));
		EXPECT_EQ("int64_t* const*", toC(info.rValueType));
		EXPECT_EQ("int64_t* const*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,false),true,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int64_t* const volatile", toC(info.lValueType));
		EXPECT_EQ("int64_t* const volatile*", toC(info.rValueType));
		EXPECT_EQ("int64_t* const volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,true),false,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t*", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t**", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t**", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,true),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t* volatile", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t* volatile*", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t* volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,true),true,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t* const", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t* const*", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t* const*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),false,true),true,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t* const volatile", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t* const volatile*", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t* const volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,false),false,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t*", toC(info.lValueType));
		EXPECT_EQ("const int64_t**", toC(info.rValueType));
		EXPECT_EQ("const int64_t**", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,false),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t* volatile", toC(info.lValueType));
		EXPECT_EQ("const int64_t* volatile*", toC(info.rValueType));
		EXPECT_EQ("const int64_t* volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,false),true,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t* const", toC(info.lValueType));
		EXPECT_EQ("const int64_t* const*", toC(info.rValueType));
		EXPECT_EQ("const int64_t* const*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,false),true,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t* const volatile", toC(info.lValueType));
		EXPECT_EQ("const int64_t* const volatile*", toC(info.rValueType));
		EXPECT_EQ("const int64_t* const volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,true),false,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int64_t*", toC(info.lValueType));
		EXPECT_EQ("const volatile int64_t**", toC(info.rValueType));
		EXPECT_EQ("const volatile int64_t**", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,true),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int64_t* volatile", toC(info.lValueType));
		EXPECT_EQ("const volatile int64_t* volatile*", toC(info.rValueType));
		EXPECT_EQ("const volatile int64_t* volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,true),true,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int64_t* const", toC(info.lValueType));
		EXPECT_EQ("const volatile int64_t* const*", toC(info.rValueType));
		EXPECT_EQ("const volatile int64_t* const*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.refType(basic.getInt8(),true,true),true,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int64_t* const volatile", toC(info.lValueType));
		EXPECT_EQ("const volatile int64_t* const volatile*", toC(info.rValueType));
		EXPECT_EQ("const volatile int64_t* const volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));


		// some examples for 3-level nesting

		type = builder.refType(builder.refType(builder.refType(basic.getInt8(),false,true),true,false),false,true);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int64_t* const* volatile", toC(info.lValueType));
		EXPECT_EQ("volatile int64_t* const* volatile*", toC(info.rValueType));
		EXPECT_EQ("volatile int64_t* const* volatile*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));


		type = builder.refType(builder.refType(builder.refType(basic.getInt8(),true,false),false,true),true,false);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int64_t* volatile* const", toC(info.lValueType));
		EXPECT_EQ("const int64_t* volatile* const*", toC(info.rValueType));
		EXPECT_EQ("const int64_t* volatile* const*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	}

	TEST(TypeManager, RefTypesUnknownSizedArrays) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		RefTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");
		core::GenericTypePtr type;

		// ----- unknow sized arrays -----

		type = builder.refType(builder.arrayType(basic.getInt4()));
		EXPECT_EQ("ref<array<int<4>,inf>,f,f,plain>", toString(*type));
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t*", toC(info.lValueType));
		EXPECT_EQ("int32_t*", toC(info.rValueType));
		EXPECT_EQ("int32_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.arrayType(basic.getInt4()), true, false);
		EXPECT_EQ("ref<array<int<4>,inf>,t,f,plain>", toString(*type));
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const int32_t*", toC(info.lValueType));
		EXPECT_EQ("const int32_t*", toC(info.rValueType));
		EXPECT_EQ("const int32_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.arrayType(basic.getInt4()), false, true);
		EXPECT_EQ("ref<array<int<4>,inf>,f,t,plain>", toString(*type));
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("volatile int32_t*", toC(info.lValueType));
		EXPECT_EQ("volatile int32_t*", toC(info.rValueType));
		EXPECT_EQ("volatile int32_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

		type = builder.refType(builder.arrayType(basic.getInt4()), true, true);
		EXPECT_EQ("ref<array<int<4>,inf>,t,t,plain>", toString(*type));
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("const volatile int32_t*", toC(info.lValueType));
		EXPECT_EQ("const volatile int32_t*", toC(info.rValueType));
		EXPECT_EQ("const volatile int32_t*", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.newOperator);
		EXPECT_TRUE((bool)info.newOperatorName);
		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));



//		// ref/vector combination
//		type = builder.refType(builder.arrayType(basic.getInt4(), 4));
//		info = typeManager.getRefTypeInfo(context, type);
//		EXPECT_EQ("ref<array<int<4>,4>,f,f,plain>", toString(*type));
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name*", toC(info.rValueType));
//		EXPECT_EQ("int32_t*", toC(info.externalType));
//		EXPECT_EQ("(int32_t*)X", toC(info.externalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_TRUE((bool)info.newOperator);
//		EXPECT_TRUE((bool)info.newOperatorName);
//		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));
//
//		// ref/vector - multidimensional
//		type = builder.refType(builder.arrayType(builder.arrayType(basic.getInt4(), 4), 2));
//		info = typeManager.getRefTypeInfo(context, type);
//		EXPECT_EQ("ref<array<array<int<4>,4>,2>,f,f,plain>", toString(*type));
//		EXPECT_EQ("name", toC(info.lValueType));
//		EXPECT_EQ("name*", toC(info.rValueType));
//		EXPECT_EQ("int32_t(*)[2]", toC(info.externalType));
//		EXPECT_EQ("(int32_t(*)[2])X", toC(info.externalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_TRUE((bool)info.newOperator);
//		EXPECT_TRUE((bool)info.newOperatorName);
//		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));
//
//		// ref/ref combination
//		type = builder.refType(builder.refType(basic.getInt4()));
//		info = typeManager.getRefTypeInfo(context, type);
//		EXPECT_EQ("ref<ref<int<4>,f,f,plain>,f,f,plain>", toString(*type));
//		EXPECT_EQ("int32_t*", toC(info.lValueType));
//		EXPECT_EQ("int32_t**", toC(info.rValueType));
//		EXPECT_EQ("int32_t**", toC(info.externalType));
//		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_TRUE((bool)info.newOperator);
//		EXPECT_TRUE((bool)info.newOperatorName);
//		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));
//
//		// ref/ref combination
//		type = builder.refType(builder.refType(basic.getInt4(), true, false), false, true);
//		info = typeManager.getRefTypeInfo(context, type);
//		EXPECT_EQ("ref<ref<int<4>,t,f>,f,t>", toString(*type));
//		EXPECT_EQ("int32_t*", toC(info.lValueType));
//		EXPECT_EQ("int32_t**", toC(info.rValueType));
//		EXPECT_EQ("int32_t**", toC(info.externalType));
//		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_TRUE((bool)info.newOperator);
//		EXPECT_TRUE((bool)info.newOperatorName);
//		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));
//
//		// test ref/ref/array
//		type = builder.refType(builder.refType(builder.arrayType(basic.getInt4())));
//		info = typeManager.getRefTypeInfo(context, type);
//		EXPECT_EQ("ref<ref<array<int<4>,inf>,f,f,plain>,f,f,plain>", toString(*type));
//		EXPECT_EQ("int32_t*", toC(info.lValueType));
//		EXPECT_EQ("int32_t**", toC(info.rValueType));
//		EXPECT_EQ("int32_t**", toC(info.externalType));
//		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
//		EXPECT_TRUE((bool)info.declaration);
//		EXPECT_TRUE((bool)info.definition);
//		EXPECT_TRUE((bool)info.newOperator);
//		EXPECT_TRUE((bool)info.newOperatorName);
//		EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	}

	TEST(TypeManager, RefTypesFixedSizedArrays) {
			core::NodeManager nodeManager;
			core::IRBuilder builder(nodeManager);
			const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

			Converter converter(nodeManager);
			ConversionContext context(converter);
			converter.setNameManager(std::make_shared<TestNameManager>());
			TypeManager& typeManager = converter.getTypeManager();

			c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
			c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

			RefTypeInfo info;
			auto lit = cManager->create<c_ast::Literal>("X");

			core::GenericTypePtr type;

			// ----- unknow sized arrays -----

			type = builder.refType(builder.arrayType(basic.getInt4(),12));
			EXPECT_EQ("ref<array<int<4>,12u>,f,f,plain>", toString(*type));
			info = typeManager.getRefTypeInfo(context, type);
			EXPECT_EQ("name", toC(info.lValueType));
			EXPECT_EQ("name*", toC(info.rValueType));
			EXPECT_EQ("int32_t(*)[12]", toC(info.externalType));
			EXPECT_EQ("(int32_t(*)[12])X", toC(info.externalize(cManager, lit)));
			EXPECT_EQ("(name*)X", toC(info.internalize(cManager, lit)));
			EXPECT_TRUE((bool)info.declaration);
			EXPECT_TRUE((bool)info.definition);
			EXPECT_TRUE((bool)info.newOperator);
			EXPECT_TRUE((bool)info.newOperatorName);
			EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

			type = builder.refType(builder.arrayType(basic.getInt4(),12), true, false);
			EXPECT_EQ("ref<array<int<4>,12u>,t,f,plain>", toString(*type));
			info = typeManager.getRefTypeInfo(context, type);
			EXPECT_EQ("const name", toC(info.lValueType));
			EXPECT_EQ("const name*", toC(info.rValueType));
			EXPECT_EQ("const int32_t(*)[12]", toC(info.externalType));
			EXPECT_EQ("(const int32_t(*)[12])X", toC(info.externalize(cManager, lit)));
			EXPECT_EQ("(const name*)X", toC(info.internalize(cManager, lit)));
			EXPECT_TRUE((bool)info.declaration);
			EXPECT_TRUE((bool)info.definition);
			EXPECT_TRUE((bool)info.newOperator);
			EXPECT_TRUE((bool)info.newOperatorName);
			EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

			type = builder.refType(builder.arrayType(basic.getInt4(),12), false, true);
			EXPECT_EQ("ref<array<int<4>,12u>,f,t,plain>", toString(*type));
			info = typeManager.getRefTypeInfo(context, type);
			EXPECT_EQ("volatile name", toC(info.lValueType));
			EXPECT_EQ("volatile name*", toC(info.rValueType));
			EXPECT_EQ("volatile int32_t(*)[12]", toC(info.externalType));
			EXPECT_EQ("(volatile int32_t(*)[12])X", toC(info.externalize(cManager, lit)));
			EXPECT_EQ("(volatile name*)X", toC(info.internalize(cManager, lit)));
			EXPECT_TRUE((bool)info.declaration);
			EXPECT_TRUE((bool)info.definition);
			EXPECT_TRUE((bool)info.newOperator);
			EXPECT_TRUE((bool)info.newOperatorName);
			EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

			type = builder.refType(builder.arrayType(basic.getInt4(),12), true, true);
			EXPECT_EQ("ref<array<int<4>,12u>,t,t,plain>", toString(*type));
			info = typeManager.getRefTypeInfo(context, type);
			EXPECT_EQ("const volatile name", toC(info.lValueType));
			EXPECT_EQ("const volatile name*", toC(info.rValueType));
			EXPECT_EQ("const volatile int32_t(*)[12]", toC(info.externalType));
			EXPECT_EQ("(const volatile int32_t(*)[12])X", toC(info.externalize(cManager, lit)));
			EXPECT_EQ("(const volatile name*)X", toC(info.internalize(cManager, lit)));
			EXPECT_TRUE((bool)info.declaration);
			EXPECT_TRUE((bool)info.definition);
			EXPECT_TRUE((bool)info.newOperator);
			EXPECT_TRUE((bool)info.newOperatorName);
			EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	}

	TEST(TypeManager, RefTypesCppRef) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		RefTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::GenericTypePtr type;

		// ----- C++ references -----

		type = builder.parseType("ref<int<4>,f,f,cpp_ref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t&", toC(info.lValueType));
		EXPECT_EQ("int32_t&", toC(info.rValueType));
		EXPECT_EQ("int32_t&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,t,f,cpp_ref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t const&", toC(info.lValueType));
		EXPECT_EQ("int32_t const&", toC(info.rValueType));
		EXPECT_EQ("int32_t const&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,f,t,cpp_ref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t volatile&", toC(info.lValueType));
		EXPECT_EQ("int32_t volatile&", toC(info.rValueType));
		EXPECT_EQ("int32_t volatile&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,t,t,cpp_ref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t const volatile&", toC(info.externalType));
		EXPECT_EQ("int32_t const volatile&", toC(info.lValueType));
		EXPECT_EQ("int32_t const volatile&", toC(info.rValueType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);
	}

	TEST(TypeManager, RefTypesCppRValueRef) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		RefTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::GenericTypePtr type;

		// ----- C++ r-value references -----

		type = builder.parseType("ref<int<4>,f,f,cpp_rref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppRValueReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t&&", toC(info.lValueType));
		EXPECT_EQ("int32_t&&", toC(info.rValueType));
		EXPECT_EQ("int32_t&&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,t,f,cpp_rref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppRValueReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t const&&", toC(info.lValueType));
		EXPECT_EQ("int32_t const&&", toC(info.rValueType));
		EXPECT_EQ("int32_t const&&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,f,t,cpp_rref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppRValueReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t volatile&&", toC(info.lValueType));
		EXPECT_EQ("int32_t volatile&&", toC(info.rValueType));
		EXPECT_EQ("int32_t volatile&&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);

		type = builder.parseType("ref<int<4>,t,t,cpp_rref>").as<core::GenericTypePtr>();
		EXPECT_PRED1(core::lang::isCppRValueReference, type);
		info = typeManager.getRefTypeInfo(context, type);
		EXPECT_EQ("int32_t const volatile&&", toC(info.lValueType));
		EXPECT_EQ("int32_t const volatile&&", toC(info.rValueType));
		EXPECT_EQ("int32_t const volatile&&", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.newOperator);
		EXPECT_FALSE((bool)info.newOperatorName);
	}

	TEST(TypeManager, FunctionTypes) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		FunctionTypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::TypePtr typeA = basic.getInt4();
		core::TypePtr typeB = basic.getBool();
		core::TypePtr typeC = basic.getFloat();

		core::FunctionTypePtr type;

		// -- test a thick function pointer first => should generate closure, constructor and caller --

		type = builder.functionType(toVector(typeA, typeB), typeC, core::FK_CLOSURE);
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("((int<4>,bool)=>real<4>)", toString(*type));
		EXPECT_FALSE(info.plain);
		EXPECT_EQ("name*", toC(info.lValueType));
		EXPECT_EQ("name*", toC(info.rValueType));
		EXPECT_EQ("name*", toC(info.externalType)); // should this be this way?
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_TRUE((bool)info.callerName);
		EXPECT_TRUE((bool)info.caller);
		EXPECT_TRUE((bool)info.constructorName);
		EXPECT_TRUE((bool)info.constructor);

		EXPECT_EQ("name_call", toC(info.callerName));
		EXPECT_EQ("name_ctr", toC(info.constructorName));

		EXPECT_PRED2(containsSubString, toC(info.declaration), "name");

		EXPECT_PRED2(containsSubString, toC(info.definition), "name");
		EXPECT_PRED2(containsSubString, toC(info.definition), "float(* call)(name*,int32_t,bool);");

		EXPECT_PRED2(containsSubString, toC(info.caller),
		             "static inline float name_call(name* closure, int32_t p1, bool p2) {\n    return closure->call(closure, p1, p2);\n}\n");

		EXPECT_PRED2(containsSubString, toC(info.constructor), "static inline name* name_ctr(name* target, float(* call)(name*,int32_t,bool)) {\n"
		                                                       "    *target = INS_INIT(name){call};\n"
		                                                       "    return target;\n"
		                                                       "}");

		EXPECT_TRUE(contains(info.caller->getDependencies(), info.definition));
		EXPECT_TRUE(contains(info.constructor->getDependencies(), info.definition));

		// check externalizing / internalizing
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));


		// -- test a plain function type --

		type = builder.functionType(toVector(typeA, typeB), typeC);
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("((int<4>,bool)->real<4>)", toString(*type));
		EXPECT_TRUE(info.plain);
		EXPECT_EQ("name", toC(info.lValueType)); // there is an implicit typedef, therefore the type is used with a symbol name
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType)); // should this be this way?
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.callerName);
		EXPECT_FALSE((bool)info.caller);
		EXPECT_FALSE((bool)info.constructorName);
		EXPECT_FALSE((bool)info.constructor);

		EXPECT_PRED2(containsSubString, toC(info.definition), "");
		EXPECT_PRED2(containsSubString, toC(info.definition), "");

		// check externalizing / internalizing
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));

		// check variable declaration
		auto decl = cManager->create<c_ast::VarDecl>(cManager->create<c_ast::Variable>(info.lValueType, cManager->create("var")));
		EXPECT_EQ("name var", toC(decl));

		// test the same with a function not accepting any arguments
		type = builder.functionType(core::TypeList(), typeA);
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("(()->int<4>)", toString(*type));

		decl = cManager->create<c_ast::VarDecl>(cManager->create<c_ast::Variable>(info.lValueType, cManager->create("var")));
		EXPECT_EQ("name var", toC(decl));


		// -- test a member function type --

		core::TypePtr classTy = builder.refType(builder.structType(toVector(builder.field("a", typeA), builder.field("b", typeA))));

		type = builder.functionType(toVector(classTy, typeA), typeC, core::FK_MEMBER_FUNCTION);
		info = typeManager.getTypeInfo(context, type);


		EXPECT_EQ("(struct {a:int<4>,b:int<4>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor(),"
		          + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}::(int<4>)->real<4>)", toString(*type));
		EXPECT_TRUE(info.plain);
		EXPECT_EQ("name", toC(info.lValueType)); // there is an implicit typedef, therefore the type is used with a symbol name
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);
		EXPECT_FALSE((bool)info.callerName);
		EXPECT_FALSE((bool)info.caller);
		EXPECT_FALSE((bool)info.constructorName);
		EXPECT_FALSE((bool)info.constructor);

		EXPECT_PRED2(containsSubString, toC(info.definition), "");
		EXPECT_PRED2(containsSubString, toC(info.definition), "");

		// check externalizing / internalizing
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));

		// check variable declaration
		decl = cManager->create<c_ast::VarDecl>(cManager->create<c_ast::Variable>(info.lValueType, cManager->create("var")));
		EXPECT_EQ("name var", toC(decl));
	}


	TEST(TypeManager, RecursiveTypes) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		auto lit = cManager->create("X");


		// -- build a recursive type --------

		core::TagTypeReferencePtr A = builder.tagTypeReference("A");

		vector<core::FieldPtr> entriesA;
		entriesA.push_back(builder.field(builder.stringValue("value"), basic.getInt4()));
		entriesA.push_back(builder.field(builder.stringValue("next"), builder.refType(A)));
		core::StructPtr structA = builder.structRecord("A",entriesA);

		core::TagTypeDefinitionPtr def = builder.tagTypeDefinition({ { A, structA } });

		core::TagTypePtr recTypeA = builder.tagType(A, def);
		EXPECT_TRUE(recTypeA->isRecursive());
		EXPECT_TRUE(core::checks::check(recTypeA).empty()) << core::checks::check(recTypeA);

		// do the checks

		TypeInfo infoA = typeManager.getTypeInfo(context, recTypeA);

		EXPECT_EQ("name", toC(infoA.lValueType));
		EXPECT_EQ("name", toC(infoA.rValueType));
		EXPECT_TRUE((bool)infoA.declaration);
		EXPECT_TRUE((bool)infoA.definition);

		EXPECT_TRUE(contains(infoA.definition->getDependencies(), infoA.declaration));
		EXPECT_FALSE(contains(infoA.definition->getDependencies(), infoA.definition));
	}


	TEST(TypeManager, MutalRecursiveTypes) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		auto lit = cManager->create("X");


		// -- build a recursive type --------

		auto A = builder.tagTypeReference("A");
		auto B = builder.tagTypeReference("B");

		vector<core::FieldPtr> entriesA;
		entriesA.push_back(builder.field(builder.stringValue("value"), basic.getInt4()));
		entriesA.push_back(builder.field(builder.stringValue("other"), builder.refType(B)));
		auto structA = builder.structRecord("A",entriesA);

		vector<core::FieldPtr> entriesB;
		entriesB.push_back(builder.field(builder.stringValue("value"), basic.getBool()));
		entriesB.push_back(builder.field(builder.stringValue("other"), builder.refType(A)));
		auto structB = builder.structRecord("B",entriesB);

		core::TagTypeDefinitionPtr def = builder.tagTypeDefinition({ { A, structA }, { B, structB } });

		core::TagTypePtr recTypeA = builder.tagType(A, def);
		core::TagTypePtr recTypeB = builder.tagType(B, def);

		EXPECT_TRUE(recTypeA->isRecursive());
		EXPECT_TRUE(recTypeB->isRecursive());

		EXPECT_TRUE(core::checks::check(recTypeA).empty()) << core::checks::check(recTypeA);
		EXPECT_TRUE(core::checks::check(recTypeB).empty()) << core::checks::check(recTypeB);


		// do the checks

		TypeInfo infoA = typeManager.getTypeInfo(context, recTypeA);
		TypeInfo infoB = typeManager.getTypeInfo(context, recTypeB);

		EXPECT_EQ("name", toC(infoA.lValueType));
		EXPECT_EQ("name", toC(infoA.rValueType));

		EXPECT_EQ("name", toC(infoB.lValueType));
		EXPECT_EQ("name", toC(infoB.rValueType));

		EXPECT_TRUE((bool)infoA.declaration);
		EXPECT_TRUE((bool)infoA.definition);

		EXPECT_TRUE((bool)infoB.declaration);
		EXPECT_TRUE((bool)infoB.definition);

		EXPECT_TRUE(contains(infoA.definition->getDependencies(), infoB.declaration));
		EXPECT_TRUE(contains(infoB.definition->getDependencies(), infoA.declaration));

		EXPECT_FALSE(contains(infoA.definition->getDependencies(), infoB.definition));
		EXPECT_FALSE(contains(infoB.definition->getDependencies(), infoA.definition));
	}


	TEST(TypeManager, TupleType) {
		core::NodeManager nodeManager;
		core::IRBuilder builder(nodeManager);
		const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

		Converter converter(nodeManager);
		ConversionContext context(converter);
		converter.setNameManager(std::make_shared<TestNameManager>());
		TypeManager& typeManager = converter.getTypeManager();

		c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
		c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

		TypeInfo info;
		auto lit = cManager->create<c_ast::Literal>("X");

		core::TypePtr type = builder.tupleType(core::TypeList());
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);

		// members should not have an effect on the types
		type = builder.tupleType(toVector(basic.getInt4(), basic.getBool()));
		info = typeManager.getTypeInfo(context, type);
		EXPECT_EQ("name", toC(info.lValueType));
		EXPECT_EQ("name", toC(info.rValueType));
		EXPECT_EQ("name", toC(info.externalType));
		EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
		EXPECT_EQ("X", toC(info.internalize(cManager, lit)));

		EXPECT_TRUE((bool)info.declaration);
		EXPECT_TRUE((bool)info.definition);

		EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t c0;");
		EXPECT_PRED2(containsSubString, toC(info.definition), "bool c1;");

		// the definition should depend on the definition of the boolean
		TypeInfo infoBool = typeManager.getTypeInfo(context, basic.getBool());
		EXPECT_TRUE((bool)infoBool.definition);
		auto dependencies = info.definition->getDependencies();
		EXPECT_TRUE(contains(dependencies, infoBool.definition));
	}

} // end namespace backend
} // end namespace insieme

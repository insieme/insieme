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

#include "insieme/utils/container_utils.h"

#include "insieme/core/ast_builder.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/converter.h"

#include "insieme/backend/c_ast/c_ast_printer.h"


namespace insieme {
namespace backend {

namespace {
	class TestNameManager : public SimpleNameManager {
	public:
		TestNameManager() : SimpleNameManager("test") {};
		virtual string getName(const core::NodePtr& ptr, const string& fragment) {
			return "name";
		}
	};
}


// a small test verifying that the given substr is contained within the given string
bool containsSubString(const string& str, const string& substr) {
	return str.find(substr) != string::npos;
}

// a small test verifying that the given substr is contained within the given string
bool notContainsSubString(const string& str, const string& substr) {
	return !containsSubString(str, substr);
}

string toC(const c_ast::NodePtr& node) {
	return toString(c_ast::CPrint(node));
}

TEST(TypeManager, Basic) {

	core::NodeManager nodeManager;
	core::ASTBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getBasicGenerator();


	TestNameManager nameManager;
	c_ast::SharedCNodeManager cManager = c_ast::CNodeManager::createShared();

	Converter converter;
	converter.setNameManager(&nameManager);
	converter.setNodeManager(&nodeManager);
	converter.setCNodeManager(cManager);

	TypeManager typeManager(converter);

	TypeInfo info;
	auto lit = cManager->create("X");

	core::TypePtr type = basic.getInt4();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int", toC(info.lValueType));
	EXPECT_EQ("int", toC(info.rValueType));
	EXPECT_EQ("int", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);

	type = basic.getInt8();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("long", toC(info.lValueType));
	EXPECT_EQ("long", toC(info.rValueType));
	EXPECT_EQ("long", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);

	type = basic.getFloat();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("float", toC(info.lValueType));
	EXPECT_EQ("float", toC(info.rValueType));
	EXPECT_EQ("float", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);


	type = basic.getDouble();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("double", toC(info.lValueType));
	EXPECT_EQ("double", toC(info.rValueType));
	EXPECT_EQ("double", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);

	type = basic.getBool();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("bool", toC(info.lValueType));
	EXPECT_EQ("bool", toC(info.rValueType));
	EXPECT_EQ("bool", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE(info.definition == info.declaration);

	EXPECT_EQ("typedef int bool;\n", toString(*info.definition));
}

TEST(TypeManager, StructTypes) {

	core::NodeManager nodeManager;
	core::ASTBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getBasicGenerator();


	TestNameManager nameManager;
	c_ast::SharedCNodeManager cManager = c_ast::CNodeManager::createShared();

	Converter converter;
	converter.setNameManager(&nameManager);
	converter.setNodeManager(&nodeManager);
	converter.setCNodeManager(cManager);

	TypeManager typeManager(converter);

	TypeInfo info;
	auto lit = cManager->create("X");


	core::TypePtr type = builder.structType(core::NamedCompositeType::Entries());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("struct name", toC(info.lValueType));
	EXPECT_EQ("struct name", toC(info.rValueType));
	EXPECT_EQ("struct name", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);

	// members should not have an effect on the types
	vector<core::NamedCompositeType::Entry> elements;
	elements.push_back(std::make_pair(builder.identifier("a"), basic.getInt4()));
	elements.push_back(std::make_pair(builder.identifier("b"), basic.getBool()));
	type = builder.structType(elements);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("struct name", toC(info.lValueType));
	EXPECT_EQ("struct name", toC(info.rValueType));
	EXPECT_EQ("struct name", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));

	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);

	EXPECT_PRED2(containsSubString, toString(*info.definition), "int a;");
	EXPECT_PRED2(containsSubString, toString(*info.definition), "bool b;");

	// the definition should depend on the definition of the boolean
	TypeInfo infoBool = typeManager.getTypeInfo(basic.getBool());
	EXPECT_TRUE((bool)infoBool.definition);
	auto dependencies = info.definition->getDependencies();
	EXPECT_TRUE(contains(dependencies, infoBool.definition));

}


TEST(TypeManager, RefTypes) {

	core::NodeManager nodeManager;
	core::ASTBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getBasicGenerator();


	TestNameManager nameManager;
	c_ast::SharedCNodeManager cManager = c_ast::CNodeManager::createShared();

	Converter converter;
	converter.setNameManager(&nameManager);
	converter.setNodeManager(&nodeManager);
	converter.setCNodeManager(cManager);

	TypeManager typeManager(converter);

	TypeInfo info;
	auto lit = cManager->create("X");


	core::TypePtr type = builder.refType(basic.getInt4());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int", toC(info.lValueType));
	EXPECT_EQ("int*", toC(info.rValueType));
	EXPECT_EQ("int*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE((bool)info.declaration);
	EXPECT_FALSE((bool)info.definition);

	type = builder.refType(basic.getInt8());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("long", toC(info.lValueType));
	EXPECT_EQ("long*", toC(info.rValueType));
	EXPECT_EQ("long*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE((bool)info.declaration);
	EXPECT_FALSE((bool)info.definition);

	type = builder.refType(basic.getFloat());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("float", toC(info.lValueType));
	EXPECT_EQ("float*", toC(info.rValueType));
	EXPECT_EQ("float*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_FALSE((bool)info.declaration);
	EXPECT_FALSE((bool)info.definition);

	type = builder.refType(builder.structType(core::NamedCompositeType::Entries()));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("struct name", toC(info.lValueType));
	EXPECT_EQ("struct name*", toC(info.rValueType));
	EXPECT_EQ("struct name*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(lit, cManager)));
	EXPECT_EQ("X", toC(info.internalize(lit, cManager)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);

	// TODO: check dependency on struct declaration

//	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
//	core::ConcreteIntTypeParamPtr size2 = builder.concreteIntTypeParam(2);
//
//	// ref/array combination
//	type = builder.refType(builder.arrayType(basic.getInt4()));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("ref<array<int<4>,1>>", toString(*type));
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name*", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name* %s", info.paramPattern);
//
//	// ref/array - multidimensional
//	type = builder.refType(builder.arrayType(basic.getInt4(), size2));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("ref<array<int<4>,2>>", toString(*type));
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name*", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name* %s", info.paramPattern);
//
//	// ref/vector combination
//	type = builder.refType(builder.vectorType(basic.getInt4(), size));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("ref<vector<int<4>,4>>", toString(*type));
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name*", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name* %s", info.paramPattern);
//
//	// ref/vector - multidimensional
//	type = builder.refType(builder.vectorType(builder.vectorType(basic.getInt4(), size2), size));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("ref<vector<vector<int<4>,2>,4>>", toString(*type));
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name*", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name* %s", info.paramPattern);

}

TEST(TypeManager, ArrayTypes) {
//
//	core::NodeManager nodeManager;
//	core::ASTBuilder builder(nodeManager);
//	const core::lang::BasicGenerator& basic = nodeManager.getBasicGenerator();
//
//
//	TestNameManager nameManager;
//	c_ast::SharedCNodeManager cManager = c_ast::CNodeManager::createShared();
//
//	Converter converter;
//	converter.setNameManager(&nameManager);
//	converter.setNodeManager(&nodeManager);
//	converter.setCNodeManager(cManager);
//
//	TypeManager typeManager(converter);
//
//	TypeInfo info;
//	auto lit = cManager->create("X");
//
//
//	core::TypePtr type = builder.arrayType(basic.getInt4());
//	info = typeManager.getTypeInfo(type);
//	EXPECT_EQ("name", toC(info.lValueType));
//	EXPECT_EQ("name", toC(info.rValueType));
//	EXPECT_TRUE((bool)info.declaration);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_PRED2(containsSubString, toString(info.definition), "unsigned size[1];");
//	EXPECT_PRED2(containsSubString, toString(info.definition), "int* data;");
//
//	type = builder.arrayType(basic.getInt8());
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "unsigned size[1];");
//	EXPECT_PRED2(containsSubString, toString(info.definition), "long* data;");
//
//	type = builder.arrayType(builder.structType(core::NamedCompositeType::Entries()));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "unsigned size[1];");
//	EXPECT_PRED2(containsSubString, toString(info.definition), "name* data;");
//
//	type = builder.arrayType(basic.getInt8(), builder.concreteIntTypeParam(2));
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "unsigned size[2];");
//	EXPECT_PRED2(containsSubString, toString(info.definition), "long** data;");
}

//TEST(TypeManager, ArrayTypesNoSize) {
//
//	core::ASTBuilder builder;
//	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;
//
//	Converter converter(false);
//	SimpleNameManager nameManager;
//	converter.setNameManager(&nameManager);
//	TypeManager typeManager(converter);
//
//	TypeManager::TypeInfo info;
//	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");
//
//
//	core::TypePtr type = builder.arrayType(basic.getInt4());
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(notContainsSubString, toString(info.definition), "unsigned size[1];");
//	EXPECT_PRED2(containsSubString, toString(info.definition), "int* data;");
//}
//
//TEST(TypeManager, VectorTypes) {
//
//	core::ASTBuilder builder;
//	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;
//
//	Converter converter;
//	SimpleNameManager nameManager;
//	converter.setNameManager(&nameManager);
//	TypeManager typeManager(converter);
//
//	TypeManager::TypeInfo info;
//	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");
//
//	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
//	core::ConcreteIntTypeParamPtr size2 = builder.concreteIntTypeParam(84);
//
//	core::TypePtr type = builder.vectorType(basic.getInt4(), size);
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "int data[4];");
//
//	type = builder.vectorType(basic.getInt8(), size);
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "long data[4];");
//
//	type = builder.vectorType(builder.structType(core::NamedCompositeType::Entries()), size);
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "name data[4];");
//
//
//	type = builder.vectorType(type, size2);
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("name", info.lValueName);
//	EXPECT_EQ("name", info.rValueName);
//	EXPECT_EQ("name %s", info.declPattern);
//	EXPECT_EQ("name %s", info.paramPattern);
//	EXPECT_TRUE((bool)info.definition);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), info.definition));
//	EXPECT_PRED2(containsSubString, toString(info.definition), "name data[84];");
//
//}
//
//TEST(TypeManager, FunctionTypes) {
//
//	core::ASTBuilder builder;
//	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;
//
//	Converter converter;
//	SimpleNameManager nameManager;
//	converter.setNameManager(&nameManager);
//	TypeManager typeManager(converter);
//
//	TypeManager::TypeInfo info;
//	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");
//
//	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
//
//	core::TypePtr typeA = basic.getInt4();
//	core::TypePtr typeB = basic.getBool();
//	core::TypePtr typeC = basic.getFloat();
//
//
//	core::FunctionTypePtr type;
//
//	type = builder.functionType(toVector(typeA, typeB), typeC);
//	info = typeManager.getTypeInfo(fragment, type);
//	EXPECT_EQ("((int<4>,bool)->real<4>)", toString(*type));
//	EXPECT_EQ("name*", info.lValueName);
//	EXPECT_EQ("name*", info.rValueName);
//	EXPECT_EQ("name* %s", info.declPattern);
//	EXPECT_EQ("name* %s", info.paramPattern);
//
//	TypeManager::FunctionTypeInfo details = typeManager.getFunctionTypeInfo(type);
//	EXPECT_EQ("name", details.closureName);
//	EXPECT_EQ("name_call", details.callerName);
//	EXPECT_TRUE(::contains(fragment->getDependencies(), details.definitions));
//}

} // end namespace backend
} // end namespace insieme


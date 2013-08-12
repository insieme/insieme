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

#include "insieme/core/ir_builder.h"

#include "insieme/backend/type_manager.h"
#include "insieme/backend/name_manager.h"
#include "insieme/backend/converter.h"

#include "insieme/backend/c_ast/c_ast_printer.h"

#include "insieme/utils/test/test_utils.h"


namespace insieme {
namespace backend {

namespace {
	class TestNameManager : public SimpleNameManager {
	public:
		TestNameManager() : SimpleNameManager("test") {};
		virtual string getName(const core::NodePtr& ptr, const string& fragment) {
			return "name";
		}
		virtual void setName(const core::NodePtr& ptr, const string& name) { /* ignore */ }
	};
}


TEST(TypeManager, Basic) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();


	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	TypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");


	core::TypePtr type = basic.getInt4();
	info = typeManager.getTypeInfo(type);
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
	info = typeManager.getTypeInfo(type);
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


	type = basic.getFloat();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("float", toC(info.lValueType));
	EXPECT_EQ("float", toC(info.rValueType));
	EXPECT_EQ("float", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);


	type = basic.getDouble();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("double", toC(info.lValueType));
	EXPECT_EQ("double", toC(info.rValueType));
	EXPECT_EQ("double", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);


	type = basic.getLongDouble();
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("long double", toC(info.lValueType));
	EXPECT_EQ("long double", toC(info.rValueType));
	EXPECT_EQ("long double", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_FALSE(info.definition);
	EXPECT_FALSE(info.declaration);

	type = basic.getBool();
	info = typeManager.getTypeInfo(type);
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
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();


	TypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");

	core::TypePtr type = builder.structType(core::NamedCompositeType::Entries());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_EQ("name", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);

	// members should not have an effect on the types
	vector<core::NamedTypePtr> elements;
	elements.push_back(builder.namedType(builder.stringValue("a"), basic.getInt4()));
	elements.push_back(builder.namedType(builder.stringValue("b"), basic.getBool()));
	type = builder.structType(elements);
	info = typeManager.getTypeInfo(type);
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
	TypeInfo infoBool = typeManager.getTypeInfo(basic.getBool());
	EXPECT_TRUE((bool)infoBool.definition);
	auto dependencies = info.definition->getDependencies();
	EXPECT_TRUE(contains(dependencies, infoBool.definition));

}


TEST(TypeManager, RefTypes) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	RefTypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");

	core::RefTypePtr type = builder.refType(basic.getInt4());
	info = typeManager.getTypeInfo(type);
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
	info = typeManager.getTypeInfo(type);
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
	info = typeManager.getTypeInfo(type);
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

	type = builder.refType(builder.structType(core::NamedCompositeType::Entries()));
	info = typeManager.getTypeInfo(type);
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

	// TODO: check dependency on struct declaration

	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
	core::ConcreteIntTypeParamPtr size2 = builder.concreteIntTypeParam(2);

	// ref/array combination
	type = builder.refType(builder.arrayType(basic.getInt4()));
	EXPECT_EQ("ref<array<int<4>,1>>", toString(*type));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int32_t*", toC(info.lValueType));
	EXPECT_EQ("int32_t*", toC(info.rValueType));
	EXPECT_EQ("int32_t*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// ref/array - multidimensional
	type = builder.refType(builder.arrayType(basic.getInt4(), size2));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<array<int<4>,2>>", toString(*type));
	EXPECT_EQ("int32_t**", toC(info.lValueType));
	EXPECT_EQ("int32_t**", toC(info.rValueType));
	EXPECT_EQ("int32_t**", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// ref/vector combination
	type = builder.refType(builder.vectorType(basic.getInt4(), size));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<vector<int<4>,4>>", toString(*type));
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name*", toC(info.rValueType));
	EXPECT_EQ("int32_t*", toC(info.externalType));
	EXPECT_EQ("(int32_t*)X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// ref/vector - multidimensional
	type = builder.refType(builder.vectorType(builder.vectorType(basic.getInt4(), size2), size));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<vector<vector<int<4>,2>,4>>", toString(*type));
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name*", toC(info.rValueType));
	EXPECT_EQ("int32_t(*)[2]", toC(info.externalType));
	EXPECT_EQ("(int32_t(*)[2])X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// ref/ref combination
	type = builder.refType(builder.refType(basic.getInt4()));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<ref<int<4>>>", toString(*type));
	EXPECT_EQ("int32_t*", toC(info.lValueType));
	EXPECT_EQ("int32_t**", toC(info.rValueType));
	EXPECT_EQ("int32_t**", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// test ref/ref/array
	type = builder.refType(builder.refType(builder.arrayType(basic.getInt4())));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<ref<array<int<4>,1>>>", toString(*type));
	EXPECT_EQ("int32_t*", toC(info.lValueType));
	EXPECT_EQ("int32_t**", toC(info.rValueType));
	EXPECT_EQ("int32_t**", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_TRUE((bool)info.newOperator);
	EXPECT_TRUE((bool)info.newOperatorName);
	EXPECT_EQ("_ref_new_name", toC(info.newOperatorName));

	// test ref/any
	type = builder.refType(basic.getAny());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<any>", toString(*type));
	EXPECT_EQ("void*", toC(info.lValueType));
	EXPECT_EQ("void*", toC(info.rValueType));
	EXPECT_EQ("void*", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));

	// test ref/ref/any
	type = builder.refType(builder.refType(basic.getAny()));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("ref<ref<any>>", toString(*type));
	EXPECT_EQ("void*", toC(info.lValueType));
	EXPECT_EQ("void**", toC(info.rValueType));
	EXPECT_EQ("void**", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));

}

TEST(TypeManager, ArrayTypes) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	ArrayTypeInfo info;
	auto lit = cManager->create("X");

	core::ArrayTypePtr type = builder.arrayType(basic.getInt4());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int32_t*", toC(info.lValueType));
	EXPECT_EQ("int32_t*", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_EQ(typeManager.getTypeInfo(basic.getInt4()).definition, info.definition);

	type = builder.arrayType(basic.getInt8());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int64_t*", toC(info.lValueType));
	EXPECT_EQ("int64_t*", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_EQ(typeManager.getTypeInfo(basic.getInt8()).definition, info.definition);

	core::TypePtr structType = builder.structType(core::NamedCompositeType::Entries());
	type = builder.arrayType(structType);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name*", toC(info.lValueType));
	EXPECT_EQ("name*", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_EQ(typeManager.getTypeInfo(structType).definition, info.definition);

	type = builder.arrayType(basic.getInt8(), builder.concreteIntTypeParam(2));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("int64_t**", toC(info.lValueType));
	EXPECT_EQ("int64_t**", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_EQ(typeManager.getTypeInfo(basic.getInt8()).definition, info.definition);

}

TEST(TypeManager, VectorTypes) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	VectorTypeInfo info;
	auto lit = cManager->create("X");

	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
	core::ConcreteIntTypeParamPtr size2 = builder.concreteIntTypeParam(84);

	core::VectorTypePtr type = builder.vectorType(basic.getInt4(), size);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
	EXPECT_PRED2(containsSubString, toC(info.definition), "int32_t data[4];");

	EXPECT_TRUE((bool)info.initUniform);
	EXPECT_TRUE((bool)info.initUniformName);
	EXPECT_TRUE(contains(info.initUniform->getDependencies(), info.definition));

	type = builder.vectorType(basic.getInt8(), size);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
	EXPECT_PRED2(containsSubString, toC(info.definition), "int64_t data[4];");

	EXPECT_TRUE((bool)info.initUniform);
	EXPECT_TRUE((bool)info.initUniformName);
	EXPECT_TRUE(contains(info.initUniform->getDependencies(), info.definition));

	core::TypePtr innerType = builder.structType(core::NamedCompositeType::Entries());
	type = builder.vectorType(innerType, size);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
	EXPECT_PRED2(containsSubString, toC(info.definition), "name data[4];");

	EXPECT_TRUE((bool)info.initUniform);
	EXPECT_TRUE((bool)info.initUniformName);
	EXPECT_TRUE(contains(info.initUniform->getDependencies(), info.definition));

	EXPECT_TRUE(contains(info.definition->getDependencies(), typeManager.getTypeInfo(innerType).definition));



	type = builder.vectorType(type, size2);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);
	EXPECT_PRED2(containsSubString, toC(info.definition), "struct name");
	EXPECT_PRED2(containsSubString, toC(info.definition), "name data[84];");

	EXPECT_TRUE((bool)info.initUniform);
	EXPECT_TRUE((bool)info.initUniformName);
	EXPECT_TRUE(contains(info.initUniform->getDependencies(), info.definition));

}

TEST(TypeManager, FunctionTypes) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();


	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	FunctionTypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");

	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);

	core::TypePtr typeA = basic.getInt4();
	core::TypePtr typeB = basic.getBool();
	core::TypePtr typeC = basic.getFloat();


	core::FunctionTypePtr type;

	// -- test a thick function pointer first => should generate closure, constructor and caller --

	type = builder.functionType(toVector(typeA, typeB), typeC, core::FK_CLOSURE);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("((int<4>,bool)=>real<4>)", toString(*type));
	EXPECT_FALSE(info.plain);
	EXPECT_EQ("name*", toC(info.lValueType));
	EXPECT_EQ("name*", toC(info.rValueType));
	EXPECT_EQ("name*", toC(info.externalType));  // should this be this way?
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

	EXPECT_PRED2(containsSubString, toC(info.caller), "static inline float name_call(name* closure, int32_t p1, bool p2) {\n    return closure->call(closure, p1, p2);\n}\n");

	EXPECT_PRED2(containsSubString, toC(info.constructor),
			"static inline name* name_ctr(name* target, float(* call)(name*,int32_t,bool)) {\n"
			"    *target = (name){call};\n"
			"    return target;\n"
			"}");

	EXPECT_TRUE(contains(info.caller->getDependencies(), info.definition));
	EXPECT_TRUE(contains(info.constructor->getDependencies(), info.definition));

	// check externalizing / internalizing
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));


	// -- test a plain function type --

	type = builder.functionType(toVector(typeA, typeB), typeC);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("((int<4>,bool)->real<4>)", toString(*type));
	EXPECT_TRUE(info.plain);
	EXPECT_EQ("float(*)(int32_t,bool)", toC(info.lValueType));
	EXPECT_EQ("float(*)(int32_t,bool)", toC(info.rValueType));
	EXPECT_EQ("float(*)(int32_t,bool)", toC(info.externalType));  // should this be this way?
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
	auto decl = cManager->create<c_ast::VarDecl>(cManager->create<c_ast::Variable>(
			info.lValueType, cManager->create("var")
	));
	EXPECT_EQ("float(* var)(int32_t,bool)", toC(decl));

	// test the same with a function not accepting any arguments
	type = builder.functionType(core::TypeList(), typeA);
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("(()->int<4>)", toString(*type));

	decl = cManager->create<c_ast::VarDecl>(cManager->create<c_ast::Variable>(
			info.lValueType, cManager->create("var")
	));
	EXPECT_EQ("int32_t(* var)()", toC(decl));

}

TEST(TypeManager, RecursiveTypes) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();

	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	auto lit = cManager->create("X");


	// -- build a recursive type --------

	core::TypeVariablePtr A = builder.typeVariable("A");

	vector<core::NamedTypePtr> entriesA;
	entriesA.push_back(builder.namedType(builder.stringValue("value"), basic.getInt4()));
	entriesA.push_back(builder.namedType(builder.stringValue("next"), builder.refType(A)));
	core::StructTypePtr structA = builder.structType(entriesA);

	vector<core::RecTypeBindingPtr> defs;
	defs.push_back(builder.recTypeBinding(A, structA));
	core::RecTypeDefinitionPtr def = builder.recTypeDefinition(defs);

	core::RecTypePtr recTypeA = builder.recType(A, def);

	// do the checks

	TypeInfo infoA = typeManager.getTypeInfo(recTypeA);

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
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	auto lit = cManager->create("X");


	// -- build a recursive type --------

	core::TypeVariablePtr A = builder.typeVariable("A");
	core::TypeVariablePtr B = builder.typeVariable("B");

	vector<core::NamedTypePtr> entriesA;
	entriesA.push_back(builder.namedType(builder.stringValue("value"), basic.getInt4()));
	entriesA.push_back(builder.namedType(builder.stringValue("other"), builder.refType(B)));
	core::StructTypePtr structA = builder.structType(entriesA);

	vector<core::NamedTypePtr> entriesB;
	entriesB.push_back(builder.namedType(builder.stringValue("value"), basic.getBool()));
	entriesB.push_back(builder.namedType(builder.stringValue("other"), builder.refType(A)));
	core::StructTypePtr structB = builder.structType(entriesB);

	vector<core::RecTypeBindingPtr> defs;
	defs.push_back(builder.recTypeBinding(A, structA));
	defs.push_back(builder.recTypeBinding(B, structB));
	core::RecTypeDefinitionPtr def = builder.recTypeDefinition(defs);

	core::RecTypePtr recTypeA = builder.recType(A, def);
	core::RecTypePtr recTypeB = builder.recType(B, def);

	// do the checks

	TypeInfo infoA = typeManager.getTypeInfo(recTypeA);
	TypeInfo infoB = typeManager.getTypeInfo(recTypeB);

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
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	TypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");

	core::TypePtr type = builder.tupleType(core::TypeList());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("name", toC(info.lValueType));
	EXPECT_EQ("name", toC(info.rValueType));
	EXPECT_EQ("name", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);

	// members should not have an effect on the types
	type = builder.tupleType(toVector(basic.getInt4(), basic.getBool()));
	info = typeManager.getTypeInfo(type);
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
	TypeInfo infoBool = typeManager.getTypeInfo(basic.getBool());
	EXPECT_TRUE((bool)infoBool.definition);
	auto dependencies = info.definition->getDependencies();
	EXPECT_TRUE(contains(dependencies, infoBool.definition));
}


TEST(TypeManager, VolatileType) {

	core::NodeManager nodeManager;
	core::IRBuilder builder(nodeManager);
	const core::lang::BasicGenerator& basic = nodeManager.getLangBasic();


	Converter converter(nodeManager);
	converter.setNameManager(std::make_shared<TestNameManager>());
	TypeManager& typeManager = converter.getTypeManager();

	c_ast::SharedCodeFragmentManager fragmentManager = converter.getFragmentManager();
	c_ast::SharedCNodeManager cManager = fragmentManager->getNodeManager();

	TypeInfo info;
	auto lit = cManager->create<c_ast::Literal>("X");

	core::TypePtr type = builder.volatileType(basic.getInt4());
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("volatile int32_t", toC(info.lValueType));
	EXPECT_EQ("volatile int32_t", toC(info.rValueType));
	EXPECT_EQ("volatile int32_t", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);


	// try pointer (ref<ref<int<4>>)
	type = builder.volatileType(builder.refType(builder.refType(basic.getInt4())));
	info = typeManager.getTypeInfo(type);
	EXPECT_EQ("volatile int32_t*", toC(info.lValueType));
	EXPECT_EQ("volatile int32_t**", toC(info.rValueType));
	EXPECT_EQ("volatile int32_t**", toC(info.externalType));
	EXPECT_EQ("X", toC(info.externalize(cManager, lit)));
	EXPECT_EQ("X", toC(info.internalize(cManager, lit)));
	EXPECT_TRUE((bool)info.declaration);
	EXPECT_TRUE((bool)info.definition);


	// TODO: test for pointers, structs, ...

}

} // end namespace backend
} // end namespace insieme


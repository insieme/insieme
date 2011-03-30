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

#include "insieme/core/ast_builder.h"
#include "insieme/simple_backend/type_manager.h"

namespace insieme {
namespace simple_backend {

namespace {
	class SimpleNameManager : public NameManager {
	public:
		virtual string getName(const core::NodePtr& ptr, const string& fragment) {
			return "name";
		}
	};
}



TEST(TypeManager, Basic) {

	core::ASTBuilder builder;
	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;

	SimpleNameManager nameManager;
	TypeManager typeManager(nameManager);

	TypeManager::Entry entry;
	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");


	core::TypePtr type = basic.getInt4();
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("int", entry.lValueName);
	EXPECT_EQ("int", entry.rValueName);
	EXPECT_EQ("int %s", entry.declPattern);
	EXPECT_EQ("int %s", entry.paramPattern);

	type = basic.getInt8();
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("long", entry.lValueName);
	EXPECT_EQ("long", entry.rValueName);
	EXPECT_EQ("long %s", entry.declPattern);
	EXPECT_EQ("long %s", entry.paramPattern);

	type = basic.getFloat();
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("float", entry.lValueName);
	EXPECT_EQ("float", entry.rValueName);
	EXPECT_EQ("float %s", entry.declPattern);
	EXPECT_EQ("float %s", entry.paramPattern);

	type = basic.getDouble();
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("double", entry.lValueName);
	EXPECT_EQ("double", entry.rValueName);
	EXPECT_EQ("double %s", entry.declPattern);
	EXPECT_EQ("double %s", entry.paramPattern);
}

TEST(TypeManager, StructTypes) {

	core::ASTBuilder builder;
	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;

	SimpleNameManager nameManager;
	TypeManager typeManager(nameManager);

	TypeManager::Entry entry;
	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");

	core::TypePtr type = builder.structType(core::NamedCompositeType::Entries());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("struct name", entry.lValueName);
	EXPECT_EQ("struct name", entry.rValueName);
	EXPECT_EQ("struct name %s", entry.declPattern);
	EXPECT_EQ("struct name %s", entry.paramPattern);

	// members should not have an effect on the types
	type = builder.structType(toVector<core::NamedCompositeType::Entry>(std::make_pair(builder.identifier("a"), basic.getInt4())));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("struct name", entry.lValueName);
	EXPECT_EQ("struct name", entry.rValueName);
	EXPECT_EQ("struct name %s", entry.declPattern);
	EXPECT_EQ("struct name %s", entry.paramPattern);
}


TEST(TypeManager, RefTypes) {

	core::ASTBuilder builder;
	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;

	SimpleNameManager nameManager;
	TypeManager typeManager(nameManager);

	TypeManager::Entry entry;
	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");


	core::TypePtr type = builder.refType(basic.getInt4());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("int", entry.lValueName);
	EXPECT_EQ("int*", entry.rValueName);
	EXPECT_EQ("int %s", entry.declPattern);
	EXPECT_EQ("int* %s", entry.paramPattern);

	type = builder.refType(basic.getInt8());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("long", entry.lValueName);
	EXPECT_EQ("long*", entry.rValueName);
	EXPECT_EQ("long %s", entry.declPattern);
	EXPECT_EQ("long* %s", entry.paramPattern);

	type = builder.refType(basic.getFloat());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("float", entry.lValueName);
	EXPECT_EQ("float*", entry.rValueName);
	EXPECT_EQ("float %s", entry.declPattern);
	EXPECT_EQ("float* %s", entry.paramPattern);

	type = builder.refType(builder.structType(core::NamedCompositeType::Entries()));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("struct name", entry.lValueName);
	EXPECT_EQ("struct name*", entry.rValueName);
	EXPECT_EQ("struct name %s", entry.declPattern);
	EXPECT_EQ("struct name* %s", entry.paramPattern);

	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);
	core::ConcreteIntTypeParamPtr size2 = builder.concreteIntTypeParam(2);

	// ref/array combination
	type = builder.refType(builder.arrayType(basic.getInt4()));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("ref<array<int<4>,1>>", toString(*type));
	EXPECT_EQ("int*", entry.lValueName);
	EXPECT_EQ("int*", entry.rValueName);
	EXPECT_EQ("int* %s", entry.declPattern);
	EXPECT_EQ("int* %s", entry.paramPattern);

	// ref/array - multidimensional
	type = builder.refType(builder.arrayType(basic.getInt4(), size2));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("ref<array<int<4>,2>>", toString(*type));
	EXPECT_EQ("int**", entry.lValueName);
	EXPECT_EQ("int**", entry.rValueName);
	EXPECT_EQ("int** %s", entry.declPattern);
	EXPECT_EQ("int** %s", entry.paramPattern);

	// ref/vector combination
	type = builder.refType(builder.vectorType(basic.getInt4(), size));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("ref<vector<int<4>,4>>", toString(*type));
	EXPECT_EQ("int[4]", entry.lValueName);
	EXPECT_EQ("int(*)[4]", entry.rValueName);
	EXPECT_EQ("int %s[4]", entry.declPattern);
	EXPECT_EQ("int(* %s)[4]", entry.paramPattern);

	// ref/vector - multidimensional
	type = builder.refType(builder.vectorType(builder.vectorType(basic.getInt4(), size2), size));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("ref<vector<vector<int<4>,2>,4>>", toString(*type));
	EXPECT_EQ("int[4][2]", entry.lValueName);
	EXPECT_EQ("int(*)[4][2]", entry.rValueName);
	EXPECT_EQ("int %s[4][2]", entry.declPattern);
	EXPECT_EQ("int(* %s)[4][2]", entry.paramPattern);

}

TEST(TypeManager, ArrayTypes) {

	core::ASTBuilder builder;
	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;

	SimpleNameManager nameManager;
	TypeManager typeManager(nameManager);

	TypeManager::Entry entry;
	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");


	core::TypePtr type = builder.arrayType(basic.getInt4());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("int*", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("int* %s", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);

	type = builder.arrayType(basic.getInt8());
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("long*", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("long* %s", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);

	type = builder.arrayType(builder.structType(core::NamedCompositeType::Entries()));
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("struct name*", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("struct name* %s", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);
}

TEST(TypeManager, VectorTypes) {

	core::ASTBuilder builder;
	const core::lang::BasicGenerator& basic = builder.getNodeManager().basic;

	SimpleNameManager nameManager;
	TypeManager typeManager(nameManager);

	TypeManager::Entry entry;
	CodeFragmentPtr fragment = CodeFragment::createNew("TestFragment");

	core::ConcreteIntTypeParamPtr size = builder.concreteIntTypeParam(4);

	core::TypePtr type = builder.vectorType(basic.getInt4(), size);
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("int[4]", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("int %s[4]", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);

	type = builder.vectorType(basic.getInt8(), size);
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("long[4]", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("long %s[4]", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);

	type = builder.vectorType(builder.structType(core::NamedCompositeType::Entries()), size);
	entry = typeManager.getTypeEntry(fragment, type);
	EXPECT_EQ("struct name[4]", entry.lValueName);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.rValueName);
	EXPECT_EQ("struct name %s[4]", entry.declPattern);
	EXPECT_EQ(TypeManager::Entry::UNSUPPORTED, entry.paramPattern);
}

} // end namespace simple_backend
} // end namespace insieme


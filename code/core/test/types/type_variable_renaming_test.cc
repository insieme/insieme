/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/types/type_variable_renamer.h"

#include "insieme/utils/name_mangling.h"

namespace insieme {
namespace core {
namespace types {

	TEST(TypeVariableRenamer, Basic) {
		NodeManager manager;
		IRBuilder builder(manager);


		// create some types
		TypePtr varA = builder.typeVariable("a");
		TypePtr varB = builder.typeVariable("b");

		TypePtr genAB = builder.genericType("T", toVector(varA, varB));
		TypePtr genABA = builder.genericType("T", toVector(varA, varB, varA));

		TypePtr genABx = builder.genericType("T", toVector(varA, varB));
		TypePtr genABAxyx = builder.genericType("T", toVector(varA, varB, varA));

		// do some testing
		VariableRenamer renamer;

		// rename once => should produce new names
		auto sub = renamer.mapVariables(varA);
		EXPECT_EQ("{'a<->'insieme_renamed_fresh_type_var_1}", toString(sub));

		auto res = sub.applyForward(varA);
		EXPECT_EQ("'insieme_renamed_fresh_type_var_1", toString(*res));

		// rename another type with the same renamer => should produce a different name
		res = renamer.rename(varB);
		EXPECT_EQ("'insieme_renamed_fresh_type_var_2", toString(*res));

		// rename a more complex type
		renamer.reset();
		EXPECT_EQ("T<'insieme_renamed_fresh_type_var_1,'insieme_renamed_fresh_type_var_2>", toString(*renamer.rename(genAB)));

		// rename a type with multiple occurrences of the same variable
		renamer.reset();
		EXPECT_EQ("T<'insieme_renamed_fresh_type_var_1,'insieme_renamed_fresh_type_var_2,'insieme_renamed_fresh_type_var_1>", toString(*renamer.rename(genABA)));

		renamer.reset();
		EXPECT_EQ("T<'insieme_renamed_fresh_type_var_1,'insieme_renamed_fresh_type_var_2>", toString(*renamer.rename(genABx)));

		renamer.reset();
		EXPECT_EQ("T<'insieme_renamed_fresh_type_var_1,'insieme_renamed_fresh_type_var_2,'insieme_renamed_fresh_type_var_1>", toString(*renamer.rename(genABAxyx)));


		// test multiple types within a set
		auto list = toVector(varA, genABx, genABAxyx, varB);
		renamer.reset();
		sub = renamer.mapVariables(manager, list);
		vector<TypePtr> renamed;
		::transform(list, std::back_inserter(renamed), [&](const TypePtr& cur) -> TypePtr { return sub.applyForward(cur); });
		EXPECT_EQ("[AP('insieme_renamed_fresh_type_var_1),AP(T<'insieme_renamed_fresh_type_var_1,'insieme_renamed_fresh_type_var_2>),AP(T<'insieme_renamed_"
			      "fresh_type_var_1,'insieme_renamed_fresh_type_var_2,'insieme_renamed_fresh_type_var_1>),AP('insieme_renamed_fresh_type_var_2)]",
			      toString(renamed));
	}

	TEST(TypeVariableRenamer, VariableMapping) {
		NodeManager manager;
		IRBuilder builder(manager);

		// create some types
		TypeVariablePtr varA = builder.typeVariable("a");
		TypeVariablePtr varB = builder.typeVariable("b");

		TypeMapping mapping;
		mapping.addMapping(varA, varB);
		EXPECT_EQ(varB, mapping.applyForward(varA));
		EXPECT_EQ(varB, mapping.applyForward(varB));
		EXPECT_EQ(varA, mapping.applyBackward(varA));
		EXPECT_EQ(varA, mapping.applyBackward(varB));

		EXPECT_EQ(varB, mapping.applyForward(mapping.applyBackward(varB)));
		EXPECT_EQ(varA, mapping.applyBackward(mapping.applyForward(varA)));
	}

	TEST(TypeVariableRenamer, RecTypeRenaming) {
		NodeManager manager;
		IRBuilder builder(manager);

		TagTypeReferencePtr tag = builder.tagTypeReference("X");
		RecordPtr type = builder.structRecord(toVector(builder.field("f", builder.refType(tag))));

		TagTypeDefinitionPtr def = builder.tagTypeDefinition({ { tag, type } });
		TagTypePtr tagType = builder.tagType(tag, def);

		EXPECT_EQ("rec ^X.{^X=struct {f:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}",
		          toString(*tagType));

		VariableRenamer renamer;
		EXPECT_EQ("rec ^X.{^X=struct {f:ref<^X,f,f,plain>,ctor(),ctor(ref<^,t,f,cpp_ref>),ctor(ref<^,f,f,cpp_rref>),dtor()," + utils::getMangledOperatorAssignName() + "(ref<^,t,f,cpp_ref>)->ref<^,f,f,cpp_ref>," + utils::getMangledOperatorAssignName() + "(ref<^,f,f,cpp_rref>)->ref<^,f,f,cpp_ref>}}",
		          toString(*renamer.rename(tagType)));
	}

} // end namespace analysis
} // end namespace core
} // end namespace insieme

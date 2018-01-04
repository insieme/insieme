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
		EXPECT_EQ("{'a<->'_rv_001}", toString(sub));

		auto res = sub.applyForward(varA);
		EXPECT_EQ("'_rv_001", toString(*res));

		// rename another type with the same renamer => should produce a different name
		res = renamer.rename(varB);
		EXPECT_EQ("'_rv_002", toString(*res));

		// rename a more complex type
		renamer.reset();
		EXPECT_EQ("T<'_rv_001,'_rv_002>", toString(*renamer.rename(genAB)));

		// rename a type with multiple occurrences of the same variable
		renamer.reset();
		EXPECT_EQ("T<'_rv_001,'_rv_002,'_rv_001>", toString(*renamer.rename(genABA)));

		renamer.reset();
		EXPECT_EQ("T<'_rv_001,'_rv_002>", toString(*renamer.rename(genABx)));

		renamer.reset();
		EXPECT_EQ("T<'_rv_001,'_rv_002,'_rv_001>", toString(*renamer.rename(genABAxyx)));


		// test multiple types within a set
		auto list = toVector(varA, genABx, genABAxyx, varB);
		renamer.reset();
		sub = renamer.mapVariables(manager, list);
		vector<TypePtr> renamed;
		::transform(list, std::back_inserter(renamed), [&](const TypePtr& cur) -> TypePtr { return sub.applyForward(cur); });
		EXPECT_EQ("[AP('_rv_001),AP(T<'_rv_001,'_rv_002>),AP(T<'_rv_001,'_rv_002,'_rv_001>),AP('_rv_002)]",
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

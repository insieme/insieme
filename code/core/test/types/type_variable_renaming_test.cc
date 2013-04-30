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

#include <vector>

#include <gtest/gtest.h>

#include "insieme/core/ir_builder.h"
#include "insieme/core/types/type_variable_renamer.h"

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

	IntTypeParamPtr paramX = builder.variableIntTypeParam('x');
	IntTypeParamPtr paramY = builder.variableIntTypeParam('y');

	TypePtr genABx = builder.genericType("T", toVector(varA, varB), toVector(paramX));
	TypePtr genABAxyx = builder.genericType("T", toVector(varA, varB, varA), toVector(paramX, paramY, paramX));

	// do some testing
	VariableRenamer renamer;

	// rename once => should produce new names
	auto sub = renamer.mapVariables(varA);
	EXPECT_EQ("{'a<->'v1}", toString(sub));

	auto res = sub.applyForward(varA);
	EXPECT_EQ("'v1", toString(*res));

	// rename another type with the same renamer => should produce a different name
	res = renamer.rename(varB);
	EXPECT_EQ("'v2", toString(*res));

	// rename a more complex type
	renamer.reset();
	EXPECT_EQ("T<'v1,'v2>", toString(*renamer.rename(genAB)));

	// rename a type with multiple occurrences of the same variable
	renamer.reset();
	EXPECT_EQ("T<'v1,'v2,'v1>", toString(*renamer.rename(genABA)));

	renamer.reset();
	EXPECT_EQ("T<'v1,'v2,#A>", toString(*renamer.rename(genABx)));

	renamer.reset();
	EXPECT_EQ("T<'v1,'v2,'v1,#A,#B,#A>", toString(*renamer.rename(genABAxyx)));


	// test multiple types within a set
	auto list = toVector(varA, genABx, genABAxyx, varB);
	renamer.reset();
	sub = renamer.mapVariables(manager, list);
	vector<TypePtr> renamed;
	::transform(list, std::back_inserter(renamed), [&](const TypePtr& cur)->TypePtr {
		return sub.applyForward(cur);
	});
	EXPECT_EQ("[AP('v1),AP(T<'v1,'v2,#A>),AP(T<'v1,'v2,'v1,#A,#B,#A>),AP('v2)]", toString(renamed));
}

TEST(TypeVariableRenamer, VariableMapping) {


	NodeManager manager;
	IRBuilder builder(manager);

	// create some types
	TypeVariablePtr varA = builder.typeVariable("a");
	TypeVariablePtr varB = builder.typeVariable("b");

	VariableIntTypeParamPtr paramX = builder.variableIntTypeParam('x');
	VariableIntTypeParamPtr paramY = builder.variableIntTypeParam('y');


	TypeMapping mapping;
	mapping.addMapping(varA, varB);
	EXPECT_EQ(varB, mapping.applyForward(varA));
	EXPECT_EQ(varB, mapping.applyForward(varB));
	EXPECT_EQ(varA, mapping.applyBackward(varA));
	EXPECT_EQ(varA, mapping.applyBackward(varB));

	EXPECT_EQ(varB, mapping.applyForward(mapping.applyBackward(varB)));
	EXPECT_EQ(varA, mapping.applyBackward(mapping.applyForward(varA)));


	// test the same for type variables
	TypePtr typeX = builder.genericType("T", TypeList(), toVector<IntTypeParamPtr>(paramX));
	TypePtr typeY = builder.genericType("T", TypeList(), toVector<IntTypeParamPtr>(paramY));

	mapping.addMapping(paramX, paramY);
	EXPECT_EQ(typeY, mapping.applyForward(typeX));
	EXPECT_EQ(typeY, mapping.applyForward(typeY));
	EXPECT_EQ(typeX, mapping.applyBackward(typeX));
	EXPECT_EQ(typeX, mapping.applyBackward(typeY));

	EXPECT_EQ(typeY, mapping.applyForward(mapping.applyBackward(typeY)));
	EXPECT_EQ(typeX, mapping.applyBackward(mapping.applyForward(typeX)));

}

TEST(TypeVariableRenamer, RecTypeRenaming) {

	NodeManager manager;
	IRBuilder builder(manager);

	TypeVariablePtr var = builder.typeVariable("X");
	TypePtr type = builder.refType(var);

	vector<RecTypeBindingPtr> defs;
	defs.push_back(builder.recTypeBinding(var, type));

	RecTypeDefinitionPtr def = builder.recTypeDefinition(defs);
	RecTypePtr recType = builder.recType(var, def);

	EXPECT_EQ("rec 'X.{'X=ref<'X>}", toString(*recType));

	VariableRenamer renamer;
	EXPECT_EQ("rec 'X.{'X=ref<'X>}", toString(*renamer.rename(recType)));



}

} // end namespace analysis
} // end namespace core
} // end namespace insieme


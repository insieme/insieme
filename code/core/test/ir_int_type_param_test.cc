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

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/ir_int_type_param.h"

#include "ir_node_test.inc"

namespace insieme {
namespace core {
namespace new_core {


	TEST(IntTypeParam, Concrete) {

		NodeManager manager;
		ConcreteIntTypeParamPtr paramA = ConcreteIntTypeParam::get(manager, 12);
		ConcreteIntTypeParamPtr paramB = ConcreteIntTypeParam::get(manager, 14);

		// check name
		EXPECT_EQ ( "12", toString(*paramA) );
		EXPECT_EQ ( "12", toString(*paramA->getParam()) );
		EXPECT_EQ ( "12", toString(paramA->getParam()->getValue()) );

		EXPECT_EQ ( "14", toString(*paramB) );
		EXPECT_EQ ( "14", toString(*paramB->getParam()) );
		EXPECT_EQ ( "14", toString(paramB->getParam()->getValue()) );

		// perform basic type tests
		basicNodeTests(paramA, toList(paramA->getParam()));
		basicNodeTests(paramB, toList(paramB->getParam()));

	}

	TEST(IntTypeParam, Variable) {

		NodeManager manager;
		VariableIntTypeParamPtr paramA = VariableIntTypeParam::get(manager, 'a');
		VariableIntTypeParamPtr paramB = VariableIntTypeParam::get(manager, 'b');

		// check name
		EXPECT_EQ ( "#a", toString(*paramA) );
		EXPECT_EQ ( "a", toString(*paramA->getSymbol()) );
		EXPECT_EQ ( "a", toString(paramA->getSymbol()->getValue()) );

		EXPECT_EQ ( "#b", toString(*paramB) );
		EXPECT_EQ ( "b", toString(*paramB->getSymbol()) );
		EXPECT_EQ ( "b", toString(paramB->getSymbol()->getValue()) );

		// perform basic type tests
		basicNodeTests(paramA, toList(paramA->getSymbol()));
		basicNodeTests(paramB, toList(paramB->getSymbol()));

	}

	TEST(IntTypeParam, Infinite) {

		NodeManager manager;
		InfiniteIntTypeParamPtr paramA = InfiniteIntTypeParam::get(manager);

		// check name
		EXPECT_EQ ( "inf", toString(*paramA) );

		// perform basic type tests
		basicNodeTests(paramA, toList());

	}

	TEST(IntTypeParam, Lists) {

		NodeManager manager;
		IntTypeParamPtr paramA = ConcreteIntTypeParam::get(manager, 12);
		IntTypeParamPtr paramB = VariableIntTypeParam::get(manager, 'a');

		IntTypeParamsPtr empty = IntTypeParams::get(manager, toList());
		IntTypeParamsPtr list = IntTypeParams::get(manager, toList(paramA, paramB));


		EXPECT_EQ(static_cast<std::size_t>(0), empty->size());
		EXPECT_EQ(static_cast<std::size_t>(2), list->size());

		EXPECT_EQ(paramA, list->getElement(0));
		EXPECT_EQ(paramB, list->getElement(1));

		// check name
		EXPECT_EQ ( "[]", toString(*empty) );
		EXPECT_EQ ( "[12,#a]", toString(*list) );


		// test access using addresses
		IntTypeParamsAddress adr1(empty);
		IntTypeParamsAddress adr2(list);


		EXPECT_EQ(static_cast<std::size_t>(0), adr1->size());
		EXPECT_EQ(static_cast<std::size_t>(2), adr2->size());

		EXPECT_EQ(adr2.getAddressOfChild(0), adr2->getElement(0));
		EXPECT_EQ(adr2.getAddressOfChild(1), adr2->getElement(1));

		// perform basic type tests
		basicNodeTests(empty, toList());
		basicNodeTests(list, toList(paramA, paramB));

	}

} // end namespace new_core
} // end namespace core
} // end namespace insieme



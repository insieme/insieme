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

#include "container_utils.h"
#include "definitions.h"

using namespace insieme::core;

TEST(Definitions, Basic) {

	NodeManager nodeManager;

//	TypePtr typeA = GenericType::get(nodeManager, "A");
//	TypePtr typeB = GenericType::get(nodeManager, "B");
//	TypePtr typeR = GenericType::get(nodeManager, "R");

	Identifier ident = "funA";
//	Function::ParameterList list;
//	list.push_back(Function::Parameter("a", typeA));
//	list.push_back(Function::Parameter("b", typeB));
//	FunctionPtr funA = Function::get(manager, ident, list, typeR);
//
//	TupleType::ElementTypeList elements;
//	elements.push_back(typeA);
//	elements.push_back(typeB);
//	TupleTypePtr argumentA = TupleType::get(NodeManager, elements);
//	FunctionTypePtr funAType = FunctionType::get(NodeManager, argumentA, typeR);
//
//	EXPECT_EQ ( funAType, funA->getType() );
//
//	FunctionPtr funB = Function::get(manager, ident, list);
//	FunctionTypePtr funBType = FunctionType::get(NodeManager, argumentA, UnitType::get(NodeManager));
//
//	EXPECT_EQ ( funBType, funB->getType() );
//
//	FunctionPtr funC = Function::get(manager, ident, Function::ParameterList());
//	FunctionTypePtr funCType = FunctionType::get(NodeManager,
//			TupleType::get(NodeManager, TupleType::ElementTypeList()),
//			UnitType::get(NodeManager));
//
//	EXPECT_EQ ( funCType, funC->getType() );
//
//	EXPECT_EQ ( "fun funA : (()->unit)", toString(*funC) );
}



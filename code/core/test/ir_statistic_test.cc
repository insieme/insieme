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

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_statistic.h"

namespace insieme {
namespace core {

TEST(IRStatistic, Basic) {
	NodeManager manager;
	IRBuilder builder(manager);

	// test a diamond
	TypePtr typeD = builder.genericType("D");
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

	EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

	IRStatistic stat = IRStatistic::evaluate(typeA);

	EXPECT_EQ(static_cast<unsigned>(25), stat.getNumAddressableNodes());
	EXPECT_EQ(static_cast<unsigned>(13), stat.getNumSharedNodes());
	EXPECT_EQ(static_cast<unsigned>(6), stat.getHeight());
	EXPECT_EQ(25/static_cast<float>(13), stat.getShareRatio());

	EXPECT_EQ(static_cast<unsigned>(0), stat.getNodeTypeInfo(NT_ArrayType).numShared);
	EXPECT_EQ(static_cast<unsigned>(0), stat.getNodeTypeInfo(NT_ArrayType).numAddressable);

	EXPECT_EQ(static_cast<unsigned>(4), stat.getNodeTypeInfo(NT_GenericType).numShared);
	EXPECT_EQ(static_cast<unsigned>(5), stat.getNodeTypeInfo(NT_GenericType).numAddressable);

}


TEST(IRStatistic, Manager) {
	NodeManager manager;
	IRBuilder builder(manager);

	// test a diamond
	TypePtr typeD = builder.genericType("D");
	TypePtr typeB = builder.genericType("B",toVector<TypePtr>(typeD));
	TypePtr typeC = builder.genericType("C",toVector<TypePtr>(typeD));
	TypePtr typeA = builder.genericType("A", toVector(typeB, typeC));

	EXPECT_EQ("A<B<D>,C<D>>", toString(*typeA));

	NodeStatistic stat = NodeStatistic::evaluate(manager);

	EXPECT_EQ(4u, stat.getNodeTypeInfo(NT_GenericType).num);
	EXPECT_EQ(4u, stat.getNodeTypeInfo(NT_StringValue).num);
	EXPECT_EQ(3u, stat.getNodeTypeInfo(NT_Types).num);
	EXPECT_EQ(1u, stat.getNodeTypeInfo(NT_IntTypeParams).num);

	EXPECT_NE(0u, stat.getNodeTypeInfo(NT_GenericType).memory);

	// check whether summes check out
	unsigned total = 0;
	unsigned totalMem = 0;
	for(int i=0; i<NUM_CONCRETE_NODE_TYPES; i++) {
		total += stat.getNodeTypeInfo((NodeType)i).num;
		totalMem += stat.getNodeTypeInfo((NodeType)i).memory;
	}

	EXPECT_EQ(total, stat.getNumNodes());
	EXPECT_EQ(totalMem, stat.getTotalMemory());

}

} // end namespace core
} // end namespace insieme


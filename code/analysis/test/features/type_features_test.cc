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

#include "insieme/analysis/features/type_features.h"

#include "insieme/core/ir_node.h"
#include "insieme/core/ir_builder.h"

namespace insieme {
namespace analysis {
namespace features {

	using namespace core;

	TEST(TypeFeature, SizeOfTest) {
		NodeManager mgr;
		IRBuilder builder(mgr);
		auto& basic = mgr.getLangBasic();

		// check some basic types
		EXPECT_EQ(1u, getSizeInBytes(basic.getChar()));
		EXPECT_EQ(1u, getSizeInBytes(basic.getBool()));

		EXPECT_EQ(1u, getSizeInBytes(basic.getInt1()));
		EXPECT_EQ(2u, getSizeInBytes(basic.getInt2()));
		EXPECT_EQ(4u, getSizeInBytes(basic.getInt4()));
		EXPECT_EQ(8u, getSizeInBytes(basic.getInt8()));

		EXPECT_EQ(1u, getSizeInBytes(basic.getUInt1()));
		EXPECT_EQ(2u, getSizeInBytes(basic.getUInt2()));
		EXPECT_EQ(4u, getSizeInBytes(basic.getUInt4()));
		EXPECT_EQ(8u, getSizeInBytes(basic.getUInt8()));

		EXPECT_EQ(4u, getSizeInBytes(basic.getFloat()));
		EXPECT_EQ(8u, getSizeInBytes(basic.getDouble()));

		// test tuples
		EXPECT_EQ(5u, getSizeInBytes(builder.tupleType(toVector(basic.getBool(), basic.getFloat()))));
		EXPECT_EQ(0u, getSizeInBytes(builder.tupleType(toVector<core::TypePtr>())));

		core::TypePtr tuple = builder.tupleType(toVector(basic.getUInt8(), basic.getFloat()));
		EXPECT_EQ(12u, getSizeInBytes(tuple));
		EXPECT_EQ(25u, getSizeInBytes(builder.tupleType(toVector(tuple, basic.getBool(), tuple))));

		// test unions
		EXPECT_EQ(4u, getSizeInBytes(builder.unionType(toVector(
				std::make_pair(builder.stringValue("a"), basic.getChar()),
				std::make_pair(builder.stringValue("b"), basic.getInt4()),
				std::make_pair(builder.stringValue("c"), basic.getBool())
		))));

		// test structs
		EXPECT_EQ(6u, getSizeInBytes(builder.structType(toVector(
				std::make_pair(builder.stringValue("a"), basic.getChar()),
				std::make_pair(builder.stringValue("b"), basic.getInt4()),
				std::make_pair(builder.stringValue("c"), basic.getBool())
		))));

		// test vectors
		EXPECT_EQ(12u * 4u, getSizeInBytes(builder.vectorType(tuple, builder.concreteIntTypeParam(4))));

		// test arrays
		EXPECT_EQ(12u * 100u, getEstimatedSizeInBytes(builder.arrayType(tuple)));
		EXPECT_EQ(12u * 100u * 100u, getEstimatedSizeInBytes(builder.arrayType(tuple, builder.concreteIntTypeParam(2))));

		// test references
		EXPECT_EQ(8u, getSizeInBytes(builder.refType(basic.getChar())));
		EXPECT_EQ(8u, getSizeInBytes(builder.refType(tuple)));
	}

} // end namespace features
} // end namespace analysis
} // end namespace insieme

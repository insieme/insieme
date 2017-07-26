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

#include <gtest/gtest.h>

#include <sstream>
#include "insieme/annotations/loop_annotations.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/dump/binary_dump.h"

namespace insieme {
namespace annotations {

	using namespace std;

	TEST(LoopAnnotation, BinaryDumpTest) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		NodePtr loop = builder.parseStmt("for(int<4> i = 0 .. 10 : 1) { }");

		EXPECT_TRUE(loop);


		// check some basic loop annotation values
		EXPECT_FALSE(LoopAnnotation::hasAttachedValue(loop));

		LoopAnnotation::attach(loop, 123);
		EXPECT_TRUE(LoopAnnotation::hasAttachedValue(loop));
		EXPECT_EQ(123u, LoopAnnotation::getValue(loop));


		// dump to binary ..
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);
		core::dump::binary::dumpIR(buffer, loop);

		// restore within different manager
		NodeManager managerB;
		NodePtr restored = core::dump::binary::loadIR(buffer, managerB);

		EXPECT_NE(loop, restored);
		EXPECT_EQ(*loop, *restored);

		ASSERT_TRUE(LoopAnnotation::hasAttachedValue(restored));
		EXPECT_EQ(123u, LoopAnnotation::getValue(restored));

		//
		//		// create conversion register
		//		AnnotationConverterRegister registry;
		//		registry.registerConverter<DummyAnnotationConverter, core::value_node_annotation<DummyAnnotation>::type>();
		//
		//		NodePtr code = builder.genericType("A");
		//		EXPECT_TRUE(code) << *code;
		//
		//		// add annotation
		//		code->attachValue(DummyAnnotation(12));
		//		EXPECT_TRUE(code->hasAttachedValue<DummyAnnotation>());
		//
		//		// create a in-memory stream
		//		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);
		//
		//		// dump IR using a binary format
		//		binary::dumpIR(buffer, code, registry);
		//
		//		// reload IR using a different node manager
		//		NodeManager managerB;
		//		NodePtr restored = binary::loadIR(buffer, managerB, registry);
		//
		//		EXPECT_NE(code, restored);
		//		EXPECT_EQ(*code, *restored);
		//
		//		// annotation should still be available
		//		EXPECT_TRUE(restored->hasAttachedValue<DummyAnnotation>());
		//
		//		buffer.seekg(0); // reset stream
		//
		//		NodeManager managerC;
		//		auto restored3 = binary::loadIR(buffer, managerC);
		//		EXPECT_NE(code, restored);
		//		EXPECT_EQ(*code, *restored3);
		//
		//		// annotation should not be available
		//		EXPECT_FALSE(restored3->hasAttachedValue<DummyAnnotation>());
	}


} // end namespace annotations
} // end namespace insieme

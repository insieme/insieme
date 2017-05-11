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
 *
 */
#include <gtest/gtest.h>

#include "insieme/core/dump/binary_dump.h"

#include <sstream>
#include "insieme/core/ir_builder.h"
#include "insieme/core/encoder/encoder.h"

using std::shared_ptr;

namespace insieme {
namespace core {
namespace dump {

	using namespace std;


	TEST(BinaryDump, StoreLoad) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1u>>"));

		NodePtr code = builder.parseStmt("{ "
		                                 "	for(uint<4> i = 10u .. 50u) { "
		                                 "		v[i]; "
		                                 "	} "
		                                 "	for(uint<4> j = 5u .. 25u) { "
		                                 "		v[j]; "
		                                 "	} "
		                                 "}",
		                                 symbols);

		EXPECT_TRUE(code) << *code;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		binary::dumpIR(buffer, code);

		// reload IR using a different node manager
		NodeManager managerB;
		NodePtr restored = binary::loadIR(buffer, managerB);

		EXPECT_NE(code, restored);
		EXPECT_EQ(*code, *restored);

		buffer.seekg(0); // reset stream

		NodePtr restored2 = binary::loadIR(buffer, managerA);
		EXPECT_EQ(code, restored2);
	}

	TEST(BinaryDump, StoreLoadAddress) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1u>>"));
		NodePtr code = builder.parseStmt("{ "
		                                 "	for(uint<4> i = 10u .. 50u) { "
		                                 "		v[i]; "
		                                 "	} "
		                                 "	for(uint<4> j = 5u .. 25u) { "
		                                 "		v[j]; "
		                                 "	} "
		                                 "}",
		                                 symbols);

		EXPECT_TRUE(code) << *code;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		NodeAddress adr(code);
		adr = adr.getAddressOfChild(1, 3);

		// dump IR using a binary format
		binary::dumpAddress(buffer, adr);

		// reload IR using a different node manager
		NodeManager managerB;
		NodeAddress restored = binary::loadAddress(buffer, managerB);

		EXPECT_EQ(adr, restored);
		EXPECT_NE(adr.getAddressedNode(), restored.getAddressedNode());
		EXPECT_EQ(*adr, *restored);
		EXPECT_EQ(*adr.getRootNode(), *restored.getRootNode());

		buffer.seekg(0); // reset stream

		NodePtr restored2 = binary::loadAddress(buffer, managerA);
		EXPECT_EQ(adr, restored2);
	}


	TEST(BinaryDump, StoreLoadAddressList) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		std::map<std::string, NodePtr> symbols;
		symbols["v"] = builder.variable(builder.parseType("ref<array<int<4>,1u>>"));
		NodePtr code = builder.parseStmt("{ "
		                                 "	for(uint<4> i = 10u .. 50u) { "
		                                 "		v[i]; "
		                                 "	} "
		                                 "	for(uint<4> j = 5u .. 25u) { "
		                                 "		v[j]; "
		                                 "	} "
		                                 "}",
		                                 symbols);

		EXPECT_TRUE(code) << *code;

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		NodeAddress adr(code);
		auto list = toVector(adr.getAddressOfChild(1, 3), adr.getAddressOfChild(1, 2));

		// dump IR using a binary format
		binary::dumpAddresses(buffer, list);

		// reload IR using a different node manager
		NodeManager managerB;
		auto restored = binary::loadAddresses(buffer, managerB);

		EXPECT_EQ(list, restored);

		buffer.seekg(0); // reset stream

		auto restored2 = binary::loadAddress(buffer, managerA);
		EXPECT_EQ(list[0], restored2);

		buffer.seekg(0); // reset stream

		auto restored3 = binary::loadIR(buffer, managerA);
		EXPECT_EQ(list[0].getRootNode(), restored3);
	}


	// ------------ Test Annotations ----------------

	struct DummyAnnotation {
		int x;
		DummyAnnotation(int x) : x(x) {}
		bool operator==(const DummyAnnotation& other) const {
			return x == other.x;
		};
	};

	struct DummyAnnotationConverter : public AnnotationConverter {
		typedef core::value_node_annotation<DummyAnnotation>::type annotation_type;

		DummyAnnotationConverter() : AnnotationConverter("DummyAnnotationConverter") {}

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only dummy annotations supported!");
			int x = static_pointer_cast<annotation_type>(annotation)->getValue().x;
			return encoder::toIR(manager, x);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert(encoder::isEncodingOf<int>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");
			return std::make_shared<annotation_type>(DummyAnnotation(encoder::toValue<int>(node)));
		}
	};

	TEST(BinaryDump, StoreLoadAnnotations) {
		// create a code fragment using manager A
		NodeManager managerA;
		IRBuilder builder(managerA);

		// create conversion register
		AnnotationConverterRegister registry;
		registry.registerConverter<DummyAnnotationConverter, core::value_node_annotation<DummyAnnotation>::type>();

		NodePtr code = builder.genericType("A");
		EXPECT_TRUE(code) << *code;

		// add annotation
		code->attachValue(DummyAnnotation(12));
		EXPECT_TRUE(code->hasAttachedValue<DummyAnnotation>());

		// create a in-memory stream
		stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

		// dump IR using a binary format
		binary::dumpIR(buffer, code, registry);

		// reload IR using a different node manager
		NodeManager managerB;
		NodePtr restored = binary::loadIR(buffer, managerB, registry);

		EXPECT_NE(code, restored);
		EXPECT_EQ(*code, *restored);

		// annotation should still be available
		EXPECT_TRUE(restored->hasAttachedValue<DummyAnnotation>());

		buffer.seekg(0); // reset stream

		NodeManager managerC;
		auto restored3 = binary::loadIR(buffer, managerC);
		EXPECT_NE(code, restored);
		EXPECT_EQ(*code, *restored3);

		// annotation should not be available
		EXPECT_FALSE(restored3->hasAttachedValue<DummyAnnotation>());
	}


	// -- create another dummy annotation --

	struct DummyAnnotation2 {
		int x;
		DummyAnnotation2(int x) : x(x) {}
		bool operator==(const DummyAnnotation2& other) const {
			return x == other.x;
		};
	};

	// create a converter which is automatically registered
	VALUE_ANNOTATION_CONVERTER(DummyAnnotation2)

		typedef core::value_node_annotation<DummyAnnotation2>::type annotation_type;

		virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
			assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only dummy annotations supported!");
			int x = static_pointer_cast<annotation_type>(annotation)->getValue().x;
			return encoder::toIR(manager, x);
		}

		virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
			assert(encoder::isEncodingOf<int>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");
			return std::make_shared<annotation_type>(DummyAnnotation2(encoder::toValue<int>(node)));
		}

	VALUE_ANNOTATION_CONVERTER_END


TEST(BinaryDump, StoreLoadAnnotations2) {
	// create a code fragment using manager A
	NodeManager managerA;
	IRBuilder builder(managerA);

	NodePtr code = builder.genericType("A");
	EXPECT_TRUE(code) << *code;

	// add annotation
	code->attachValue(DummyAnnotation2(12));
	EXPECT_TRUE(code->hasAttachedValue<DummyAnnotation2>());

	// create a in-memory stream
	stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

	// dump IR using a binary format
	binary::dumpIR(buffer, code);

	// reload IR using a different node manager
	NodeManager managerB;
	NodePtr restored = binary::loadIR(buffer, managerB);

	EXPECT_NE(code, restored);
	EXPECT_EQ(*code, *restored);

	// annotation should still be available
	EXPECT_TRUE(restored->hasAttachedValue<DummyAnnotation2>());

	// check properly restored value
	EXPECT_EQ(12, restored->getAttachedValue<DummyAnnotation2>().x);
}

// --- test multiple annotations on the same node ---

struct DummyAnnotation3 {
	int x;
	DummyAnnotation3(int x) : x(x) {}
	bool operator==(const DummyAnnotation3& other) const {
		return x == other.x;
	};
};

// create a converter which is automatically registered
VALUE_ANNOTATION_CONVERTER(DummyAnnotation3)

	typedef core::value_node_annotation<DummyAnnotation3>::type annotation_type;

	virtual ExpressionPtr toIR(NodeManager& manager, const NodeAnnotationPtr& annotation) const {
		assert(dynamic_pointer_cast<annotation_type>(annotation) && "Only dummy annotations supported!");
		int x = static_pointer_cast<annotation_type>(annotation)->getValue().x;
		return encoder::toIR(manager, x);
	}

	virtual NodeAnnotationPtr toAnnotation(const ExpressionPtr& node) const {
		assert(encoder::isEncodingOf<int>(node.as<ExpressionPtr>()) && "Invalid encoding encountered!");
		return std::make_shared<annotation_type>(DummyAnnotation3(encoder::toValue<int>(node)));
}

VALUE_ANNOTATION_CONVERTER_END


TEST(BinaryDump, StoreLoadMultipleAnnotations) {
	// create a code fragment using manager A
	NodeManager managerA;
	IRBuilder builder(managerA);

	NodePtr code = builder.genericType("A");
	EXPECT_TRUE(code) << *code;

	// add annotation
	code->attachValue(DummyAnnotation2(12));
	code->attachValue(DummyAnnotation3(14));
	EXPECT_TRUE(code->hasAttachedValue<DummyAnnotation2>());
	EXPECT_TRUE(code->hasAttachedValue<DummyAnnotation3>());

	// create a in-memory stream
	stringstream buffer(ios_base::out | ios_base::in | ios_base::binary);

	// dump IR using a binary format
	binary::dumpIR(buffer, code);

	// reload IR using a different node manager
	NodeManager managerB;
	NodePtr restored = binary::loadIR(buffer, managerB);

	EXPECT_NE(code, restored);
	EXPECT_EQ(*code, *restored);

	// annotation should still be available
	EXPECT_TRUE(restored->hasAttachedValue<DummyAnnotation2>());
	EXPECT_TRUE(restored->hasAttachedValue<DummyAnnotation3>());

	// check properly restored values
	EXPECT_EQ(12, restored->getAttachedValue<DummyAnnotation2>().x);
	EXPECT_EQ(14, restored->getAttachedValue<DummyAnnotation3>().x);
}

} // end namespace dump
} // end namespace core
} // end namespace insieme

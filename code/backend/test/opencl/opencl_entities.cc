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

#include "insieme/backend/opencl/opencl_entities.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/lang/reference.h"

namespace insieme {
namespace backend {
namespace opencl {

    TEST(DataRange, Entities) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto size = builder.uintLit(10);
		auto start = builder.uintLit(3);
		auto end = builder.uintLit(7);

        auto expected = DataRange::get(manager, size, start, end);
		EXPECT_TRUE(*expected->getSize() == *size);
		EXPECT_TRUE(*expected->getStart() == *start);
		EXPECT_TRUE(*expected->getEnd() == *end);

		auto encoded = DataRange::encode(manager, expected);
		EXPECT_TRUE(core::encoder::isEncodingOf<DataRange>(encoded));

		auto actual = DataRange::decode(encoded);
		EXPECT_TRUE(*actual == *expected);
		EXPECT_TRUE(*actual->getSize() == *size);
		EXPECT_TRUE(*actual->getStart() == *start);
		EXPECT_TRUE(*actual->getEnd() == *end);
    }

	TEST(DataRequirement, Entities) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto accessMode = DataRequirement::AccessMode::RW;
		auto numRanges = builder.uintLit(1);
		// note: this is not the actual signature of a rangeExpr as the return type is different.
		//       however, for the sake of this test, it does not matter.
		auto rangeExpr = builder.parseExpr(R"(
			(wi: ref<irt_wi>, nd: ref<opencl_ndrange>, dim: uint<4>, arg: uint<4>) -> unit {
				return;
			})").as<core::LambdaExprPtr>();

		auto expected = std::make_shared<DataRequirement>();
		expected->setAccessMode(accessMode);
		expected->setNumRanges(numRanges);
		expected->setType(numRanges->getType());
		expected->setRangeExpr(rangeExpr);
		EXPECT_TRUE(expected->getAccessMode() == accessMode);
		EXPECT_TRUE(*expected->getNumRanges() == *numRanges);
		EXPECT_TRUE(*expected->getType() == *numRanges->getType());
		EXPECT_TRUE(*expected->getRangeExpr() == *rangeExpr);

		auto encoded = DataRequirement::encode(manager, expected);
		EXPECT_TRUE(core::encoder::isEncodingOf<DataRequirement>(encoded));

		auto actual = DataRequirement::decode(encoded);
		EXPECT_TRUE(*actual == *expected);
		EXPECT_TRUE(actual->getAccessMode() == accessMode);
		EXPECT_TRUE(*actual->getNumRanges() == *numRanges);
		EXPECT_TRUE(*actual->getType() == *numRanges->getType());
		EXPECT_TRUE(*actual->getRangeExpr() == *rangeExpr);
	}

	TEST(NDRange, Entities) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto workDim = builder.uintLit(2);

		core::ExpressionList globalOffsets;
		globalOffsets.push_back(builder.uintLit(4));
		globalOffsets.push_back(builder.uintLit(8));

		core::ExpressionList globalWorkSizes;
		globalWorkSizes.push_back(builder.uintLit(256));
		globalWorkSizes.push_back(builder.uintLit(512));

		core::ExpressionList localWorkSizes;
		localWorkSizes.push_back(builder.uintLit(32));
		localWorkSizes.push_back(builder.uintLit(64));

		auto expected = std::make_shared<NDRange>();
		expected->setWorkDim(workDim);
		expected->setGlobalOffsets(globalOffsets);
		expected->setGlobalWorkSize(globalWorkSizes);
		expected->setLocalWorkSize(localWorkSizes);
		EXPECT_TRUE(*expected->getWorkDim() == *workDim);
		EXPECT_TRUE(expected->getGlobalOffsets() == globalOffsets);
		EXPECT_TRUE(expected->getGlobalWorkSizes() == globalWorkSizes);
		EXPECT_TRUE(expected->getLocalWorkSizes() == localWorkSizes);

		auto encoded = NDRange::encode(manager, expected);
		EXPECT_TRUE(core::encoder::isEncodingOf<NDRange>(encoded));

		auto actual = NDRange::decode(encoded);
		EXPECT_TRUE(*actual == *expected);
		EXPECT_TRUE(*actual->getWorkDim() == *workDim);
		EXPECT_TRUE(actual->getGlobalOffsets() == globalOffsets);
		EXPECT_TRUE(actual->getGlobalWorkSizes() == globalWorkSizes);
		EXPECT_TRUE(actual->getLocalWorkSizes() == localWorkSizes);
	}

	TEST(Optional, Entities) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		auto value = builder.uintLit(7);
		auto size = builder.callExpr(manager.getLangBasic().getSizeof(), builder.getTypeLiteral(value->getType()));
		auto modifier = Optional::Modifier::HOST_PRIMITIVE;

		auto expected = std::make_shared<Optional>();
		expected->setModifier(modifier);
		expected->setValue(value);
		expected->setSize(size);
		EXPECT_TRUE(expected->getModifier() == modifier);
		EXPECT_TRUE(*expected->getValue() == *value);
		EXPECT_TRUE(*expected->getSize() == *size);

		auto encoded = Optional::encode(manager, expected);
		EXPECT_TRUE(core::encoder::isEncodingOf<Optional>(encoded));

		auto actual = Optional::decode(encoded);
		EXPECT_TRUE(*actual == *expected);
		EXPECT_TRUE(actual->getModifier() == modifier);
		EXPECT_TRUE(*actual->getValue() == *value);
		EXPECT_TRUE(*actual->getSize() == *size);
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme

